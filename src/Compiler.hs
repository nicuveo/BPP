{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler where



-- imports

import           Control.Monad
import           Control.Monad.Base
import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R
import qualified Control.Monad.RWS    as A
import qualified Control.Monad.State  as S
import qualified Control.Monad.Writer as W
import           Data.Char
import           Data.List            as L
import qualified Data.Map             as M
import           System.FilePath

import           BuiltIn
import           Diagnostics
import           Grammar
import           Object
import           Module
import           Parser
import           Types



-- compiler

type FileResolver m = Filename -> m String

newtype Dependencies m =
  Dependencies { fileResolver :: FileResolver m
               }

data CompilerState =
  CompilerState { moduleCache :: M.Map Filename ObjectMap
                , objects     :: ObjectMap
                }

type CompilerT     m = E.ExceptT () (A.RWST (Dependencies m) Diagnostics CompilerState m)
type CompilerMonad m = (Monad m, MonadBase m m)


moduleName :: Filename -> String
moduleName = takeBaseName

runCompiler :: (Monad m, MonadBase m m) => FileResolver m -> Filename -> m (Diagnostics, Maybe ObjectMap)
runCompiler fs fn = do
  (result, s, d) <- A.runRWST (E.runExceptT expr) deps st
  case result of
    Right _ -> return (d, Just $ objects s)
    Left  _ -> return (d, Nothing)
  where deps = Dependencies fs
        st   = CompilerState M.empty builtinFunctions
        expr = compile "Prelude" >> compile fn

compile :: CompilerMonad m => Filename -> CompilerT m ()
compile filename = do
  (_, diagnostics) <- W.listen $ do
    resolver <- R.asks fileResolver
    content  <- liftBase $ resolver filename
    state    <- S.get
    let mName   = moduleName filename
    case parseProgram mName content of
      Left diagnostic -> W.tell [diagnostic]
      Right program   -> do
        sequence_ $ step <$> program
        S.modify $ addModule mName $ objects state
  when (any isError diagnostics) $ E.throwError ()

step :: CompilerMonad m => WithLocation Statement -> CompilerT m ()
step (WL _ (Comment _)) = return ()
step (WL _ (Include filename)) = do
  cache <- S.gets moduleCache
  -- TODO: handle file not found
  when (moduleName filename `M.notMember` cache) $ compile filename
step wp@(WL _ (ConstantDecl n)) = do
  objs <- S.gets objects
  let cName = constName n
  if cName `M.member` objs
  then report wp $ ConstantAlreadyDefinedError cName $ objs M.! cName
  else case eval objs (constType n) $ constExpr n of
    Left  err   -> report wp err
    Right value -> S.modify $ addObject cName $ ValueObject value <$ wp
step wp@(WL _ (FunctionDecl f)) = do
  objs <- S.gets objects
  let fName = funcName f
  if fName `M.member` objs
  then report wp $ FunctionAlreadyDefinedError fName $ objs M.! fName
  else E.catchError (doTheThing fName objs) $ const $ return ()
  where doTheThing fName objs = do
          checkFunction objs $ f <$ wp
          S.modify $ addObject fName $ FunctionObject f <$ wp



eval :: ObjectMap -> Type -> Expression -> Either Error Value
eval _ kind (LiteralString s) = case kind of
  BFString -> Right $ VString s
  _        -> Left  $ StringLiteralError kind s
eval _ kind (LiteralChar c) = case kind of
  BFChar   -> Right $ VChar c
  BFBool   -> Right $ VBool $ ord c /= 0
  _        -> Left  $ CharLiteralError kind c
eval _ kind (LiteralInt  i) = case kind of
  BFString -> Left  $ IntLiteralError kind i
  BFChar   -> if i < 0 || i > 256
              then Left  $ IntLiteralError kind i
              else Right $ VChar $ chr i
  BFBool   -> Right $ VBool $ i /= 0
  BFInt    -> Right $ VInt i
eval objs kind (ConstantName n) =
  if n `M.notMember` objs
  then Left $ ConstantNotFoundError n
  else case objs M.! n of
         WL _ (ValueObject val) -> cast val kind
         wp                     -> Left $ ExpectedValueGotFunctionError n wp

cast :: Value -> Type -> Either Error Value
cast (VChar c) BFInt = Right $ VInt  $ ord c
cast value kind
  | typeof value == kind = Right value
  | otherwise            = Left $ ImplicitCastError kind $ typeof value

canCastTo :: Type -> Type -> Bool
BFChar `canCastTo` BFInt = True
a      `canCastTo` b     = a == b


checkFunction :: CompilerMonad m => ObjectMap -> WithLocation Function -> CompilerT m ()
checkFunction objs wp@(WL _ f) = do
  let argNames = snd <$> funcArgs f
      dupNames = argNames \\ nub argNames
      body     = funcBody f $ error "ICE: tried to compile a built-in function"
  if isPure && anyImpure (getEntry <$> body)
  then report wp $ PureFunctionsContainsImpureCodeError $ funcName f
  else do
    if not $ null dupNames
    then report wp $ DuplicateArgumentNamesError dupNames
    else forM_ (funcArgs f) $ \(_, argName) ->
       when (argName `M.member` objs) $ report wp $ ArgumentNameShadowsObjectWarning argName $ objs M.! argName
    result <- checkInstructions objs f (funcPure f) (reverse $ funcInput f) body
    when (isPure && result /= reverse (funcOutput f)) $ report wp $ FunctionTypeDeclarationError f $ reverse result
  where isPure = funcPure f

checkInstructions :: CompilerMonad m => ObjectMap -> Function -> Bool -> [Type] -> [WithLocation Instruction] -> CompilerT m [Type]
checkInstructions objs f typeCheck = foldM $ checkInstruction objs f typeCheck

checkInstruction :: CompilerMonad m => ObjectMap -> Function -> Bool -> [Type] -> WithLocation Instruction -> CompilerT m [Type]
checkInstruction _    _ _         stack (WL _ (RawBrainfuck _)) = return stack
checkInstruction objs f typeCheck stack wp@(WL _ (FunctionCall n v)) =
  case getFunction objs n of
    Left err -> reportAndStop wp err
    Right  g ->
      if length (funcArgs g) /= length v
      then report wp (FunctionCallWrongArgumentsNumberError g $ length v) >> return stack
      else do
        forM_ (zip (funcArgs g) v) $ \(arg@(kind, _), expr) -> do
          let thisKind = case expr of
                ConstantName cn -> parameterType kind cn
                _               -> typeof <$> eval objs kind expr
          case thisKind of
            Left err -> report wp err
            Right k  -> when (k /= kind) $ report wp $ FunctionCallWrongArgumentTypeError g arg k
        if typeCheck
        then case stripPrefix (reverse $ funcInput g) stack of
               Nothing -> reportAndStop wp $ FunctionCallStackTypeError g $ reverse stack
               Just ns -> return $ reverse (funcOutput g) ++ ns
        else return stack
  where parameterType k name =
          case find (\(_, arg) -> arg == name) $ funcArgs f of
            Just (kind, _) -> Right $ if kind `canCastTo` k then k else kind
            Nothing        -> case objs M.!? name of
              Just (WL _ (ValueObject   vo)) -> Right $ if typeof vo `canCastTo` k then k else typeof vo
              Just other                     -> Left  $ ExpectedValueGotFunctionError name other
              Nothing                        -> Left  $ ConstantNotFoundError name
checkInstruction objs f typeCheck stack wp@(WL _ (Loop lb)) = do
  newStack <- checkInstructions objs f typeCheck stack lb
  when (typeCheck && newStack /= stack) $ reportAndStop wp $ BlockLoopNotStackNeutralError (stack, newStack)
  return newStack
checkInstruction objs f typeCheck stack wp@(WL _ (If ic ib)) = do
  cs@(cStackIn, cStackOut) <- foldM (guessStack objs f) ([], []) ic
  when (cStackOut /= BFBool : cStackIn) $ report wp $ ConditionWrongTypeError cs
  newStack <- checkInstructions objs f typeCheck stack ib
  when (typeCheck && newStack /= stack) $ reportAndStop wp $ BlockIfNotStackNeutralError (stack, newStack)
  return newStack
checkInstruction objs f typeCheck stack wp@(WL _ (While wc wb)) = do
  cs@(cStackIn, cStackOut) <- foldM (guessStack objs f) ([], []) wc
  when (cStackOut /= BFBool : cStackIn) $ report wp $ ConditionWrongTypeError cs
  newStack <- checkInstructions objs f typeCheck stack wb
  when (typeCheck && newStack /= stack) $ reportAndStop wp $ BlockWhileNotStackNeutralError (stack, newStack)
  return newStack

guessStack :: CompilerMonad m => ObjectMap -> Function -> ([Type], [Type]) -> WithLocation Instruction -> CompilerT m ([Type], [Type])
guessStack objs f s@(initStack, currentStack) wp@(WL _ (FunctionCall n _)) =
  case getFunction objs n of
    Left err -> report wp err >> return s
    Right  g -> do
      let missing = drop (length currentStack) $ reverse $ funcInput g
          newInitStack = initStack ++ missing
      newCurrentStack <- checkInstruction objs f True (currentStack ++ missing) wp
      return (newInitStack, newCurrentStack)
guessStack _ _ s wp = report wp (ConditionWrongInstructionError $ getEntry wp) >> return s

getFunction :: ObjectMap -> Name -> Either Error Function
getFunction objs n =
  case objs M.!? n of
    Just (WL _ (FunctionObject g)) -> Right g
    Just wp                        -> Left $ ExpectedFunctionGotValueError n wp
    Nothing                        -> Left $ FunctionNotFoundError n



-- helpers

addModule :: String -> ObjectMap -> CompilerState -> CompilerState
addModule name object state = state { moduleCache = M.insert name object $ moduleCache state }

addObject :: String -> WithLocation Object -> CompilerState -> CompilerState
addObject name object state = state { objects = M.insert name object $ objects state }

report :: CompilerMonad m => WithLocation a -> Error -> CompilerT m ()
report wp e = W.tell [e <$ wp]

reportAndStop :: CompilerMonad m => WithLocation a -> Error -> CompilerT m b
reportAndStop = (>> E.throwError ()) ... report
