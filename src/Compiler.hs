{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler where



-- imports

import           Control.Monad
import           Control.Monad.Base
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State  as S
import           Data.Char
import           Data.List            as L
import qualified Data.Map             as M
import           System.FilePath

import           Grammar
import           Object
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

type CompilerT     m = R.ReaderT (Dependencies m) (S.StateT CompilerState m)
type CompilerMonad m = (Monad m, MonadBase m m)


moduleName :: Filename -> String
moduleName = takeBaseName

runCompiler :: CompilerMonad m => FileResolver m -> Filename -> m ObjectMap
runCompiler fs fn = fmap objects $ flip S.execStateT s $ flip R.runReaderT deps $ do
  compile "Prelude"
  compile fn
  where deps = Dependencies fs
        s    = CompilerState M.empty M.empty

compile :: CompilerMonad m => Filename -> CompilerT m ()
compile filename = do
  resolver <- R.asks fileResolver
  content  <- liftBase $ resolver filename
  state    <- S.get
  let mName   = moduleName filename
      program = parseProgram mName content
  sequence_ $ step <$> program
  S.modify $ addModule mName $ objects state

step :: CompilerMonad m => Statement -> CompilerT m ()
step (Comment _) = return ()
step (Include filename) = do
  cache <- S.gets moduleCache
  when (moduleName filename `M.notMember` cache) $ compile filename
step (ConstantDecl c) = do
  objs <- S.gets objects
  let cName = constName c
  when (cName `M.member` objs) $ fail "constant already exists"
  value <- eval objs (constType c) $ constExpr c
  S.modify $ addObject cName $ ValueObject value
step (FunctionDecl f) = do
  objs <- S.gets objects
  let fName = funcName f
  when (fName `M.member` objs) $ fail "function already exists"
  checkFunction objs f
  S.modify $ addObject fName $ FunctionObject f



eval :: Monad m => ObjectMap -> Type -> Expression -> m Value
eval _ kind (LiteralString s) = case kind of
  BFString -> return $ VString s
  _        -> fail "type error expecting non-string"
eval _ kind (LiteralChar c) = case kind of
  BFChar   -> return $ VChar c
  BFBool   -> return $ VBool $ ord c /= 0
  _        -> fail "type error expecting not char"
eval _ kind (LiteralInt  i) = case kind of
  BFString -> fail "type error expecting string"
  BFChar   -> if i < 0 || i > 256
              then fail "type error does not fit in one byte"
              else return $ VChar $ chr i
  BFBool   -> return $ VBool $ i /= 0
  BFInt    -> return $ VInt i

eval objs kind (ConstantName n) = do
  when (n `M.notMember` objs) $ fail "name error"
  case objs M.! n of
    FunctionObject _ -> fail "name error function not value"
    ValueObject val  -> cast val kind

cast :: Monad m => Value -> Type -> m Value
cast (VChar c) BFInt  = return $ VInt  $ ord c
cast value kind
  | typeof value == kind = return value
  | otherwise            = fail "cast type error"

canCastTo :: Type -> Type -> Bool
BFChar `canCastTo` BFInt = True
a      `canCastTo` b     = a == b


checkFunction :: Monad m => ObjectMap -> Function -> m ()
checkFunction objs f = do
  let argNames = snd <$> funcArgs f
      dupNames = argNames \\ nub argNames
  when (isPure == anyImpure (funcBody f)) $ if isPure
    then fail "error pure function contains impure code"
    else fail "warning impure function is pure"
  unless (null dupNames) $ fail "duplicate argument name"
  forM_ (funcArgs f) $ \(_, argName) ->
    when (argName `M.member` objs) $ fail "warning arg shadows other binding"
  result <- checkInstructions objs f (funcPure f) (reverse $ funcInput f) $ funcBody f
  when (isPure && result /= reverse (funcOutput f)) $ fail "type error: wrong declared function type"
  where isPure = funcPure f

checkInstructions :: Monad m => ObjectMap -> Function -> Bool -> [Type] -> [Instruction] -> m [Type]
checkInstructions objs f typeCheck = foldM $ checkInstruction objs f typeCheck

checkInstruction :: Monad m => ObjectMap -> Function -> Bool -> [Type] -> Instruction -> m [Type]
checkInstruction _    _ _         stack (RawBrainfuck _) = return stack
checkInstruction objs f typeCheck stack (FunctionCall n v) = do
  g <- getFunction objs n
  when (length (funcArgs g) /= length v) $ fail "wrong number of arguments"
  forM_ (zip (funcArgs g) v) $ \((kind, _), expr) -> do
    thisKind <- case expr of
      ConstantName cn -> parameterType kind cn
      _               -> typeof <$> eval objs kind expr
    when (thisKind /= kind) $ fail "wrong type arg"
  if typeCheck
    then case stripPrefix (reverse $ funcInput f) stack of
           Nothing -> fail "function call stack type error"
           Just ns -> return $ reverse (funcOutput f) ++ ns
    else return stack
  where parameterType k name =
          case find (\(_, arg) -> arg == name) $ funcArgs f of
            Just (kind, _) -> return kind
            Nothing        -> case objs M.!? name of
              Just (FunctionObject _) -> fail "error functions aren't first class yet"
              Just (ValueObject   vo) -> return $ if typeof vo `canCastTo` k then k else typeof vo
              Nothing                 -> fail "name error"
checkInstruction objs f typeCheck stack (Loop lb) = do
  newStack <- checkInstructions objs f typeCheck stack lb
  when (typeCheck && newStack /= stack) $ fail "error loop not stack neutral"
  return newStack
checkInstruction objs f typeCheck stack (If ic ib) = do
  (cStackIn, cStackOut) <- foldM (guessStack objs) ([], []) ic
  when (cStackOut /= BFBool : cStackIn) $ fail $ "wrong type in if condition, got " ++ show (reverse cStackIn) ++ " -> " ++ show (reverse cStackOut)
  newStack <- checkInstructions objs f typeCheck stack ib
  when (typeCheck && newStack /= stack) $ fail "error while not stack neutral"
  return newStack
checkInstruction objs f typeCheck stack (While wc wb) = do
  (cStackIn, cStackOut) <- foldM (guessStack objs) ([], []) wc
  when (cStackOut /= BFBool : cStackIn) $ fail $ "wrong type in while condition, got " ++ show (reverse cStackIn) ++ " -> " ++ show (reverse cStackOut)
  newStack <- checkInstructions objs f typeCheck stack wb
  when (typeCheck && newStack /= stack) $ fail "error while not stack neutral"
  return newStack

guessStack :: Monad m => ObjectMap -> ([Type], [Type]) -> Instruction -> m ([Type], [Type])
guessStack objs (initStack, currentStack) i@(FunctionCall n _) = do
  g <- getFunction objs n
  let newInitStack = initStack ++ drop (length currentStack) (reverse $ funcInput g)
  newCurrentStack <- checkInstruction objs g True newInitStack i
  return (newInitStack, newCurrentStack)
guessStack _ _ _ = fail "while or if condition must only be pure function calls"

getFunction :: Monad m => ObjectMap -> Name -> m Function
getFunction objs n =
  case objs M.!? n of
    Just (FunctionObject g) -> return g
    Just (ValueObject    _) -> fail "not callable"
    Nothing                 -> fail "function not found"



-- helpers

addModule :: String -> ObjectMap -> CompilerState -> CompilerState
addModule name object state = state { moduleCache = M.insert name object $ moduleCache state }

addObject :: String -> Object -> CompilerState -> CompilerState
addObject name object state = state { objects = M.insert name object $ objects state }
