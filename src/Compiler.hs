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
moduleName = id

runCompiler :: CompilerMonad m => FileResolver m -> Filename -> m ObjectMap
runCompiler fs fn = fmap objects $ flip S.execStateT s $ flip R.runReaderT deps $ compile fn
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
  check objs f
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


check :: Monad m => ObjectMap -> Function -> m ()
check objs (Function _ p _ a i o b) = do
  let argNames = snd <$> a
      dupNames = argNames \\ nub argNames
  when (p == or [True | RawBrainfuck _ <- b]) $ if p
    then fail "error pure function contains impure code"
    else fail "warning impure function is pure"
  unless (null dupNames) $ fail "duplicate argument name"
  forM_ a $ \(_, argName) ->
    when (argName `M.member` objs) $ fail "warning arg shadows other binding"
  result <- foldM checkInstruction (reverse i) b
  when (p && result /= reverse o) $ fail "type error: wrong declared function type"
  where checkInstruction stack (RawBrainfuck _  ) = return stack
        checkInstruction stack (FunctionCall n v) = do
          f <- case objs M.!? n of
            Just (FunctionObject g) -> return g
            Just (ValueObject    _) -> fail "not callable"
            Nothing                 -> fail "function not found"
          when (length (funcArgs f) /= length v) $ fail "wrong number of arguments"
          forM_ (zip (funcArgs f) v) $ \((kind, _), expr) -> do
            thisKind <- case expr of
                          ConstantName cn -> parameterType kind cn
                          _               -> typeof <$> eval objs kind expr
            when (thisKind /= kind) $ fail "wrong type arg"
          case stripPrefix (funcInput f) stack of
            Nothing -> fail "function call stack type error"
            Just ns -> return $ reverse (funcInput f) ++ ns
        parameterType k name =
          case find (\(_, n) -> n == name) a of
            Just (kind, _) -> return kind
            Nothing        -> case objs M.!? name of
              Just (FunctionObject _) -> fail "error functions aren't first class yet"
              Just (ValueObject    v) -> return $  if typeof v `canCastTo` k then k else typeof v
              Nothing                 -> fail "name error"


-- helpers

addModule :: String -> ObjectMap -> CompilerState -> CompilerState
addModule name object state = state { moduleCache = M.insert name object $ moduleCache state }

addObject :: String -> Object -> CompilerState -> CompilerState
addObject name object state = state { objects = M.insert name object $ objects state }

--           return $ M.insert (constName c) (OValue (constType c, value)) context
--         step context (FunctionDecl f) = do
--           when (funcName f `M.member` context) $ fail "function already exists"
--           return $ M.insert (funcName f) (OFunction f) context
--
-- eval :: Monad m => Context -> Type -> Expression -> m Value
-- eval context kind (ConstantName n) = do
--   when (n `M.notMember` context) $ fail "name error"
--   case context M.! n of
--     OFunction _           -> fail "name error function not value"
--     OValue (otherKind, x) -> if otherKind `canCastTo` kind
--                              then return (kind, x)
--                              else fail "type error"
-- eval _ kind (LiteralString s) = do
--   when (kind /= BFString) $ fail "type error expecting non-string"
--   return (kind, 0) -- FIXME
-- eval _ kind (LiteralChar c) = do
--   when (kind == BFString) $ fail "type error expecting string"
--   return (kind, ord c)
-- eval _ kind (LiteralInt  i) = do
--   when (kind == BFString) $ fail "type error expecting string"
--   when (kind == BFChar && (i < 0 || i > 256)) $ fail "type error does not fit in one byte"
--   return (kind, i)
--
--
-- canCastTo :: Type -> Type -> Bool
-- BFChar   `canCastTo` BFChar   = True
-- BFChar   `canCastTo` BFInt    = True
-- BFInt    `canCastTo` BFInt    = True
-- BFString `canCastTo` BFString = True
-- canCastTo _ _ = False
