{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Flix.FlixMonad where

import           Control.Applicative            ( liftA2 )
import           Control.Lens
import           Control.Monad.ExceptT
import           Control.Monad.StateT
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Flix.Capabilities
import           Flix.FlixState

newtype FlixMonadT m a = FlixMonadT(StateT FlixState (ExceptT String m) a)
  deriving (Monad, Applicative, Functor)

type FlixMonad a = FlixMonadT Identity a

runFlixMonad :: FlixMonad a -> FlixState -> Either String (a, FlixState)
runFlixMonad m = runIdentity . runFlixMonadT m

runFlixMonadT :: FlixMonadT m a -> FlixState -> m (Either String (a, FlixState))
runFlixMonadT (FlixMonadT m) = runExceptT . runStateT m

liftEitherToFlixM :: (Monad m) => Either String a -> FlixMonadT m a
liftEitherToFlixM = FlixMonadT . liftStateM . liftExceptT

getState :: (Monad m) => FlixMonadT m FlixState
getState = FlixMonadT get

setState :: (Monad m) => FlixState -> FlixMonadT m ()
setState = FlixMonadT . put

updateState :: (Monad m) => (FlixState -> FlixState) -> FlixMonadT m ()
updateState f = getState >>= setState . f

-- FlixMonadT instances

instance (Monad m) => MonadFail (FlixMonadT m) where
  fail = FlixMonadT . liftStateM . liftExceptT . Left

instance (Monad m) => UniqueNameGeneration (FlixMonadT m) where
  generateUniqueName = do
    updateState $ over state_uniqueNameCounter (+ 1)
    format . view state_uniqueNameCounter <$> getState
    where format = ("_$" ++) . show

instance (Monad m, MonadFail m) => ConstructorRegistry (FlixMonadT m) where
  bindConstructor con ty = updateState insertCon
    where insertCon = over state_constructors (Map.insert con ty)

  lookupConstructor name = getState >>= lookup' . view state_constructors
    where
      lookup' = unpack . (Map.!? name)
      unpack (Just c) = return c
      unpack Nothing  = fail $ "Constructor '" ++ name ++ "' not found"

instance (Monad m) => ModuleRegistry (FlixMonadT m) where
  registerModule m = updateState $ over state_moduleName (const $ Just m)
  getCurrentModule = view state_moduleName <$> getState

instance (Monad m, ModuleRegistry m) => SymbolAliasRegistry (FlixMonadT m) where
  lookupSymbolAlias name = liftA2 format getCurrentModule getSymbolId
    where
      getSymbolId = lookup name . view state_substitutions <$> getState
      format _        Nothing    = name -- skip formatting altogether when the symbol is not registered
      format Nothing  (Just id') = name ++ "_$" ++ show id'
      format (Just m) (Just id') = m ++ "." ++ name ++ "_$" ++ show id'

  registerSymbol name = do
    nextId <- generateNewId <$> getState
    m      <- getCurrentModule
    let newSymbolName = formatName m nextId
    updateState $ over state_symbolCounter (Map.insert name nextId)
    updateState $ over state_substitutions ((name, newSymbolName) :)
    return newSymbolName
    where
      generateNewId = fromMaybe 1 . (Map.!? name) . view state_symbolCounter
      formatName (Just m) id' = m ++ "." ++ name ++ "_$" ++ show id'
      formatName Nothing  id' = name ++ "_$" ++ show id'

  pushFrame f = do
    subs   <- view state_substitutions <$> getState
    result <- f
    getState >>= setState . set state_substitutions subs
    return result

