{-# LANGUAGE TemplateHaskell #-}

module Flix.FlixState where

import           Control.Lens
import           Core                           ( Ty )
import           Data.Map

data FlixState = FlixState
  { _state_substitutions     :: [(String, String)]
  , _state_symbolCounter     :: Map String Int
  , _state_uniqueNameCounter :: Int
  , _state_constructors      :: Map String Ty
  , _state_moduleName        :: Maybe String
  }

makeLenses ''FlixState

makeEmptyState :: FlixState
makeEmptyState = FlixState [] mempty 0 mempty Nothing
