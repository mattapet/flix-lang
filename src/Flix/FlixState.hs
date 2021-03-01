{-# LANGUAGE TemplateHaskell #-}

module Flix.FlixState where

import           Control.Lens
import           Core                           ( Ty )
import           Data.Map

data FlixState = FlixState
  { _state_substitutions     :: Map String Int
  , _state_uniqueNameCounter :: Int
  , _state_constructors      :: Map String Ty
  , _state_moduleName        :: Maybe String
  }

makeLenses ''FlixState
