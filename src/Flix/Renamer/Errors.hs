module Flix.Renamer.Errors
  ( uniqueNameError
  ) where

import           Data.List                      ( intercalate )
import           Flix.Syntax                    ( Name )

uniqueNameError :: [Name] -> String
uniqueNameError = wrapInMessage . sep . (quote <$>)
  where
    sep           = intercalate ", "
    quote         = ("'" ++) . (++ "'")
    wrapInMessage = ("Conflicting definition for symbols " ++)
