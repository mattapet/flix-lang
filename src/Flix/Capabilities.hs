module Flix.Capabilities where

import           Core                           ( Ty )

class UniqueNameGeneration m where
  generateUniqueName :: m String

class ConstructorRegistry m where
  bindConstructor :: String -> Ty -> m ()
  lookupConstructor :: String -> m Ty

class ModuleRegistry m where
  registerModule :: String -> m ()
  getCurrentModule :: m (Maybe String)

class (ModuleRegistry m) => SymbolAliasRegistry m where
  registerSymbol :: String -> m String
  lookupSymbolAlias :: String -> m String
