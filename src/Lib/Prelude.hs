{-# LANGUAGE FlexibleContexts #-}

{-
Welcome to your custom Prelude
Export here everything that should always be in your library scope
For more info on what is exported by Protolude check:
https://github.com/sdiehl/protolude/blob/master/Symbols.md
-}
module Lib.Prelude
  ( readDef
  ) where

import Data.String.Conversions (ConvertibleStrings, convertString)
import Protolude as Exports
import Text.Read (readMaybe)

readDef
  :: (ConvertibleStrings s [Char], Read a)
  => a -> s -> a
readDef defVal string = fromMaybe defVal $ readMaybe $ convertString string
