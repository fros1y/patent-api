module Data.Patent.Providers.Types where

import           Protolude

type ProviderError = Text

type ProviderResult a = Either ProviderError a
