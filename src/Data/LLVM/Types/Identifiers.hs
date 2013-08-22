module Data.LLVM.Types.Identifiers (
  -- * Types
  Identifier,
  -- * Accessor
  identifierAsString,
  identifierContent,
  isAnonymousIdentifier,
  -- * Builders
  makeAnonymousLocal,
  makeLocalIdentifier,
  makeGlobalIdentifier,
  makeMetaIdentifier
  ) where

import Control.DeepSeq
import Data.Hashable
import Data.Text ( Text, unpack, pack )
import Data.Monoid ( (<>) )

data Identifier = LocalIdentifier { _identifierContent :: !Text
                                  , _identifierHash :: !Int
                                  }
                | AnonymousLocalIdentifier { _identifierNumber :: !Int }
                | GlobalIdentifier { _identifierContent :: !Text
                                   , _identifierHash :: !Int
                                   }
                | MetaIdentifier { _identifierContent :: !Text
                                 , _identifierHash :: !Int
                                 }
                  deriving (Eq)

instance Show Identifier where
  show LocalIdentifier { _identifierContent = t } = '%' : unpack t
  show AnonymousLocalIdentifier { _identifierNumber = n } = '%' : show n
  show GlobalIdentifier { _identifierContent = t } = '@' : unpack t
  show MetaIdentifier { _identifierContent = t } = '!' : unpack t

-- More efficient than default instance
instance Ord Identifier where
  LocalIdentifier cont1 hash1 `compare` LocalIdentifier cont2 hash2
    = (hash1 `compare` hash2) <> (cont1 `compare` cont2)
  LocalIdentifier{} `compare` _ = LT
  _ `compare` LocalIdentifier{} = GT
  GlobalIdentifier cont1 hash1 `compare` GlobalIdentifier cont2 hash2
    = (hash1 `compare` hash2) <> (cont1 `compare` cont2)
  GlobalIdentifier{} `compare` _ = LT
  _ `compare` GlobalIdentifier{} = GT
  MetaIdentifier cont1 hash1 `compare` MetaIdentifier cont2 hash2
    = (hash1 `compare` hash2) <> (cont1 `compare` cont2)
  MetaIdentifier{} `compare` _ = LT
  _ `compare` MetaIdentifier{} = GT
  AnonymousLocalIdentifier n1 `compare` AnonymousLocalIdentifier n2 = n1 `compare` n2

instance Hashable Identifier where
  hashWithSalt s (AnonymousLocalIdentifier n) = s `hashWithSalt` n
  hashWithSalt s i = s `hashWithSalt` _identifierHash i

instance NFData Identifier where
  rnf AnonymousLocalIdentifier {} = ()
  rnf i = _identifierContent i `seq` _identifierHash i `seq` ()

makeAnonymousLocal :: Int -> Identifier
makeAnonymousLocal = AnonymousLocalIdentifier

makeLocalIdentifier :: Text -> Identifier
makeLocalIdentifier t =
  LocalIdentifier { _identifierContent = t
                  , _identifierHash = hash t
                  }

makeGlobalIdentifier :: Text -> Identifier
makeGlobalIdentifier t =
  GlobalIdentifier { _identifierContent = t
                   , _identifierHash = hash t
                   }

makeMetaIdentifier :: Text -> Identifier
makeMetaIdentifier t =
  MetaIdentifier { _identifierContent = t
                 , _identifierHash = hash t
                 }

identifierAsString :: Identifier -> String
identifierAsString (AnonymousLocalIdentifier n) = show n
identifierAsString i = unpack (identifierContent i)

identifierContent :: Identifier -> Text
identifierContent (AnonymousLocalIdentifier n) = pack (show n)
identifierContent i = _identifierContent i

isAnonymousIdentifier :: Identifier -> Bool
isAnonymousIdentifier AnonymousLocalIdentifier {} = True
isAnonymousIdentifier _ = False