module NodeFileSystem where

import Prelude
import AbstractFileSystem (class MonadFileSystem)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Node.FS (FS)
import Node.FS.Sync (readdir)

newtype FSEff eff a = FSEff (Eff (fs :: FS, err :: EXCEPTION | eff) a)

derive newtype instance functorFSEff     :: Functor     (FSEff eff)
derive newtype instance applyFSEff       :: Apply       (FSEff eff)
derive newtype instance applicativeFSEff :: Applicative (FSEff eff)
derive newtype instance bindFSEff        :: Bind        (FSEff eff)
derive newtype instance monadFSEff       :: Monad       (FSEff eff)

-- gets the Eff out of the FSEff
runFSEff :: forall eff a. FSEff eff a -> Eff (fs :: FS, err :: EXCEPTION | eff) a
runFSEff (FSEff fse) = fse

instance monadFileSystemFSEff :: MonadFileSystem (FSEff eff) where
  cd "."  = pure unit
  cd ".." = pure unit
  cd _    = pure unit
  ls = pure $ unsafePerformEff $ readdir "."
  cat fs = pure $ show fs


-- | below is another way of getting the MonadFileSystem instance for the Eff,
-- but this necessarily requires that the extension classes be defined in the
-- same file as the abstract class, thus defeating the idea of extensibility.
-- Example by @garyb

{-
instance monadFileSystemEff :: EffectRowEquals r (fs :: FS, err :: EXCEPTION | eff) => MonadFileSystem (Eff r) where
  cd _ = pure unit
  ls = pure []
  cat _ = pure "meow"

-- This should probably go in `purescript-type-equality`:

class EffectRowEquals (a ∷ # !) (b ∷ # !) | a → b, b → a where
  toER ∷ ∀ r. r a → r b
  fromER ∷ ∀ r. r b → r a

instance reflER ∷ EffectRowEquals r r where
  toER er = er
  fromER er = er
-}
