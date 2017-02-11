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
