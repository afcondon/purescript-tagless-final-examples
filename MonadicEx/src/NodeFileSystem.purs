module NodeFileSystem where

import Prelude
import AbstractFileSystem (class MonadFileSystem)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.State (class MonadState)
import Control.Monad.State.Trans (StateT, get, put, runStateT)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)
import Node.FS (FS)
import Node.FS.Sync (readdir)

newtype FSEff eff a = FSEff (StateT String (Eff eff) a)

instance monadFileSystemFSEff :: MonadFileSystem (FSEff eff) where
  cd "."  = pure unit
  cd ".." = pure unit
  cd _    = pure unit
  ls = pure $ unsafePerformEff $ readdir "." -- don't want to do this unsafePerformEff here. do i have to?
  cat fs = pure $ show fs

derive newtype instance functorFSEff     :: Functor           (FSEff eff)
derive newtype instance applyFSEff       :: Apply             (FSEff eff)
derive newtype instance applicativeFSEff :: Applicative       (FSEff eff)
derive newtype instance bindFSEff        :: Bind              (FSEff eff)
derive newtype instance monadFSEff       :: Monad             (FSEff eff)
derive newtype instance monadEffFSEff    :: MonadEff   eff    (FSEff eff)
derive newtype instance monadStateFSEff  :: MonadState String (FSEff eff)

-- runFSEff :: forall eff a. FSEff eff a -> Eff eff a
runFSEff :: ∀ eff a. FSEff eff a -> String -> Eff eff (Tuple a String)
runFSEff (FSEff fse) = runStateT fse

fsState :: ∀ a eff. FSEff eff a -> Eff eff String
fsState (FSEff fse) = liftA1 snd $ runStateT fse "initial state"

fsRun :: ∀ a eff. FSEff eff a -> Eff eff a
fsRun (FSEff fse) = liftA1 fst $ runStateT fse "initial state"

-- runFSEff'' :: ∀ eff. FSEff eff String -> FSEff eff Int
-- runFSEff'' = do
--     put "updated"
--     pure 1

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
