module NodeFileSystem where

import Prelude
import AbstractFileSystem (class MonadFileSystem)
import Effect 
import Effect.Class
import Effect.Unsafe
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State.Trans (StateT, runStateT)
import Data.Tuple (Tuple, fst, snd)
import Node.FS.Sync (readdir)

newtype FSEff a = FSEff (StateT String a)

instance monadFileSystemFSEff :: MonadFileSystem FSEff where
  cd "."  = pure unit
  cd ".." = pure unit
  cd _    = pure unit
  ls = pure $ unsafePerformEffect $ readdir "." -- don't want to do this unsafePerformEff here. do i have to?
  cat fs = pure $ show fs

derive newtype instance functorFSEff     :: Functor           FSEff
derive newtype instance applyFSEff       :: Apply             FSEff
derive newtype instance applicativeFSEff :: Applicative       FSEff
derive newtype instance bindFSEff        :: Bind              FSEff
derive newtype instance monadFSEff       :: Monad             FSEff
derive newtype instance monadEffFSEff    :: MonadEffect       FSEff
derive newtype instance monadStateFSEff  :: MonadState String FSEff

-- runFSEff :: forall a. FSEff a -> Effect a
runFSEff :: ∀ a. FSEff a -> String -> Effect (Tuple a String)
runFSEff (FSEff fse) = runStateT fse

fsState :: ∀ a. FSEff a -> Effect String
fsState (FSEff fse) = liftA1 snd $ runStateT fse "initial state"

fsRun :: ∀ a. FSEff a -> Effect a
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
