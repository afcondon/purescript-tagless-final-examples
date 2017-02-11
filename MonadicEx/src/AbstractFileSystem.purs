module AbstractFileSystem where

import Prelude (class Eq, class Monad, class Show, Unit, pure, unit)
import Node.FS (FS)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Tuple (Tuple)

type FilePath = String
data FileType = File | Directory

derive instance eqFileType :: Eq FileType

instance showFileType :: Show FileType where
    show File      = "File"
    show Directory = "Dir"

class (Monad m) <= MonadFileSystem m where
    cd  :: FilePath       -> m Unit
    ls  ::                   m (Array FilePath)
    cat :: Array FilePath -> m String

{-
-- | An alternative way of defining the monadFileSystemEff as suggested by garyb
-- in Stack Overflow answer. orphan instance rule seems to want to drag it back
-- into base class file which won't work for extensibility i think
instance monadFileSystemEff :: EffectRowEquals r (fs :: FS, err :: EXCEPTION | eff) => MonadFileSystem (Eff r) where
  cd _  = pure unit
  ls    = pure []
  cat _ = pure "meow"

-- This should probably go in `purescript-type-equality`:
class EffectRowEquals (a ∷ # !) (b ∷ # !) | a → b, b → a where
  toER ∷ ∀ r. r a → r b
  fromER ∷ ∀ r. r b → r a

instance reflER ∷ EffectRowEquals r r where
  toER er = er
  fromER er = er
  -}
