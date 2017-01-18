module AbstractFileSystem where

import Prelude
import Data.Tuple (Tuple)

type FilePath = String
data FileType = File | Directory

derive instance eqFileType :: Eq FileType

instance showFileType :: Show FileType where
  show File      = "File"
  show Directory = "Dir"

class (Monad m) <= MonadFileSystem m where
  cd :: FilePath -> m Unit
  ls :: m (Array (Tuple FilePath FileType))
  cat :: Array FilePath -> m String
