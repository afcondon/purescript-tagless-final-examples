module NodeFileSystem where

import Prelude
import AbstractFileSystem (class MonadFileSystem, FilePath, FileType)
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (filterM)
import Data.Maybe (maybe)
import Data.Profunctor.Strong (first)
import Data.String (charAt, singleton)
import Data.Tuple (Tuple(..), fst, snd)
import Node.FS (FS)
import Node.FS.Aff (stat, readdir)
import Node.FS.Stats (isDirectory)

main2 :: forall eff. Eff ( err::EXCEPTION, fs::FS, console::CONSOLE| eff) Unit
main2 = void $ launchAff do
  files <- readdir "."
  files' <- flip filterM files \file -> do
    stat <- stat file
    pure $ isDirectory stat
      && (maybe false (singleton >>> (_ /= ".")) $ charAt 0 file)
  liftEff $ log $ show files'

data Cursor = Cursor String

runFS :: ∀ a. NodeFS a -> Cursor -> a
runFS (NodeFS f) = fst <<< f

data NodeFS a = NodeFS (Cursor -> Tuple a Cursor)

instance functorNodeFS :: Functor NodeFS where
  map f (NodeFS g) = NodeFS $ first f <<< g

instance applyNodeFS :: Apply NodeFS where
  apply = ap

instance applicativeNodeFS :: Applicative NodeFS where
  pure a = NodeFS $ \c -> Tuple a c

instance bindNodeFS :: Bind NodeFS where
  bind (NodeFS f) k = NodeFS $ \c0 ->
    let ac1 = f c0
    in  (cursor $ k $ fst ac1) (snd ac1)

cursor :: ∀ a. NodeFS a -> (Cursor -> Tuple a Cursor)
cursor (NodeFS f) = f

instance monadNodeFS :: Monad NodeFS

-- readdir :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) (Array FilePath)

instance monadfilesystemFakeFS :: MonadFileSystem NodeFS where
  cd _  = pure unit
  ls    = NodeFS $ ls'
  cat fs = NodeFS $ cat' fs

ls' :: Cursor -> Tuple (Array (Tuple FilePath FileType)) Cursor
ls' c@(Cursor s) = Tuple [] c

cat' :: Array String -> Cursor -> Tuple String Cursor
cat' fs c@(Cursor s) = Tuple "file contents" c
