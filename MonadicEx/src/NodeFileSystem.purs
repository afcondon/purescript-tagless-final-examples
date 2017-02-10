module NodeFileSystem where

import Prelude
import AbstractFileSystem (class MonadFileSystem)
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (filterM)
import Data.Maybe (maybe)
import Data.String (charAt, singleton)
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

data NodeFS a = NodeFS a

instance showNodeFS :: Show a => Show (NodeFS a) where
  show (NodeFS a) = "NodeFS: " <> show a

instance functorNodeFS :: Functor NodeFS where
  map f (NodeFS a) = NodeFS $ f a

instance applyNodeFS :: Apply NodeFS where
  apply = ap

instance applicativeNodeFS :: Applicative NodeFS where
  pure a = NodeFS a

instance bindNodeFS :: Bind NodeFS where
  bind (NodeFS a) k = k a

instance monadNodeFS :: Monad NodeFS

instance monadfilesystemFakeFS :: MonadFileSystem NodeFS where
  cd _  = NodeFS unit
  ls    = NodeFS $ []
  cat _ = NodeFS $ "contents"
