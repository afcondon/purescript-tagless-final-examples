module NodeFileSystem where

import Prelude
import AbstractFileSystem (class MonadFileSystem, FilePath, FileType(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..), fst, snd)
import Node.FS (FS)
import Node.FS.Sync (readdir)

data FileSystemState = FileSystemState String

type FST e a = Eff (fs::FS, err::EXCEPTION|e) a

data NodeFS a = NodeFS (FileSystemState -> Tuple a FileSystemState)

runFS :: ∀ a. NodeFS a -> FileSystemState -> a
runFS (NodeFS f) = fst <<< f

-- | instances for NodeFS
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

cursor :: ∀ a. NodeFS a -> (FileSystemState -> Tuple a FileSystemState)
cursor (NodeFS f) = f

instance monadNodeFS :: Monad NodeFS

-- | instance of the underlying abstract class for tagless interpreter
instance monadfilesystemFakeFS :: MonadFileSystem NodeFS where
  cd _  = pure unit
  -- ls    = NodeFS $ ls' -- not compatible with underlying class definition due to Eff
  ls    = pure []
  cat fs = pure "not implemented"

ls' :: ∀ e. FileSystemState -> Tuple (FST e (Array (Tuple FilePath FileType))) FileSystemState
ls' c@(FileSystemState _) = Tuple files c
    where
    files :: FST e (Array (Tuple FilePath FileType))
    files = (map (\s -> Tuple s File)) <$> readdir "."  -- hardwired to "File" for time being

-- cat' :: Array String -> FileSystemState -> Tuple String FileSystemState
-- cat' fs c@(FileSystemState s) = Tuple "file contents" c
--
-- -- | Example main from purescript-node-fs
-- main2 :: forall eff. Eff ( err::EXCEPTION, fs::FS, console::CONSOLE| eff) Unit
-- main2 = do
--   files <- readdir "."
--   files' <- flip filterM files \file -> do
--     stat <- stat file
--     pure $ isDirectory stat
--       && (maybe false (singleton >>> (_ /= ".")) $ charAt 0 file)
--   liftEff $ log $ show files'
