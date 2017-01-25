module Main where

import AbstractFileSystem (class MonadFileSystem, cat, ls)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple(..), fst)
import FakeFileSystem (FS(FS), Zipper(Zipper), fake)
import Prelude (Unit, bind, map, ($))

joinFiles :: ∀ m. (MonadFileSystem m) => m String
joinFiles = do
    files <- ls
    cat $ map fst files

-- dummy file system provides the Zipper that we use against the joinFiles "script"
emptyFS :: FS
emptyFS = FS { files: [], directories: [] }

myFS :: FS
myFS = FS { files: [ (Tuple "awn" "awn contents")
                   , (Tuple "bel" "bel contents")
                   , (Tuple "cep" "cep contents")
                   ]
          , directories: [ (Tuple "dir1" emptyFS)
                         , (Tuple "dir2" emptyFS)
                         , (Tuple "dir3" emptyFS)
                         ]
          }

myZipper :: Zipper
myZipper = Zipper myFS []

-- use the "fake" function to unwrap the FakeFS monad and get at the result so that we can log it

main :: forall eff. Eff ( console :: CONSOLE | eff ) Unit
main = do
  let x = fake joinFiles myZipper
  log x
