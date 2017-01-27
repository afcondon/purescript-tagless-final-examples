module Main where

import AbstractFileSystem (class MonadFileSystem, cat, cd, ls)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Tuple (Tuple(..), fst)
import FakeFileSystem (FS(FS), Zipper(Zipper), run)
import Prelude (Unit, bind, map, ($))

joinFiles :: ∀ m. (MonadFileSystem m) => m String
joinFiles = do
    dir1  <- cd "dir1"
    files <- ls
    cat    $ map fst files

-- dummy file system provides the Zipper that we use against the joinFiles "script"
myFS :: FS
myFS = FS { files: [ (Tuple "awn" "awn contents")
                   , (Tuple "bel" "bel contents")
                   , (Tuple "cep" "cep contents")
                   ]
          , directories: [ (Tuple "dir1" (FS { files: [ (Tuple "dof" "dof contents")
                                                      , (Tuple "erg" "erg contents")
                                                      , (Tuple "fid" "fid contents")
                                                      ]
                                             , directories: []
                                             }))
                         , (Tuple "dir2" (FS { files: [], directories: [] }) )
                         , (Tuple "dir3" (FS { files: [], directories: [] }) )
                         ]
          }

myZipper :: Zipper
myZipper = Zipper myFS []

-- use the "fake" function to unwrap the FakeFS monad and get at the result so that we can log it

main :: forall eff. Eff ( console :: CONSOLE | eff ) Unit
main = do
    logShow myZipper
    log $ run joinFiles myZipper
