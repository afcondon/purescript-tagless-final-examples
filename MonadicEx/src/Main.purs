module Main where

import AbstractFileSystem (class MonadFileSystem, cat, cd, ls)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Tuple (Tuple(..), fst)
import FakeFileSystem (FS(FS), Zipper(Zipper), run)
import Node.FS (FS) as N
import NodeFileSystem (runFSEff)
import Prelude (Unit, bind, map, ($), (<>))

joinFiles :: ∀ m. (MonadFileSystem m) => m String
joinFiles = do
    dir1  <- cd "test/dir1"
    files <- ls
    cat files

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

-- this is the signature that PSC derives automatically but it won't compile (Could not match kind Effect with kind Type )
-- foo :: ∀ eff. Eff ( fs :: FS, err :: EXCEPTION | eff ) String
foo = runFSEff joinFiles

main :: forall eff. Eff ( err :: EXCEPTION, fs :: N.FS, console :: CONSOLE | eff ) Unit
main = do
    log "First the Eff version: "
    fs <- runFSEff joinFiles
    log $ "\n\t" <> fs
    log "\nNow the fakeFS version of same script\n\t"
    log $ run joinFiles myZipper
