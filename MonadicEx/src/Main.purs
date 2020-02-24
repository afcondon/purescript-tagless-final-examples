module Main where

import AbstractFileSystem (class MonadFileSystem, FilePath, cat, cd, ls)
import Effect
import DummyData (sampleFakeFS)
import FakeFileSystem (run)
import Node.FS (FS) as N
import NodeFileSystem (fsRun, fsState)
import Prelude (Unit, bind, show, ($), (<>), discard)

joinFiles :: ∀ m. (MonadFileSystem m) => m String
joinFiles = do
    dir1  <- cd "test/dir1"
    files <- ls
    cat files

listFiles :: ∀ m. (MonadFileSystem m) => m (Array FilePath)
listFiles = ls

main :: Effect Unit
main = do
    log "First the Eff version: "
    fs <- fsState joinFiles
    log $ "This is the state of the NodeFileSystem: " <> fs
    filelist <- fsRun listFiles
    log $ "This is the result of _ls_ in file system: " <> show filelist
    log "\nNow the fakeFS version of same script\n\t"
    log $ run joinFiles sampleFakeFS
