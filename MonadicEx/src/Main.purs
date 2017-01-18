module Main where

import AbstractFileSystem (class MonadFileSystem, FileType, cat, cd, ls)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple, fst)
import Prelude (Unit, bind, map, ($))

joinFiles :: ∀ m. (MonadFileSystem m) => m String
joinFiles = do
    cd "."
    files1 <- ls
    cd ".."
    files2 <- ls
    cd "doesnt/exist"
    cat $ map fst files1

justLS :: forall m. (MonadFileSystem m) => m (Array (Tuple String FileType))
justLS = do
    ls

justCD :: forall m. (MonadFileSystem m) => m Unit
justCD = do
    cd "."

justCAT :: forall m. (MonadFileSystem m) => m String
justCAT = do
  cat [".", "some/other/path"]


main :: forall eff. Eff ( console :: CONSOLE | eff ) Unit
main = do
    log "hello sailor"
