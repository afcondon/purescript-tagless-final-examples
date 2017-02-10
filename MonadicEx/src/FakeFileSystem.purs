module FakeFileSystem where

import AbstractFileSystem (class MonadFileSystem, FilePath, FileType(..))
import Data.Array (foldl, mapMaybe, uncons, (:))
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), fst, snd, lookup)
import Data.Unit (Unit)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, class Show, ap, pure, show, unit, ($), (<<<), (<>))

data FS = FS { files :: Array (Tuple FilePath String), directories :: Array (Tuple FilePath FS) }

-- mimic the Haskell record accessors and refactor out later
getFiles :: FS -> Array (Tuple FilePath String)
getFiles (FS { files }) = files

getDirectories :: FS -> Array (Tuple FilePath FS)
getDirectories (FS { directories }) = directories

listing :: ∀ a. FileType -> Array (Tuple FilePath a) -> Array (Tuple FilePath FileType)
listing ft = foldl (\fs tup -> (Tuple (fst tup) ft):fs) []

instance showFS :: Show FS where
  show fs = show $ ((listing File) $ getFiles fs) <> ((listing Directory) $ getDirectories fs)

data Zipper = Zipper FS (Array FS)

instance showZipper :: Show Zipper where
  show (Zipper fs fss) = show fs <> show fss

data FakeFS a = FakeFS (Zipper -> Tuple a Zipper)

run :: ∀ a. FakeFS a -> Zipper -> a
run (FakeFS f) = fst <<< f

instance functorFakeFS :: Functor FakeFS where
    map f (FakeFS g) = FakeFS $ first f <<< g

instance applyFakeFS :: Apply FakeFS where
    apply = ap

instance applicativeFakeFS :: Applicative FakeFS where
    pure a = FakeFS $ \z -> Tuple a z

instance bindFakeFS :: Bind FakeFS where
  bind (FakeFS f) k = FakeFS $ \z0 ->
    let az1 = f z0   -- f z0 = (Tuple a z1)
    in (zipperFn $ k $ fst az1) (snd az1)

zipperFn :: ∀ a. FakeFS a -> (Zipper -> Tuple a Zipper)
zipperFn (FakeFS f) = f

instance monadFakeFS :: Monad FakeFS

instance monadfilesystemFakeFS :: MonadFileSystem FakeFS where
  cd "."  = pure unit
  cd ".." = FakeFS $ cdDotDot
  cd dir  = FakeFS $ cdDir dir
  ls      = FakeFS $ ls'
  cat fs  = FakeFS $ cat' fs

-- implementations of each of the FakeFS "methods"
cdDotDot :: Zipper -> Tuple Unit Zipper
cdDotDot (Zipper dir ctx) =
    case uncons ctx of
        (Just { head, tail }) -> Tuple unit (Zipper head tail)
        Nothing               -> Tuple unit (Zipper dir ctx)

cdDir :: FilePath -> Zipper -> Tuple Unit Zipper
cdDir dir (Zipper cur ups) =
    case lookup dir (getDirectories cur) of
        Just fs' -> Tuple unit (Zipper fs' (cur : ups))
        _        -> Tuple unit (Zipper cur ups)

ls' :: Zipper -> Tuple (Array (Tuple FilePath FileType)) Zipper
ls' (Zipper cur ctx) =
    let fs = listing File $ getFiles cur
        ds = listing Directory $ getDirectories cur
    in Tuple (fs <> ds) (Zipper cur ctx)

cat' :: Array String -> Zipper -> Tuple String Zipper
cat' fs (Zipper cur ctx) =
    Tuple (joinWith "\n" $ mapMaybe (\f -> lookup f (getFiles cur)) fs) (Zipper cur ctx)
