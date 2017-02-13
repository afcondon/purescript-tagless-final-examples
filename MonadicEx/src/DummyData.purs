module DummyData where

import FakeFileSystem (FS(..), Zipper(..))
import Data.Tuple (Tuple(..))

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

sampleFakeFS :: Zipper
sampleFakeFS = Zipper myFS []
