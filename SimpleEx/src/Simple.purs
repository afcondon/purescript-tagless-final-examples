module Simple where

import Prelude (identity, negate, show, (+), (<>))

class ExpSYM repr where
    lit :: Int -> repr
    neg :: repr -> repr
    add :: repr -> repr -> repr

-- An interpreter for the grammar instantiated for Int
instance expsymInt :: ExpSYM Int where
    lit n = n
    neg e = - e
    add e1 e2 = e1 + e2

eval :: Int -> Int
eval = identity               -- amazingly the evaluator is just the Identity operation...

-- Now we can write a different interpreter
instance expsymString :: ExpSYM String where
    lit n = show n
    neg e = "(-" <> e <> ")"
    add e1 e2 = "(" <> e1 <> " + " <> e2 <> ")"

view :: String -> String
view = identity              -- ...and so is the viewer!
