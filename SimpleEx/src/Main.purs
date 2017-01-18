module Main where

import Prelude (Unit, bind, map)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array ((:))
import Simple (add, eval, lit, neg, view)
import Extended (mul)

-- examples of the simple example in practice
sampleExpression = add (lit 8) (neg (add (lit 1) (lit 2)))

-- we can "eval" this sample expression with one interpreter...
evaluated = eval sampleExpression     -- 5
-- ...and view it with another
viewed    = view sampleExpression     -- "(8 + (-(1 + 2)))"

-- we can makes lists and other functorial structures with these expressions...
listOfExpr = [lit 1, add (lit 1) (lit 3), sampleExpression]
-- ...and then eval them by mapping eval over the list...
list_evaluated = map eval listOfExpr   -- [1,4,5]
-- ...or view them by mapping view instead
list_viewed = map view listOfExpr   -- ["1","(1 + 3)","(8 + (-(1 + 2)))"]


-- examples of the extended grammar

-- An extended sample expression
extendedExpression = add (lit 7) (neg (mul (lit 1) (lit 2)))

extExpression_evaluated = eval extendedExpression   -- 5
extExpression_viewed = view extendedExpression      -- "(7 + (-(1 * 2)))"

-- We can even use a previously defined unextended expression (sampleExpression)
-- as a part of the extended expression.
-- We can indeed mix-and-match
mixedExpression = mul (lit 7) sampleExpression

mixExpression_evaluated = eval mixedExpression      -- 35
mixExpression_viewed = view mixedExpression         -- "(7 * (8 + (-(1 + 2))))"

-- We can put the original, unextended expressions
-- and the extended ones (which we have just defined)
-- into the same list
mixedExprList = extendedExpression : mixedExpression : [sampleExpression]   -- add extended expressions
-- and eval or view using map just as before
mixedExprList_eval = map eval mixedExprList   -- [5,35,5]
mixedExprList_view = map view mixedExprList   -- ["(7 + (-(1 * 2)))","(7 * (8 + (-(1 + 2))))","(8 + (-(1 + 2)))"]


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    logShow evaluated
    log viewed
    logShow list_evaluated
    logShow list_viewed
    logShow extExpression_evaluated
    logShow mixExpression_evaluated
    log mixExpression_viewed
    logShow mixedExprList_eval
    logShow mixedExprList_view
