module Data.Zipper (Zipper(..), init, empty, length, push, back, forward, atCursor, beforeCursor, afterCursor) where

import Prelude

import Data.List (List(..))
import Data.List (length) as L
import Data.Maybe (Maybe(..))

-- this Zipper DOES have a null value and cannot therefore have a semigroup /
-- monoid instance because associativity would depend on contents of cursors
data Zipper a = Zipper {
    before :: List a
  , cursor :: Maybe a
  , after  :: List a
}

init :: forall a. a -> Zipper a
init a = Zipper { before: Nil, cursor: Just a, after: Nil }

empty :: forall a. Zipper a
empty = Zipper { before: Nil, cursor: Nothing, after: Nil }

instance showZipper :: (Show a) => Show (Zipper a) where
  show (Zipper z) =
    let after  = show $ L.length z.after
        before = show $ L.length z.before
    in
      case z.cursor of
        Nothing -> "[empty]"
        (Just a) -> prettyShow "Before:" z.before <> "\n" <>
                    "Current: <" <> show a <> ">\n" <>
                    prettyShow "After:" z.after

prettyShow :: forall a. (Show a) => String -> List a -> String
prettyShow s Nil = s <> " is empty"
prettyShow s xs  = s <> show xs

instance eqZipper :: (Eq a) => Eq (Zipper a) where
  eq (Zipper z1) (Zipper z2) = z1 == z2

-- the semantics are - push will always set the cursor to the given element but
-- will only move the old cursor to the before list IFF it's not Nothing, this enabled
push :: forall a. a -> Zipper a -> Zipper a
push element (Zipper z) =
  case z.cursor of
    (Just a) -> Zipper z { before = Cons a z.before, cursor = Just element }
    Nothing  -> Zipper z {                           cursor = Just element }

-- || Back and forward are written to handle the possibility of failed invariant
-- because...better safe than sorry. If the data structure is manipulated in
-- such a way that there is a Nothing at the cursor but there is history or
-- future then it will move the cursor back or forward but it won't put the
-- Nothing on the past or future list (obviously?)
back :: forall a. Zipper a -> Zipper a
back z@(Zipper { before: Nil }) = z -- can't go back if back is empty
back (Zipper { before: Cons b bs -- case where we have something in the before...
             , cursor: Just c    -- ... and we have a value at the cursor
             , after })          -- and we don't care what's in the after list
  = Zipper { before: bs          -- we've gone back, which means head of back...
           , cursor: Just b      -- goes into cursor position and...
           , after: Cons c after } -- ... what was in cursor goes on the after list
back (Zipper { before: Cons b bs -- if only used thru API will never arise
             , cursor: Nothing -- failed invariant situation
             , after })
  = Zipper { before: bs      -- we take the head of the before list...
           , cursor: Just b  -- ...and we put it in the cursor position
           , after: after }  -- ...but there was no old cursor so after is unchanged

forward :: forall a. Zipper a -> Zipper a
forward z@(Zipper { after: Nil }) = z -- can't go forward if forward is empty
forward (Zipper { before
                , cursor: Just c
                , after: Cons n ns })
  = Zipper { before: Cons c before
           , cursor: Just n
           , after: ns }
forward (Zipper { before
                , cursor: Nothing -- failed invariant situation, shouldn't arise but good to cover
                , after: Cons n ns })
  = Zipper { before: before
           , cursor: Just n
           , after: ns }

atCursor :: forall a. Zipper a -> Maybe a
atCursor (Zipper z) = z.cursor

beforeCursor :: forall a. Zipper a -> Maybe a
beforeCursor (Zipper { before: Nil, cursor }) = cursor
beforeCursor (Zipper { before: Cons b bs })  = Just b

afterCursor :: forall a. Zipper a -> Maybe a
afterCursor (Zipper { cursor, after: Nil }) = cursor
afterCursor (Zipper { after: Cons n ns })  = Just n

length :: forall a. Zipper a -> Int
length (Zipper z) = L.length z.before + cl + L.length z.after
  where
    cl = case z.cursor of
          (Just a) -> 1
          Nothing  -> 0
