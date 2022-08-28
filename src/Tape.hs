{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Tape where

import Prettyprinter

data Tape a = Tape {
    focus       :: a,
    index       :: Integer,
    lSide       :: [a],
    rSide       :: [a]
} deriving (Show, Eq)

blankTape :: a -> Tape a
blankTape blank = Tape {
    focus       = blank,
    index       = 0,
    lSide       = repeat blank,
    rSide       = repeat blank
}

moveLeft :: Tape a -> Tape a
moveLeft (Tape fcs idx (l:ls) rs) = Tape l (idx - 1) ls (fcs:rs)
moveLeft _ = error "Impossible! Infinite tape is empty!"

moveRight :: Tape a -> Tape a
moveRight (Tape fcs idx ls (r:rs)) = Tape r (idx + 1) (fcs:ls) rs
moveRight _ = error "Impossible! Infinite tape is empty!"

instance Functor Tape where
    fmap f tape = tape {
        focus = f  $  focus tape,
        lSide = f <$> lSide tape,
        rSide = f <$> rSide tape
    }

data TapeSlice a = TapeSlice (Tape a) Integer Integer

slice :: Tape a -> Integer -> Integer -> TapeSlice a
slice tape lo hi = TapeSlice tape lo hi
              
instance Eq a => Eq (TapeSlice a) where
    TapeSlice t1 lo1 hi1 == TapeSlice t2 lo2 hi2 =
        let (Tape fcs1 idx1 ls1 rs1) = t1
            (Tape fcs2 idx2 ls2 rs2) = t2
            take' = take . fromIntegral
        in idx1 == idx2 && fcs1 == fcs2 && lo1 == lo2 && hi1 == hi2
        && take' (idx1 - lo1) ls1 == take' (idx2 - lo2) ls2
        && take' (hi1 - idx1) rs1 == take' (hi2 - idx2) rs2

instance (Pretty a, Monoid a) => Pretty (TapeSlice a) where
    pretty (TapeSlice tape lo hi) =
        let (Tape fcs idx ls rs) = tape
            take' = take . fromIntegral
        in brackets $
           pretty (mconcat $ take' (idx - lo) ls)
        <> (if idx > hi || idx < lo then mempty else angles $ pretty fcs)
        <> pretty (mconcat $ take' (hi - idx) rs)
