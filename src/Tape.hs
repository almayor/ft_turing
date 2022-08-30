module Tape (Tape(..), TapeSlice(), makeTape, sliceTape, writeTape, moveL, moveR) where

import Prettyprinter

data Tape a = Tape {
    focus       :: a,
    index       :: Integer,
    lSide       :: [a],
    rSide       :: [a]
} deriving (Show, Eq)

makeTape :: a -> [a] -> Tape a
makeTape blank (fcs:rs) = Tape {
    focus = fcs,
    index = 0,
    lSide = repeat blank,
    rSide = rs ++ repeat blank
}
makeTape blank [] = Tape {
    focus = blank,
    index = 0,
    lSide = repeat blank,
    rSide = repeat blank
}

writeTape :: a -> Tape a -> Tape a
writeTape x tape = tape {focus = x}

moveL :: Tape a -> Tape a
moveL (Tape fcs idx (l:ls) rs) = Tape l (idx - 1) ls (fcs:rs)
moveL _ = error "Impossible! Infinite tape is empty!"

moveR :: Tape a -> Tape a
moveR (Tape fcs idx ls (r:rs)) = Tape r (idx + 1) (fcs:ls) rs
moveR _ = error "Impossible! Infinite tape is empty!"

instance Functor Tape where
    fmap f tape = tape {
        focus = f  $  focus tape,
        lSide = f <$> lSide tape,
        rSide = f <$> rSide tape
    }

data TapeSlice a = TapeSlice (Tape a) Integer Integer

sliceTape :: Integer -> Integer -> Tape a -> TapeSlice a
sliceTape lo hi tape = TapeSlice tape lo hi
              
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
            colorBackRed = pretty "\x1b[41m"
            colorBrightWhite = pretty "\x1b[1;32m" 
            colorReset = pretty "\x1b[0m"
        in brackets $
           pretty (mconcat . reverse $ take' (idx - lo) ls)
        <> colorBrightWhite <> colorBackRed
        <> (if idx > hi || idx < lo then mempty else pretty fcs)
        <> colorReset
        <> pretty (mconcat $ take' (hi - idx) rs)
