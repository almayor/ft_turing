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
    lSide = [],
    rSide = rs ++ repeat blank
}
makeTape blank [] = Tape {
    focus = blank,
    index = 0,
    lSide = [],
    rSide = repeat blank
}

writeTape :: a -> Tape a -> Tape a
writeTape x tape = tape {focus = x}

moveL :: Tape a -> Maybe (Tape a)
moveL (Tape fcs idx (l:ls) rs) = Just $ Tape l (idx - 1) ls (fcs:rs)
moveL _ = Nothing -- can't move left from the leftmost position

moveR :: Tape a -> Maybe (Tape a)
moveR (Tape fcs idx ls (r:rs)) = Just $ Tape r (idx + 1) (fcs:ls) rs
moveR _ = error "Impossible! Reached end of infinite tape!"

instance Functor Tape where
    fmap f tape = tape {
        focus = f  $  focus tape,
        lSide = f <$> lSide tape,
        rSide = f <$> rSide tape
    }

data TapeSlice a = TapeSlice (Tape a) Integer

sliceTape :: Tape a -> Integer -> TapeSlice a
sliceTape = TapeSlice

instance Eq a => Eq (TapeSlice a) where
    TapeSlice t1 nC1 == TapeSlice t2 nC2 =
        let (Tape fcs1 idx1 ls1 rs1) = t1
            (Tape fcs2 idx2 ls2 rs2) = t2
            take' = take . fromIntegral
        in idx1 == idx2 && fcs1 == fcs2 && nC1 == nC1
        && take' idx1 ls1 == take' idx2 ls2
        && take' (nC1 - 1 - idx1) rs1 == take' (nC2 - 1 - idx2) rs2

instance (Pretty a) => Pretty (TapeSlice a) where
    pretty (TapeSlice tape nC) =
        let (Tape fcs idx ls rs) = tape
            take' = take . fromIntegral
            colorBackRed = pretty "\x1b[41m"
            colorBrightWhite = pretty "\x1b[1;37m" 
            colorReset = pretty "\x1b[0m"
            padR = if nC < 12 then 12 - (idx + 1) else nC - (idx + 1) + 4
        in brackets $
           (mconcat . map pretty . reverse $ take' idx ls)
        <> colorBrightWhite <> colorBackRed
        <> (if idx > nC then mempty else pretty fcs)
        <> colorReset
        <> (mconcat . map pretty $ take' padR rs)
