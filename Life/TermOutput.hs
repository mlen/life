{-# LANGUAGE ViewPatterns #-}
module Life.TermOutput where

import qualified Data.Foldable                as F
import           Data.List                    (intersperse)
import qualified Data.Vector                  as V
import           System.Console.Terminfo.Base (TermOutput, termText)

import           Life.Board
import           Life.Game

renderCell :: Cell -> TermOutput
renderCell Alive = termText "O"
renderCell Dead  = termText "."

renderBoard :: FocusedBoard Cell -> TermOutput
renderBoard (FocusedBoard _ _ (Board w _ r)) =
    F.fold . intersperse (termText "\n") . map F.fold $ (split w (fmap renderCell r))


split :: Int -> V.Vector a -> [V.Vector a]
split n v = go v []
  where
    go (V.null -> True) acc = acc
    go vec              acc = let (i, t) = V.splitAt n vec
                              in go t (i:acc)
