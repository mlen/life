module Life.Game where

import           Control.Comonad     (extract, extend)
import           Data.Maybe          (maybeToList)
import qualified Data.Vector         as V

import           Life.Board

data Cell = Dead | Alive deriving (Show, Eq)

nextState :: FocusedBoard Cell -> Cell
nextState b = willLive (extract b) (aliveNeigh neighbours)
  where
    neighbours = [extract n | x <- [-1, 0, 1],
                              y <- [-1, 0, 1],
                              x /= 0 || y /= 0,
                              n <- maybeToList (neighbour x y b)]

aliveNeigh :: [Cell] -> Int
aliveNeigh = length . filter (== Alive)

willLive :: Cell -> Int -> Cell
willLive Dead  3 = Alive
willLive Alive 3 = Alive
willLive Alive 2 = Alive
willLive _     _ = Dead

tick :: FocusedBoard Cell -> FocusedBoard Cell
tick = extend nextState

game :: FocusedBoard Cell -> [FocusedBoard Cell]
game = iterate tick

drawCell :: Cell -> Char
drawCell Alive = 'O'
drawCell Dead  = '.'

drawLine :: Board Cell -> String
drawLine b = map drawCell $ V.toList (rep b)

drawBoard :: FocusedBoard Cell -> String
drawBoard (FocusedBoard _ _ b) = unlines (go (drawLine b) [])
  where
    go [] acc = acc
    go s  acc = let (i, t) = splitAt (width b) s
                in go t (i:acc)

render :: FocusedBoard Cell -> IO ()
render = putStr . drawBoard
