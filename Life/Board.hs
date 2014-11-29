{-# LANGUAGE BangPatterns #-}

module Life.Board where

import           Data.Functor.Identity
import           Control.Comonad
import qualified Data.Vector     as V

data Board a = Board { width  :: !Int
                     , height :: !Int
                     , rep    :: !(V.Vector a)
                     } deriving Show

data FocusedBoard a = FocusedBoard { bx   :: !Int
                                   , by   :: !Int
                                   , brep :: !(Board a)
                                   } deriving Show

inBounds :: Board a -> Int -> Int -> Bool
inBounds b x y = x >= 0 && x < width b &&
                 y >= 0 && y < height b

boardM :: (Functor m, Monad m) => Int -> Int -> (Int -> Int -> m a) -> m (FocusedBoard a)
boardM w h f = FocusedBoard 0 0 . Board w h <$> V.generateM (w * h) go
  where
    go i = let (y, x) = i `divMod` w
           in f x y

board :: Int -> Int -> (Int -> Int -> a) -> FocusedBoard a
board w h f = runIdentity $ boardM w h ((Identity .) . f)

instance Functor Board where
    fmap f (Board w h b) = Board w h (fmap f b)

instance Functor FocusedBoard where
    fmap f (FocusedBoard x y b) = FocusedBoard x y (fmap f b)

instance Comonad FocusedBoard where
    extract (FocusedBoard x y b) = rep b V.! (y * width b + x)

    extend f (FocusedBoard x y b) = FocusedBoard x y (b {rep = newRep})
      where
        newRep = V.generate (width b * height b) $ \i ->
          let (y', x') = i `divMod` width b
          in f (FocusedBoard x' y' b)
      
neighbour :: Int -> Int -> FocusedBoard a -> Maybe (FocusedBoard a)
neighbour dx dy (FocusedBoard x y b) | inBounds b x' y' = Just (FocusedBoard x' y' b)
                                     | otherwise        = Nothing
  where
    y' = y + dy
    x' = x + dx
