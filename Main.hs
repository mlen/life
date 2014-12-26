import Control.Monad
import Control.Concurrent
import System.IO
import System.Console.Terminfo

import Life.Board
import Life.BoardGenerator
import Life.Game
import Life.TermOutput

main :: IO ()
main = do
    term <- setupTermFromEnv

    let lines = maybe 20 id $ getCapability term termLines
        cols  = maybe 20 id $ getCapability term termColumns

    b <- boardM cols lines randomBoard
    forM_ (game b) $ \b -> do
      runTermOutput term (renderBoard b)
      threadDelay 80000
