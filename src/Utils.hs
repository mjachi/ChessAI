module Utils (reset, pause, cPrint, printError, printDebug, printInfo) where

import Control.Concurrent (threadDelay)
import System.Console.ANSI
import System.IO (hFlush, stdout)

-- | Reset IO state; clears screen and moves cursor to origin
reset :: IO ()
reset = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

pause :: Int -> IO ()
pause x = do
  hFlush stdout
  threadDelay x

-- | Prints with color
cPrint :: String -> String -> IO ()
cPrint msg color = do
  setSGR [SetColor Foreground Vivid (parseColor color)]
  putStrLn msg
  setSGR [SetColor Foreground Dull White]

printError :: String -> IO ()
printError msg = do
  cPrint ("ERROR" ++ msg) "red"
  pause 2000000

printDebug :: String -> IO ()
printDebug msg = do
  cPrint ("DEBUG: " ++ msg) "yellow"
  pause 200000

printInfo :: String -> IO ()
printInfo msg = do
  cPrint ("INFO: " ++ msg) "blue"
  pause 200000

--------------------
-- PRIVATE
--------------------

parseColor :: String -> Color
parseColor x = case x of
  "red" -> Red
  "green" -> Green
  "blue" -> Blue
  "yellow" -> Yellow
  "white" -> White
  _ -> White
