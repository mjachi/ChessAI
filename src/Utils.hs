module Utils (reset, pause, cPrint, printError, clearUntilEnd, printDebug, setCursor, printInfo) where

import Control.Concurrent (threadDelay)
import System.Console.ANSI
import System.IO (hFlush, stdout)

-- | Reset IO state; clears screen and moves cursor to origin
reset :: IO ()
reset = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

-- | Explicit pause after flushing stdout
pause :: Int -> IO ()
pause x = do
  hFlush stdout
  threadDelay x

-- | Wrapper around setCursorPosition to simplify imports
setCursor :: Int -> Int -> IO ()
setCursor = setCursorPosition

-- | Wrapper around clearFromCursorToScreenEnd to simplify imports
clearUntilEnd :: IO ()
clearUntilEnd = clearFromCursorToScreenEnd

-- | Prints with color
cPrint :: String -> String -> IO ()
cPrint msg color = do
  setSGR [SetColor Foreground Vivid (parseColor color)]
  putStrLn msg
  setSGR [SetColor Foreground Dull White]

-- | Logging function : errors
printError :: String -> IO ()
printError msg = do
  cPrint ("ERROR" ++ msg) "red"
  pause 2000000

-- | Logging function : debug
printDebug :: String -> IO ()
printDebug msg = do
  cPrint ("DEBUG: " ++ msg) "yellow"
  pause 200000

-- | Logging function : info
printInfo :: String -> IO ()
printInfo msg = do
  cPrint ("INFO: " ++ msg) "blue"
  pause 200000

--------------------
-- PRIVATE
--------------------

-- | Helper for the above printing functions; given a string, returns color Term color
-- text formatting
parseColor :: String -> Color
parseColor x = case x of
  "red" -> Red
  "green" -> Green
  "blue" -> Blue
  "yellow" -> Yellow
  "white" -> White
  _ -> White
