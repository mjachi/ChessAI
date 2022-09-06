{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Player where

import Control.DeepSeq
import GHC.Generics (Generic)

-----------------------------------------------------------------------------------------
-- COLOR DENOTION
-----------------------------------------------------------------------------------------

-- | Several structrs are associated to a color; used for Player,
--   board tiles, pieces, etc.
data Color = White | Black deriving (Eq, Generic, NFData)

instance Show Color where
  show Black = "-"
  show White = "+"

-- | Simple case match function for Color above
colorSwitch :: Color -> Color
colorSwitch x = case x of
  Black -> White
  White -> Black

-----------------------------------------------------------------------------------------
-- PLAYER DEFINITIONS
-----------------------------------------------------------------------------------------

-- A few simple type defintions...
newtype PlayerColor = PlayerColor {c :: Color}

newtype PlayerName = PlayerName {name :: String}

newtype ComputerDifficulty = ComputerDifficulty {val :: Integer}

-- | A player is either a human instance (ie, we look for moves coming from stdin) or
-- a computer instance, for which we generate the moves.
data Player
  = Human PlayerName PlayerColor
  | Computer ComputerDifficulty PlayerColor

-- | Quick getter method by pattern matching to retrieve the actual Color type
getPlayerColor :: Player -> Color
getPlayerColor (Human _ cc) = c cc
getPlayerColor (Computer _ cc) = c cc
