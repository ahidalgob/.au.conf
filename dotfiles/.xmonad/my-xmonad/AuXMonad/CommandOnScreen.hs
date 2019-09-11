module AuXMonad.CommandOnScreen
  ( Dzens(..)
  , setDzenCommands
  , setDzens
  ) where

import Data.Maybe

import XMonad
import XMonad.Util.ExtensibleState as XS
import XMonad.Util.Run (spawnPipe)

import System.IO


newtype DzenCommands = DzenCommands [(String, ScreenId)] deriving Typeable
instance ExtensionClass DzenCommands where
  initialValue = DzenCommands []

newtype Dzens = Dzens [(Handle, ScreenId)] deriving Typeable
instance ExtensionClass Dzens where
  initialValue = Dzens []

setDzenCommands :: [(String, ScreenId)] -> X ()
setDzenCommands = XS.put . DzenCommands

-- TODO: instead of killing and spawning all, just do the necessary
-- BUG: I was trying to run this in every LogHook but it seems like it runs
-- in parallel and lazily, and ends up running too many dzens.
setDzens :: X ()
setDzens = do
  Dzens dzens <- XS.get
  nScreens <- getNScreens

  DzenCommands dzenCommands <- XS.get
  ndzens <- mapM (\(command, screen) ->
    spawnPipe command >>= (\handle -> return (handle, screen)))
      (take nScreens dzenCommands)
  XS.put (Dzens ndzens)
  where
  getNScreens = length . filter isJust <$> mapM screenWorkspace [S i | i <- [0..4]]
