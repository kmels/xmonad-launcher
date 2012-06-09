{-# LANGUAGE FlexibleInstances #-}
{- | Module      :  XMonad.Actions.Launcher
   Copyright   :  (C) 2012 Carlos López-Camey
   License     :  None; public domain

   Maintainer  :  <c.lopez@kmels.net>
   Stability   :  unstable

   A module for opening files and directories.
   -}


{- $usage
   ----------------------------------------
   - Description
   ----------------------------------------
   A prompt with several modes that operate on file paths.


   ----------------------------------------
   - How to add it to your .xmonad
   ----------------------------------------


   ----------------------------------------
   - How to use it:
   ----------------------------------------

   ----------------------------------------
   - How to add a mode
   ----------------------------------------

   ----------------------------------------
   - TODO
   ----------------------------------------
   - XPrompt:
       - Switch to mode by name, 1. ':' at an empty buffer, 2. autocomplete name in buffer should happen, 3. switch to mode with enter (cancel stich with C-g)
  -}

module XMonad.Actions.Launcher(
  defaultLauncherModes
  , ExtensionActions
  , LocateFileMode
  , LocateFileRegexMode
  , launcherPrompt
) where

import qualified Data.Map        as M
import           XMonad          hiding (config)
import           XMonad.Prompt
import           XMonad.Util.Run

data LocateFileMode = LMode ExtensionActions
data LocateFileRegexMode = LRMode ExtensionActions

type ExtensionActions = M.Map String (String -> X())

-- | This function takes a path file and uses the map of extensions to find the one that matches
-- to spawn the process that corresponds.
spawnWithActions :: ExtensionActions -> FilePath -> X()
spawnWithActions actions fp = let
  fileExt = takeExtension fp
  takeExtension = \p -> "." ++ (reverse . takeWhile (/= '.') $ reverse p) --it includes the dot
  in case M.lookup fileExt actions of
    Just action -> action fp -- action :: FilePath -> X()
    Nothing -> case M.lookup ".*" actions of --if no extension matches, look for .*
      Just action -> action fp
      Nothing -> spawn $ "xmessage No action specified for file extension " ++ fileExt ++ ", add a default action by matching the extension \".*\" in the action map sent to launcherPrompt"

instance XPrompt LocateFileMode where
  showXPrompt (LMode _) = "locate %s"
  completionFunction (LMode _) = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5",s] s)
  modeAction (LMode actions) fp = spawnWithActions actions fp

instance XPrompt LocateFileRegexMode where
  showXPrompt (LRMode _) = "locate --regexp %s"
  completionFunction (LRMode _) = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5","--regexp",s] s)
  modeAction (LRMode actions) fp = spawnWithActions actions fp

-- | Uses the program `locate` to list files
{-locateFileXPMode = XPMode {
    name = "locate %s"
  , modeCompletionFunction = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5",s] s)
  } -}

-- | Uses the program `locate --regex` to list files
{-locateRegexpXPMode = XPMode {
    name = "locate --regexp %s"
  , modeCompletionFunction = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5","--regexp",s] s)
  } -}

-- | Uses the program `recollq -a -b` to list files
{-recollqXPMode = XPMode {
    name = "recollq -a -b %s"
  , modeCompletionFunction = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "recollq" ["-a","-b","-t","-n","5",s] s)
  }-}

--creates an autocompletion function given a command, a list of args and a filepath.
completionFunctionWith :: String -> [String] -> String -> IO[String]
completionFunctionWith cmd args s | s == "" || last s == ' ' = return []
                             | otherwise                = do
  paths <- fmap lines $ runProcessWithInput cmd args ""
  return $ paths

defaultLauncherModes :: ExtensionActions -> [XPMode]
defaultLauncherModes actions = [XPT (LMode actions), XPT (LRMode actions)]

launcherPrompt :: XPConfig -> [XPMode] -> X()
launcherPrompt config modes = mkXPromptWithModes modes config
