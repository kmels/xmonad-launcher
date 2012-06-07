{- | Module      :  XMonad.Actions.Launcher
   Copyright   :  (C) 2012 Carlos López-Camey
   License     :  None; public domain

   Maintainer  :  <c.lopez@kmels.net>
   Stability   :  stable

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
  Launcher (..)
  , launcherPrompt
) where

import qualified Data.Map        as M
import           XMonad          hiding (config)
import           XMonad.Prompt
import           XMonad.Util.Run

data Launcher = Launcher

instance XPrompt Launcher where
  showXPrompt Launcher = "launch> "

data LocateMode = LocateMode

instance XPrompt LocateMode where
  showXPrompt LocateMode = "locate 5 %s"
  completionFunction LocateMode = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5",s] s)

data LocateRegexMode = LocateRegexMode

instance XPrompt LocateRegexMode where
  showXPrompt LocateRegexMode = "locate --regex %s"
  completionFunction LocateRegexMode = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5",s] s)

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

launcherPModes :: [XPMode]
launcherPModes = [XPT LocateMode, XPT LocateRegexMode]

launcherPrompt :: XPConfig -> M.Map String (String -> X ()) -> X()
launcherPrompt config extensionActions = do
    --cmds <- io getCommands
    mkXPromptWithModes Launcher config launcherPModes action
    where
      action :: String -> X()
      action s = let
        ext = takeExtension s
        in case M.lookup ext extensionActions of
          Just f -> f s -- f :: String -> X()
          Nothing -> case M.lookup ".*" extensionActions of
            Just f -> f s
            Nothing -> spawn $ "xmessage No action specified for file extension " ++ ext ++ ", add a default action by matching the extension \".*\" in the action map sent to launcherPrompt"
      takeExtension s = "." ++ (reverse . takeWhile (/= '.') $ reverse s) --has the dot inclusive

