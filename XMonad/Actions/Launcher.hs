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

   - Wished commands
     at - execute commands at a later time
     hoogle ?
     bash - is it possible with autocompletion ?
  -}

module XMonad.Actions.Launcher(
  defaultLauncherModes
  , ExtensionActions
  , LocateFileMode
  , LocateFileRegexMode
  , launcherPrompt
) where

import qualified Data.Map         as M
import           Data.Maybe       (fromMaybe)
import           System.Directory (doesDirectoryExist)
import           XMonad           hiding (config)
import           XMonad.Prompt
import           XMonad.Util.Run

data LocateFileMode = LMode ExtensionActions
data LocateFileRegexMode = LRMode ExtensionActions
data CalculatorMode = CalcMode

type ExtensionActions = M.Map String (String -> X())

-- | Uses the program `locate` to list files
instance XPrompt LocateFileMode where
  showXPrompt (LMode _) = "locate %s> "
  completionFunction (LMode _) = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5",s] s)
  modeAction (LMode actions) fp = spawnWithActions actions fp

-- | Uses the program `locate --regex` to list files
instance XPrompt LocateFileRegexMode where
  showXPrompt (LRMode _) = "locate --regexp %s> "
  completionFunction (LRMode _) = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5","--regexp",s] s)
  modeAction (LRMode actions) fp = spawnWithActions actions fp

-- | Uses the command `calc` to compute arithmetic expressions
instance XPrompt CalculatorMode where
  showXPrompt CalcMode = "calc %s> "
  commandToComplete CalcMode s = s --send the whole string to `calc`
  completionFunction CalcMode = \s -> if (length s == 0) then return [] else (completionFunctionWith "calc" [s] s)
  modeAction CalcMode _ = return () -- do nothing; this might copy the result to the clipboard

-- | Creates an autocompletion function given a command name, a list of args to send to the command and a filepath.
completionFunctionWith :: String -> [String] -> String -> IO [String]
completionFunctionWith cmd args s | s == "" || last s == ' ' = return []
                             | otherwise                = do
  paths <- fmap lines $ runProcessWithInput cmd args ""
  return $ paths

-- | Creates a prompt with the given modes
launcherPrompt :: XPConfig -> [XPMode] -> X()
launcherPrompt config modes = mkXPromptWithModes modes config

-- | Create a list of modes based on a list of extensions mapped to actions
defaultLauncherModes :: ExtensionActions -> [XPMode]
defaultLauncherModes actions = [XPT (LMode actions)
                               , XPT (LRMode actions)
                               , XPT CalcMode]

-- | This function takes a path file and uses the map of extensions to find the one that matches
-- to spawn the process that corresponds.
spawnWithActions :: ExtensionActions -> FilePath -> X()
spawnWithActions actions fp = do
  isDirectoryPath <- liftIO $ doesDirectoryExist fp
  let
    takeExtension = \p -> "." ++ (reverse . takeWhile (/= '.') $ reverse p) --it includes the dot
    -- Patterns defined by the user
    extAction = M.lookup (takeExtension fp) actions
    dirAction = if (isDirectoryPath) then M.lookup "/" actions else Nothing -- / represents a directory
    anyFileAction = M.lookup ".*" actions  -- .* represents any file
    action = fromMaybe (spawnNoPatternMessage (takeExtension fp)) $ extAction `orElse1` dirAction `orElse1` anyFileAction
  action fp
     where
       -- | This function is defined in Data.Generics.Aliases (package syb "Scrap your boilerplate"), defined here to avoid dependency
       orElse1 :: Maybe a -> Maybe a -> Maybe a
       x `orElse1` y = case x of
         Just _  -> x
         Nothing -> y
       spawnNoPatternMessage :: String -> String -> X ()
       spawnNoPatternMessage fileExt _ = spawn $ "xmessage No action specified for file extension " ++ fileExt ++ ", add a default action by matching the extension \".*\" in the action map sent to launcherPrompt"
