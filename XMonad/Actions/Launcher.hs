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

import           Data.List        (findIndex)
import qualified Data.Map         as M
import           Data.Maybe       (fromMaybe)
import           System.Directory (doesDirectoryExist)
import           XMonad           hiding (config)
import           XMonad.Prompt
import           XMonad.Util.Run

data LocateFileMode = LMode ExtensionActions
data LocateFileRegexMode = LRMode ExtensionActions
data HoogleMode = HMode
data CalculatorMode = CalcMode

type ExtensionActions = M.Map String (String -> X())

-- | Uses the program `locate` to list files
instance XPrompt LocateFileMode where
  showXPrompt (LMode _) = "locate %s> "
  completionFunction (LMode _) = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5",s])
  modeAction (LMode actions) fp = spawnWithActions actions fp

-- | Uses the program `locate --regex` to list files
instance XPrompt LocateFileRegexMode where
  showXPrompt (LRMode _) = "locate --regexp %s> "
  completionFunction (LRMode _) = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5","--regexp",s])
  modeAction (LRMode actions) fp = spawnWithActions actions fp

-- | Uses the command `calc` to compute arithmetic expressions
instance XPrompt CalculatorMode where
  showXPrompt CalcMode = "calc %s> "
  commandToComplete CalcMode = id --send the whole string to `calc`
  completionFunction CalcMode = \s -> if (length s == 0) then return [] else do
    fmap lines $ runProcessWithInput "calc" [s] ""
  modeAction CalcMode _ = return () -- do nothing; this might copy the result to the clipboard

-- | Uses the program `hoogle` to search for functions
instance XPrompt HoogleMode where
  showXPrompt _ = "hoogle %s> "
  commandToComplete _ = id
  completionFunction _ = \s -> completionFunctionWith "/home/kmels/.cabal/bin/hoogle" ["--count","5",s]
  modeAction _ ac = do
    completions <- liftIO $ completionFunctionWith "/home/kmels/.cabal/bin/hoogle" ["--count","5","+"++ac]
    completionsWithLink <- liftIO $ completionFunctionWith "/home/kmels/.cabal/bin/hoogle" ["--count","5","--link","+"++ac]
    let link = do
          index <- findIndex ((==) ac) completions --index in completions of the selected autocompletion item
          let
            itemWithLink = (!!) completionsWithLink index
            indexOfLink  = findSeqIndex itemWithLink "-- http"
          case indexOfLink of
            Just li -> return $ drop (li + 3) itemWithLink
            _      -> Nothing
    case link of
       Just l -> spawn $ "conkeror " ++ l
       _      -> return ()

-- | Receives a sequence and a subsequence, returns, if it exists, the index in which the subsequence appears in sequence
-- Example:
-- findSeqIndex "aababb" "b" == Just 2
-- findSeqIndex "aababb" "bb" == Just 4
findSeqIndex :: (Eq a) => [a] -> [a] -> Maybe Int
findSeqIndex [] _ = Nothing
findSeqIndex list sublist = let
  findSequence' :: (Eq a) => [a] -> [a] -> Int -> Maybe Int
  findSequence' [] _ _ = Nothing
  findSequence' list'@(x:xs) sublist' i = let
    firstN = take (length sublist') list'
    in if (sublist' == firstN) then Just i else findSequence' xs sublist' (i+1)
  in findSequence' list sublist 0


-- | Creates an autocompletion function for a programm given the program's name and a list of args to send to the command.
completionFunctionWith :: String -> [String] -> IO [String]
completionFunctionWith cmd args = do fmap lines $ runProcessWithInput cmd args ""

-- | Creates a prompt with the given modes
launcherPrompt :: XPConfig -> [XPMode] -> X()
launcherPrompt config modes = mkXPromptWithModes modes config

-- | Create a list of modes based on a list of extensions mapped to actions
defaultLauncherModes :: ExtensionActions -> [XPMode]
defaultLauncherModes actions = [XPT $ HMode
                               , XPT  $ LMode  actions
                               , XPT $ LRMode actions
                               , XPT $ CalcMode
                               ]

-- | This function takes a map of extensions and a path file. It uses the map to find the pattern that matches the file path, then the corresponding program (listed in the map) is spawned.
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
