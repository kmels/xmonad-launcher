{- | Module      :  XMonad.Actions.Launcher
   Copyright   :  (C) 2012 Carlos López-Camey
   License     :  None; public domain

   Maintainer  :  <c.lopez@kmels.net>
   Stability   :  unstable
   Portability :  unportable; LauncherPrompt

   A module for opening files and directories.
   -}


{- $usage

   ----------------------------------------
   - Description
   ----------------------------------------

   This module is intended to do operations on (mainly) files, it uses locate to autocomplete files matching the arguments and depending on the command, executes an action on it.

   ----------------------------------------
   - Syntax
   ----------------------------------------
     
   <command> ARG_0
   
   Parameters:   
      * <command>: the command to execute, if empty, defaults to `open-file`. The command describes how autocomplete results are handled.
     
      * ARG_0: the first argument sent to <command>, <command> uses the argument to autocomplete.     
      
   ----------------------------------------
   - Examples
   ----------------------------------------

      * ~/.xmonad/xmonad.hs -- opens 
      * open-file ~/.xmonad
   
   ----------------------------------------   
   - Current commands
   ----------------------------------------
   
       * updatedb <RET> -- executes program `updatedb`, available on most Unix-like systems
   
       * open-file FILE <RET>
   
         Opens a file or directory, default programs are listed by a matching expression. The file is opened with the related program, see defaultLauncherPrograms. This is also the default action           
   
       * regex-open-file FILE
   
          Behaves like command `open-file` but the autocompletion works based on a regular expression
      
          Example: open-regex \\.hs$
          (autocomplete list is given by: `locate --limit 5 --regexp \\.$`)            

   ----------------------------------------
   - Writing/adding a command
   ----------------------------------------
      
   A command action takes a filePath (file returned by the user) and does something on X.
   command :: FilePath -> X()
    
   ----------------------------------------
   - TODO 
   ----------------------------------------
   
        * autocomplete commands. op <TAB> should autocomplete to available commands.
   
  -}

module XMonad.Actions.Launcher(
  Launcher (..)
  , launcherPrompt
  , completionFunction
) where

import XMonad hiding (config)
import XMonad.Prompt

import qualified Data.Map as M

import XMonad.Util.Run
--import System.Posix.Files (getFileStatus, isDirectory)
-- Types

-- open_regex :: String
-- commandspec: name :: String, query0_AC:: Autocomplete, [autocompleteSpec]
-- autocomplete spec: autocomplete:: String -> [String]

data Launcher = Launcher

--type Command = FilePath -> X()
instance XPrompt Launcher where 
  showXPrompt Launcher = "launch> "
  
--  completionToCommand _ = escape

--  nextCompletion Launcher = \buffer_cmd compls -> compls !! 0

--openFileC :: Command
--openFileC f = spawn "xmessage 'Hola'"

--launcherCommands :: Map.Map String Command
--launcherCommands = Map.fromList [ ("open-file",openFileC) ]

-- todo: add action that return something inside the X monad. For example, we could contain in the autocomplete command, binaries. An autocompletion item could have metadata e.g. a map of properties, "type" -> "binary", and the action could act on binary autocompletions only. 
-- in the current implementation, we have only strings as autocompletion. To keep backward compatibility, mkPrompt creates a "defaultStringAutocomplete" (a string value without metadata items as siblings)

--autocomplete mode that uses `locate --limit 5 %s`

-- TODO: Buscar ernesto con locate y abrir el pdf de cc3005 -> error
-- TODO: Shift tab en navegar autocompletions para ir al previous

locateFileXPMode = XPMode {
    name = "locate %s"
  , modeCompletionFunction = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5",s] s)
  }
  
--autocomplete mode that uses `locate --limit 5 --regexp %s`
locateRegexpXPMode = XPMode {
    name = "locate --regexp %s"
  , modeCompletionFunction = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5","--regexp",s] s)
  }

--recollq mode
recollqXPMode = XPMode {
    name = "recollq -a -b %s"
  , modeCompletionFunction = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "recollq" ["-a","-b","-t","-n","5",s] s)
  }
  
--creates an autocompletion function given a command, a list of args and a filepath.
completionFunctionWith :: String -> [String] -> String -> IO[String]
completionFunctionWith cmd args s | s == "" || last s == ' ' = return []
                             | otherwise                = do
  paths <- fmap lines $ runProcessWithInput cmd args ""
  return $ paths

launcherPModes :: [XPMode]
launcherPModes = [locateFileXPMode, locateRegexpXPMode,recollqXPMode]

launcherPrompt :: XPConfig -> M.Map String (String -> X ()) -> X()
launcherPrompt config extensionActions = do
    --cmds <- io getCommands
    mkXPromptWithModes Launcher config launcherPModes (\s -> action s)
    where
      action :: String -> X()
      action s = let 
        ext = takeExtension s
        in case M.lookup ext extensionActions of
          Just f -> f s
          Nothing -> case M.lookup "." extensionActions of
            Just f -> f s
            Nothing -> spawn $ "xmessage No action specified for file extension " ++ ext ++ ", add a default action by matching the extension \".\" in the action map sent to launcherPrompt"
      takeExtension s = "." ++ (reverse . takeWhile (/= '.') $ reverse s)
      
-- Takes a String and returns a list of suggestions inside IO
completionFunction :: String -> IO [String]
completionFunction q | q == "" || last q == ' ' = return []
                     | otherwise                = do
  paths <- fmap lines $ runProcessWithInput "locate" ["--limit","5",q] "" 
  --paths <- fmap lines $ runProcessWithInput "recollq" ["-a","-b","-t","-n","5",q] "" 
  return $ paths
  
--  nt <- spawn $ ("xmessage '" ++ line ++ "'")
--  return . uniqSort $ line
                          {-do
  line <- fmap lines $ runProcessWithInput "locate" ["--limit 10"] q
  paths <- case line of
               [x] -> do fs <- getFileStatus (encodeString x)
                         if isDirectory fs then return [x ++ "/"]
                                           else return [x]
               _   -> return line
  return . uniqSort $ line-}
        
--wrapWithApostrophe :: String -> String
--wrapWithApostrophe s = "'"++s++"'"

-- Formats a path
--formatPath :: String -> IO String
--formatPath =  

escape :: String -> String
escape []       = ""
escape (x:xs)
    | isSpecialChar x = '\\' : x : escape xs
    | otherwise       = x : escape xs
    
isSpecialChar :: Char -> Bool
isSpecialChar =  flip elem "&\\@\"'#?$*()[]{};"
