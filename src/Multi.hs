{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
-- Multi-project ghcid variant (ghcid-multi)
--
-- Plan
-- * parse cabal.project to get all cabal files
-- ** handle globbing (easy and low-prio)
-- * parse cabal files to get libs and testsuites
-- * in Cabal.hs
--
-- This shouldn't depend on cabal for now, as we don't need
-- a full structure so simply parsers that ignore
-- most of the cabal file should do job.
--
-- * Run a Session for each of the libs (and optionally test-suites)
-- ** prefix title for each
-- ** restart when cabal file changes
-- ** restart all other sessions when one lib changes
-- *** figure out dependency graph so we can be smart about this
--       and only restart the ones that need it
-- **** algebraic-graphs
-- **** fgl
-- **** Data.Graph from containers and we don't need any extra deps
--
-- * Test-suites
-- ** for type: exitcode-stdio just run main-is: <main>.main
-- ** how to run these, hspec is easy - just run a main-is: <main>.spec
-- ** hspec/tasty-discover? maybe should just run a main from main-is
-- which typically is a driver that does the heavy-lifting
-- ** allow disabling this alltoghether or include/exclude
--
-- * Output all of these in term
-- ** should be ordered the same way as in cabal.project
-- ** might be easier/prettier with brick (but extra dep)
--
-- * Extra features
-- ** There probably is a GHC error parser somewhere
--     -> Language.Haskell.Ghcid.Parser ... ^^
-- *** This would allow us to prefix filepaths with the sub-projects dir
-- ** Benchmarks
-- ** Haddocks
--
-- * Absorb ghcid
-- ** Since it is "just" a singular ghcid-multi

module Multi where

import Control.Monad (unless, when)
import Control.Monad.Extra (whenJust)
import Data.Ord (Down(Down))
import Data.Maybe (isJust)
import Data.Tuple.Extra (both)
import Data.List (intercalate, partition, sortOn, isPrefixOf)
import Data.List.Extra (nubOrd, nubOrdOn)
import System.Console.ANSI (setTitle)
import System.Console.CmdArgs (whenLoud, whenNormal)
import System.Directory (getCurrentDirectory)
import System.IO (hFlush, stdout)
import System.Exit (exitFailure)
import System.FilePath (takeFileName)

import Session (Session, sessionStart, sessionExecAsync, sessionReload)
import Types (Options(..), TermSize, Continue(..), ReloadMode(..))
import Wait (Waiter, waitFiles)
import Term (outputFill')
import Language.Haskell.Ghcid.Escape (unescape)
import Language.Haskell.Ghcid.Types (EvalResult, Load(..), Severity(..), isMessage)
import Language.Haskell.Ghcid.Util (allGoodMessage, ghciFlagsUseful, ghciFlagsUsefulVersioned, getModTime, getShortTime, outStrLn)


runGhcids
  :: Session
  -> Waiter
  -> IO TermSize
  -> ([String] -> IO ())
  -- ^ output to terminal
  -> Options
  -> IO Continue
runGhcids session waiter termSize termOutput opts@Options{..} = do
  let limitMessages = maybe id (take . max 1) max_messages

  let outputFill :: String -> Maybe (Int, [Load]) -> [EvalResult] -> [String] -> IO ()
      outputFill = outputFill' opts termSize termOutput

  nextWait <- waitFiles waiter
  (messages, loaded) <- sessionStart session command $
     map (":set " ++) (ghciFlagsUseful ++ ghciFlagsUsefulVersioned) ++ setup

  when (null loaded && not ignoreLoaded) $ do
    putStrLn $ "\nNo files loaded, meaning ghcid will never refresh, so aborting.\nCommand: " ++ command
    exitFailure

  restart <- pure $ nubOrd $ restart ++ [x | LoadConfig x <- messages]
  -- Note that we capture restarting items at this point, not before invoking the command
  -- The reason is some restart items may be generated by the command itself
  restartTimes <- mapM getModTime restart

  project <- if project /= "" then pure project else takeFileName <$> getCurrentDirectory

  -- fire, given a waiter, the messages/loaded/touched
  let
    fire
      :: ([(FilePath, ReloadMode)] -> IO (Either String [(FilePath, ReloadMode)]))
      -> ([Load], [FilePath], [FilePath])
      -> IO Continue
    fire nextWait (messages, loaded, touched) = do
          currTime <- getShortTime
          let loadedCount = length loaded
          whenLoud $ do
              outStrLn $ "%MESSAGES: " ++ show messages
              outStrLn $ "%LOADED: " ++ show loaded

          let evals = [e | Eval e <- messages]
          let (countErrors, countWarnings) = both sum $ unzip
                  [if loadSeverity == Error then (1,0) else (0,1) | m@Message{..} <- messages, loadMessage /= []]
          let hasErrors = countErrors /= 0 || (countWarnings /= 0 && not warnings)
          test <- pure $
              if null test || hasErrors then Nothing
              else Just $ intercalate "\n" test

--          unless no_title $ setWindowIcon $
--              if countErrors > 0 then IconError else if countWarnings > 0 then IconWarning else IconOK

          let updateTitle extra = unless no_title $ setTitle $ unescape $
                  let f n msg = if n == 0 then "" else show n ++ " " ++ msg ++ ['s' | n > 1]
                  in (if countErrors == 0 && countWarnings == 0 then allGoodMessage ++ ", at " ++ currTime else f countErrors "error" ++
                     (if countErrors >  0 && countWarnings >  0 then ", " else "") ++ f countWarnings "warning") ++
                     " " ++ extra ++ [' ' | extra /= ""] ++ "- " ++ project

          updateTitle $ if isJust test then "(running test)" else ""

          -- order and restrict the messages
          -- nubOrdOn loadMessage because module cycles generate the same message at several different locations
          ordMessages <- do
              let (msgError, msgWarn) = partition ((==) Error . loadSeverity) $ nubOrdOn loadMessage $ filter isMessage messages
              -- sort error messages by modtime, so newer edits cause the errors to float to the top - see #153
              errTimes <- sequence [(x,) <$> getModTime x | x <- nubOrd $ map loadFile msgError]
              let f x = lookup (loadFile x) errTimes
                  moduleSorted = sortOn (Down . f) msgError ++ msgWarn
              pure $ (if reverse_errors then reverse else id) moduleSorted

          outputFill currTime (Just (loadedCount, ordMessages)) evals [test_message | isJust test]
          --forM_ outputfile $ \file ->
          --    writeFile file $
          --        if takeExtension file == ".json" then
          --            showJSON [("loaded",map jString loaded),("messages",map jMessage $ filter isMessage messages)]
          --        else
          --            unlines $ map unescape $ prettyOutput currTime loadedCount (limitMessages ordMessages) evals
          when (null loaded && not ignoreLoaded) $ do
              putStrLn "No files loaded, nothing to wait for. Fix the last error and restart."
              exitFailure
          whenJust test $ \t -> do
              whenLoud $ outStrLn $ "%TESTING: " ++ t
              sessionExecAsync session t $ \stderr -> do
                  whenLoud $ outStrLn "%TESTING: Completed"
                  hFlush stdout -- may not have been a terminating newline from test output
                  if "*** Exception: " `isPrefixOf` stderr then do
                      updateTitle "(test failed)"
                      --setWindowIcon IconError
                   else do
                      updateTitle "(test done)"
                      whenNormal $ outStrLn "\n...done"
          --whenJust lint $ \lintcmd ->
          --    unless hasErrors $ do
          --        (exitcode, stdout, stderr) <- readCreateProcessWithExitCode (shell . unwords $ lintcmd : map escape touched) ""
          --        unless (exitcode == ExitSuccess) $ do
          --            let output = stdout ++ stderr
          --            outStrLn output
          --            forM_ outputfile $ flip writeFile output

          reason <- nextWait $ map (,Restart) restart
                            ++ map (,Reload) reload
                            ++ map (,Reload) loaded

          let reason1 = case reason of
                Left err ->
                  (Reload, ["Error when waiting, if this happens repeatedly, raise a ghcid bug.", err])
                Right files ->
                  case partition (\(f, mode) -> mode == Reload) files of
                    -- Prefer restarts over reloads. E.g., in case of both '--reload=dir'
                    -- and '--restart=dir', ghcid would restart instead of reload.
                    (_, rs@(_:_)) -> (Restart, map fst rs)
                    (rl, _) -> (Reload, map fst rl)

          currTime <- getShortTime
          case reason1 of
            (Reload, reason2) -> do
              unless no_status $ outputFill currTime Nothing evals $ "Reloading..." : map ("  " ++) reason2
              nextWait <- waitFiles waiter
              fire nextWait =<< sessionReload session
            (Restart, reason2) -> do
              -- exit cleanly, since the whole thing is wrapped in a forever
              unless no_status $ outputFill currTime Nothing evals $ "Restarting..." : map ("  " ++) reason2
              pure Continue

  fire nextWait (messages, loaded, loaded)

  pure Continue
