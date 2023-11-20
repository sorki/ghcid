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

import Session
import Types
import Wait

runGhcids
  :: Session
  -> Waiter
  -> IO TermSize
  -> ([String] -> IO ())
  -- ^ output to terminal
  -> Options
  -> IO Continue
runGhcids = undefined
