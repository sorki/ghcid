{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: deps
module Cabal
  ( Subproject(..)
  , TargetType(..)
  , parseSubprojectsWithTargets
  ) where

import Data.Map (Map)
import qualified Data.Char
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.IO
import qualified System.FilePath

data Subproject = Subproject
  { subprojectName :: String
  , subprojectDir :: FilePath
  , subprojectFullCabalPath :: FilePath
  } deriving (Eq, Ord, Show)

parseSubprojectsWithTargets :: IO (Map Subproject [(TargetType, Maybe String)])
parseSubprojectsWithTargets = do
  cs <- parseCabalsFromProject
  Data.Map.fromList
    <$> mapM (\c ->
          parseTargetsFromCabal c
          >>= \t -> pure (mkSubproject c, t))
        cs
  where
    mkSubproject fp = Subproject
      { subprojectName = subprojectName fp
      , subprojectDir = System.FilePath.takeDirectory fp
      , subprojectFullCabalPath = fp
      }
    subprojectName fp =
        Data.Maybe.fromMaybe (error "cant extract subproject name from: " ++ fp)
      . System.FilePath.stripExtension ".cabal"
      $ System.FilePath.takeFileName fp

-- | Read cabal.project and extract
-- a list of *.cabal files
-- TODO(srk): expand globs
parseCabalsFromProject :: IO [FilePath]
parseCabalsFromProject = do
  ls <-
      map Data.Text.strip
    . dropWhile (not . ("packages:"`Data.Text.isPrefixOf`))
    . Data.Text.lines
    <$> Data.Text.IO.readFile "cabal.project"
  pure $ map Data.Text.unpack $ case ls of
    [] -> error "No cabal.project or no packages: stanza in it"
    ("packages:":ls) -> takeWhile (".cabal"`Data.Text.isSuffixOf`) ls
    (onOneLine:_) ->
        Data.Text.words
      . Data.Text.strip
      . Data.Maybe.fromMaybe (error "No cabal files after packages: stanza")
      $ Data.Text.stripPrefix "packages:" onOneLine

data TargetType =
    Benchmark
  | Executable
  | Library
  | Testsuite
  deriving (Bounded, Eq, Enum, Ord, Show)

toCabalStanza :: TargetType -> String
toCabalStanza Testsuite = "test-suite"
toCabalStanza x = map Data.Char.toLower $ show x

fromCabalStanza :: String -> TargetType
fromCabalStanza "benchmark" = Benchmark
fromCabalStanza "executable" = Executable
fromCabalStanza "library" = Library
fromCabalStanza "test-suite" = Testsuite
fromCabalStanza x = error $ "Unknown cabal target type: " ++ x

allTargetTypes :: [TargetType]
allTargetTypes = [minBound .. maxBound]

-- | Read cabal file and extract targets from it
parseTargetsFromCabal
  :: FilePath
  -> IO [(TargetType, Maybe String)]
parseTargetsFromCabal cabalPath = do
      map
        (\case
          [x] -> (fromCabalStanza x, Nothing)
          [x, name] -> (fromCabalStanza x, Just name)
          x -> error $ "Weird cabal stanza: " ++ show x
        )
    . map (fmap Data.Text.unpack)
    . filter (\case
        (x:xs) -> x `elem` (map (Data.Text.pack . toCabalStanza) allTargetTypes)
        _ -> False
      )
    . map (Data.Text.words . Data.Text.strip)
    . Data.Text.lines
    <$> Data.Text.IO.readFile cabalPath
