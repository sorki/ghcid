module Term
  ( prettyOutput
  ) where

import Language.Haskell.Ghcid.Types (EvalResult(..), Load, loadMessage)
import Language.Haskell.Ghcid.Util (allGoodMessage)

-- | Given an available height, and a set of messages to display, show them as best you can.
prettyOutput :: String -> Int -> [Load] -> [EvalResult] -> [String]
prettyOutput currTime loadedCount [] evals =
    (allGoodMessage ++ " (" ++ show loadedCount ++ " module" ++ ['s' | loadedCount /= 1] ++ ", at " ++ currTime ++ ")")
        : concatMap printEval evals
prettyOutput _ _ xs evals = concatMap loadMessage xs ++ concatMap printEval evals

printEval :: EvalResult -> [String]
printEval (EvalResult file (line, col) msg result) =
  [ " "
    , concat
        [ file
        , ":"
        , show line
        , ":"
        , show col
        ]
    ] ++ map ("$> " ++) (lines msg)
      ++ lines result
