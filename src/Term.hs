{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Term
  ( prettyOutput
  , outputFill'
  ) where

import Language.Haskell.Ghcid.Escape (Esc(..), WordWrap(WrapSoft), wordWrapE)
import Language.Haskell.Ghcid.Types (EvalResult(..), Load, loadMessage, isMessage)
import Language.Haskell.Ghcid.Util (allGoodMessage, takeRemainder)

import Types (TermSize(..), Options(Options, reverse_errors))

outputFill'
  :: Options
  -> IO TermSize -- ^ get terminal size
  -> ([String] -> IO ()) -- ^ output to terminal
  -> String
  -> Maybe (Int, [Load])
  -> [EvalResult]
  -> [String]
  -> IO ()
outputFill' Options{reverse_errors} termSize termOutput currTime load evals msg = do
    load <- pure $ case load of
        Nothing -> []
        Just (loadedCount, msgs) -> prettyOutput currTime loadedCount (filter isMessage msgs) evals
    TermSize{..} <- termSize
    let wrap = concatMap (wordWrapE termWidth (termWidth `div` 5) . Esc)
    (msg, load, pad) <-
        case termHeight of
            Nothing -> pure (wrap msg, wrap load, [])
            Just termHeight -> do
                (termHeight, msg) <- pure $ takeRemainder termHeight $ wrap msg
                (termHeight, load) <-
                    let takeRemainder' =
                            if reverse_errors
                            then -- When reversing the errors we want to crop out
                                 -- the top instead of the bottom of the load
                                 fmap reverse . takeRemainder termHeight . reverse
                            else takeRemainder termHeight
                    in pure $ takeRemainder' $ wrap load
                pure (msg, load, replicate termHeight "")
    let mergeSoft ((Esc x,WrapSoft):(Esc y,q):xs) = mergeSoft $ (Esc (x++y), q) : xs
        mergeSoft ((x,_):xs) = x : mergeSoft xs
        mergeSoft [] = []

        applyPadding x =
            if reverse_errors
            then pad ++ x
            else x ++ pad
    termOutput $ applyPadding $ map fromEsc ((if termWrap == WrapSoft then mergeSoft else map fst) $ load ++ msg)

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
