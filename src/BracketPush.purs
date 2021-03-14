module BracketPush where

import Data.Array (foldl, snoc, null, last, tail, singleton, fromFoldable, take, length, init)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Prelude (($), (==), (||), (/), (+), (-))

bracketToInt :: Char -> Maybe Int
bracketToInt '(' = Just 1
bracketToInt '[' = Just 2
bracketToInt '{' = Just 3
bracketToInt ')' = Just 4
bracketToInt ']' = Just 5
bracketToInt '}' = Just 6
bracketToInt _ = Nothing

stackBrackets :: Maybe (Array Int) -> Char -> Maybe (Array Int)
stackBrackets Nothing _ = Nothing
stackBrackets (Just brackets) c
  | c == '[' || c == '(' || c == '{' = case (bracketToInt c) of
    Just bracketInt -> Just $ snoc brackets bracketInt
    Nothing -> Nothing
stackBrackets (Just brackets) c =
  case { l: last brackets, bi: (bracketToInt c) } of
    {l: _, bi: Nothing} -> Just brackets
    {l: Just lastBracket, bi: Just bracketInteger} -> if (lastBracket + 3) == bracketInteger then init brackets else Nothing
    _ -> Nothing

isPaired :: String -> Boolean
isPaired pairs = case foldl stackBrackets (Just ([] :: Array Int)) $ toCharArray pairs of
  Just brackets -> null brackets
  Nothing -> false
