module BracketPush where

import Data.Array (foldl)
import Data.Maybe (Maybe(..))
import Data.Stack (Stack, stackIsEmpty, stackNew, stackPop, stackPush)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Prelude (($), (==), (||))

stackBrackets :: Maybe (Stack Char) -> Char -> Maybe (Stack Char)
stackBrackets Nothing _ = Nothing
stackBrackets (Just stack) c
  | c == '[' || c == '(' || c == '{' = Just $ stackPush stack c
stackBrackets (Just stack) c =
  case c of
    ']' -> case (stackPop stack) of
      Just (Tuple poppedStack nc) -> if '[' == nc then Just $ poppedStack else Nothing
      Nothing -> Nothing
    '}' -> case (stackPop stack) of
      Just (Tuple poppedStack nc) -> if '{' == nc then Just $ poppedStack else Nothing
      Nothing -> Nothing
    ')' -> case (stackPop stack) of
      Just (Tuple poppedStack nc) -> if '(' == nc then Just $ poppedStack else Nothing
      Nothing -> Nothing
    otherwise -> Just stack

isPaired :: String -> Boolean
isPaired pairs = case foldl stackBrackets (Just (stackNew :: Stack Char)) $ toCharArray pairs of
  Just stack -> stackIsEmpty stack
  Nothing -> false
