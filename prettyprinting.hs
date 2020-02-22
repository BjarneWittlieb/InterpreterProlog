module Prettyprinting where

import Data.List

import Type

class Pretty a where
  pretty :: a -> String

-- Hier fehlen Kommentare
instance Pretty Term where
  pretty (Var v) = v
  pretty (Comb c []) = c
  pretty (Comb "." [Comb c1 xs, Comb "[]" []]) = "[" ++ (pretty (Comb c1 xs)) ++ "]"
  pretty (Comb "." [Comb c1 xs, Comb "." ys]) = "[" ++ (intercalate ", " (c1 : toList (Comb "." ys))) ++ "]" where
    toList :: Term -> [String]
    toList (Comb "." [Comb c1 xs, Comb "[]" []]) = [pretty (Comb c1 xs)]
    toList (Comb "." [Comb _ xs, Comb "." _]) = (fmap pretty xs)
    toList (Comb "." [x, y]) = [(pretty x) ++ "|" ++ (pretty y)]
    toList x = [pretty x]
  pretty (Comb "." [x, y]) = "[" ++ (pretty x) ++ "|" ++ (pretty y) ++ "]"
  pretty (Comb c xs) = c ++ "(" ++ (intercalate ", " (fmap pretty xs)) ++ ")"

