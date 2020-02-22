 module Prettyprinting where

import Data.List

import Type

class Pretty a where
  pretty :: a -> String


-- TODO zweistellige prädikate korrekt verarbeiten
instance Pretty Term where
  -- if the Term is a Var, just return the varName
  pretty (Var v) = v
  -- if the Term is a constant, return it
  pretty (Comb c []) = c
  pretty (Comb "." [a, b]) = "[" ++ (intercalate ", " (toList (a, b))) ++ "]" where
    toList :: (Term, Term) -> [String]
    toList (x, Comb "[]" []) = [pretty x]
    toList (x, Comb "." [y, z]) = (pretty x):(toList (y, z))
    toList (x, y) = [(pretty x) ++ "|" ++ (pretty y)]
  -- otherwise it's a function, so put the function name first and the arguments in brackets ()
  pretty (Comb c xs) = c ++ "(" ++ (intercalate ", " (fmap pretty xs)) ++ ")" 

instance Pretty Rule where
  pretty (Rule t ts) = (pretty t) ++ " :- " ++ (intercalate ", " (fmap pretty ts)) ++ "."

instance Pretty Prog where
  pretty (Prog rs) = intercalate "\n" (fmap pretty rs)

instance Pretty Goal where
  pretty (Goal ts) = intercalate ", " (fmap pretty ts) 

