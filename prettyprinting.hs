module Prettyprinting where

import Data.List

import Type

class Pretty a where
  pretty :: a -> String

-- Hier fehlen Kommentare
instance Pretty Term where
  pretty (Var v) = v
  pretty (Comb c []) = c
  pretty (Comb "." [Comb c xs, Comb "[]" []]) = "[" ++ (pretty (Comb c xs)) ++ "]"
  pretty (Comb "." [Comb c xs, Comb "." ys]) = "[" ++ (intercalate ", " (c : toList (Comb "." ys))) ++ "]" where 
   toList :: Term -> [String]
   toList (Comb "." [Comb c xs, Comb "[]" []]) = [pretty (Comb c xs)]
   toList (Comb "." [Comb c xs, Comb "." ys]) = (pretty (Comb c xs)):(toList (Comb "." ys))
   toList (Comb "." [x, y]) = [(pretty x) ++ "|" ++ (pretty y)]
   toList x = [pretty x]
  pretty (Comb "." [x, y]) = "[" ++ (pretty x) ++ "|" ++ (pretty y) ++ "]"
  pretty (Comb c xs) = c ++ "(" ++ (intercalate ", " (fmap pretty xs)) ++ ")" 

instance Pretty Rule where
  pretty (Rule t ts) = (pretty t) ++ " :- " ++ (intercalate ", " (fmap pretty ts))

instance Pretty Prog where
  pretty (Prog rs) = intercalate "\n" (fmap pretty rs)

instance Pretty Goal where
  pretty (Goal ts) = intercalate ", " (fmap pretty ts) 
  
