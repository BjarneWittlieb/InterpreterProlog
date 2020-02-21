module Prettyprinting where

import Data.List

import Type

class Pretty a where
  pretty :: a -> String

instance Pretty Term where
  -- if the Term is a Var, just return the varName
  pretty (Var v) = v
  -- if the Term is a constant, return it
  pretty (Comb c []) = c
  -- if the term is an array with only one element, return the element in brackets []
  pretty (Comb "." [Comb c xs, Comb "[]" []]) = "[" ++ (pretty (Comb c xs)) ++ "]"
  -- -if the term is an arry with several elements, get the list with the toList function
  pretty (Comb "." [Comb f ts, Comb "." zs]) = "[" ++ (intercalate ", " (toList (Comb "." [Comb f ts, Comb "." zs]))) ++ "]" where 
    toList :: Term -> [String]
    -- if the list has only one element, return a list with that element
    toList (Comb "." [Comb c xs, Comb "[]" []]) = [pretty (Comb c xs)]
    -- if the list has more than one element, go through the list one by one
    toList (Comb "." [Comb c xs, Comb "." ys]) = (pretty (Comb c xs)):(toList (Comb "." ys))
    -- if the second argument isn't another list, it's a list comprehension
    toList (Comb "." [x, y]) = [(pretty x) ++ "|" ++ (pretty y)]
    -- otherwise it's a function, so just call pretty
    toList x = [pretty x]
  -- if the second argument isn't another list, it's a list comprehension
  pretty (Comb "." [x, y]) = "[" ++ (pretty x) ++ "|" ++ (pretty y) ++ "]"
  -- otherwise it's a function, so put the function name first and the arguments in brackets ()
  pretty (Comb c xs) = c ++ "(" ++ (intercalate ", " (fmap pretty xs)) ++ ")" 

instance Pretty Rule where
  pretty (Rule t ts) = (pretty t) ++ " :- " ++ (intercalate ", " (fmap pretty ts)) ++ "."

instance Pretty Prog where
  pretty (Prog rs) = intercalate "\n" (fmap pretty rs)

instance Pretty Goal where
  pretty (Goal ts) = intercalate ", " (fmap pretty ts) 
  
