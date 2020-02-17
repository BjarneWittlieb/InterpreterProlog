import Data.List

-- Alias type for variables
type VarName = String

-- Alias type for combinators
type CombName = String

-- Data type for terms
data Term = Var VarName | Comb CombName [Term]
  deriving Show

-- Data type for program rules
data Rule = Rule Term [Term]
  deriving Show

-- Data type for programs
data Prog = Prog [Rule]
  deriving Show

-- Data type for goals
data Goal = Goal [Term]
  deriving Show
  
class Pretty a where
  pretty :: a -> String

instance Pretty Term where
  -- pretty :: Term -> String
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
  
