module Type
  ( VarName, CombName, Term(Var, Comb), Rule(Rule), Prog(Prog), Goal(Goal)
  ) where

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
