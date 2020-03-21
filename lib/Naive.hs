module Naive where

{- | A naive lambda calculus interpreter.

Construct a variable "x":

>>> Var "x"
Var "x"

Construct the identity function:

>>> Abstr "x" (Var "x")
Abstr "x" (Var "x")

Apply the identity function to "y":

>>> App (Abstr "x" (Var "x")) (Var "y")
App (Abstr "x" (Var "x")) (Var "y")

Beta reduce it:

>>> reduce $ App (Abstr "x" (Var "x")) (Var "y")
Var "y"

This is naive though, in that it does not rename variables to avoid capture.
For example:

>>> reduce $ App (Abstr "x" (Abstr "y" (App (Var "x") (Var "y")))) (Var "y")
Abstr "y" (App (Var "y") (Var "y"))

-} 

{- | For convenience/clarity. -}
type VarName = String

{- | Lambda terms. -}
data Term =
    Var VarName        -- ^Ex: 'Var "x"'
  | Abstr VarName Term -- ^Ex: 'Abstr "x" (Var "x")'
  | App Term Term      -- ^Ex: 'App (Var "x") (Var "y")'
  deriving (Show)

{- | 'subst "x" m n' will replace every "x" in 'n' with 'm'. -}
subst :: VarName -> Term -> Term -> Term
subst name term term' =
  case term' of
    Var name'
      | name == name' -> term
      | otherwise -> term'
    Abstr name' term'' ->
      Abstr name' (subst name term term'')
    App term'' term''' ->
      App (subst name term term'') (subst name term term''')

{- | Performs left-most (lazy) beta reduction on a term. -}
reduceOnce :: Term -> Term
reduceOnce term = 
  case term of
    App (Abstr name term') term'' -> subst name term'' term'
    _ -> term

{- | Reduce repeatedly until no more reductions can be done. -}
reduce :: Term -> Term
reduce term =
  let result = reduceOnce term
  in case result of
    App (Abstr name term') term'' -> reduce result
    _ -> result
