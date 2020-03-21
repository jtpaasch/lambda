module Lambda where

{- | A lambda calculus interpreter.

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

To prevent variable capture, bound variables are renamed:

>>> reduce $ App (Abstr "x" (Abstr "y" (App (Var "x") (Var "y")))) (Var "y")
Abstr "a1" (App (Var "y") (Var "a1"))

-}

{- | For convenience/clarity. -}
type VarName = String

{- | A simple container to hold a fresh name. -}
data Names = Names { 
    counter :: Int
  , name :: VarName
  } deriving (Show)

{- | Initialize a 'Names' container. -}
initialize :: unit -> Names
initialize _ = Names { counter = 0, name = "a0" }

{- | Freshen the name in the 'Fresh' container. -}
freshen :: Names -> Names
freshen names = 
  let new_counter = (counter names) + 1
      new_name = "a" ++ (show new_counter)
  in Names { counter = new_counter, name = new_name }

{- | Lambda terms. -}
data Term =
    Var VarName
  | Abstr VarName Term
  | App Term Term
  deriving (Show)

{- | Renames all bound variables to avoid conflict. -}
rename :: Names -> VarName -> VarName -> Term -> Term
rename names old new term =
  case term of
    Var name'
      | name' == old -> Var new
      | otherwise -> term
    Abstr name' term' ->
      let new' = name names
          names' = freshen names
          term'' = (rename names' name' new' term')
      in Abstr new' (rename names' old new term'') 
    App term' term'' ->
      App (rename names old new term') (rename names old new term'')

{- | 'subst "x" m n' replaces every "x" with 'm' in 'n'. -}
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

{- | Left-most reduces once. -}
reduceOnce :: Term -> Term
reduceOnce term =
  case term of
    App (Abstr name term') term'' -> subst name term'' term'
    _ -> term

{- | Reduce repeatedly until no more reductions can be done. -}
reduceFixpoint :: Term -> Term
reduceFixpoint term =
  let result = reduceOnce term
  in case result of
    App (Abstr name term') term'' -> reduceFixpoint result
    _ -> result 

{- | Rename bound variables, then reduce. -}
reduce :: Term -> Term
reduce term =
  let names = initialize ()
      renamed_term = rename names "" "" term
  in reduceFixpoint renamed_term
