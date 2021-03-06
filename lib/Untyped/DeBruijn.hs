module Untyped.DeBruijn (
    Name
  , mkFree
  , mkAbstr
  , mkApp
  , reduce ) where

{- | A lambda calculus interpreter, using de Bruijn indexes.

Construct a free variable:

>>> mkFree "x"
Free "x"

Construct the identity function:

>>> mkAbstr "x" (mkFree "x")
Abstr (Idx 1)

Apply the free variable "y" to the identity function:

>>> mkApp (mkAbstr "x" (mkFree "x")) (mkFree "y")
App (Abstr (Idx 1)) (Free "y")

Reduce it:

>>> reduce $ mkApp (mkAbstr "x" (mkFree "x")) (mkFree "y")
Free "y" 

-}

{- For convenience/clarity. -}
type Name = String

{- Lambda terms. -}
data Term =
    Free Name      -- ^A free variable.
  | Idx Int        -- ^A de Bruijn index.
  | Abstr Term     -- ^An abstraction (with de Bruijn indexes).
  | App Term Term  -- ^An application.
  deriving (Show)

{- Constructs a free variable term. -}
mkFree :: Name -> Term
mkFree name = Free name

{- Constructs a de Bruijn index. -}
mkIdx :: Int -> Term
mkIdx i = Idx i

{- Constructs an abstraction term. For example, 'mkAbstr "x" t' abstracts 
the free variable "x" in term 't'. The result converts 'x' to de Bruijn 
indices in 't'. -}
mkAbstr :: Name -> Term -> Term
mkAbstr name term = Abstr $ indices 1 name term

{- Constructs an application term. -} 
mkApp :: Term -> Term -> Term
mkApp term term' = App term term'

{- Converts a term into a de Bruijn indexed version. For example, 
'indices 1 "x" t' recursively replaces free variables "x" in term 't',
starting at depth 1. -}
indices :: Int -> Name -> Term -> Term
indices depth name term =
  case term of
    Free name'
      | name == name' -> Idx depth
      | otherwise -> term
    Idx _ -> term
    Abstr body ->
      let depth' = depth + 1
      in Abstr $ indices depth' name body
    App term1 term2 ->
      App (indices depth name term1) (indices depth name term2)

{- Substitutes one term for another. For example, 'subst 1 t1 t2' 
substituse the term 't1' in the term 't2', at depth 1. -}
subst :: Int -> Term -> Term -> Term
subst idx term term' =
  case term' of
    Free name -> term'
    Idx i
      | i == idx -> term
      | otherwise -> term'
    Abstr body -> Abstr $ subst (idx + 1) term body
    App term1 term2 ->
      App (subst idx term term1) (subst idx term term2)

{- (Left-most) reduce a term one time. -}
reduceOnce :: Term -> Term
reduceOnce term =
  case term of
    App (Abstr body) arg -> subst 1 arg (body)
    _ -> term

{- Reduce a term repeatedly until no more reductions can be done. -}
reduce :: Term -> Term
reduce term =
  let result = reduceOnce term
  in case result of
    App (Abstr _) _ -> reduce result
    _ -> result
