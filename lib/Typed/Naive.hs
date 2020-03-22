module Typed.Naive where

{- | A simply typed lambda calculus interpreter.

Create a context:

>>> let ctx = mkCtx [("x", Bool), ("y", Bool), ("z", Foo)]

Check the type of "x":

>>> getType ctx (Var "x")
Right Bool

Check the type of "w" (which is not defined in the context):

>>> getType ctx (Var "w")
Left Cannot assign type to: Var "w"

Reduce a term:

>>> reduce ctx $ App (Abstr "x" Bool (Var "x")) (Var "y")
Right (Var "y")

-}

import qualified Data.Map as Map

{- | For convenience/clarity. -}
type Name = String

{- | Types that terms can have. -}
data Type =
    Bool             -- ^For booleans.
  | Foo              -- ^Just some other arbitrary type.
  | Arrow Type Type  -- ^Function/arrow types.
  deriving (Show, Eq)

{- | Terms for the calculus. -}
data Term =
    Tt                    -- ^True (Bool)
  | Ff                    -- ^False (Bool)
  | Var Name              -- ^A free variable
  | Abstr Name Type Term  -- ^Abstract a typed variable in a term.
  | App Term Term         -- ^Application
  deriving (Show)

{- | Custom errors. -}
data Error =
    NoType Term                -- ^Cannot type a term.
  | NotAbstr Term              -- ^Expected an abstr on the left of an app.
  | BadArgType Term Type Type  -- ^An arg has one type, but must have another.

{- | Some custom messages to make the above errors clearer. -}
instance Show Error where
  show (NoType term) = "Cannot assign type to: " ++ (show term) 
  show (NotAbstr term) = "Expected abstraction on left in: " ++ (show term)
  show (BadArgType term binding binding') =
    "Argument '" ++ (show term)  ++ "' " ++
    "has type '" ++ (show binding) ++ "' " ++
    "but must have type '" ++ (show binding') ++ "'" 

{- | The context maps names to types. -}
type Context = Map.Map Name Type

{- | Constructs a context from a list of '(name, type)' pairs. -}
mkCtx :: [(Name, Type)] -> Context
mkCtx entries = Map.fromList entries

{- | Looks up the type bound to a name (if there is one) in the context. -}
getBinding :: Context -> Name -> Maybe Type
getBinding ctx name = Map.lookup name ctx

{- | Gets the type of a term, given a context. Or returns an error. -}
getType :: Context -> Term -> Either Error Type
getType ctx term =
  case term of
    Tt -> Right Bool
    Ff -> Right Bool
    Var name ->
      case getBinding ctx name of
        Just binding -> Right binding
        Nothing -> Left $ NoType term
    Abstr name binding body ->
      case getType ctx body of
        Right binding' -> Right $ Arrow binding binding'
        Left err -> Left err
    App term' term'' ->
      case term' of
        Abstr name binding body ->
          case getType ctx term'' of
            Right binding' ->
              case binding == binding' of
                False -> Left $ BadArgType term'' binding' binding
                True -> getType ctx body
            Left err -> Left err
        _ -> Left $ NotAbstr term

{- | Substitute a term for a name (bound to a type) in another term. 
For example, 'subst "x" binding term1 term2' will put 'term1' in
the place of every variable '"x"' (with type 'binding') in 'term2'. -} 
subst :: Name -> Type -> Term -> Term -> Term
subst name binding term term' =
  case term' of
    Var name'
      | name == name' -> term
      | otherwise -> term'
    Abstr name' binding' body ->
      Abstr name' binding' (subst name binding term body)
    App term'' term''' ->
      App (subst name binding term term'') 
          (subst name binding term term''')

{- | (Left-most) reduce a term one time. -}
reduceOnce :: Context -> Term -> Either Error Term
reduceOnce ctx term =
  case getType ctx term of
    Left err -> Left err
    Right _ ->
      case term of
        App (Abstr name binding body) term' ->
          Right $ subst ctx name binding term' body
        _ -> Right term

{- | Reduce a term repeatedly until no more reductions can be done. -}
reduce :: Context -> Term -> Either Error Term
reduce ctx term =
  let reducedTerm = reduceOnce ctx term
  in case reducedTerm of
    Left err -> Left err
    Right term' ->
      case term' of
        App (Abstr name binding body) term'' -> reduce ctx term'
        _ -> Right term'
