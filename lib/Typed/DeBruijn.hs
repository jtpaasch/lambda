module Typed.DeBruijn where

{- | A simply typed lambda calculus interpreter, using de Bruijn indexes.

Specify a context:

>>> let ctx = mkCtx [("x", Bool), ("y", Bool), ("z", Foo)]

Construct a free variable:

>>> mkFree "x"
Free "x"

Construct the identity function for bools:

>>> let f = mkAbstr "x" Bool (mkFree "x")

This is converted into a de Bruijn indexed term:

>>> f
Abstr Bool (Idx 1)

Apply that function to an argument:

>>> let arg = mkFree "y"
>>> let t = mkApp f arg
>>> t
App (Abstr Bool (Idx 1)) (Free "y")

Type check the term:

>>> getType ctx (initStack) t 
Right Bool

Reduce it:

>>> reduce ctx t
Right (Free "y")

-}

import qualified Data.Map as Map

{- | For convenience/clarity. -}
type Name = String
type Index = Int

{- | Types that terms can have. -}
data Type =
    Bool             -- ^For booleans.
  | Foo              -- ^Just some other arbitrary type.
  | Arrow Type Type  -- ^Function/arrow types.
  deriving (Show, Eq)

{- | Terms for the calculus. -}
data Term =
    Tt                    -- ^True (Bool).
  | Ff                    -- ^False (Bool).
  | Free Name             -- ^A free variable.
  | Idx Index             -- ^A de Bruijn index.
  | Abstr Type Term       -- ^An abstraction (with de Bruijn indexes).
  | App Term Term         -- ^An application.
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

{- Constructs a free variable term. -}
mkFree :: Name -> Term
mkFree name = Free name

{- Constructs a de Brujin index. -}
mkIdx :: Index -> Term
mkIdx idx = Idx idx

{- Constructs an abstraction term. For example, 'mkAbstr "x" t' abstracts
the free variable "x" in the term 't'. the result converts 'x' to de Bruijn
indices in 't'. -}
mkAbstr :: Name -> Type -> Term -> Term
mkAbstr name binding term = Abstr binding $ indices 1 name term

{- Constructs an application term. -}
mkApp term term' = App term term'

{- | The context maps names to types. -}
type Context = Map.Map Name Type

{- | Constructs a context from a list of '(name, type)' pairs. -}
mkCtx :: [(Name, Type)] -> Context
mkCtx entries = Map.fromList entries

{- | Looks up the type bound to a name (if there is one) in the context. -}
getNameBinding :: Context -> Name -> Maybe Type
getNameBinding ctx name = Map.lookup name ctx

{- | A stack maps de Bruijn indices to types. -}
type Stack = Map.Map Index Type

{- | Constructs an empty 'Stack'. -}
initStack :: Stack
initStack = Map.fromList []

{- | Looks up the type for a given de Bruijn index. -}
getIndexBinding :: Stack -> Index -> Maybe Type
getIndexBinding stack idx = Map.lookup idx stack

{- | Pushes a new de Bruijn index (and its type) onto the stack.
This increments all other de Bruijn indexes (and their types) by 1. -}
pushIndexBinding :: Stack -> Index -> Type -> Stack
pushIndexBinding stack idx binding =
  let stack' = Map.mapKeys (\k -> k + 1) stack
  in Map.insert idx binding stack'

{- | Gets the type of a term. Or returns an error. -}
getType :: Context -> Stack -> Term -> Either Error Type
getType ctx stack term =
  case term of
    Tt -> Right Bool
    Ff -> Right Bool
    Free name ->
      case getNameBinding ctx name of
        Just binding -> Right binding
        Nothing -> Left $ NoType term
    Idx idx ->
      case getIndexBinding stack idx of
        Just binding -> Right binding
        Nothing -> Left $ NoType term
    Abstr binding body ->
      let stack' = pushIndexBinding stack 1 binding
      in case getType ctx stack' body of
        Right binding' -> Right $ Arrow binding binding'
        Left err -> Left err
    App term1 term2 ->
      case term1 of
        Abstr binding body ->
          case getType ctx stack term2 of
            Right binding' ->
              case binding == binding' of
                False -> Left $ BadArgType term2 binding' binding
                True ->
                  let stack' = pushIndexBinding stack 1 binding 
                  in getType ctx stack' body
            Left err -> Left err
        _ -> Left $ NotAbstr term

{- Converts a term into a de Bruijn indexed version. For example,
'indices 1 'x" t' recursively replaces free variables "x" in term 't',
starting at de Bruijn index 1. -}
indices :: Index -> Name -> Term -> Term
indices idx name term =
  case term of
    Free name'
      | name == name' -> Idx idx
      | otherwise -> term
    Idx _ -> term
    Abstr binding body ->
      Abstr binding $ indices (idx + 1) name body
    App term1 term2 ->
      App (indices idx name term1) (indices idx name term2)

{- Substitute one term for another. For example, 'subst 1 t1 t2'
recursively substituse the term 't1' in the term 't2', starting 
at de Bruijn index 1. -}
subst :: Index -> Term -> Term -> Term
subst idx replacement target =
  case target of
    Free name -> target
    Idx i
      | i == idx -> replacement
      | otherwise -> target
    Abstr binding body -> Abstr binding $ subst (idx + 1) replacement body
    App term1 term2 ->
      App (subst idx replacement term1) (subst idx replacement term2)

{- | (Left-most) reduce a term one time. -}
reduceOnce :: Context -> Term -> Either Error Term
reduceOnce ctx term =
  case getType ctx initStack term of
    Left err -> Left err
    Right _ ->
      case term of
        App (Abstr binding body) arg ->
          Right $ subst 1 arg body
        _ -> Right term

{- | Reduce a term repeatedly until no more reductions can be done. -}
reduce :: Context -> Term -> Either Error Term
reduce ctx term =
  let reducedTerm = reduceOnce ctx term
  in case reducedTerm of
    Left err -> Left err
    Right result ->
      case result of
        App (Abstr _ _) _ -> reduce ctx result
        _ -> Right result
