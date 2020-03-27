module Typed.DeBruijn where

{- An interpreter for extended typed lambda calculus.

This version uses de Bruijn indices.

This calculus allows custom base types, options, and records,
as well as the usual free variables, abstraction, and application.

To create custom types, there are two steps:

1. Create your type and its inhabitants.
2. Register those types and inhabitants with the calculus,
   using the generic 'Type' and 'Term' constructors.

Examples are at the bottom of the file. You can experiment
with them by loading this file into GHCi.

-}

import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

{- | For convenience/clarity. -}
type Name = String
type Value = String
type Index = Int


-- BASE TYPES ---------------------------------------

{- | A base type will have a name. -}
data BaseType = BaseType {
    name :: Name
  } deriving (Eq, Ord)

instance Show BaseType where
  show binding = (name binding)

{- | An inhabitant has a value, and the base type it's bound to. -}
data BaseTerm = BaseTerm {
    value :: Value
  , termBinding :: BaseType
  } deriving (Eq, Ord)

instance Show BaseTerm where
  show t = value t

{- | Creates a base type with a given name. -}
mkBaseType :: Name -> BaseType
mkBaseType name = BaseType { name = name }

{- | Creates a term with the given value and base type. -}
mkBaseTerm :: Value -> BaseType -> BaseTerm
mkBaseTerm value binding =
  BaseTerm { value = value, termBinding = binding }


-- OPTIONS ------------------------------------------

{- | An option type is bound to another type. -}
data OptionType = OptionType {
    optionBinding :: Type
  } deriving (Eq, Ord)

instance Show OptionType where
  show binding = "Optional " ++ (show $ optionBinding binding)

{- | You can select either nothing, or precisely some particular term. -}
data Selection =
    None
  | Precisely Term
  deriving (Eq, Ord)

instance Show Selection where
  show None = "Nothing"
  show (Precisely t) = "Precisely " ++ (show t)

{- | An inhabitant has a selection, and the option type it's bound to. -}
data OptionTerm = OptionTerm {
    selection :: Selection
  , selectionBinding :: OptionType
  } deriving (Eq, Ord)

instance Show OptionTerm where
  show t = show $ selection t

{- | Takes a type and creates an optional version of it. -}
mkOptionType :: Type -> OptionType
mkOptionType binding = OptionType { optionBinding = binding }

{- | Creates an option term, given a selection and an optional type. -}
mkOptionTerm :: Selection -> OptionType -> OptionTerm
mkOptionTerm option binding = 
  OptionTerm { selection = option, selectionBinding = binding }


-- RECORDS ------------------------------------------

{- | Labels have a name and a type. -}
data Label = Label {
    label :: Name,
    labelBinding :: Type
  } deriving (Eq, Ord)

instance Show Label where
  show l = (label l)

{- | Fields assign a term to a label. -}
data Field = Field {
    field :: Term
  , fieldBinding :: Label
  } deriving (Eq, Ord)

instance Show Field where
  show f = (show $ fieldBinding f) ++ " = " ++ (show $ field f)

{- | Creates a label with the given name and type. -}
mkLabel :: Name -> Type -> Label
mkLabel name binding = Label { label = name, labelBinding = binding }

{- | Creates a field with the given term and label -}
mkField :: Term -> Label -> Field
mkField t l = Field { field = t, fieldBinding = l }

{- A record type is a set of labels. -}
data RecordType = RecordType {
    labels :: Set.Set Label
  } deriving (Eq, Ord)

instance Show RecordType where
  show binding = 
    let fmt l = (show l) ++ " : " ++ (show $ labelBinding l)
        ls = Set.toList $ labels binding
        labelStr = map fmt ls
    in "{" ++ (intercalate ", " labelStr) ++ "}"

{- | A record is a set of fields for a record type. -}
data RecordTerm = RecordTerm {
    fields :: Set.Set Field
  , recordBinding :: RecordType
  } deriving (Eq, Ord)

instance Show RecordTerm where
  show t = 
    let fds = Set.toList $ fields t
        binding = recordBinding t
    in "{" ++ (intercalate ", " $ map show fds) ++ "}"

{- | Takes a set of labels and constructs a record type of them. -}
mkRecordType :: [Label] -> RecordType
mkRecordType labels = RecordType { labels = Set.fromList labels }

{- | Takes a list of fields and a record type, and creates a record. -}
mkRecordTerm :: [Field] -> RecordType -> RecordTerm
mkRecordTerm fields binding = 
  RecordTerm { fields = Set.fromList fields, recordBinding = binding }


-- TERMS/TYPES --------------------------------------

{- | The types of the calculus. -}
data Type =
    BaseT BaseType
  | RecordT RecordType
  | OptionT OptionType
  | Arrow Type Type
  deriving (Eq, Ord)

instance Show Type where
  show (BaseT binding) = show binding
  show (RecordT binding) = show binding
  show (OptionT binding) = show binding
  show (Arrow t1 t2) = (show t1) ++ " -> " ++ (show t2)

{- | The terms of the calculus. -}
data Term =
    Base BaseTerm
  | Record RecordTerm
  | Option OptionTerm
  | Free Name
  | Idx Index
  | Abstr Type Term
  | App Term Term
  deriving (Eq, Ord)

instance Show Term where
  show (Base term) = show term
  show (Record term) = show term
  show (Option term) = show term
  show (Free name) = show name
  show (Idx idx) = show idx
  show (Abstr bind term) = 
    "λ : " ++ (show bind) ++ ".(" ++ (show term) ++ ")"
  show (App t1 t2) = "(" ++ (show t1) ++ ") " ++ (show t2)

{- | Constructs a free variable term. -}
mkFree :: Name -> Term
mkFree name = Free name

{- | Constructs a de Bruijn index. -}
mkIdx :: Index -> Term
mkIdx idx = Idx idx

{- | Constructs an abstraction term. -}
mkAbstr :: Name -> Type -> Term -> Term
mkAbstr name binding term = Abstr binding $ indices 1 name term

{- | Constructs an application term. -}
mkApp :: Term -> Term -> Term
mkApp func arg = App func arg

{- | A context is a mapping of names to types. -}
type Context = Map.Map Name Type

{- | For pretty printing contexts. -}
ctxToString :: Context -> String
ctxToString ctx =
  let fmt (name, binding) = name ++ " : " ++ (show binding)
      bindings = map fmt (Map.toList ctx)
   in intercalate ", " bindings

{- | Creates a context from a list of name/type pairs. -}
mkCtx :: [(Name, Type)] -> Context
mkCtx entries = Map.fromList entries

{- | Find the type bound to a name in the context (if any). -}
getCtxBinding :: Context -> Name -> Maybe Type
getCtxBinding ctx name = Map.lookup name ctx

{- A stack maps de Bruijn indices to types. -}
type Stack = Map.Map Index Type

{- | An empty 'Stack'. -}
initStack :: Stack
initStack = Map.fromList []

{- | Find the type bound to a given de Bruijn index (if any). -}
getIdxBinding :: Stack -> Index -> Maybe Type
getIdxBinding stack idx = Map.lookup idx stack

{- | Pushes a new de Bruijn index (and its type) onto the stack. -}
pushIdxBinding :: Stack -> Type -> Stack
pushIdxBinding stack binding =
  let stack' = Map.mapKeys (\k -> k + 1) stack
  in Map.insert 1 binding stack'


-- TYPE CHECKING ------------------------------------

{- | Types of errors that can occur in type checking. -}
data TypeError =
    WrongFields RecordTerm
  | WrongOption OptionTerm
  | NoType Term
  | NotAbstr Term
  | BadArgType Term Term Type Type

instance Show TypeError where
  show (WrongFields t) =
    "Wrong fields in: " ++ (show t) ++ " : " ++ 
    (show $ recordBinding t)
  show (WrongOption t) =
    "Wrong option in: " ++ (show t) ++ " : " ++ 
    (show $ selectionBinding t)
  show (NoType term) = 
    "Cannot assign type to: " ++ (show term)
  show (NotAbstr term) = 
    "Expected abstraction on left in: " ++ (show term)
  show (BadArgType term arg binding binding') =
    "Argument '" ++ (show arg)  ++ "' " ++
    "has type '" ++ (show binding) ++ "' " ++
    "but must have type '" ++ (show binding') ++ "' " ++
    "in: " ++ (show term)

{- | Derive a term's type, given a context and an initial empty stack. -}
getType :: Context -> Stack -> Term -> Either TypeError Type
getType ctx stack term =
  case term of
    Base t -> Right $ BaseT (termBinding t)
    Option t -> 
      let binding = selectionBinding t
      in case (selection t) of 
        None -> Right $ OptionT binding
        Precisely t' ->
          case getType ctx stack t' of
            Left e -> Left e
            Right subterm_type -> 
              case subterm_type == optionBinding binding of
                True -> Right $ OptionT binding
                False -> Left $ WrongOption t
    Record t ->
      let binding = recordBinding t
          bindingLabels = labels binding 
          recordFields = fields t
          fieldLabels = Set.map (\f -> fieldBinding f) recordFields 
      in case fieldLabels == bindingLabels of
        True -> Right $ RecordT binding
        False -> Left $ WrongFields t
    Free name ->
      case getCtxBinding ctx name of
        Just binding -> Right $ binding
        Nothing -> Left $ NoType term
    Idx idx ->
      case getIdxBinding stack idx of
        Just binding -> Right binding
        Nothing -> Left $ NoType term
    Abstr binding body ->
      let stack' = pushIdxBinding stack binding
      in case getType ctx stack' body of
        Right binding' -> Right $ Arrow binding binding'
        Left err -> Left err
    App func arg ->
      case func of
        Abstr binding body ->
          case getType ctx stack arg of
            Right binding' ->
              case binding == binding' of
                False -> Left $ BadArgType term arg binding' binding
                True ->
                  let stack' = pushIdxBinding stack binding
                  in getType ctx stack' body
            Left err -> Left err
        _ -> Left $ NotAbstr term


-- EVALUATION ---------------------------------------

{- | Converts a term into a de Bruijn indexed version. -}
indices :: Index -> Name -> Term -> Term
indices idx name term =
  case term of
    Base _ -> term
    Option body ->
      let option = selection body
          binding = selectionBinding body
      in case option of
        None -> term
        Precisely body' ->
          let body'' = indices idx name body'
          in Option $ mkOptionTerm (Precisely body'') binding
    Record body ->
      let binding = recordBinding body
          recordFields = fields body
          indexField fld =
            let fld' = indices idx name (field fld)
                binding' = fieldBinding fld
            in mkField fld' binding'
          newFields = Set.map indexField recordFields
      in Record $ mkRecordTerm (Set.toList newFields) binding
    Free name'
      | name == name' -> Idx idx
      | otherwise -> term
    Idx _ -> term
    Abstr binding body ->
      Abstr binding $ indices (idx + 1) name body
    App func arg ->
      App (indices idx name func) (indices idx name arg)

{- | Substitute one term into another term. -}
subst :: Index -> Term -> Term -> Term
subst idx replacement target =
  case target of
    Base _ -> target
    Option body ->
      let option = selection body
          binding = selectionBinding body
      in case option of
        None -> target
        Precisely body' ->
          let body'' = subst idx replacement body'
          in Option $ mkOptionTerm (Precisely body'') binding
    Record body ->
      let binding = recordBinding body
          recordFields = fields body
          substField fld =
            let fld' = subst idx replacement (field fld)
                binding' = fieldBinding fld
            in mkField fld' binding'
          newFields = Set.map substField recordFields
      in Record $ mkRecordTerm (Set.toList newFields) binding
    Free name -> target 
    Idx i
      | i == idx -> replacement
      | otherwise -> target
    Abstr binding body -> Abstr binding $ subst (idx + 1) replacement body
    App func arg ->
      App (subst idx replacement func) (subst idx replacement arg)

{- | (Left-most) reduce a term one time. -}
reduceOnce :: Context -> Term -> Either TypeError Term
reduceOnce ctx term =
  case getType ctx initStack term of
    Left err -> Left err
    Right _ ->
      case term of
        App (Abstr _ body) arg ->
          Right $ subst 1 arg body
        _ -> Right term

{- | Reduce a term repeatedly until no more reductions can be done. -}
reduce :: Context -> Term -> Either TypeError Term
reduce ctx term =
  let reducedTerm = reduceOnce ctx term
  in case reducedTerm of
    Left err -> Left err
    Right result ->
      case result of
        App (Abstr _ _) _ -> reduce ctx result
        _ -> Right result


-- EXAMPLES -----------------------------------------

wkndT = mkBaseType "Weekend"
sat = mkBaseTerm "sat" wkndT
sun = mkBaseTerm "sun" wkndT

wknd_type = BaseT wkndT
sat_term = Base sat
sun_term = Base sun

boolT = mkBaseType "Bool" 
tt = mkBaseTerm "tt" boolT
ff = mkBaseTerm "ff" boolT

bool_type = BaseT boolT
tt_term = Base tt
ff_term = Base ff

isP_label = mkLabel "isP" bool_type
isQ_label = mkLabel "isQ" bool_type
tt_isP_field = mkField tt_term isP_label
ff_isQ_field = mkField tt_term isQ_label

pRecordT = mkRecordType [isP_label]
qRecordT = mkRecordType [isQ_label]
pRecord = mkRecordTerm [tt_isP_field] pRecordT
qRecord = mkRecordTerm [tt_isP_field] qRecordT

pRecord_type = RecordT pRecordT
qRecord_type = RecordT qRecordT
pRecord_term = Record pRecord
bad_qRecord_term = Record qRecord -- Doesn't type check.

optBoolT = mkOptionType (BaseT boolT)
no_optBool = mkOptionTerm None optBoolT
tt_optBool = mkOptionTerm (Precisely tt_term) optBoolT

optBoolT_type = OptionT optBoolT
no_optBool_term = Option no_optBool
tt_optBool_term = Option tt_optBool

pRecord_optBool = mkOptionTerm (Precisely pRecord_term) optBoolT
bad_pRecord_optBool_term = Option pRecord_optBool -- Doesn't type check.

ctx = mkCtx [("x", bool_type), ("y", bool_type)]

x = mkFree "x"
y = mkFree "y"

f = mkAbstr "x" bool_type x
app = mkApp f x
bad_app = App f tt_optBool_term -- Doesn't type check.

x_optBool = mkOptionTerm (Precisely x) optBoolT
x_optBool_term = Option x_optBool
g = mkAbstr "x" bool_type x_optBool_term
opt_app = mkApp g tt_term
bad_opt_app = mkApp g sat_term -- Doesn't type check.

x_isP_field = mkField x isP_label
xpRecord = mkRecordTerm [x_isP_field] pRecordT
xpRecord_term = Record xpRecord

h = mkAbstr "x" bool_type xpRecord_term
record_app = mkApp h tt_term
bad_record_app = mkApp h sat_term -- Doesn't type check.

k = mkAbstr "y" bool_type x
abstr_x_from_k = mkAbstr "x" bool_type k
capture_app = mkApp abstr_x_from_k y -- Capture doesn't happen.
