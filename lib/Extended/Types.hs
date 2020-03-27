module Typed.Types where

{- This module provides finite base types, records, and options. 

To define your types in the calculus, there are two steps: 

1. Create the specific types and inhabitants you want.
2. Register your types and inhabitants with the calculus,
   using the generic 'Type' and 'Term' constructors.

Create a base type:

>>> let boolT = mkBaseType "Bool" 

Create some inhabitants:

>>> let tt = mkBaseTerm "tt" boolT
>>> let ff = mkBaseTerm "ff" boolT

Register 'boolT' as a base type in the calculus:

>>> let bool_type = BaseT boolT

Register 'tt' and 'ff' as terms of the calculus:

>>> let tt_term = Base tt
>>> let ff_term = Base ff

Check their types:

>>> getType tt_term
>>> getType ff_term

Create an optional bool type:

>>> let optBoolT = mkOptionType (BaseT boolT)

Select 'None':

>>> let no_optBool = mkOptionTerm None optBoolT

Select exactly 'tt_term':

>>> let tt_optBool = mkOptionTerm (Precisely tt_term) optBoolT

Register 'optBoolT' as an option type in the calculus:

>>> let optBoolT_type = OptionT optBoolT

Register the two selections as terms in the calculus:

>>> let no_optBool_term = Option no_optBool
>>> let tt_optBool_term = Option tt_optBool

Check their types:

>>> getType no_optBool_term
>>> getType tt_optBool_term

Create a label called "isP" that takes a bool value,
and another label called "isQ" that takes a bool value:

>>> isP_label = mkLabel "isP" bool_type
>>> isQ_label = mkLabel "isQ" bool_type

Create a record type with those fields:

>>> pqRecordT = mkRecordType [isP_label, isQ_label]

Create an "isP" and "isQ" field with boolean values:

>>> tt_isP_field = mkField tt_term isP_label
>>> ff_isQ_field = mkField ff_term isQ_label

Create a record with those fields in it:

>>> pqFields = [tt_isP_field, ff_isQ_field]
>>> pqRecord = mkRecordTerm pqFields pqRecordT

Register 'pqRecordT' and 'pqRecord' with the calculus:

>>> pqRecord_type = RecordT pqRecordT
>>> pqRecord_term = Record pqRecord

Type check the record:

>>> getType pqRecord_term

-}

import Data.List (intercalate)
import qualified Data.Set as Set

type Name = String
type Value = String


-- BASE TYPES ---------------------------------------

data BaseType = BaseType {
    name :: Name
  } deriving (Eq, Ord)

instance Show BaseType where
  show binding = (name binding)

data BaseTerm = BaseTerm {
    value :: Value
  , termBinding :: BaseType
  } deriving (Eq, Ord)

instance Show BaseTerm where
  show t = value t

mkBaseType :: Name -> BaseType
mkBaseType name = BaseType { name = name }

mkBaseTerm :: Value -> BaseType -> BaseTerm
mkBaseTerm value binding =
  BaseTerm { value = value, termBinding = binding }


-- OPTIONS ------------------------------------------

data OptionType = OptionType {
    optionBinding :: Type
  } deriving (Eq, Ord)

instance Show OptionType where
  show binding = "Optional " ++ (show $ optionBinding binding)

data Selection =
    None
  | Precisely Term
  deriving (Eq, Ord)

instance Show Selection where
  show None = "Nothing"
  show (Precisely t) = "Precisely " ++ (show t)

data OptionTerm = OptionTerm {
    selection :: Selection
  , selectionBinding :: OptionType
  } deriving (Eq, Ord)

instance Show OptionTerm where
  show t = show $ selection t

mkOptionType :: Type -> OptionType
mkOptionType binding = OptionType { optionBinding = binding }

mkOptionTerm :: Selection -> OptionType -> OptionTerm
mkOptionTerm option binding = 
  OptionTerm { selection = option, selectionBinding = binding }


-- RECORDS ------------------------------------------

data Label = Label {
    label :: Name,
    labelBinding :: Type
  } deriving (Eq, Ord)

instance Show Label where
  show l = (label l)

data Field = Field {
    field :: Term
  , fieldBinding :: Label
  } deriving (Eq, Ord)

instance Show Field where
  show f = (show $ fieldBinding f) ++ " = " ++ (show $ field f)

mkLabel :: Name -> Type -> Label
mkLabel name binding = Label { label = name, labelBinding = binding }

mkField :: Term -> Label -> Field
mkField t l = Field { field = t, fieldBinding = l }

data RecordType = RecordType {
    labels :: Set.Set Label
  } deriving (Eq, Ord)

instance Show RecordType where
  show binding = 
    let fmt l = (show l) ++ " : " ++ (show $ labelBinding l)
        ls = Set.toList $ labels binding
        labelStr = map fmt ls
    in "{" ++ (intercalate ", " labelStr) ++ "}"

data RecordTerm = RecordTerm {
    fields :: Set.Set Field
  , recordBinding :: RecordType
  } deriving (Eq, Ord)

instance Show RecordTerm where
  show t = 
    let fds = Set.toList $ fields t
        binding = recordBinding t
    in "{" ++ (intercalate ", " $ map show fds) ++ "}"

mkRecordType :: [Label] -> RecordType
mkRecordType labels = RecordType { labels = Set.fromList labels }

mkRecordTerm :: [Field] -> RecordType -> RecordTerm
mkRecordTerm fields binding = 
  RecordTerm { fields = Set.fromList fields, recordBinding = binding }


-- TERMS/TYPES --------------------------------------

data Type =
    BaseT BaseType
  | RecordT RecordType
  | OptionT OptionType
  deriving (Eq, Ord)

instance Show Type where
  show (BaseT binding) = show binding
  show (RecordT binding) = show binding
  show (OptionT binding) = show binding

data Term =
    Base BaseTerm
  | Record RecordTerm
  | Option OptionTerm
  deriving (Eq, Ord)

instance Show Term where
  show (Base t) = show t
  show (Record t) = show t
  show (Option t) = show t


-- TYPE CHECKING ------------------------------------

data TypeError =
    WrongFields RecordTerm
  | WrongOption OptionTerm

instance Show TypeError where
  show (WrongFields t) =
    "Wrong fields in '" ++ (show t) ++ " : " ++ 
    (show $ recordBinding t) ++ "'"
  show (WrongOption t) =
    "Wrong option in '" ++ (show t) ++ " : " ++ 
    (show $ selectionBinding t) ++ "'"

getType :: Term -> Either TypeError Type
getType term =
  case term of
    Base t -> Right $ BaseT (termBinding t)
    Option t -> 
      let binding = selectionBinding t
      in case (selection t) of 
        None -> Right $ OptionT binding
        Precisely t' ->
          case getType t' of
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


-- EXAMPLES -----------------------------------------

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
qRecord_term = Record qRecord -- Doesn't type check.

optBoolT = mkOptionType (BaseT boolT)
no_optBool = mkOptionTerm None optBoolT
tt_optBool = mkOptionTerm (Precisely tt_term) optBoolT

optBoolT_type = OptionT optBoolT
no_optBool_term = Option no_optBool
tt_optBool_term = Option tt_optBool

pRecord_optBool = mkOptionTerm (Precisely pRecord_term) optBoolT
pRecord_optBool_term = Option pRecord_optBool -- Doesn't type check.

