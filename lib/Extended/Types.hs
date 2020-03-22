module Typed.Types where

import Data.List (intercalate)
import qualified Data.Set as Set

type Name = String
type Value = String

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

getType :: Term -> Type
getType term =
  case term of
    Base t -> BaseT (termBinding t)
    Record t -> RecordT (recordBinding t)
    Option t -> OptionT (selectionBinding t)

