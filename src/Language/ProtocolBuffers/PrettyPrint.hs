{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.ProtocolBuffers.PrettyPrint where

import Prelude hiding (Enum, (<>))
import Data.List

import qualified Data.Text as T
import Language.ProtocolBuffers.Types
import Language.ProtocolBuffers.PrimTypes
import Text.PrettyPrint

class Pretty a where
  pretty :: a -> Doc

text' :: T.Text -> Doc
text' = text . T.unpack

punct :: Char -> [T.Text] -> Doc
punct ch = sep . punctuate (char ch) . map text'

instance Pretty a => Pretty [a] where
  pretty = vcat . map pretty

instance Pretty IntLit where
  pretty (Dec t) = text' t
  pretty (Hex t) = text "0x" <> text' t
  pretty (Oct t) = text "0" <> text' t

instance Pretty FloatLit where
  pretty (Fl1 t (Just d) e) = text' t <> text' "." <> text' d <> text' (maybe "" id e)
  pretty (Fl1 t Nothing e) = text' t <> text' (maybe "" id e)
  pretty (Fl2 d e) = text' d <> text' e
  pretty (Fl3 d e) = text' "." <> text' d <> text' (maybe "" id e)
  pretty Inf = text' "inf"
  pretty NaN = text' "nan"
  
instance Pretty StringLit where
  pretty = text' . strVal

instance (Pretty i, Pretty f, Pretty s) => Pretty (ProtocolBuffer i f s) where
  pretty (ProtocolBuffer decls) = pretty decls

instance (Pretty i, Pretty f, Pretty s) => Pretty (Option i f s) where
  pretty (Option name cnst) = text' "option" <+> pretty name <+> equals <+> pretty cnst

instance (Pretty i, Pretty f, Pretty s) => Pretty (Declaration i f s) where
  pretty (DSyntax t) = text' "syntax" <+> equals <+> doubleQuotes (text' t) <> semi
  pretty (DImport it t) = text' "import" <+> pretty it <+> doubleQuotes (punct '.' t) <> semi
  pretty (DPackage fids) = text' "package" <+> (punct '.' fids) <> semi
  pretty (DOption opt) = pretty opt <> semi
  pretty (DMessage msg) = pretty msg <> semi
  pretty (DEnum enm) = pretty enm <> semi  
  pretty (DService n sfs) = text' "service" <+> text' n <> braces (sep $ map pretty sfs) <> semi
  pretty DEmpty = semi

instance Pretty OptionName where
  pretty (Regular fs) = (punct '.' fs)
  pretty (Custom fs fs') = parens (punct '.' fs) <> char '.' <> (punct '.' fs')
    

instance Pretty ImportType where
  pretty Normal = empty
  pretty Weak = text' "weak"
  pretty Public = text' "public"
  
instance (Pretty i, Pretty f, Pretty s) => Pretty (Constant i f s) where
  pretty (KIdentifier fids) = punct '.' fids
  pretty (KInt i) = pretty i
  pretty (KString s) = pretty s
  pretty (KBool True) = text' "true"
  pretty (KBool False) = text' "false"
  pretty (KObject _) = text' "KObject" -- 


instance (Pretty i, Pretty f, Pretty s) => Pretty (ServiceField i f s) where
  pretty (SOption o) = pretty o

--  RPC :: Identifier -> Bool  -> FullIdentifier -> Bool -> FullIdentifier
--    -> [Option i f s] -> ServiceField i f s
  pretty SEmpty = semi

instance (Pretty i, Pretty f, Pretty s) => Pretty (Service i f s) where
  pretty (Service id fs) = text' id <> pretty fs <> semi
  
instance (Pretty i, Pretty f, Pretty s) => Pretty (Method i f s) where
  pretty (Method id l1 ft1 l2 ft2 opts) = text' "Method"

instance (Pretty i, Pretty f, Pretty s) => Pretty (Enum i f s) where
  pretty (Enum id efs) = text' id <> braces(sep $ map pretty efs)

instance (Pretty i, Pretty f, Pretty s) => Pretty (EnumField i f s) where
  pretty (EnumField fn i opts) = text' fn <+> equals <+> pretty i <+> (brackets . sep . punctuate (char ',') $ map pretty opts) <> semi

instance Pretty FieldType where
  pretty TInt32 = text' "int32"
  pretty TInt64 = text' "int64"
  pretty TUInt32 = text' "uint32"
  pretty TUInt64 = text' "uint64"
  pretty TSInt32 = text' "sint32"
  pretty TSInt64 = text' "sint64"
  pretty TFixed32 = text' "fixed32"
  pretty TFixed64 = text' "fixed64"
  pretty TSFixed32 = text' "sfixed32"
  pretty TSFixed64 = text' "sfixed64"
  pretty TDouble = text' "double"
  pretty TBool = text' "bool"
  pretty TBytes = text' "bytes"
  pretty (TOther fids) = punct '.' fids


instance (Pretty i, Pretty f, Pretty s) => Pretty (Message i f s) where
  pretty (Message id mfs) = text' "message" <+> text' id <> braces (sep $ map pretty mfs)
    

instance (Pretty i, Pretty f, Pretty s) => Pretty (OneOf i f s) where
  pretty (OneOf id ofs) = text' "oneof" <+> text' id <> braces (sep $ map pretty ofs)

instance (Pretty i, Pretty f, Pretty s) => Pretty (OneOfField i f s) where
  pretty (OneOfField t id i ops) = pretty t <+> text' id  <+> equals <+> pretty i <+> brackets (sep $ map pretty ops)
  
instance (Pretty i, Pretty f, Pretty s) => Pretty (MessageField i f s) where
  pretty (MField l ft fn i opts) = pretty l <+> pretty ft <+> text' fn <+> equals <+> pretty i
    <+> (brackets . sep . punctuate (char ',') $ map pretty opts) <> semi
  pretty (MOneOfDef d) = pretty d

  pretty (MapDef ft1 ft2 fn i opts) = text' "MapField" <> semi
  pretty MEmpty = semi
    
instance Pretty Label where
  pretty Single = empty
  pretty Repeated = text' "repeted"
  pretty Optional = text' "optional"
  pretty Required = text' "required"

instance Pretty (ReservedValue i f s) where
  pretty (RRanges is m) = text' "RRanges"
  pretty (RNames is) = text' "RNames"
