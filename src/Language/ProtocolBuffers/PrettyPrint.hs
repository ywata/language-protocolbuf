{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.ProtocolBuffers.PrettyPrint where

import Prelude hiding ((<>))
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
  pretty (DImport it t) = text' "import" <+> pretty it <+> doubleQuotes (text' t) <> semi
  pretty (DPackage fids) = text' "package" <+> (punct '.' fids) <> semi
  pretty (DOption opt) = pretty opt <> semi
  pretty (DType tdcl) = pretty tdcl <> semi
  pretty (DService svc) = text' "DService" <> semi

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

instance (Pretty i, Pretty f, Pretty s) => Pretty (TypeDeclaration i f s) where
  pretty (DEnum id opts efs) = text' "enum" <+> text' id <+>
    (braces (nest 2 $
            text ""
            $$ pretty opts
            $$ pretty efs))
  pretty (DMessage id opts rsvds mfs tyds) = text' "message" <+> text' id <+>
    (braces (nest 2 $
              text ""
              $$ pretty opts 
              $$ pretty rsvds
              $$ pretty mfs
              $$ pretty tyds
            ))
  pretty DEmptyTyDecl = semi
  
instance (Pretty i, Pretty f, Pretty s) => Pretty (ServiceDeclaration i f s) where
  pretty (Service id opts ms) = text' id <> semi
  
instance (Pretty i, Pretty f, Pretty s) => Pretty (Method i f s) where
  pretty (Method id l1 ft1 l2 ft2 opts) = text' "Method"

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

instance (Pretty i, Pretty f, Pretty s) => Pretty (MessageField i f s) where
  pretty (NormalField l ft fn i opts) = pretty l <+> pretty ft <+> text' fn <+> equals <+> pretty i
    <+> (brackets . sep . punctuate (char ',') $ map pretty opts) <> semi
  pretty (OneOfField fn ms opts) = text' "oneof" <+> text' fn <+>
    braces (
      sep (punctuate (char ';') (map pretty opts)
           ++ punctuate (char ';') (map pretty ms))) <> semi

  pretty (MapField ft1 ft2 fn i opts) = text' "MapField" <> semi
  pretty EmptyMessageField = semi
    
instance Pretty Label where
  pretty Single = empty
  pretty Repeated = text' "repeted"
  pretty Optional = text' "optional"
  pretty Required = text' "required"

instance Pretty (ReservedValue i f s) where
  pretty (RRanges is m) = text' "RRanges"
  pretty (RNames is) = text' "RNames"
