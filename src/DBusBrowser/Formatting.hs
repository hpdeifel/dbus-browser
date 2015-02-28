{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Routines for formatting dbus values
--
-- In particular, types and variants
module DBusBrowser.Formatting where

import DBus
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe

import Unparse

formatType :: Type -> Text
formatType = T.pack . unparse

formatVariant :: Variant -> Text
formatVariant v = fromJust $ T.stripPrefix "Variant " $ T.pack $ show v

instance Unparse Type where
  precedence (TypeArray _) = 0
  precedence (TypeDictionary _ _) = 0
  precedence (TypeStructure _) = 0
  precedence _ = 1

  associativity _ = NoneAssoc

  printRec f t = case t of
    TypeBoolean -> "Bool"
    TypeWord8 -> "UInt8"
    TypeWord16 -> "UInt16"
    TypeWord32 -> "UInt32"
    TypeWord64 -> "UInt64"
    TypeInt16 -> "Int16"
    TypeInt32 -> "Int32"
    TypeInt64 -> "Int64"
    TypeDouble -> "Double"
    TypeString -> "String"
    TypeSignature -> "Signature"
    TypeObjectPath -> "ObjectPath"
    TypeVariant -> "Variant"
    TypeArray t' -> "Array " +> f t'
    TypeDictionary t1 t2 -> "Dict " +> f t1 ++ " " +> f t2
    TypeStructure ts -> foldr (\t' s -> ((s ++ " ") +> f t')) "Struct" ts
