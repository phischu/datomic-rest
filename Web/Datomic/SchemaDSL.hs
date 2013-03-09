{-# LANGUAGE OverloadedStrings #-}
module Web.Datomic.SchemaDSL where

import Web.Datomic.TransactionDSL

import Data.Text hiding (concat)

import Control.Monad (forM)

--ATTRIBUTES

schema :: Keyword -> ValueType -> Cardinality -> Transaction TempId
schema ident valuetype cardinality = do
    tempid <- newTempId (key "db.part" "db")
    multiAdd tempid [key "db" "ident"             |~> ident,
                     key "db" "valueType"         |~> valueTypeKeyword valuetype,
                     key "db" "cardinality"       |~> cardinalityKeyword cardinality,
                     key "db.install" "attribute" |<~ key "db.part" "db"]
    return tempid

schemaEx :: Keyword -> ValueType -> Cardinality -> OptionalAttributes -> Transaction TempId
schemaEx ident valuetype cardinality optionalAttributes = do
    tempid <- schema ident valuetype cardinality
    multiAdd tempid (interpretOptionalAttributes optionalAttributes)
    return tempid

interpretOptionalAttributes :: OptionalAttributes -> [AttributeValue]
interpretOptionalAttributes optionalAttributes = concat $ [
    maybe [] (\v -> [key "db" "doc" |~> str v]) (_doc oa),
    maybe [] (\v -> [key "db" "unique" |~> interpretUnique v]) (_unique oa),
    maybe [] (\v -> [key "db" "index" |~> v]) (_index oa),
    maybe [] (\v -> [key "db" "fulltext" |~> v]) (_fulltext oa),
    maybe [] (\v -> [key "db" "isComponent" |~> v]) (_isComponent oa),
    maybe [] (\v -> [key "db" "noHistory" |~> v]) (_noHistory oa)] where
        oa = optionalAttributes

data ValueType = TypeRef
               | TypeKeyword
               | TypeString
               | TypeBoolean
               | TypeLong
               | TypeBigInt
               | TypeFloat
               | TypeDouble
               | TypeBigDec
               | TypeInstant
               | TypeUUID
               | TypeURI
               | TypeBytes

valueTypeKeyword :: ValueType -> Keyword
valueTypeKeyword TypeRef = key "db.type" "ref"
valueTypeKeyword TypeKeyword = key "db.type" "keyword"
valueTypeKeyword TypeString = key "db.type" "string"
valueTypeKeyword TypeBoolean = key "db.type" "boolean"
valueTypeKeyword TypeLong = key "db.type" "long"
valueTypeKeyword TypeBigInt = key "db.type" "bigint"
valueTypeKeyword TypeFloat = key "db.type" "float"
valueTypeKeyword TypeDouble = key "db.type" "double"
valueTypeKeyword TypeBigDec = key "db.type" "bigdec"
valueTypeKeyword TypeInstant = key "db.type" "instant"
valueTypeKeyword TypeUUID = key "db.type" "uuid"
valueTypeKeyword TypeURI = key "db.type" "uri"
valueTypeKeyword TypeBytes = key "db.type" "bytes"

data Cardinality = One
                 | Many

cardinalityKeyword :: Cardinality -> Keyword
cardinalityKeyword One = key "db.cardinality" "one"
cardinalityKeyword Many = key "db.cardinality" "many"

data OptionalAttribute = Doc Text
                       | Unique Unique
                       | Index Bool
                       | Fulltext Bool
                       | IsComponent Bool
                       | NoHistory Bool

data Unique = UniqueValue
            | UniqueIdentity

interpretUnique :: Unique -> Keyword
interpretUnique UniqueValue = key "db.unique" "value"
interpretUnique UniqueIdentity = key "db.unique" "identity"

data OptionalAttributes = OptionalAttributes {
    _doc :: Maybe Text,
    _unique :: Maybe Unique,
    _index :: Maybe Bool,
    _fulltext :: Maybe Bool,
    _isComponent :: Maybe Bool,
    _noHistory :: Maybe Bool}

defaultOptionalAttributes :: OptionalAttributes
defaultOptionalAttributes = OptionalAttributes Nothing Nothing Nothing Nothing Nothing Nothing

--PARTITIONS

partition :: Keyword -> Transaction TempId
partition partitionname = do
    tempid <- newTempId (key "db.part" "db")
    multiAdd tempid [key "db" "ident"             |~> partitionname,
                     key "db.install" "partition" |<~ key "db.part" "db"]
    return tempid

--ENUMS

enum :: Keyword -> Keyword -> [Keyword] -> Transaction TempId
enum part enumname enumvalues = do
    enumid <- schema enumname TypeRef One
    forM enumvalues $ (\enumvalue -> do
        enumvalueid <- newTempId part
        add enumvalueid (key "db" "ident") enumvalue)
    return enumid



