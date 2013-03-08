{-# LANGUAGE OverloadedStrings #-}
module Web.Datomic.SchemaDSL where

import Web.Datomic.TransactionDSL

import Data.Text

--ATTRIBUTES

schema :: Keyword -> ValueType -> Cardinality -> [OptionalAttribute] -> Transaction TempId
schema ident valuetype cardinality [] = do
    tempid <- newTempId (key "db.part" "db")
    multiAdd tempid [key "db" "ident"             |~> ident,
                     key "db" "valueType"         |~> valueTypeKeyword valuetype,
                     key "db" "cardinality"       |~> cardinalityKeyword cardinality,
                     key "db.install" "attribute" |<~ key "db.part" "db"]
    return tempid
schema _ _ _ _ = undefined

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

data OptionalAttribute = Doc
                       | Unique
                       | Index
                       | Fulltext
                       | IsComponent
                       | NoHistory

--PARTITIONS

partition :: Text -> Transaction TempId
partition = undefined

--ENUMS

enum :: Text -> [Text] -> Transaction TempId
enum = undefined



