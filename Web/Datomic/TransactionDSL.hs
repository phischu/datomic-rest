{-# LANGUAGE OverloadedStrings #-}
module Web.Datomic.TransactionDSL where

import Control.Monad.Free (Free(Pure,Free),liftF)
import Control.Monad.Writer.Lazy (execWriterT,WriterT,tell)
import Control.Monad.State.Lazy (evalState,State,modify,get,lift)

import Data.EDN (ToEDN)
import qualified Data.EDN as EDN (TaggedValue,Value,ToEDN(toEDN),tag,notag,stripTag,keyword,makeVec,makeMap,Pair)

import Data.Word (Word8)

import Data.Text (Text)
import qualified Data.Text as T ()
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as T (encodeUtf8)


type Transaction = Free TransactionF

data TransactionF a = NewTempId Text (TempId -> a)
                    | Add Entity Attribute Value a
                    | MultiAdd Entity [AttributeValue] a
                    | Retract Entity Attribute Value a
                    | DataFunction DataFunction a

data Entity = EntityTempId TempId
            | EntityExistingId ExistingId
            | EntityKeyword Keyword

data Attribute = AttributeTempId TempId
               | AttributeExistingId ExistingId
               | AttributeKeyword Keyword

data Value = ValueTempId TempId
           | ValueExistingId ExistingId
           | ValueKeyword Keyword
           | ValueString Text
           | ValueBoolean Bool
           | ValueLong Integer
           | ValueBigInt Integer
           | ValueFloat Float
           | ValueDouble Double
           | ValueBigDec Rational
           | ValueInstant Integer
           | ValueUUID Integer
           | ValueURI Text
           | ValueBytes [Word8]

data AttributeValue = AttributeValue Attribute Value
                    | ReverseAttributeValue Attribute Value

data TempId = TempId Text Integer

data ExistingId = ExistingId Integer

data Keyword = Keyword Text Text

data DataFunction = RetractEntity Entity

instance Functor TransactionF where
    fmap f (NewTempId p c) = NewTempId p (f . c)
    fmap f (Add e a v c) = Add e a v (f c)
    fmap f (Retract e a v c) = Retract e a v (f c)
    fmap f (MultiAdd e avs c) = MultiAdd e avs (f c)

-- Enities

entityTempId :: TempId -> Entity
entityTempId = EntityTempId

entityExistingId :: ExistingId -> Entity
entityExistingId = EntityExistingId

entityKeyword :: Keyword -> Entity
entityKeyword = EntityKeyword

-- Attributes

attributeTempId :: TempId -> Attribute
attributeTempId = AttributeTempId

attributeExistingId :: ExistingId -> Attribute
attributeExistingId = AttributeExistingId

attributeKeyword :: Text -> Text -> Attribute
attributeKeyword namespace name = AttributeKeyword (Keyword namespace name)

-- Values

valueTempId :: TempId -> Value
valueTempId = ValueTempId

valueExistingId :: ExistingId -> Value
valueExistingId = ValueExistingId

valueKeyword :: Keyword -> Value
valueKeyword = ValueKeyword

valueString :: Text -> Value
valueString = ValueString

valueBoolean :: Bool -> Value
valueBoolean = ValueBoolean

valueBigInt :: Integer -> Value
valueBigInt = ValueBigInt

valueDouble :: Double -> Value
valueDouble = ValueDouble

-- Statements

newTempId :: Text -> Transaction TempId
newTempId part = liftF (NewTempId part (\tempid -> tempid))

add :: Entity -> Attribute -> Value -> Transaction ()
add e a v = liftF (Add e a v ())

multiAdd :: Entity -> [AttributeValue] -> Transaction ()
multiAdd e avs = liftF (MultiAdd e avs ())

retract :: Entity -> Attribute -> Value -> Transaction ()
retract e a v = liftF (Retract e a v ())

retractEntity :: Entity -> Transaction ()
retractEntity e = liftF (DataFunction (RetractEntity e) ())

-- Attribute Value Pairs

(|~>) :: Attribute -> Value -> AttributeValue
(|~>) = AttributeValue

-- Interpretation

instance ToEDN Entity where
    toEDN (EntityTempId tempid) = EDN.toEDN tempid
    toEDN (EntityExistingId existingid) = EDN.toEDN existingid
    toEDN (EntityKeyword keyword) = EDN.toEDN keyword

instance ToEDN Attribute where
    toEDN (AttributeTempId tempid) = EDN.toEDN tempid
    toEDN (AttributeExistingId existingid) = EDN.toEDN existingid
    toEDN (AttributeKeyword keyword) = EDN.toEDN keyword

instance ToEDN Value where
    toEDN (ValueTempId tempid) = EDN.toEDN tempid
    toEDN (ValueExistingId existingid) = EDN.toEDN existingid
    toEDN (ValueKeyword keyword) = EDN.toEDN keyword
    toEDN (ValueString text) = EDN.toEDN text
    toEDN (ValueBoolean bool) = EDN.toEDN bool
    toEDN (ValueLong long) = EDN.toEDN long -- Should be Int64 or sth.
    toEDN (ValueBigInt integer) = EDN.toEDN integer
    toEDN (ValueFloat float) = EDN.toEDN (fromRational (toRational float) :: Double)
    toEDN (ValueDouble double) = EDN.toEDN double
    toEDN _ = error "Instance ToEDN Attribute not yet complete"

instance ToEDN TempId where
    toEDN (TempId part integer) = EDN.tag (T.encodeUtf8 "db") (T.encodeUtf8 "id") (EDN.makeVec [EDN.keyword (T.encodeUtf8 ("db.part/" <> part)),EDN.toEDN integer])

instance ToEDN ExistingId where
    toEDN (ExistingId integer) = EDN.toEDN integer

instance ToEDN Keyword where
    toEDN (Keyword _ "") = error "Empty name in keyword"
    toEDN (Keyword "" name) = EDN.keyword (T.encodeUtf8 name)
    toEDN (Keyword prefix name) = EDN.keyword (T.encodeUtf8 (prefix <> "/" <> name))

dbadd :: EDN.TaggedValue
dbadd = EDN.keyword (T.encodeUtf8 ("db/add"))

dbid :: EDN.Value
dbid = EDN.stripTag (EDN.keyword (T.encodeUtf8 ("db/id")))

attributeValueToEDNPair :: AttributeValue -> EDN.Pair
attributeValueToEDNPair (AttributeValue attribute value) = (EDN.stripTag (EDN.toEDN attribute),EDN.toEDN value)
attributeValueToEDNPair (ReverseAttributeValue (AttributeKeyword (Keyword prefix name)) value) = (EDN.stripTag (EDN.toEDN (attributeKeyword prefix ("_" <> name))),EDN.toEDN value)

dbretract :: EDN.TaggedValue
dbretract = EDN.keyword (T.encodeUtf8 ("db/retract"))

dbfnretractentity :: EDN.TaggedValue
dbfnretractentity = EDN.keyword (T.encodeUtf8 ("db.fn/retractEntity"))

decrement :: Integer -> Integer
decrement x = x - 1

interpretTransaction :: Transaction a -> WriterT [EDN.TaggedValue] (State Integer) ()
interpretTransaction (Pure _) = return ()
interpretTransaction (Free (NewTempId part c)) = lift (modify decrement >> get) >>= interpretTransaction . c . TempId part
interpretTransaction (Free (Add e a v c)) = tell [EDN.notag (EDN.makeVec [dbadd,EDN.toEDN e,EDN.toEDN a,EDN.toEDN v])] >> interpretTransaction c
interpretTransaction (Free (MultiAdd e avs c)) = tell [EDN.toEDN (EDN.makeMap ((dbid,EDN.toEDN e):map attributeValueToEDNPair avs))] >> interpretTransaction c
interpretTransaction (Free (Retract e a v c)) = tell [EDN.notag (EDN.makeVec [dbretract,EDN.toEDN e,EDN.toEDN a,EDN.toEDN v])] >> interpretTransaction c
interpretTransaction (Free (DataFunction (RetractEntity e) c)) = tell [EDN.notag (EDN.makeVec [dbfnretractentity,EDN.toEDN e])] >> interpretTransaction c

literalRepresentation :: Transaction a -> EDN.TaggedValue
literalRepresentation = EDN.notag . EDN.makeVec . flip evalState 0 . execWriterT . interpretTransaction

