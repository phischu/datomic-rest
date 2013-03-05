{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Web.Datomic.TransactionDSL (
    Transaction,
    newTempId,add,multiAdd,retract,retractEntity,
    tid,eid,key,str,dbl,
    (|~>),(|<~),
    literalRepresentation,
    TransactionF,
    Entity(..),Attribute(..),Value(..),AttributeValue(..),
    Ent(ent),Att(att),Val(val),
    TempId,ExistingId(ExistingId),Keyword(Keyword)) where

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


-- | The free monad of Transactions.
type Transaction a = Free TransactionF a

-- | The Transaction Functor from which the free monad is built.
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

-- | Fix a value to have type TempId.
tid :: TempId -> TempId
tid = id

-- | Fix a value to have type ExistingId.
eid :: ExistingId -> ExistingId
eid = id

-- | Create a Keyword.
key :: Text -> Text -> Keyword
key = Keyword

-- | Fix a value to have type Text.
str :: Text -> Text
str = id

-- | Fix a value to have type Double.
dbl :: Double -> Double
dbl = id

instance Functor TransactionF where
    fmap f (NewTempId p c) = NewTempId p (f . c)
    fmap f (Add e a v c) = Add e a v (f c)
    fmap f (Retract e a v c) = Retract e a v (f c)
    fmap f (MultiAdd e avs c) = MultiAdd e avs (f c)

-- Enities

class Ent a where
    ent :: a -> Entity

instance Ent TempId where
    ent = EntityTempId

instance Ent ExistingId where
    ent = EntityExistingId

instance Ent Keyword where
    ent = EntityKeyword

instance Ent Entity where
    ent = id

entityTempId :: TempId -> Entity
entityTempId = EntityTempId

entityExistingId :: ExistingId -> Entity
entityExistingId = EntityExistingId

entityKeyword :: Keyword -> Entity
entityKeyword = EntityKeyword

-- Attributes

class Att a where
    att :: a -> Attribute

instance Att TempId where
    att = AttributeTempId

instance Att ExistingId where
    att = AttributeExistingId

instance Att Keyword where
    att = AttributeKeyword

instance Att Attribute where
    att = id

attributeTempId :: TempId -> Attribute
attributeTempId = AttributeTempId

attributeExistingId :: ExistingId -> Attribute
attributeExistingId = AttributeExistingId

attributeKeyword :: Text -> Text -> Attribute
attributeKeyword namespace name = AttributeKeyword (Keyword namespace name)

-- Values

class Val a where
    val :: a -> Value

instance Val TempId where
    val = ValueTempId

instance Val ExistingId where
    val = ValueExistingId

instance Val Keyword where
    val = ValueKeyword

instance Val Text where
    val = ValueString

instance Val Bool where
    val = ValueBoolean

instance Val Integer where
    val = ValueBigInt

instance Val Double where
    val = ValueDouble

instance Val Value where
    val = id

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

-- | Create a new TempId. The argument is the part in which to create it but without 
--   \":db.part\/\". Example: newTempId \"user\".
newTempId :: Text -> Transaction TempId
newTempId part = liftF (NewTempId part (\tempid -> tempid))

-- | Add a triple to the database.
add :: (Ent e,Att a,Val v) => e -> a -> v -> Transaction ()
add e a v = liftF (Add (ent e) (att a) (val v) ())

-- | Add multiple attribute-value-pairs to the same entity.
multiAdd :: (Ent e) => e -> [AttributeValue] -> Transaction ()
multiAdd e avs = liftF (MultiAdd (ent e) avs ())

-- | Retract a triple.
retract :: (Ent e,Att a,Val v) => e -> a -> v -> Transaction ()
retract e a v = liftF (Retract (ent e) (att a) (val v) ())

-- | Retract an entity, also retracting all attributes referencing it and
--   all of its components.
retractEntity :: (Ent e) => e -> Transaction ()
retractEntity e = liftF (DataFunction (RetractEntity (ent e)) ())

-- Attribute Value Pairs

-- | A simple attribute-value-pair.
(|~>) :: (Att a,Val v) => a -> v -> AttributeValue
a |~> v = AttributeValue (att a) (val v)

-- | A reverse attribute-value-pair.
(|<~) :: (Att a,Ent e) => a -> e -> AttributeValue
a |<~ e = case ent e of
    (EntityTempId tempid)         -> ReverseAttributeValue (att a) (ValueTempId tempid)
    (EntityExistingId existingid) -> ReverseAttributeValue (att a) (ValueExistingId existingid)
    (EntityKeyword keyword)       -> ReverseAttributeValue (att a) (ValueKeyword keyword)


-- Interface

instance ToEDN (Free TransactionF a) where
    toEDN = literalRepresentation

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

dbretract :: EDN.TaggedValue
dbretract = EDN.keyword (T.encodeUtf8 ("db/retract"))

dbfnretractentity :: EDN.TaggedValue
dbfnretractentity = EDN.keyword (T.encodeUtf8 ("db.fn/retractEntity"))

attributeValueToEDNPair :: AttributeValue -> EDN.Pair
attributeValueToEDNPair (AttributeValue attribute value) = (EDN.stripTag (EDN.toEDN attribute),EDN.toEDN value)
attributeValueToEDNPair (ReverseAttributeValue (AttributeKeyword (Keyword prefix name)) value) = (EDN.stripTag (EDN.toEDN (attributeKeyword prefix ("_" <> name))),EDN.toEDN value)

decrement :: Integer -> Integer
decrement x = x - 1

interpretTransaction :: Transaction a -> WriterT [EDN.TaggedValue] (State Integer) ()
interpretTransaction (Pure _) = do
    return ()
interpretTransaction (Free (NewTempId part c)) = do
    tempid <- lift (modify decrement >> get)
    interpretTransaction (c (TempId part tempid))
interpretTransaction (Free (Add e a v c)) = do
    tell [EDN.notag (EDN.makeVec [dbadd,EDN.toEDN e,EDN.toEDN a,EDN.toEDN v])]
    interpretTransaction c
interpretTransaction (Free (MultiAdd e avs c)) = do
    tell [EDN.notag (EDN.makeMap ((dbid,EDN.toEDN e):map attributeValueToEDNPair avs))]
    interpretTransaction c
interpretTransaction (Free (Retract e a v c)) = do
    tell [EDN.notag (EDN.makeVec [dbretract,EDN.toEDN e,EDN.toEDN a,EDN.toEDN v])]
    interpretTransaction c
interpretTransaction (Free (DataFunction (RetractEntity e) c)) = do
    tell [EDN.notag (EDN.makeVec [dbfnretractentity,EDN.toEDN e])]
    interpretTransaction c

-- | Interpret a Transaction into its EDN represetation. Also available through
--   the `ToEDN` instance for Transaction.
literalRepresentation :: Transaction a -> EDN.TaggedValue
literalRepresentation = EDN.notag . EDN.makeVec . flip evalState 0 . execWriterT . interpretTransaction

