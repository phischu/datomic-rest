module DatomicDSL where

import Control.Monad.Free

import Control.Monad.Writer.Lazy

import Control.Monad.State.Lazy

import Data.EDN (ToEDN)

import qualified Data.EDN as EDN

import Data.Word

import qualified Data.ByteString.Char8 as BS

test :: Transaction ()
test = do
    bob_id   <- newTempId "user"
    alice_id <- newTempId "user"
    multiAdd (entityTempId bob_id)   [attributeKeyword "person" "name"   |~> valueString "Bob",
                                      attributeKeyword "person" "spouse" |~> valueTempId alice_id]
    multiAdd (entityTempId alice_id) [attributeKeyword "person" "name"   |~> valueString "Alice",
                                      attributeKeyword "person" "spouse" |~> valueTempId bob_id]
    add      (entityTempId bob_id)   (attributeKeyword "person" "height") (valueDouble 1.87)

type Transaction = Free TransactionF

data TransactionF a = NewTempId String (TempId -> a)
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
           | ValueString String
           | ValueBoolean Bool
           | ValueLong Integer
           | ValueBigInt Integer
           | ValueFloat Float
           | ValueDouble Double
           | ValueBigDec Rational
           | ValueInstant Integer
           | ValueUUID Integer
           | ValueURI String
           | ValueBytes [Word8]

data AttributeValue = AttributeValue Attribute Value
                    | ReverseAttributeValue Attribute Value

data TempId = TempId String Integer

data ExistingId = ExistingId Integer

data Keyword = Keyword String String

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

attributeKeyword :: String -> String -> Attribute
attributeKeyword namespace name = AttributeKeyword (Keyword namespace name)

-- Values

valueTempId :: TempId -> Value
valueTempId = ValueTempId

valueExistingId :: ExistingId -> Value
valueExistingId = ValueExistingId

valueKeyword :: Keyword -> Value
valueKeyword = ValueKeyword

valueString :: String -> Value
valueString = ValueString

valueBoolean :: Bool -> Value
valueBoolean = ValueBoolean

valueBigInt :: Integer -> Value
valueBigInt = ValueBigInt

valueDouble :: Double -> Value
valueDouble = ValueDouble

-- Statements

newTempId :: String -> Transaction TempId
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
(|~>) = undefined

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
    toEDN (ValueString string) = EDN.toEDN string
    toEDN (ValueBoolean bool) = EDN.toEDN bool
    toEDN (ValueLong long) = EDN.toEDN long -- Should be Int64 or sth.
    toEDN (ValueBigInt integer) = EDN.toEDN integer
    toEDN (ValueFloat float) = EDN.toEDN (fromRational (toRational float) :: Double)
    toEDN (ValueDouble double) = EDN.toEDN double
    toEDN _ = error "Instance ToEDN Attribute not yet complete"

instance ToEDN TempId where
    toEDN (TempId part integer) = EDN.tag (BS.pack "db") (BS.pack "id") (EDN.makeList [EDN.keyword (BS.pack (":db.part/"++part)),EDN.toEDN integer])

-------- NEEEED UTF-8 SUPPORT

instance ToEDN ExistingId where

instance ToEDN Keyword where

dbadd :: EDN.TaggedValue
dbadd = undefined

dbid :: EDN.Value
dbid = undefined

attributeValueToEDNPair :: AttributeValue -> EDN.Pair
attributeValueToEDNPair = undefined

dbretract :: EDN.TaggedValue
dbretract = undefined

dbfnretractentity :: EDN.TaggedValue
dbfnretractentity = undefined

decrement :: Integer -> Integer
decrement x = x - 1

interpretTransaction :: Transaction a -> WriterT [EDN.TaggedValue] (State Integer) ()
interpretTransaction (Pure _) = return ()
interpretTransaction (Free (NewTempId part c)) = lift (modify decrement >> get) >>= interpretTransaction . c . TempId part
interpretTransaction (Free (Add e a v c)) = tell [EDN.toEDN [dbadd,EDN.toEDN e,EDN.toEDN a,EDN.toEDN v]] >> interpretTransaction c
interpretTransaction (Free (MultiAdd e avs c)) = tell [EDN.toEDN (EDN.makeMap ((dbid,EDN.toEDN e):map attributeValueToEDNPair avs))] >> interpretTransaction c
interpretTransaction (Free (Retract e a v c)) = tell [EDN.toEDN [dbretract,EDN.toEDN e,EDN.toEDN a,EDN.toEDN v]] >> interpretTransaction c
interpretTransaction (Free (DataFunction (RetractEntity e) c)) = tell [EDN.toEDN [dbfnretractentity,EDN.toEDN e]] >> interpretTransaction c

literalRepresentation :: Transaction a -> EDN.TaggedValue
literalRepresentation = EDN.toEDN . flip evalState 0 . execWriterT . interpretTransaction

{-

data TransactionF a = NewTempID (TempID -> a)
                    | Add EAV a
                    | Retract EAV a

type TempID = Int

data EAV = EAV EntityId Attribute Value

data Transaction = Transaction [TransactionStatement]

data TransactionStatement = Addition EntityId Attribute Value
                          | Retraction EntityId Attribute Value
                          | DataFunction DataFunction
                          | MultiAddition EntityId [AttributeValuePair]

data AttributeValuePair = AttributeValuePair Attribute Value
                        | BackwardAttributeValuePair Attribute Value

data DataFunction = RetractEntity EntityId

data EntityId = Temporary Partition
              | TemporaryId Partition NegativeInt
              | Existing PositiveInt
              | Identifier Identifier

data Attribute = Attribute

data Value = Value

data PositiveInt = PositiveInt Int

data NegativeInt = NegativeInt Int

data Partition = Partition

data Identifier = Ident
-}
