module DatomicDSL where

import Control.Monad.Free

import Control.Monad.Writer.Lazy

import Control.Monad.State.Lazy

import Data.EDN (ToEDN)

import qualified Data.EDN as EDN

test :: Transaction ()
test = do
    bob_id   <- newTempId
    alice_id <- newTempId
    multiAdd (entityTempId bob_id)   [attributeKeyword "person" "name"   |~> valueString "Bob",
                                      attributeKeyword "person" "spouse" |~> valueTempId alice_id]
    multiAdd (entityTempId alice_id) [attributeKeyword "person" "name"   |~> valueString "Alice",
                                      attributeKeyword "person" "spouse" |~> valueTempId bob_id]
    add      (entityTempId bob_id)   (attributeKeyword "person" "height") (valueDouble 1.87)

type Transaction = Free TransactionF

data TransactionF a = NewTempId (TempId -> a)
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
           | ValueBigInt Integer
           | ValueDouble Double
           -- And More

data AttributeValue = AttributeValue Attribute Value
                    | ReverseAttributeValue Attribute Value

data TempId = TempId Integer

data ExistingId = ExistingId Integer

data Keyword = Keyword String String

data DataFunction = RetractEntity Entity

instance Functor TransactionF where
    fmap f (NewTempId c) = NewTempId (f . c)
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

newTempId :: Transaction TempId
newTempId = liftF (NewTempId (\tempid -> tempid))

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

instance ToEDN Attribute where

instance ToEDN Value where

dbadd :: EDN.TaggedValue
dbadd = undefined

decrement :: TempId -> TempId
decrement (TempId x) = TempId (x - 1)

interpretTransaction :: Transaction a -> WriterT [EDN.TaggedValue] (State TempId) ()
interpretTransaction (Pure _) = return ()
interpretTransaction (Free (NewTempId c)) = lift (modify decrement >> get) >>= interpretTransaction . c
interpretTransaction (Free (Add e a v c)) = tell [EDN.notag (EDN.makeList [dbadd,EDN.toEDN e,EDN.toEDN a,EDN.toEDN v])] >> interpretTransaction c
interpretTransaction (Free (MultiAdd e avs c)) = undefined
interpretTransaction (Free (Retract e a v c)) = undefined
interpretTransaction (Free (DataFunction (RetractEntity e) c)) = undefined

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
