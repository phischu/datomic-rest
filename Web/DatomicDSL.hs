module DatomicDSL where

import Control.Monad.Free

test :: Transaction ()
test = do
    bob_id   <- newTempId
    alice_id <- newTempId
    multiAdd (entityTempId bob_id)   [keywordNS "person" "name"   |~> string      "Bob",
                                      keywordNS "person" "spouse" |~> valueTempId alice_id]
    multiAdd (entityTempId alice_id) [keywordNS "person" "name"   |~> string      "Alice",
                                      keywordNS "person" "spouse" |~> valueTempId bob_id]
    add (entityTempId bob_id) (keywordNS "person" "height") (floating 1.87)

type Transaction = Free TransactionF

data TransactionF a = TransactionF

data AttributeValuePair = AttributeValuePair

data TempId = TempId

instance Functor TransactionF where
    fmap _ _ = TransactionF

data Attribute = Attribute

data Value = Value

keywordNS :: String -> String -> Attribute
keywordNS = undefined

string :: String -> Value
string = undefined

entityTempId :: TempId -> Entity
entityTempId = undefined

valueTempId :: TempId -> Value
valueTempId = undefined

(|~>) :: Attribute -> Value -> AttributeValuePair
(|~>) = undefined

multiAdd :: Entity -> [AttributeValuePair] -> Transaction ()
multiAdd = undefined

add :: Entity -> Attribute -> Value -> Transaction ()
add = undefined

data Entity = Entity

floating :: Double -> Value
floating = undefined

newTempId :: Transaction TempId
newTempId = undefined

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
