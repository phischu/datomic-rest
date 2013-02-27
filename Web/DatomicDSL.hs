module DatomicDSL where



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

