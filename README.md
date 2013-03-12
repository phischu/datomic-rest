datomic-rest
============

Talk to datomic from Haskell.

Do not use it as it is work in progress!

What is implemented?

A few servercalls:

```haskell
main = do
    createDatabase address "hello" "world"
    transact address "hello" "world" transaction
    q address query queryinput

Just address = parseURI "http://127.0.0.1:9834"

Just transaction = maybeResult (EDN.parseS "[{:db/id #db/id[:db.part/user] :db/doc \"Made up String\"}]")

Just query = maybeResult (EDN.parseS "[:find ?e ?v :in $ :where [?e :db/doc ?v]]")

Just queryinput = maybeResult (EDN.parseS "[{:db/alias \"hello/world\"}]")
```
A DSL for constructing transactions:

```haskell
transaction = toEDN $ do
    bob_id   <- newTempId (key "db.part" "user")
    alice_id <- newTempId (key "db.part" "user")
    multiAdd bob_id   [key "person" "name"   |~> str "Bob",
                       key "person" "spouse" |~> alice_id ]
    multiAdd alice_id [key "person" "name"   |~> str "Alice",
                       key "person" "spouse" |~> bob_id   ]
    add      bob_id   (key "person" "height")   (dbl 1.87)
```

A DSL for constructing schemas:

```haskell
personSchema = toEDN $ do
    schema (key "person" "name") TypeString One
    schema (key "person" "spouse") TypeRef One
    schema (key "person" "height") TypeDouble One
    enum (key "db.part" "user")
         (key "person" "gender")
         [key "person.gender" "male",key "person.gender" "female"]
```



