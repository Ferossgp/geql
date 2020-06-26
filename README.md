# GEQL

A Clojure(Script) qraphql query client library. Generate graphql queries using [EQL](https://edn-query-language.org).
Code extracted from [Pathom](https://github.com/wilkerlucio/pathom) and extended to support extra graphql features.

# Usage

- `geql.core/query->graphql` is a function that can transform data in format of
EQL to graphql string.
- `geql.core/defquery` is a macros that generates a var containing the compiled
string and the query itself. This is useful for the ClojureScript app when you
want to remove some extra computations. The EQL tree is left so it can be used
at the discretion of the developer to parse the response, normalization and other
useful computations with AST.


To make use of macros, it was necessary to introduce the support for variables.
Graphql variables allow having dynamic arguments. This way a query in a form of a
static string and a dynamic map of vars can be sent together to graphql server 
without recompiling the query everytime.

### Example queries with variables

```clojure
; Query
(geql.core/query->graphql [{'(:hero {:episode {:variable/name "episode"
                                               :variable/type :String}})
                            [:name {:friends [:name]}]}])
=> "query ($episode:String){hero(episode:$episode){name,friends{name}}}"

; Mutation
(geql.core/query->graphql [{'(createReview {:episode {:variable/name "episode"
                                                      :variable/type :Episode!}
                                            :stars   {:variable/name    "stars"
                                                      :variable/type    :ReviewStars!
                                                      :variable/default 5}})
                            [:name :stars]}])
=> "mutation ($episode:Episode!,$stars:ReviewStars!=5){createReview(episode:$episode,stars:$stars){name,stars}}"
```
