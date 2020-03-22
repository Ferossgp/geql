(ns geql.core-test
  "Taken from pathom"
  (:require [clojure.test :refer [deftest are]]
            [geql.core :as geql]
            [edn-query-language.core :as eql])
  #?(:clj (:import [java.util UUID])))

(defn aliased [alias key]
  (eql/update-property-param key assoc ::geql/alias alias))

(defn uuid* [s]
  #?(:clj (UUID/fromString s)
     :cljs (uuid s)))

(deftest test-query->graphql
  (are [query out] (= (geql/query->graphql query) out)
                                        ; properties
    [] "query {}"
    [:property] "query {property}"
    [:qualified/property] "query {property}"

    ;; on
    '[:hello (:other {::geql/on "User"})]
    "query {hello,... on User{other}}"

    ;; params
    '[(:parameterized {:foo "bar"})]
    "query {parameterized(foo:\"bar\")}"

    '[(:parameterized {:foo [a b]})]
    "query {parameterized(foo:[a, b])}"

    `[(:parameterized {:foo ~(uuid* "ead34300-0ef6-4c31-9626-90bf18fa22c0")})]
    "query {parameterized(foo:\"ead34300-0ef6-4c31-9626-90bf18fa22c0\")}"

    ;; aliasing
    '[(:property {::geql/alias "aliased"})]
    "query {aliased:property}"

    '[{(:property {::geql/alias "aliased" :another "param"})
       [:subquery]}]
    "query {aliased:property(another:\"param\"){subquery}}"

    ;; ident

    [{[:Item/id 123] [:id :name]}]
    "query {_Item_id_123:Item(id:123){id,name}}"

    [{[:service.Item/id 123] [:id :name]}]
    "query {_service_Item_id_123:Item(id:123){id,name}}"

    [{[:item/name-and-owner ["NAM" "OWN"]] [:id :name]}]
    "query {_item_name_and_owner_NAM_OWN:item(name:\"NAM\",owner:\"OWN\"){id,name}}"

    [{[:Item/slug "some-post"] [:id :slug]}]
    "query {_Item_slug_some_post:Item(slug:\"some-post\"){id,slug}}"

    [{[:Item/id "123,45"] [:id :name]}]
    "query {_Item_id_123_45:Item(id:\"123,45\"){id,name}}"

    [{[:Item/id 123] [:id :name]}
     {[:Item/id 321] [:id :name]}]
    "query {_Item_id_123:Item(id:123){id,name},_Item_id_321:Item(id:321){id,name}}"

    '[({[:Item/id 123] [:id :name]} {:name "bla"})]
    "query {_Item_id_123:Item(id:123,name:\"bla\"){id,name}}"

    [{:all-items [:id :name]}]
    "query {all-items{id,name}}"

    '[{:all-items [:hello
                   (:other {::geql/on "User"})
                   (:foo {::geql/on "User"})
                   (:location {::geql/on "Place"})]}]
    "query {all-items{hello,... on User{other,foo},... on Place{location}}}"

    '[({:nodes [:id :user/name]} {:last 10})]
    "query {nodes(last:10){id,name}}"

    [{:search
      {:User  [:username]
       :Movie [:director]
       :Book  [:author]}}]
    "query {search{__typename,... on User{username},... on Movie{director},... on Book{author}}}"

    [:id {:parent 3}]
    "query {id,parent{id,parent{id,parent{id}}}}"

    [:id {:parent '...}]
    "query {id,parent{id,parent{id,parent{id,parent{id,parent{id}}}}}}"

    '[(call {:param "value"})]
    "mutation {call(param:\"value\")}"

    '[(call {:enum HEY})]
    "mutation {call(enum:HEY)}"

    [(list 'call {:id "id" :param "value"})]
    "mutation {call(id:\"id\",param:\"value\")}"

    [(list 'call {:id "id" :param "value" ::geql/mutate-join []})]
    "mutation {call(id:\"id\",param:\"value\")}"

    '[{(call {:param "value" :item/value 42}) [:id :foo]}]
    "mutation {call(param:\"value\",value:42){id,foo}}"

    '[{(call {:param "value" :item/value 42}) [*]}]
    "mutation {call(param:\"value\",value:42)}"

    '[(call {:param {:nested "value"}})]
    "mutation {call(param:{nested:\"value\"})}"

    '[(call {:param "value" :item/value 42 ::geql/mutate-join [:id :foo]})]
    "mutation {call(param:\"value\",value:42){id,foo}}"

    ;; Variables
    [{'(:hero {:episode {:variable/name "episode"
                         :variable/type :String}})

      [:name {:friends [:name]}]}]
    "query ($episode:String){hero(episode:$episode){name,friends{name}}}"

    [{'(createReview {:episode {:variable/name "episode"
                                :variable/type :Episode!}
                      :stars   {:variable/name    "stars"
                                :variable/type    :ReviewStars!
                                :variable/default 5}})

      [:name :stars]}]
    "mutation ($episode:Episode!,$stars:ReviewStars!=5){createReview(episode:$episode,stars:$stars){name,stars}}"))

(deftest variables->str-test
  (are [query out] (= (geql/variables->str query) out)
    [{:variable/name "id"
      :variable/type :Int}]
    "$id:Int"

    [{:variable/name    "id"
      :variable/type    :Int
      :variable/default 2}]
    "$id:Int=2"

    [{:variable/name "id"
      :variable/type :Int}
     {:variable/name "name"
      :variable/type :String}]
    "$id:Int,$name:String"

    [{:variable/name    "id"
      :variable/type    :Int
      :variable/default 1}
     {:variable/name    "name"
      :variable/type    :String
      :variable/default "my-name"}]
    "$id:Int=1,$name:String=\"my-name\""

    nil ""
    []  ""))
