(ns geql.core
  "This code is taken from https://github.com/wilkerlucio/pathom/blob/master/src/com/wsscode/pathom/graphql.cljc"
  #?(:cljs (:require-macros [geql.core :refer [defquery]]))
  (:require #?(:clj [clojure.data.json :as json])
            [geql.eql-ast :as eql-ast]
            [clojure.string :as cstr]))

(def ^:dynamic *unbounded-recursion-count* 5)

(defn has-call? [children]
  (->> children
       (filter (fn [{:keys [type]}] (= :call type)))
       first boolean))

(defn find-id [m tempid?]
  (->> m
       (filter (fn [[_ v]] (tempid? v)))
       first))

(defn stringify [x]
  #?(:clj  (json/write-str (cond-> x
                             (uuid? x) str))
     :cljs (js/JSON.stringify (clj->js x))))

(defn var->graphql [x]
  (str "$" (get x :variable/name)))

(defn var-type->str [variable-type]
  (cond
    (keyword? variable-type)
    (name variable-type)

    (and (vector? variable-type)
         (= (first variable-type) :required))
    (str (var-type->str (second variable-type)) "!")

    (and (coll? variable-type) (= (count variable-type) 1))
    (str "[" (name (first variable-type)) "]")))

(defn variables->str
  "Given a vector of variable maps, formats them and concatenates to string.
  E.g. (variables->str [{:variable/name \"id\" :variable/type :Int}]) => \"$id: Int\""
  [variables]
  (->> (for [{var-name :variable/name var-type :variable/type var-default :variable/default} variables]
         (str "$" var-name ":" (var-type->str var-type) (when var-default (str "=" (stringify var-default)))))
       (interpose ",")
       (apply str)))

(defn params->graphql
  ([x js-name tempid?] (params->graphql x js-name tempid? true))
  ([x js-name tempid? root?]
   (cond
     (and (map? x) (contains? x :variable/name))
     {:graphql   (var->graphql x)
      :variables [x]}

     (map? x)
     (let [params         (->> (into [] (comp
                                         (remove (fn [[_ v]] (tempid? v)))
                                         (map (fn [[k v]] (assoc (params->graphql v js-name tempid? false)
                                                                 :k k)))) x))
           graphql-params (->> (map (fn [param]
                                      (str (js-name (:k param)) ":" (:graphql param))) params)
                               (cstr/join ","))
           variables      (mapcat :variables params)]
       {:graphql   (if root?
                     (str "(" graphql-params ")")
                     (str "{" graphql-params "}"))
        :variables variables})

     (sequential? x)
     {:graphql (str "[" (cstr/join ", " (mapv (comp :graphql #(params->graphql % js-name tempid? false)) x)) "]")}

     (symbol? x)
     {:graphql (name x)}

     :else
     {:graphql (stringify x)})))

(defn ident->alias
  "Convert ident like [:Contact/by-id 123] to an usable GraphQL alias (eg: _COLON_Contact_SLASH_by_id_123)."
  [[base value]]
  (let [value (if (vector? value) (cstr/join "_" value) value)]
    (-> (str base "_" value) (cstr/replace #"[^a-zA-Z0-9_]" "_"))))

(defn ident-transform [[key value]]
  (let [fields (if-let [field-part (name key)]
                 (cstr/split field-part #"-and-|And") ["id"])
        value  (if (vector? value) value [value])]
    (if-not (= (count fields) (count value))
      (throw (ex-info "The number of fields on value needs to match the entries" {:key key :value value})))
    {::selector (-> (namespace key) (cstr/split #"\.") last)
     ::params   (zipmap fields value)}))

(defn group-inline-unions [children]
  (let [{general nil :as groups} (group-by #(get-in % [:params ::on]) children)
        groups                   (->> (dissoc groups nil)
                                      (into [] (map (fn [[k v]] {:type      :union-entry
                                                                 :union-key k
                                                                 :children  (mapv #(update % :params dissoc ::on) v)}))))]
    (concat general groups)))

(def special-params #{::on ::alias})

(defn update-child
  "Given an AST, find the child with a given key and run update against it."
  [ast key & args]
  (if-let [idx (some->> (:children ast)
                        (map-indexed vector)
                        (filter (comp #{key} :key second))
                        ffirst)]
    (apply update-in ast [:children idx] args)
    ast))

(defn update-recursive-depth
  "Given an AST, find the child with a given key and run update against it."
  [ast key & args]
  (if-let [idx (some->> (:children ast)
                        (map-indexed vector)
                        (filter (comp #(and (= key (:key %))
                                            (pos-int? (:query %))) second))
                        ffirst)]
    (apply update-in ast [:children idx :query] args)
    ast))

(defn node->graphql [{:keys  [type children key dispatch-key params union-key query]
                      ::keys [js-name ident-transform parent-children tempid?]
                      :or    {tempid? (constantly false)}}]
  (letfn [(continue [x]
            (node->graphql (assoc x
                                  ::parent-children (or (::parent-children x) children)
                                  ::js-name js-name
                                  ::tempid? tempid?
                                  ::ident-transform ident-transform)))]
    (let [{::keys [alias]} params
          params           (apply dissoc params special-params)]
      (case type
        :root
        (let [grouped-unions (map continue (group-inline-unions children))
              variables      (mapcat :variables grouped-unions)]
          ;; TODO: Add support for subscription, can be added as a namespace keyword :subscription/entity
          (str (if (has-call? children) "mutation " "query ")
               (when (pos? (count variables))
                 (str "(" (variables->str variables) ")"))
               "{" (cstr/join "," (map :graphql grouped-unions)) "}"))

        :join
        (if (= 0 query)
          ""
          (let [header         (if (vector? key)
                                 (assoc (ident-transform key)
                                        ::index (ident->alias key))
                                 {::index    alias
                                  ::selector dispatch-key
                                  ::params   nil})
                params         (merge (::params header) params)
                graphql-params (when (seq params)
                                 (params->graphql params js-name tempid?))
                children       (cond
                                 (= '... query)
                                 (let [parent (-> (update-child {:children parent-children}
                                                                key
                                                                assoc
                                                                :query
                                                                (dec *unbounded-recursion-count*))
                                                  :children)]
                                   (mapv #(assoc % ::parent-children parent) parent))

                                 (pos-int? query)
                                 (let [parent (-> (update-recursive-depth {:children parent-children} key dec)
                                                  :children)]
                                   (mapv #(assoc % ::parent-children parent) parent))

                                 :else
                                 children)
                grouped-unions (->> children
                                    group-inline-unions
                                    (map continue)
                                    (filter seq))]
            {:graphql   (str
                         (when (::index header) (str (::index header) ":"))
                         (js-name (::selector header))
                         (:graphql graphql-params)
                         "{"
                         (cstr/join "," (map :graphql grouped-unions))
                         "}")
             :variables (concat (mapcat :variables grouped-unions)
                                (:variables graphql-params))}))

        :call
        (let [{::keys [mutate-join]} params
              graphql-params         (params->graphql (dissoc params ::mutate-join) js-name tempid?)
              children               (->> (or (some-> mutate-join eql-ast/query->ast :children)
                                              children)
                                          (remove (comp #{'*} :key)))]
          {:graphql   (str (js-name dispatch-key)
                           (:graphql graphql-params)
                           (if (seq children)
                             (str "{" (cstr/join "," (map (comp :graphql continue) children)) "}")
                             (when-let [[k _] (find-id params tempid?)]
                               (str
                                "{" (js-name k) "}"))))
           :variables (concat (mapcat :variables children)
                              (:variables graphql-params))})

        :union
        {:graphql   (cstr/join "," (into ["__typename"] (map (comp :graphql continue) children)))
         :variables (mapcat :variables children)}

        :union-entry
        {:graphql   (str "... on " (if (string? union-key) union-key (js-name union-key)) "{"
                         (cstr/join "," (map (comp :graphql continue) children))
                         "}")
         :variables (mapcat :variables children)}

        :prop
        {:graphql (str (when alias (str alias ":"))
                       (js-name dispatch-key)
                       (when (seq params) (:graphql (params->graphql params js-name tempid?))))}))))

(defn query->graphql
  "Convert query from EDN format to GraphQL string."
  ([query] (query->graphql query {}))
  ([query options]
   (let [ast (eql-ast/query->ast query)]
     (node->graphql (merge
                     ast
                     {::js-name         name
                      ::ident-transform ident-transform
                      ::parent-children (:children ast)}
                     options)))))

#?(:clj
   (defmacro defquery [sym query]
     (let [source (query->graphql query)]
       `(def ~sym {:query-eql ~query
                   :query-str ~source}))))
