(ns geql.eql-ast
  "This code is taken from https://github.com/wilkerlucio/pathom/blob/master/src/com/wsscode/pathom/parser.cljc"
  )

(declare expr->ast)

(defn- mark-meta [source target]
  (cond-> target
    (meta source) (assoc :meta (meta source))))

(defn symbol->ast [k]
  {:dispatch-key k
   :key          k})

(defn keyword->ast [k]
  {:type         :prop
   :dispatch-key k
   :key          k})

(defn union-entry->ast [[k v]]
  (let [component (-> v meta :component)]
    (merge
     {:type      :union-entry
      :union-key k
      :query     v
      :children  (into [] (map expr->ast) v)}
     (when-not (nil? component)
       {:component component}))))

(defn union->ast [m]
  {:type     :union
   :query    m
   :children (into [] (map union-entry->ast) m)})

(defn call->ast [[f args :as call]]
  (if (= 'quote f)
    (assoc (expr->ast args) :target (or (-> call meta :target) :remote))
    (let [ast (update-in (expr->ast f) [:params] merge (or args {}))]
      (cond-> (mark-meta call ast)
        (symbol? (:dispatch-key ast)) (assoc :type :call)))))

(defn query->ast
  "Convert a query to its AST representation."
  [query]
  (let [component (-> query meta :component)]
    (merge
     (mark-meta query
                {:type     :root
                 :children (into [] (map expr->ast) query)})
     (when-not (nil? component)
       {:component component}))))

(defn join->ast [join]
  (let [query-root? (-> join meta :query-root)
        [k v]       (first join)
        ast         (expr->ast k)
        type        (if (= :call (:type ast)) :call :join)
        component   (-> v meta :component)]
    (merge ast
           (mark-meta join {:type type :query v})
           (when-not (nil? component)
             {:component component})
           (when query-root?
             {:query-root true})
           (when-not (or (number? v) (= '... v))
             (cond
               (vector? v) {:children (into [] (map expr->ast) v)}
               (map? v)    {:children [(union->ast v)]}
               :else       (throw
                            (ex-info (str "Invalid join, " join)
                                     {:type :error/invalid-join})))))))

(defn ident->ast [[k :as ref]]
  {:type         :prop
   :dispatch-key k
   :key          ref})

(defn expr->ast
  "Given a query expression convert it into an AST."
  [x]
  (cond
    (symbol? x) (symbol->ast x)
    (keyword? x) (keyword->ast x)
    (map? x) (join->ast x)
    (vector? x) (ident->ast x)
    (seq? x) (call->ast x)
    :else (throw
           (ex-info (str "Invalid expression " x)
                    {:type :error/invalid-expression}))))
