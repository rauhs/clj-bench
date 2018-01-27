(ns clj-bench.apply
  (:require
    [criterium.core :as crit]
    [clojure.reflect :as reflect])
  (:import
    (com.squareup.javapoet MethodSpec TypeName ArrayTypeName MethodSpec$Builder
                           ParameterSpec ClassName JavaFile TypeSpec CodeBlock)
    (javax.lang.model.element Modifier)
    (java.util Arrays)
    (clojure.lang RestFn AFn ArrayChunk ChunkedCons)
    (java.lang.reflect Type)
    (javax.lang.model.type TypeMirror)
    (java.util.function ObjDoubleConsumer)))

(set! *warn-on-reflection* false)

(defn array [& items] (into-array items))

(defn modifiers
  [& mods]
  (into-array Modifier
              (map #(Modifier/valueOf (.toUpperCase ^String (name %))) mods)))

(defn add-modifier
  [x & mods]
  (.addModifiers x (apply modifiers mods)))

(defn method
  ([name]
   (method name #{}))
  ([name modifiers]
   (let [meth (MethodSpec/methodBuilder name)]
     (apply add-modifier meth modifiers)
     meth)))

(defn array-type
  [x]
  (ArrayTypeName/of x))

(defn clsname
  ([simple-name]
   (if (string? simple-name)
     (clsname "" simple-name)
     simple-name))
  ([pckg simple-name & args]
    ;(ClassName/bestGuess "Fooo")
   (ClassName/get pckg simple-name (into-array String args))))

(defn returns
  [x t]
  (.returns x (clsname t)))

(defn bclass
  [class-name super]
  (let [cb (TypeSpec/classBuilder class-name)]
    (when super (.superclass cb (clsname super)))
    cb))

(defn add-method
  [x m]
  (.addMethod x m))

(defn code!
  [x]
  (str (.build x)))

;(defprotocol ) ;; Use protocols?
(defn add-param
  [x type name]
  (.addParameter x (clsname type) name (modifiers)))

(defn statement
  ([_])
  ([x s & args]
   (.addStatement x s (into-array Object args))))

(defn add-code
  "Adds a CodeBlock to the method"
  ([x cb]
   (.addCode x cb))
  ([x cb & args]
   (.addCode x cb (into-array args))))

(defn else
  ([_])
  ([x s & args]
   (.nextControlFlow x s (into-array Object args))))

(defn package!
  "Outputs the a file inside clojure.lang package."
  [cls]
  (str (.build (JavaFile/builder "clojure.lang" cls))))

(defn do-invoke
  "Generates a doInvoke fn returning null."
  [num-pre]
  (let [meth
        (-> (method "doInvoke" #{:protected})
            (returns "Object")
            (statement "return null"))]
    (dotimes [i num-pre]
      (add-param meth "Object" (str "arg" (inc i))))
    (add-param meth "Object" "args")))
#_(code! (do-invoke 0))  ;; 0..20
#_(code! (do-invoke 20))  ;; 0..20

(defmacro >>
  [x s args & body]
  `(-> ~x
       (.beginControlFlow ~s (into-array Object ~args))
       ~@body
       (.endControlFlow)))

(defn ret1 [sym]
  (str "Util.ret1(" sym ","sym " = null)"))

(defn do-fn-invoke!
  "Generates a "
  [f ident & [farg & args]]
  (let [cb (CodeBlock/builder)]
    (.add cb "$L(" (array f))
    (.add cb (ret1 farg) (array))
    (when (some? args)
      (.add cb "\n" (array))
      (dotimes [_ ident]
        (.indent cb)))
    (doseq [arg (butlast args)]
      (.add cb (str ", " (ret1 arg) "\n") (array)))
    (when-some [lar (last args)]
      (.add cb (str ", " (ret1 lar)) (array)))
    (when (some? args)
      (dotimes [_ ident]
        (.unindent cb)))
    (.build (.add cb ");\n" (array)))))

(defn do-invoke!
  "Generates a doInvoke"
  [ident & args]
  (apply do-fn-invoke! "doInvoke" ident args))
(defn invoke!
  [ident & args]
  (if (some? args)
    (apply do-fn-invoke! "invoke" ident args)
    (CodeBlock/of "invoke();\n" (into-array []))))

(defn arg [n] (str "a" n))

;; NOTE: Can'tt change existings .applyTo arities in IFn (people reify it)
;; Add a new interface IApplyMult => .applyToMult with arities:
;; ([xs], ([arg0 xs]), ([arg0 arg1 xs]) ...)
(def atf-iname "IApplyMult")
(def atf-fname "applyToMult")

;; WORKING CODE FOR RestFn:
(let [mra "mra"
      args "args"
      m (-> (method "applyTo" #{:public})
            (returns "Object")
            (add-param "ISeq" args))]
  (statement m "int $L = getRequiredArity()" mra)
  (doseq [i (range 0 21)] ;; 21 for applyTo
    (when (pos? i)
      (-> m
          (statement "Object $L = $L.first()" (arg i) args)
          (statement "$L = $L.next()" args args)))
    (>> m "if($L == null)" [args]
        (add-code "return $L" (apply invoke! 6 (map arg (range 1 (inc i))))))
    (>> m "if($L == $L)" [mra i]
        ;; (fn [x & xs]) +
        (add-code "return $L" (apply do-invoke! 7
                                     (conj (mapv arg (range 1 (inc i))) args)))))
  (statement m "return throwArity(-1)")
  (code! m))

;; Make apply also see AFn, not just RestFn for realistic workload
(comment
  (let [xs [1 2 3 4 5 6 7] ;(vec (range 4))
        rfn (fn
              ([_ _, _ _, _ _ & xs]))
        afn (fn ([])
              ([x] x)
              ([x y] y)
              ([x y z] z)
              ([x y z, a] a)
              ([x y z, a b] b)
              ([x y z, a b c] c)
              ([x y z, a b c, d] d))]
    ;(prn "RestFn:")
    ;(crit/quick-bench (apply rfn xs))
    (prn "AFn:")
    (crit/quick-bench (apply afn xs))
    (prn "RestFn:")
    (crit/quick-bench (apply rfn xs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment
  (let [f (fn ([] "") ([& s] [s]))]
    (list (apply f [])
          (f 1)
          (bean f)))

  (-> (bclass "RestFn" "AFunction")
      (add-modifier :public :abstract)
      (add-method (-> (method "getRequiredArity" #{:abstract :public})
                      (returns Integer/TYPE)
                      (.build)))
      (code!))

  (-> (method "main")
      (add-modifier :public :static)
      (returns Object)
      (.addParameter (array-type String) "args" (modifiers))
      (code!)))
