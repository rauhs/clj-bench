(ns clj-bench.apply
  (:require
    [criterium.core :as crit]
    [clojure.reflect :as reflect]
    [clojure.set :as set])
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

(defmacro IF
  [x s args & body]
  `(-> ~x
       (.beginControlFlow ~s (into-array Object ~args))
       ~@body
       (.endControlFlow)))

(defn ret1
  ([sym]
   (if (symbol? sym)
     (str sym)
     (str "Util.ret1(" sym ", " sym " = null)")))
  ([expr sym]
   (str "Util.ret1(" expr ", " sym " = null)") ))

;; TODO: RT.seqToArray(
(defn do-fn-invoke!
  "Generates a "
  [f ident [farg & args] & [arr-last?]]
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
      (.add cb (str ", "
                    (if arr-last?
                      (str "RT.seqToArray(" (ret1 lar) ")")
                      (ret1 lar)))
            (array)))
    (when (some? args)
      (dotimes [_ ident]
        (.unindent cb)))
    (.build (.add cb ");\n" (array)))))

(defn invoke!
  [sym ident args & [arr-last?]]
  (if (empty? args)
    (CodeBlock/of (str sym "();\n") (into-array []))
    (do-fn-invoke! sym ident args arr-last?)))

(defn arg [n] (str "a" n))

;; AFn: Gen applyToHelper:
(let [args "args"
      ifn "ifn"
      n 4 ;; Number of given static args 0..4
      m (-> (method (str (if (zero? n) "applyTo" "multiApplyTo") "Helper")
                    #{:public :static})
            (returns "Object"))]
  (add-param m "IFn" ifn)
  (dotimes [i n]
    (add-param m "Object" (arg (inc i))))
  (add-param m "ISeq" args)
  (doseq [i (range n 21)] ;; 21 for applyTo
    (when (< n i)
      (-> m
          (statement "Object $L = $L.first()" (arg i) args)
          (statement "$L = $L.next()" args args)))
    (IF m "if($L == null)" [args]
        (add-code "return $L" (invoke! (str ifn ".invoke") 8 (map arg (range 1 (inc i)))))))
  (add-code m "return $L" (invoke! (str ifn ".invoke") 8
                                   (conj (mapv arg (range 1 21)) args) true))
  (code! m))

(defn args-list*
  [ar args]
  (if (empty? ar)
    (ret1 args)
    (reduce
      (fn [s arg]
        (str "RT.cons(" (ret1 arg) ", " s ")"))
      (str "RT.cons(" (ret1 (last ar)) ", " (ret1 args) ")")
      (reverse (butlast ar)))))

;; RestFn: Code for multiApplyTo and applyTo (n==0)
(let [mra "mra"
      n 1 ;; number of given args, eg. 2: [Object a1, Object a2, ISeq args] [0..4]
      args "args"
      m (-> (method (if (zero? n) "applyTo" "multiApplyTo") #{:public})
            (returns "Object"))]
  (dotimes [i n]
    (add-param m "Object" (arg (inc i))))
  (add-param m "ISeq" args)
  (statement m "int $L = getRequiredArity()" mra)
  ;; We still need to check mra < i or the code will walk the ENTIRE seq and then call
  ;; invoke() on it. (this would fail: (apply (fn [& x] x) 3 (range 22)) ) if not doing it.
  (doseq [i (range n)]
    (IF m "if($L == $L)" [mra i]
        (add-code
          "return $L"
          (invoke! "doInvoke" 7 (conj (mapv arg (range 1 (inc i)))
                                      (symbol (args-list* (mapv arg (range (inc i) (inc n)))
                                                          args)))))))
  (doseq [i (range n (inc n))] ;; 21
    (when (< n i)
      (-> m
          (statement "Object $L = $L.first()" (arg i) args)
          (statement "$L = $L.next()" args args)))
    (IF m "if($L == null)" [args]
        (add-code "return $L" (invoke! "invoke" 6 (map arg (range 1 (inc i))))))
    (IF m "if($L == $L)" [mra i]
        (add-code "return $L" (invoke! "doInvoke" 7 (conj (mapv arg (range 1 (inc i))) args)))))
  (statement m "return throwArity(-1)")
  (code! m))

;;;;;;;;;;;;;;;;;;;; BENCH ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; BENCH ;;;;;;;;;;;;;;;;;;;;;;;
(defn bench!
  [rfn fixed xs]
  (let [fixed (filter number? fixed)
        afn (fn
              ([] 1)
              ([x] x)
              ([x y] y)
              ([x y z] z)
              ([x y z, a] a)
              ([x y z, a b] b)
              ([x y z, a b c] c)
              ([x y z, a b c, d] d)
              ([x y z, a b c, d e] e)
              ([x y z, a b c, d e f] f)
              ([x y z, a b c, d e f, g] g)
              ([x y z, a b c, d e f, g h] h))]
    ;; Make apply also see types other than vecs:
    (apply apply (concat [afn] fixed [(into () xs)]))
    (prn "Args:" (count xs) " Fixed:" (count fixed) " -- " (clojure-version))
    ;; Always run both, even benching just one:
    ;; Make apply also see AFn, not just RestFn for realistic workload
    (doseq [f [afn rfn]]
      (prn (bases (class f)))
      (case (count fixed)
        0 (crit/quick-bench (apply f xs))
        1 (let [[x1] fixed]
            (crit/quick-bench (apply f x1 xs)))
        2 (let [[x1 x2] fixed]
            (crit/quick-bench (apply f x1 x2 xs)))
        3 (let [[x1 x2 x3] fixed]
            (crit/quick-bench (apply f x1 x2 x3 xs)))
        4 (let [[x1 x2 x3 x4] fixed]
            (crit/quick-bench (apply f x1 x2 x3 x4 xs)))))))
(defn r-0
  ([] 1)
  ([& xs] 1))

(defn r-3
  ([] 1)
  ([x] x)
  ([x y] x)
  ([x y z] z)
  ([x y z & xs] z))
(comment
  ;; Notation:
  ;; [old-time-ns, new-time-ns] <-- for RestFn
  ;; #{old-time-ns, new-time-ns} <-- for AFn
  ;; A string means the bench is redundant and was already done above
  
  ;;;;;;;;;;;;;;;;; FIXED args ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Testing RT.cons usage: (ie too many args given, MRA is very small (0))
  ;; Giving nil:
  (bench! r-0 [1 - - -] nil) #{? 18} [? 15]
  (bench! r-0 [1 2 - -] nil) #{? 19} [?, 12]
  (bench! r-0 [1 2 3 -] nil) #{? 20} [?, 9]
  (bench! r-0 [1 2 3 4] nil) #{? 45} [? 33]
  ;; Providing one arg in addition:
  (bench! r-0 [1 - - -] [1]) #{?  26} [?  21]
  (bench! r-0 [1 2 - -] [1]) #{?  25} [?, 20]
  (bench! r-0 [1 2 3 -] [1]) #{?  25} [?, 17]
  (bench! r-0 [1 2 3 4] [1]) #{?  40} [?  31]
  ;; "Common" config, ie 3 args and then rest:
  (bench! r-3 [1 - - -] [1]) #{? ""} [""]
  (bench! r-3 [1 2 - -] [1]) #{? 21} [""]
  (bench! r-3 [1 2 3 -] [1]) #{? 19} [""]
  (bench! r-3 [1 2 3 4] [1]) #{? 44} [""]


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; No fixed args
  (bench! r-0 [] [1]) #{?  21} [? 33]

  )


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
