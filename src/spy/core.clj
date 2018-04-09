(ns spy.core)

(defn- get-hostname []
  (.. java.net.InetAddress getLocalHost getHostName))

(def debug?
  "ADAPT ME!!"
  (delay (or (= "dev" (System/getProperty "nomad.env"))
             (= (get-hostname) "sky"))))

(defonce spy-data (atom {#_ns #_{form {line data}}}))
#_(reset! spy-data {})

(defn find-form
  [data form seek-line]
  ;; If form is nil, we seek the closest line
  ;; If form is given, we seek for the closest which have the given form
  (let [filt (if (some? form) #(= % form) (constantly true))
        [form line]
        (reduce-kv
          (fn [[found-form found-line] form line->data]
            (reduce-kv
              (fn [[curr-form curr-line] line-candidate _]
                (if (and (filt form) ;; We have a match: See if it's closer than prev match
                         (< (Math/abs ^long (- seek-line line-candidate))
                            (Math/abs ^long (- curr-line line-candidate))))
                  [form line-candidate]
                  [curr-form curr-line]))
              [found-form found-line] line->data))
          [form 0] data)]
    (get-in data [form line])))

(defmacro spy
  "Always gets run. Maybe with spying, maybe not.
   Returns the result of the last argumennt"
  [& forms]
  (if @debug?
    (let [line (:line (meta &form))
          local-bindings (set (keys &env))
          ns *ns*]
      (if (and (some? forms)
               (not (empty? local-bindings)))
        `(let [res# (do ~@forms)]
           (swap! spy-data assoc-in [~ns (quote ~forms) ~line] res#)
           res#)
        `(find-form (get (deref spy-data) ~ns) (quote ~forms) ~line)))
    `(do ~@forms)))

(defmacro spyo
  "Does NOT emit forms unless debug is true."
  [& forms]
  (when @debug?
    (let [line (:line (meta &form))
          local-bindings (set (keys &env))
          ns *ns*]
      (if (and (some? forms)
               (not (empty? local-bindings)))
        `(let [res# (do ~@forms)]
           (swap! spy-data assoc-in [~ns (quote ~forms) ~line] res#)
           res#)
        `(find-form (get (deref spy-data) ~ns) (quote ~forms) ~line)))))


