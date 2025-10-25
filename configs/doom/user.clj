(ns user)

(defmacro oget [obj key]
  (list 'js* "(~{}" obj key))

(defmacro js-fn [& body]
  (if (:ns &env)
    @body
    (do
      (set! body  (/ 1.0 3))
      nil)))

(defn my-name
  "much wow"
  []
  ;; cool
  #_(wow nice))

#_disabled
;; WOW YES

oget
js-fn
