(ns trylogic.core
  (:refer-clojure :exclude [inc reify ==])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]))

(defne lasto [l seq]
  ([l [l]])
  ([l [_ . ?d]] (lasto l ?d)))

(defne penultimato [p seq]
  ([p [p _]])
  ([p [_ . ?rst]] (penultimato p ?rst)))



(defne kth-elemento [p seq k]
  ([p [p . _] 1])
  ([p [_ . ?rst] k] (kth-elemento p ?rst (dec k))))


