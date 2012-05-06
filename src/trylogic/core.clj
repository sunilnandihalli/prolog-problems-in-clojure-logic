(ns trylogic.core
  (:refer-clojure :exclude [inc reify ==])
  (:require [trylogic.arithmetic :as arith])
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]))

(defne lasto [l seq]
  ([l [l]])
  ([l [_ . ?d]] (lasto l ?d)))

(defne penultimato [p seq]
  ([p [p _]])
  ([p [_ . ?rst]] (penultimato p ?rst)))


;; k-th element incomplete..

;;(defne deco [p p-1] (fresh [q q-1]         ))


(defne kth-elemento [p seq k]
  ([p [p . _] 1])
  ([p [_ . ?rst] k] (kth-elemento p ?rst (dec k))))


;; number of elements


(defn num-elements [n seq])




;; reverse a list


(defne revo [x y]
  ([[] []])
  ([[?x . ?xs] _]
     (fresh [a]
            (revo ?xs a)
            (appendo a [?x] y))))





#_ (run* [q] (revo q [1 2 3 4 5]))


;;palindrome test

(defn palindromo [q]
  (revo q q))


#_ (run* [q] (palindromo [1 2 1]))

;; check it input is a list

(defne listo [a]
  ([[]])
  ([[?af . ?arst]]))

#_ (run* [q] (listo [2]))

;; flatten
(defne flato [q qflat]
  ([[] []])
  ([[?qf . ?qs] _] (conda
                    ((listo ?qf) (fresh [a b]
                                        (flato ?qf a)
                                        (flato ?qs b)
                                        (appendo a b qflat)))
                    ((fresh [a]
                            (flato ?qs a)
                            (conso ?qf a qflat))))))
;; does not work reverse works forward..
#_ (run 5 [q] (flato q [1 2 3 4 5 6 7]))

;; 1.08 eliminate consecutive duplicates..


(defna compresso [q qc]
  ([[] []])
  ([[?a ?a . ?ars] _] (fresh [a]
                         (conso ?a ?ars a)
                         (compresso a qc)))
  ([[?a . ?ars] _] (fresh [a]
                          (compresso ?ars a)
                          (conso ?a a qc))))

#_ (run* [q] (compresso [1 1 2 2 2 3 3 3] q))
#_ (run 3 [q] (compresso q [1 2 3]))

;; 1.09 pack...



(defne hpack [q equals accumulator p]
  ([[] _ _ _] (conso equals accumulator p))
  ([[?q . ?qs] [] _ _]
     (hpack ?qs [?q] accumulator p))
  ([[?e . ?qs] [?e . _] _ _]
     (fresh [a]
            (conso ?e equals a)
            (hpack ?qs a accumulator p)))
  ([[?q . ?qs] [?e . _] _ _]
     (fresh [a b]
            (!= ?e ?q)
            (conso equals accumulator b)
            (hpack ?qs [?q] b p))))

(defn pack [p q]
  (hpack p [] [] q))
#_ (run* [q] (pack [1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 4] q))


;; 1.10 run-length encoding ..

;;-notdone--


;; 1.11 modified-run-length

;;----


;; 1.12 run-length direct

;;----


;; 1.13 duplicates




(defne hdup [a b acc]
  ([[] _ _] (== b acc))
  ([[?a . ?as] _ _] (fresh [c]
                           (appendo [?a ?a] acc c)
                           (hdup ?as b c))))

(defn dup [a b]
  (hdup a b []))

#_ (run 3 [q] (dup [1 2 3 4] q))


;; 1.15 duplicate given number of times...

(let [one '(1)]
  (defn inco [p p+1]
   (arith/pluso p one p+1)))



(defn hcopyn [a n b acc]
  )
(defn copyn [a n b]
  )

(defne hdup [a b acc]
  ([?a . ?as] b ))

(defn dupn [a b]
  )

;; 1.16 drop every nth from list


;; 1.17 split the list at n

;; 1.18 rotat n spaces left ...


;; 