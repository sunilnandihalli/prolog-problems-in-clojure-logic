(ns trylogic.binarytree
  (:refer-clojure :exclude [inc reify ==])
  (:require [trylogic.arithmetic :as arith]
            [clojure.contrib.core :as ccc])
  
  (:use [clojure.core.logic minikanren prelude nonrel match disequality]))


;; check-if-tree

(defne istree [t]
  ([[]])
  ([[:node ?val ?c1 ?c2]] (fresh []
                              (istree ?c1)
                              (istree ?c2))))


#_ (run* [q] (istree [:node 10 [:node 20 [] []] []]))
#_ (run 5 [q] (istree q))
;; balanced-trees
(let [zero (arith/build-num 0)
      one (arith/build-num 1)]
 (defnu count-nodes [t cnt]
   ([[] zero])
   ([[:node _ ?c1 ?c2] _]
      (fresh [nc1 nc2 a]
             (count-nodes ?c1 nc1)
             (count-nodes ?c2 nc2)
             (arith/pluso one nc1 a)
             (arith/pluso nc2 a cnt)))))

#_ (map arith/spit-number (run* [q]
                                (count-nodes [:node 10 [] []] q)))

(defn logic-num? [num _]
  (cond
   (number? num) :regular-num
   (seq? num) :logic-num
   (lvar? num) :unbound-logic-num))

(defmacro def-numeric-goal [numeric-goal-name [& args] & body]
  (let [numeric-goal-args (group-by #(cond
                                      (-?> :type) :integral) args)]
    `(defn ~numeric-goal-name ~(vec args)
       ~numeric-goal-args
       ~@body)))


(def-numeric-goal add [^Integer a ^Integer b c]
  (with-out-str
    (clojure.pprint/pprint {:a a :b b :c c})))

(defmulti counted-tree logic-num?)
(defmethod counted-tree :logic-num [logic-num tree]
  (fresh []
         (istree tree)
         (count-nodes tree logic-num)))

(defmethod counted-tree :regular-num [reg-num tree]
  (let [logic-num (arith/build-num reg-num)]
    (counted-tree logic-num tree)))

(defn diffo [a b diff]
  (condu
   ((arith/<=lo a b) (arith/minuso b a diff))
   ((arith/>lo a b) (arith/minuso a b diff))))

(defmulti cbal-tree logic-num?)

(defmethod cbal-tree :regular-num [reg-num tree]
  (let [logic-num (arith/build-num num)]
    (cbal-tree logic-num tree)))

(defmethod cbal-tree :logic-num [logic-num tree]
  (let [one (arith/build-num 1)]
    (fresh [a b c val c1 c2]
           (counted-tree logic-num tree)
           (== [:node val c1 c2] tree)
           (count-nodes c1 a)
           (count-nodes c2 b)
           (diffo a b c)
           (arith/<=lo c one))))


#_ (let [tr [:node 20 [:node 10 [] []] []]]
     (run 10 [q] (counted-tree q tr)))
#_ (let [n (arith/build-num 2)] (run* [q] (counted-tree n q)))