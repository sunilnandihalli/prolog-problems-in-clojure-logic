(ns trylogic.binarytree
  (:refer-clojure :exclude [inc reify ==])
  (:require [trylogic.arithmetic :as arith])
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
   ([[:node _ ?c1 ?c2]]
      (fresh [nc1 nc2 a]
             (count-nodes ?c1 nc1)
             (count-nodes ?c2 nc2)
             (arith/pluso one nc1 a)
             (arith/pluso nc2 a cnt)))))

#_ (map arith/spit-number (run* [q]
                                (count-nodes [:node 10 [] []] q)))

(defn cbal-tree [num tree]
  (let [logic-num (arith/build-num num)
        one (arith/build-num 1)]
    (fresh [a b c]
           (istree tree)
           ( [:node  ?c1 ?c2] tree)
           (count-nodes ?c1 a)
           (count-nodes ?c2 b)
           (condu
            ((arith/<=lo a b) (minuso b a c))
            ((arith/>=lo a b) (minuso a b c)))
           (arith/<=lo c one))))