(ns yes.logic
  (:refer-clojure :exclude [==])

  (:use [clojure.core.logic]
        [clojure.core.logic.pldb]))

(db-rel -terrain x)
(db-rel -building x)
(db-rel -actor x)


(db-rel -type x)
(db-rel -type-of x y)

(db-rel -on-tile x y)
(db-rel -is-terrain x y)
(db-rel -covers-area b vxy8)

(db-rel -sex x y)

(db-rel -shelter x)
(db-rel -requires x y)
(db-rel -neighbor x y)

(db-rel -owner x y)
(db-rel -entity x y)
(db-rel -renter x y)
(db-rel -owner x y)
(db-rel -member x y)

(db-rel -x8 x y)
(db-rel -y8 x y)

(db-rel -group x)
(db-rel -title x y)
(db-rel -subordinate t st)
;leader is implied if there is noone he follows
(db-rel -follower x y)


(def -known (atom
              (db )))

(defn record [rel & args]
  (swap! -known (fn [a] (apply db-fact @-known rel args))))


(def -semantic
  (db [-subordinate :king :queen]
      [-subordinate :queen :maid]
      [-subordinate :chief :cutpurse]
      [-subordinate :innkeeper :maid]))

(defn owner [x y z]
  (conde
   ((-owner x y)) ((-member z x)
                   (-owner z y))))

(defn shelter [x]
  (fresh [y]
  (conde
   ((-shelter x)) ((-shelter y)(-type-of x y)) )))

(defn title [k v]

  (-title k v)
  (!= v :none))

(defn are-type [k & more])

(def -common
  (db [-terrain :sea]
      [-terrain :grass]
      [-terrain :desert]
      [-type :castle]
      [-type :inn]
      [-type :hut]
      [-type :farm]
      [-type :small-farm]
      [-type :harbor]
      [-type :mine]
      [-shelter :castle]
      [-shelter :inn]
      [-shelter :hut]
      [-requires :harbor :sea]
      ))




(defne righto [x y l]
  ([_ _ [x y . ?r]])
  ([_ _ [_ . ?r]] (righto x y ?r)))

(defn nexto [x y l]
  (conde
    ((righto x y l))
    ((righto y x l))))


(defn neighbors-of [b n]
  (fresh [a1]
  (-covers-area b a1)
  (-covers-area n a1)
   (!= n b)))


(def MASK-UID false)

(defn mask! [uids]
  (def MASK-UID
    (set (flatten uids))))



(comment
 (mask!
  (with-dbs
    [@-known -common -semantic ]
    (run 100 [q]
         (fresh [k v r f m]
                (-owner k r)
                (-sex k :female)
                (== q [k r])
                )))))







(def -pattern (atom
              (db )))



(defn precord [rel & args]
  (swap! -pattern (fn [a] (apply db-fact @-pattern rel args))))

(db-rel -material m)
(db-rel -proud p)
(db-rel -shy s)
(db-rel -tile t)
(db-rel -is t m)
(db-rel -t->n t t)
(db-rel -t->s t t)
(db-rel -t->e t t)
(db-rel -t->w t t)





(comment
  (def TEST [[:g :d :g :s :s]
           [:g :d :m :s :d]
           [:m :g :m :s :m]
           [:d :s :m :s :g]
           [:g :d :m :s :d]])
  (dorun (for [y (range (count TEST))
               x (range (count (first TEST)))
               :let [tile (get (get TEST y) x)]]
           (do
             (precord -tile [x y])
             (precord -is [x y] tile)
             (precord -t->e [x y] [(inc x) y])
             (precord -t->s [x y] [x (inc y)])))))




(def -tile-patterns
  (db [-material :grass]
      [-material :desert]
      [-material :sea]
      [-material :mountains]
      [-proud :grass]
      [-proud :desert]
      [-shy :sea]
      [-shy :mountains]))


(defn t->n [t1 t2]
  (conde
   ((-t->n t1 t2))
   ((-t->s t2 t1))))

(defn t->s [t1 t2]
  (conde
   ((-t->s t1 t2))
   ((-t->n t2 t1))))

(defn t->e [t1 t2]
  (conde
   ((-t->e t1 t2))
   ((-t->w t2 t1))))

(defn t->w [t1 t2]
  (conde
   ((-t->w t1 t2))
   ((-t->e t2 t1))))

(defn find-pattern [pat t n s e w]
  (onceo
   (matche [n s e w]
         ([t t t t] (== pat :nsew))
          ([_ t t t] (== pat :sew))
          ([t _ t t] (== pat :new))
          ([t t t _] (== pat :nse))
          ([_ _ t t] (== pat :ew))
          ([t t _ _] (== pat :ns))
          ([t _ t _] (== pat :ne))
          ([t _ _ t] (== pat :nw))
          ([_ t _ t] (== pat :sw))
          ([_ t t _] (== pat :se))
          ([t _ _ _] (== pat :n))
          ([_ _ t _] (== pat :e))
          ([_ t _ _] (== pat :s))
          ([_ _ _ t] (== pat :w))
          ([_ _ _ _] (== pat :i))

           )))





(defn tile-textures [board]
  (mapv (fn [e]
          (let [[x y] [(:x e)(:y e)]
                terrain (get @(:state e) :terrain)]
            (precord -tile [x y])
            (precord -is [x y] terrain)
            (precord -t->e [x y] [(inc x) y])
            (precord -t->s [x y] [x (inc y)])
            [x y];terrain
            )) (vals board))

  (with-dbs
    [@-pattern ]
    (run* [q]
          (fresh [t n s e w pt pn ps pe pw pat]
                 (-tile t)
                 (onceo
                  (all
                   (t->n t n)
                   (t->s t s)
                   (t->w t w)
                   (t->e t e)
                   (-is t pt)(-is n pn)(-is s ps)(-is w pw)(-is e pe)
                   (find-pattern pat pt pn ps pe pw)))

                 (== q [t [pat pt]])))))



