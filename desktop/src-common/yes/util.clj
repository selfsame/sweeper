(ns yes.util
  (:use [clojure.set :only [union difference intersection]]))

(def UID (atom 0))

(defn get-uid
  ([]
   (get-uid ""))
  ([prefix]
   (keyword (str prefix (swap! UID inc)) )))

(defn uid-map [col]
  (into {} (map (fn [e]
         {(:uid e) e}) col)))

(defn atom? [x] (= (type x)(type (atom []))))

(defn get-atomic
  ([label n]
   (when (and (label n)(atom? (label n)) )
     @(label n)))
  ([label n k]
   (when (and (label n)(atom? (label n)) )
     (get @(label n) k))))

(defn set-atomic! [label t k v]
  (let [ks (if (coll? k) k [k])]
    (if (label t)
      (do (swap! (label t) assoc-in  ks v)))))

(defn update-atomic! [label t k f]
  (let [ks (if (coll? k) k [k])]
    (if (label t)
      (do (swap! (label t) update-in  ks f)))))

(defn state [& more]
  (apply get-atomic (cons :state more)))

(defn state! [& more]
  (apply set-atomic! (cons :state more)))

(defn update-state! [& more]
  (apply update-atomic! (cons :state more)))


(defn rect [& more]
  (apply get-atomic (cons :state more)))

(defn rect! [& more]
  (apply set-atomic! (cons :state more)))

(defn update-rect! [& more]
  (apply update-atomic! (cons :state more)))

(defn ^:private quick-rect-vect [thing kvec]
  (when (map? thing)
    (let [mutable (or (:rect thing) (:state thing))]
    (let [return (if (and mutable
                          (atom? mutable))
                   (mapv #(float (% @mutable)) kvec)
                   (mapv #(float (% thing)) kvec))]
      (if (= (count return) 1) (first return) return )))))

(defn x- [r] (quick-rect-vect r [:x]))
(defn y- [r] (quick-rect-vect r [:y]))
(defn w- [r] (quick-rect-vect r [:w]))
(defn h- [r] (quick-rect-vect r [:h]))
(defn xy- [r] (quick-rect-vect r [:x :y]))
(defn wh- [r] (quick-rect-vect r [:w :h]))
(defn xywh- [r] (quick-rect-vect r [:x :y :w :h]))


(defn to-grid [from to v]
  (cond (number? v) (int (/ (* v from) to))
        (vector? v) (mapv #(to-grid from to %) v)
        (sequential? v) (map #(to-grid from to %) v)
        (map? v) (into {} (map (fn [[k vv]] {k (to-grid from to vv)})
                               (select-keys v [:x :y :w :h]) ))))



(defn xywh->buckets [[x y w h]]
  "all vec coords that are described by xywh"
  (let [xrange (if (= w 0) [x] (range x (+ x w)))
        yrange (if (= h 0) [y] (range y (+ y h)))]
 (vec
    (for [gx xrange
         gy yrange]
    [gx gy]))))

(defn xywh->neighbors [xywh]
  "all vec coords that surround the xywh"
  (let [[x y w h] (mapv + xywh [-1 -1 2 2])
        xrange (if (= w 0) [x] (range x (+ x w)))
        yrange (if (= h 0) [y] (range y (+ y h)))]
 (vec
    (for [gx xrange
         gy yrange
          :when (or (or (= gx x) (= gx (+ x w -1)))
                    (or (= gy y) (= gy (+ y h -1))))]
    [gx gy]))))

(xywh->neighbors [2 2 2 2])

(defn make-hash [size]
  {:size size
   :data (atom {})
   :membership (atom {})} )

(defn hash-store [thing H]
  (when (:uid thing)
  (let [members @(:membership H)
        data @(:data H)
        size (:size H)
        uid (:uid thing)
        [x y w h] (xywh- thing)
        bucket (to-grid 1 size [x y])
        bucket-set (set (xywh->buckets (to-grid 1 size [x y w h])))
        member-record (or (get members uid) #{})]

      (when (not= member-record bucket-set)
        (let [diff-old (difference member-record bucket-set)
              diff-new (difference bucket-set member-record)]
          (do
            (dorun (for [new-bucket diff-new]
              (if (nil? (get data bucket))
                ;add the bucket key with uid set
                (swap! (:data H) conj {new-bucket (atom #{uid})})
                ;otherwise just add to existant atomic bucket
                (swap! (get data new-bucket) conj uid))))
            (dorun (for [old-bucket diff-old]
              (do (swap! (get data old-bucket) disj uid)
                (when (empty? @(get data old-bucket))
                  (swap! (:data H) dissoc old-bucket)))))
            (swap! (:membership H) conj {uid bucket-set} )))))))





(defn hash-remove [thing H]
    (let [uid (or (:uid thing) thing)
          members @(:membership H)
          data @(:data H)
          member-record (get members uid)]
      (when member-record
        (do
          (swap! (get data member-record) disj uid)
          (when (empty? @(get data member-record))
            (swap! (:data H) dissoc member-record))
          (swap! (:membership H) dissoc uid)))))



