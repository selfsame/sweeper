(ns yes.map
  (:require [yes.util :refer :all])
  (:use [clojure.set :only [union difference intersection]]))

(defrecord Tile [x y terrain known state mine])

(declare neighbors erode-tile)
(def BOARD {})

(def WIDTH 18)
(def HEIGHT 18)
(def MARGIN (int (* 0.2 WIDTH)))

(def HASHES {:64 (make-hash 64)
             :32 (make-hash 32)
             :16 (make-hash 16)
             :8 (make-hash 8)})

(def ACTHASHES {:64 (make-hash 64)
             :32 (make-hash 32)
             :16 (make-hash 16)
             :8 (make-hash 8)})



(defn note-hash [e]
  (when (:uid e)
    (dorun (for [h (vals HASHES)]
             (hash-store e h)))))

(defn note-actor-hash [e]
  (when (:uid e)
    (dorun (for [h (vals ACTHASHES)]
             (hash-store e h)))))

(def rand-map
  (vec (for [x (range 0 WIDTH)]
    (vec (for [y (range 0 HEIGHT)] (rand-int 10) )))))


(defn tiles-near [t1 t2 d]
  (let [[dx dy] (mapv - t2 t1)]
    (if (and (< dx d) (> dx (- d))
             (< dy d) (> dy (- d))) true false)))


(defn pos->tile [x y]
  (let [[tx ty] (mapv int (mapv / [x y] [32 32]))]
    (get BOARD [tx ty])))


rand-map

(defn filter-terrain [k col]
  (filter #(= k (:terrain %)) col))

(defn look
  ([x y]
   (get BOARD [x y]))
  ([x y k]
   (get (get BOARD [x y]) k)))

(defn edge? [t]
  (when (instance? Tile t)
    (let [x (:x t)
          y (:y t)]
         (if (or (= x 0) (= y 0)
             (= x WIDTH) (= y HEIGHT))
           true false))))

(defn known? [t]
  (if  @(:known t) true false))
(defn mined? [t]
  (if (= 0 (:mine t)) false true))

(defn neighbors [t]
  (when (instance? Tile t)
    (let [tx (:x t)
          ty (:y t)]
      (for [x (range (dec tx) (+ tx 2))
            y (range (dec ty) (+ ty 2))
            :let [tile (get BOARD [x y])]
            :when (and (not= [x y] [tx ty]) tile)] tile ))))

(defn card-neighbors [t]
  (when (instance? Tile t)
    (let [tx (:x t)
          ty (:y t)]
      (for [[x y] [[(dec tx) ty] [(inc tx) ty]
                   [tx (dec ty)] [tx (inc ty)]]
            :let [tile (get BOARD [x y])]
            :when  tile] tile))))





(defn make-board [STARTPOS]
  (println "making board")
  (let [[s-x s-y] STARTPOS
        board (apply merge
                     (for [y (range 0 HEIGHT)
                           x (range 0 WIDTH)
                           :let [near-start (tiles-near STARTPOS [x y] 2)
                                 terrain (if near-start
                                           (rand-nth [:grass :desert])
                                           (rand-nth [ :mountain :mountain  :grass :grass :grass :grass :sea :sea :sea]))]]
                       {[x y]
                        (Tile. x y
                               terrain
                               (atom false)
                               (atom {:danger 0
                                      :terrain terrain})
                               (if (and (not near-start)
                                    (< (rand-int 100) 18)) 1 0)) }))]
    (def BOARD board)))



(defn setup-board []
  (let [mined (filter (fn [t] (pos? (:mine t))) (vals BOARD))]
    (dorun (map (fn [tile]
                  (state! tile :terrain (erode-tile tile)) ) (vals BOARD)))



    (dorun
     (for [t mined
          :let [nei (neighbors t)]]
      (dorun (map (fn [tt]
               (when (not (pos? (:mine tt)))
                 (state! tt :danger (inc (state tt :danger))) )) nei))))))


(defn erode-tile [tile]
  (let [terrain (state tile :terrain)
        nei (neighbors tile)
        card (card-neighbors tile)]
    (cond
     (and (= :mountain terrain)
          (= 0 (count (filter-terrain :mountain card)))) (rand-nth [:desert])
     (and (= :sea terrain)
          (pos? (count (filter-terrain :mountain card)))) (rand-nth [:grass])
     (and (= :sea terrain)
          (< (count (filter-terrain :sea nei)) 2)) :grass
     (and (= :desert terrain)
          (> (count (filter-terrain :sea card)) 2)) :grass
     :else (cond
            (> (count (filter-terrain :mountain card)) 2) :mountain
            (> (count (filter-terrain :mountain nei)) 3) :mountain
            (> (count (filter-terrain :sea nei)) 3) :sea
            :else terrain))))




(defn sweep-mines [tiles checked CLICKED-TILE]
    (when (first tiles)
    (let [t (first tiles)]
      (if (and
           (not (mined? t))
           (not= 0 (state t :danger))) ;(println (prn-tile t)))
        (do
          (swap! (:known t) false? )
          {:tiles (rest tiles) :checked checked})
      (let [nei (difference (set (card-neighbors t)) checked)
            valid (set (filter (fn [v] (and
                                        (tiles-near CLICKED-TILE [(:x v) (:y v)] 3)
                                        (not @(:known v))
                                        (= 0 (:mine v)) )) nei))
            ;unsafe (filter #(pos? (state % :danger)) valid)
            ;safe (difference valid unsafe)
            to-sweep (set (concat (rest tiles) valid) )
            invalid (union checked (difference nei valid) #{t})]

        (swap! (:known t) false? )

        {:tiles (vec to-sweep) :checked invalid})))))

(defn start-sweep [t CLICKED-TILE]
  (loop [-tiles [t] -checked #{} -failsafe 0]
    (let [{:keys [tiles checked]} (sweep-mines -tiles -checked CLICKED-TILE)]
      (if (and (not (empty? tiles)) (< -failsafe 80))
        (recur tiles checked (inc -failsafe))))))

