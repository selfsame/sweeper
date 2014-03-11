(ns yes.actors
  (:require [yes.util :refer :all]

            [yes.map :refer [ACTHASHES HASHES note-hash note-actor-hash BOARD WIDTH HEIGHT
                             pos->tile look card-neighbors filter-terrain]])
  (:use [yes.logic :only [MASK-UID record -actor -owner -building -type-of
                          -sex -title -on-tile -is-terrain -covers-area -neighbor -x8 -y8]]
        [clojure.set :only [union difference intersection]]))

(declare ACTORS BUILDINGS make-actor make-building)

(defrecord Actor [uid race sex name state])

(def ACTORS (atom {}))




(def group-defs
  {:kingdom {:king {:queen {:maid [3]}
                    :captain {:guard [10]}
                    }}
   :bandits {:chief {:bandit [5]}}
   :merchants {:merchant [10]}})

(defn pos->things [x y kw]
  (let [[mx my] [(mod x 32) (mod y 32)]
        [lx ux] [(- x mx) (+ x (- 32 mx))]
        [ly uy] [(- y my) (+ y (- 32 my))]
        col (kw {:actors @ACTORS :buildings @BUILDINGS})]
    (filter (fn [e]
              (let [[ex ey] (mapv int (xy- e))]
                (and (<= lx ex ux)
                     (<= ly ey uy)) ))  (vals col))))



(defn make-actor [opts x y]
  (let [uid (get-uid "a")
        sex (rand-nth [:male :female])
        {:keys [group title]} opts
        actor {:uid uid
          :race :human
          :sex sex
          :name "villager"
          :state (atom {:x x :y y :w 8 :h 8
                 :title title
                 :group group
                 :target false
                 :money (rand-int 250)
                 :property #{}})}]
    (swap! ACTORS conj {uid actor})
    (record -actor uid)
    (record -sex uid sex)
    (when title
    (record -title uid title))
    (note-actor-hash actor)
    actor))

(defn buid->pos [uid]
  (when-let [b (get @BUILDINGS uid)]
    (xy- b)))

(defn walkable-card [tile]
  (let [n (card-neighbors tile)]
    (vec (filter-terrain n :grass))))

(defn check-hashes [sizek posv]
  (when-let [H (get HASHES sizek)]
    (let [results (get @(:data H) posv)]
      (when results
        @results))))

(defn check-actor-hashes [sizek posv]
  (when-let [H (get ACTHASHES sizek)]
    (let [results (get @(:data H) posv)]
      (when results
        @results))))

(defn filter-uid-prefix [chs col]
   (filter #(= chs (str (first (rest (str %))))) col))

(filter-uid-prefix "b" [:b12 :a78 :b99])


(defn build-thing [actor kw]
  (when-let [{:keys [cost size]} (kw {:hut {:cost 60 :size [8 8]}
                                      :small-farm {:cost 50 :size [8 8]}
                                      :farm {:cost 170 :size [16 16]}
                                      :inn {:cost 200 :size [16 16]}})]
    (let [[px py] (xy- actor)
          [x y] (to-grid 1 8 [px py])
          [w h] (to-grid 1 8 size)
          money (state actor :money)
          tile (pos->tile px py)
          terrain (state tile :terrain)]
      (when (and (< 10 (rand-int 200))
                 (> money cost)
                 (#{:grass :desert} terrain))
        (do
              (state! actor :money (- money cost))
              (let [[hx hy] (to-grid 8 1 [x y])]
                (make-building kw hx hy)))))))

(def PROPERTY {:hut {:cost 60 :size [8 8]}
                     :small-farm {:cost 50 :size [8 8]}
                     :farm {:cost 170 :size [16 16]}
                     :inn {:cost 200 :size [16 16]}})

(defn desire-property [actor]
  (let [[x y] (xy- actor)
        money (state actor :money)
        properties (map #(get % @BUILDINGS) (state actor :property))
        have (set (map :type properties))]
    (when-let [desire (cond (> money 200) (if (have :inn) :farm :inn)
          (> money 170) (if (have :hut) :farm :small-farm)
          (> money 60) (if (have :hut) :small-farm :hut))]
      (state! actor :desired-property desire))))

(defn on-empty-plot? [actor]
  (when-let [desired (state actor :desired-property)]
    (let [size (:size (desired PROPERTY))
          [px py] (xy- actor)
          [x y] (to-grid 1 8 [px py])
          [w h] (to-grid 1 8 size)
          area (xywh->buckets [x y w h])
          vacant (filter #(not (empty? (filter-uid-prefix "b" (check-hashes :8 %)))) area)]
      (empty? vacant))))

(defn evaluate-land [actor]
  (let [desire (state actor :desired-property)
        [px py] (xy- actor)
        tile (pos->tile px py)
        terrain (state tile :terrain)]
    (cond (#{:farm :small-farm} desire) (if (= terrain :grass) 1.5 0.5)
          :else 1.0)))


(defn update []
  (dorun
   (for [actor (vals @ACTORS)
         :let [[x y] (xy- actor)

               target (state actor :target)]]
     (if target
       (let [[tx ty] target
             [ox oy] (mapv - target [x y])
             [mx my] [(if (pos? ox) 0.3 -0.3) (if (pos? oy) 0.3 -0.3)] ]
         (state! actor :x (+ x mx))
         (state! actor :y (+ y my))
         (note-actor-hash actor)
         (when (and (< (+ ox oy) 5)
                    (> (+ ox oy) -5)) (state! actor :target false)))
       ;not walking anywhere
       (let [property (state actor :property)
             desire (state actor :desired-property)
             actors-here false];(check-actor-hashes 16 (to-grid 1 16 [x y]))]

         (state! actor :target (mapv + [x y] [-32 -32] [(rand-int 64) (rand-int 64)]))

         (when-not (= (:sex actor) :cat)
         (if desire
           (when (on-empty-plot? actor)
             (when-let [my-building (build-thing actor desire)]
               (do
                 (update-state! actor :property #(conj % (:uid my-building)))
                 (record -owner (:uid actor) (:uid my-building))
                 (when-not (state actor :title)
                    (cond (= :inn desire) (do (state! actor :title :innkeeper)
                                          (record -title (:uid actor) :innkeeper))
                 (= :small-farn desire) (state! actor :title :farmer)))
               (desire-property actor))))
           (desire-property actor)))

        ;(when actors-here (println actors-here))

         (when-not (empty? property)
           (when (< 10 (rand-int 20))
             (state! actor :target (buid->pos (rand-nth (vec property)))))))))))




(defn visible [kw]
  (let [col (kw {:actors @ACTORS :buildings @BUILDINGS})]
  (filter
   (fn [e]
  (let [[x y] (xy- e)
        tile (pos->tile x y)
        mask (if MASK-UID (MASK-UID (:uid e)) true)]
    (and mask tile @(:known tile))))
   (vals col))))



(defrecord Building [uid type state])

(defn make-building [typ x y]
  (let [tile (pos->tile x y)
        [tx ty] (to-grid 1 32 [x y])
        uid (get-uid "b")
        [w h] (typ {:castle [32 32] :inn [16 16] :farm [16 16] :small-farm [8 8] :hut [8 8]
                    :road-ns [8 8] :road-nsew [8 8] :road-ew [8 8]})
        building {:uid uid
                  :type typ
                  :state (atom {:x x :y y :w w :h h})}]
    (swap! BUILDINGS conj {uid building})
    (record -building uid)
    (record -type-of uid typ)
    (record -on-tile uid [tx ty])
    (record -is-terrain [tx ty] (state tile :terrain))
    (let [[x8 y8] (to-grid 1 8 [x y])
          [w8 h8] (to-grid 1 8 [w h])
          nei (xywh->neighbors [x8 y8 w8 h8])
          bors (apply union (mapv #(filter-uid-prefix "b" (check-hashes :8 %)) nei))]
    (dorun (map #(do (record -neighbor uid %)
                   (record -neighbor % uid)) bors))
    (record -x8 uid x8)
    (record -y8 uid y8)
    (note-hash building)
    building)))

(def BUILDINGS (atom {}))


