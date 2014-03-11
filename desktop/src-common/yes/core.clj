(ns yes.core
  (:import [com.badlogic.gdx Game Gdx Graphics Screen Input$Keys]
           [com.badlogic.gdx.graphics Color GL20 Texture]
           [com.badlogic.gdx.graphics.g2d BitmapFont SpriteBatch TextureRegion]
           [com.badlogic.gdx.math Rectangle Vector3 MathUtils]
           [com.badlogic.gdx.scenes.scene2d Stage]
           [com.badlogic.gdx.scenes.scene2d.ui Label Label$LabelStyle])
  (:require [yes.map :refer :all]
            [yes.data :refer :all]
            [yes.util :refer :all]
            [yes.logic :as logic]
            [yes.actors :as actors]
            )
  (:use [clojure.set :only [union difference intersection]]))

(gen-class
 :name yes.core.Game
 :extends com.badlogic.gdx.Game)

; TODO
; macro for screen defs, needs to pass in functions that can be REPL'd
;     textures could be a provided arg with auto dispose?


(declare batch sprites sheet)

(def bg-color [0.02 0.08 0.1 0] )

(defn texture [path]
  (Texture. (.internal (Gdx/files)  path)))

(defn region [texture]
  (TextureRegion. texture))



(defn new-game []
  (def STARTPOS [(+ (rand-int (- WIDTH MARGIN MARGIN)) MARGIN)
               (+ (rand-int (- HEIGHT MARGIN MARGIN)) MARGIN)])
  (def CLICKED-TILE STARTPOS))

;;=============================================




(def eggy (atom 0))

(defn repl-render [delta]

  (if (.justTouched Gdx/input)
        (let [gh (.getHeight Gdx/graphics)
              [x y] [(.getX Gdx/input) (- gh (.getY Gdx/input))]
              [tx ty] (mapv int (mapv / [x y] [32 32]))]
          (def CLICKED-TILE [tx ty])
          (when-let [t (get BOARD [tx ty])]
            (start-sweep t  CLICKED-TILE))))

  (let [[r g b a] bg-color
        sin-mod (MathUtils/sin (float (* 0.6 (swap! eggy inc)) ))
        known-tiles (filter (fn [t] @(:known t)) (vals BOARD))
        known-neighbors (set (flatten (mapv neighbors known-tiles)))]

    (.glClearColor (Gdx/gl) r g b a)
    (.glClear (Gdx/gl) GL20/GL_COLOR_BUFFER_BIT)
    (actors/update)
    (.begin batch)


     (dorun (mapv (fn [[k e]]
                      (let [[x y] (mapv * k [32 32])
                            known @(:known e)
                            terrain (if known (state e :terrain) :hidden)
                            texture (terrain sprites)]
                        (.draw batch texture
                               (float x)
                               (float y)
                               (float 32)
                               (float 32))
                        (when (and known (pos? (:mine e) ))
                          (.draw batch (:skull sprites)
                                 (float x)
                                 (float y)
                                 (float 32)
                                 (float 32))))) BOARD))
     (dorun (mapv (fn [e]
                   (.draw batch ((:type e) sprites)
                          (x- e) (y- e)
                          (w- e) (h- e))) (actors/visible :buildings)))
    (dorun (mapv (fn [e]
                   (.draw batch ((:sex e) sprites)
                          (x- e)
                          (float (+ (y- e) sin-mod))
                          (float 8)
                          (float 8))) (actors/visible :actors)))

    (.end batch)))








(defn make-road [[x y] & offsets8]
  (actors/make-building :road-ns x y)
  (dorun (for [off offsets8
        :let[[ox oy] (to-grid 8 1 off)]]
    (actors/make-building :road-nsew (+ x ox) (- y oy)
    ))))


(def main-screen
  (let [stage (atom nil)]
    (proxy [Screen] []
      (show []
            (new-game)
            (make-board STARTPOS)
            (setup-board)


            (let [[sx sy] (to-grid 32 1 STARTPOS)]
              (actors/make-building :castle sx sy)
              (make-road [sx sy] [1 -5] [1 -6] [2 -6] [3 -6] [4 -6]
                         [5 -5] [5 -4] [5 -3] [6 -3] [7 -3] [5 -2] [5 -2])
              (make-road [sx sy] [-1 1] [-1 2] [0 2] [1 2] [1 3]
                         [1 4] [2 4] [3 4] [3 3] [3 2] [4 2] [5 2])

              (dotimes [i 70]
                (actors/make-actor (or (get {0 {:title :king :group :kingdom}
                                             3 {:title :queen :group :kingdom}
                                             4 {:title :maid :group :kingdom}
                                             1 {:title :chief :group :bandits}
                                             5 {:title :cutpurse :group :bandits}
                                             2 {:title :merchant :group :merchants}} i) {}) sx sy))
              (start-sweep (look (first STARTPOS) (last STARTPOS)) CLICKED-TILE)))
      (render [delta]
              (repl-render delta))
      (dispose[]
              (.dispose batch))
      (hide [])
      (pause [])
      (resize [w h])
      (resume []))))

(defn -create [^Game this]
  (.setScreen this main-screen)
  (def batch (SpriteBatch.))
  (def sheet (.split (region (texture "sprites.png")) 16 16))
  (def materials (.split (region (texture "materials.png")) 16 16))
  (def x8 (.split (region (texture "8x8.png")) 8 8))
  (def sprites
    {:hidden (aget sheet 0 2 )
     :hidden-border (aget sheet 0 3 )
     :grass (aget sheet 0 0 )
     :mountain (aget sheet 0 1 )
     :sea (aget sheet 1 0 )
     :desert (aget sheet 1 1 )
     :castle (aget sheet 2 0 )
     :skull (aget sheet 2 1 )
     :road-ew (aget x8 3 0 )
     :road-ns (aget x8 3 1 )
     :road-nsew (aget x8 3 2 )
     :small-farm (aget x8 2 0)
     :farm (aget sheet 2 3)
     :hut (aget x8 2 1)
     :inn (aget sheet 2 2)
     :male (aget x8 0 0)
     :female (aget x8 1 0)
     :cat (aget x8 0 1)}))


