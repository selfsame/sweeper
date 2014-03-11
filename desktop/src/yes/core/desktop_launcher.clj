(ns yes.core.desktop-launcher
  (:require [yes.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. (yes.core.Game.) "yes" 800 600 true)
  (Keyboard/enableRepeatEvents true))

(-main)
