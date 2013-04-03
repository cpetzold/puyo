(ns puyo.core
  (:require
   [penumbra.app :as app]
   [penumbra.opengl :as gl]
   [penumbra.opengl.effects :as effects]
   [penumbra.text :as text])
  (:import
   [org.lwjgl.opengl DisplayMode]))

(defn draw-circle [pos r]
  (gl/push-matrix
   (apply gl/translate pos)
   (gl/draw-triangle-fan
    (gl/vertex 0 0)
    (doseq [i (range 361)
            :let [rad (* 2 Math/PI (/ i 360.0))]]
      (gl/vertex
       (* r (. Math cos rad))
       (* r (. Math sin rad)))))))

(def +width+ 6)
(def +height+ 12)
(def +ball-size+ 100)
(def +rotations+ [[0 -1] [1 0] [0 1] [-1 0]])
(def +balls+
  [[1 0 0]
   [1 1 0]
   [0 1 0]
   [0 1 1]
   [0 0 1]])

(defn world->ortho [dim xy]
  (map
   (fn [c d]
     (dec (double (* 2 (/ c d)))))
   xy dim))

(defn make-piece []
  {:rotations 0
   :parts (repeatedly 2 #(rand-nth +balls+))})

(defn draw-piece [{:keys [parts rotations]} pos]
  (let [rotation (nth +rotations+ (mod rotations (count +rotations+)))]
    (gl/color (first parts))
    (draw-circle pos +ball-size+)
    (gl/color (second parts))
    (draw-circle
     (map
      (fn [coord rot-off]
        (+ coord (* 2 rot-off +ball-size+)))
      pos rotation)
     +ball-size+)))

(defn init-state [state]
  (assoc state
    :board (repeat +height+ (repeat +width+ nil))
    :offset [(/ +width+ 2) 0]
    :piece (make-piece)
    :next-piece (make-piece)))

(defn update [[dt t] state]
  state)

(defn display [[dt t] state]
  (text/write-to-screen (format "%s" (int (/ 1 dt))) 10 10)
  (draw-piece (:piece state) [400 300])
  (app/repaint!))

(defn get-display-mode [mode-map]
  (->> (app/display-modes)
       (filter
        #(= (select-keys % (keys mode-map)) mode-map))
       first))

(defn init [state]
  (app/display-mode!
   (get-display-mode {:resolution [800 600]}))
  (app/title! "Puyo")
  (app/vsync! true)
  (app/key-repeat! true)
  (effects/enable-high-quality-rendering)
  state)

(defn reshape [[x y w h] state]
  (gl/viewport x y w h)
  (gl/ortho-view x (+ x w) (+ y h) y -1 1)
  (gl/load-identity)
  state)

(defn key-press [key state]
  #_(println key)
  (update-in state [:piece :rotations] inc))

(defn start [state]
  (app/start
   {:init init
    :reshape reshape
    ;:update (fn [& args] (apply update args))
    :display (fn [& args] (apply display args))
    :key-press (fn [& args] (apply key-press args))}
   (init-state state)))

(comment

  (future (start {}))
  
)