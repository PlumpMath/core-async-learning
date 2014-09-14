(ns keyboard-driven-om.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! >! alts! timeout close!]]
            [goog.dom.classes :as classes]
            [goog.events :as events]
            [goog.style :as style])
  (:import [goog.events EventType]))

(enable-console-print!)

(def app-state (atom {:title "CSP Examples (core.async)"
                      :keys-pressed #{}
                      :processes []
                      :tiles []}))

(defn listen [el type]
  (let [c (chan)]
    (events/listen el type #(put! c %))
    c))

(defn set-html! [el s]
  (set! (.-innerHTML el) s))

(defn by-id [id]
  (.getElementById js/document id))

(defn offset [el]
  [(style/getPageOffsetLeft el) (style/getPageOffsetTop el)])

(defn event-chan [el type]
  (let [out (chan)]
    (.addEventListener el type #(put! out %))
    ;; if event has a match in the app, preventDefault
    ;(.addEventListener el type (fn [x] (.preventDefault x)
                                 ;(put! out x)))
    out))

(defn handle-key-event [event]
  (let [k (.-keyCode event)]
    (condp = k
      34 "pagedown"
      32 "space"
      13 "enter"
      16 "shift"
      17 "control"
      18 "alt"
      (str "unbound key: " k))))

(let [move (event-chan js/window "mousemove")]
  (go (while true
        (prn (<! move)))))

(defn gen-key-chan [event func]
  (let [c (event-chan js/window event)]
    (go (while true
          (let [key (<! c)
                keycode (.-keyCode key)]
            (prn (str "keycode: " keycode " -- func: " event))

            ;; dirty special case to prevent ignored keyup event
            ;; when cmd key held (mac only)
            (when (and (= event "keyup") (or (= 91 keycode) (= 224 keycode)))
              (swap! app-state assoc :keys-pressed #{}))

            (swap! app-state assoc :keys-pressed
                   (func (:keys-pressed @app-state) keycode)))))))

(gen-key-chan "keydown" conj)
(gen-key-chan "keyup" disj)

(let [winchan (event-chan js/window "blur")]
  (go (while true
        (<! winchan)
        (swap! app-state assoc :keys-pressed #{}))))

(let [winchan (event-chan js/window "beforeunload")]
  (go (while true
        (<! winchan)
        ;; need to actually respect user's choice
        ;; need to set 'cancel' as default
        (js/confirm "are you sure you want to close?"))))

(go
  (prn "hello")
  (<! (timeout 1000))
  (prn "async")
  (<! (timeout 1000))
  (prn "world"))

(defn chanmap [f in]
  (let [c (chan)]
    (go (loop []
          (if-let [v (<! in)]
            (do (>! c (f v))
                (recur))
            (close! c))))
    c))

(def c (chan))
(go (while true (<! (timeout 250)) (>! c 1)))
(go (while true (<! (timeout 1000)) (>! c 2)))
(go (while true (<! (timeout 1500)) (>! c 3)))

(defn peekn [v n]
  (if (> (count v) n)
    (subvec v (- (count v) n))
    v))

(defn app-state-watcher [app owner]
  (om/component
    (dom/div nil
             (dom/h2 nil "AppState Watcher")
             (apply dom/ul nil
                    (map #(dom/li nil (str %)) app)))))

(om/root app-state-watcher app-state
  {:target (by-id "state-area")})

(defn process-view [proc owner]
  (om/component
    (dom/li nil (str "Process " proc))))

(defn process-area [app owner]
  (om/component
    (dom/div nil
             (dom/h2 nil "Process Area")
             (apply dom/ul nil
                    (om/build-all process-view (:processes @app-state))))))

(om/root process-area app-state
  {:target (by-id "process-area")})

(go (loop [q []]
      (let [procs (-> (conj q (<! c)) (peekn 10))]
        (swap! app-state assoc :processes (reverse procs))
        (recur procs))))

(defn location [el]
  (let [[left top] (cljs.core/map int (offset el))]
    (fn [e]
      {:x (+ (.-offsetX e) left)
       :y (+ (.-offsetY e) top)})))

(defn clear-keys []
  (prn "trying to clear keys")
  (swap! app-state assoc :keys-pressed #{}))

(defn add-tile []
  (swap! app-state assoc :tiles
         (let [tiles (:tiles @app-state)
               n (count tiles)]
           (if (< 29 n)
             tiles
             (conj tiles {:val (inc n)})))))

(defn remove-tile []
  (swap! app-state assoc :tiles
         (let [tiles (:tiles @app-state)]
           (if (empty? tiles)
             tiles
             (pop tiles)))))

(def prefix #{16 17})

(defn handle-key-chord [chord-set]
  (condp = chord-set
    #{16} (prn "you're pressing shift!")
    #{16 17} (prn "holding the prefix")
    #{16 17 71} (clear-keys)
    #{16 17 74} (prn "J")
    #{16 17 75} (prn "K")
    #{16 17 76} (prn "L")
    #{16 17 80} (prn "P")
    #{16 17 68} (remove-tile)
    #{16 17 84} (add-tile)
    (prn "not bound")))

(let [el (by-id "ex1")
      outm (by-id "ex1-mouse")
      outk (by-id "ex1-key")
      mc (chanmap (location el) (listen el "mousemove"))
      kc (listen js/window "keydown")]
  (go (while true
        (let [[v c] (alts! [mc kc])]
          (-> @app-state :keys-pressed handle-key-chord)
          (condp = c
            mc (set-html! outm (str (:x v) ", " (:y v)))
            kc (set-html! outk (handle-key-event v)))))))

(defn simple-component [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/h2 nil (:title app)))))

(om/root simple-component app-state
  {:target (. js/document (getElementById "app"))})

(defn map-loader-component [app owner]
  (reify
    om/IRender
    (render [_]
      (let [pressed (:keys-pressed app)]
        (dom/p nil (str pressed))))))

(om/root map-loader-component app-state
  {:target (. js/document (getElementById "map"))})

(defn tile-view [tile owner]
  (om/component
    (dom/li nil (str "Tile " tile))))

(defn tile-holder-component [app state]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
               (dom/h2 nil "tiling area")
               (apply dom/ul nil
                      (om/build-all tile-view (:tiles @app-state)))))))

(om/root tile-holder-component app-state
  {:target (by-id "tile-area")})
