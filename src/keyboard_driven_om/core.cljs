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

(def app-state (atom {:text "Hello world!"
                      :keys-pressed #{}}))

(defn event-chan [el type]
  (let [out (chan)]
    (.addEventListener el type #(put! out %))
    out))

(defn handle-key-event [event]
  (let [k (.-keyCode event)]
    (condp = k
      34 "pagedown"
      13 "enter"
      16 "shift"
      17 "control"
      (str "unbound key: " k))))

(let [move (event-chan js/window "mousemove")]
  (go (while true
        (prn (<! move)))))

(defn gen-key-chan [event func]
  (let [c (event-chan js/window event)]
    (go (while true
          (let [key (<! c)
                keycode (.-keyCode key)]
            (swap! app-state assoc :keys-pressed
                   (func (:keys-pressed @app-state) keycode)))))))

(gen-key-chan "keydown" conj)
(gen-key-chan "keyup" disj)

(let [winchan (event-chan js/window "blur")]
  (go (while true
        (<! winchan)
        (swap! app-state assoc :keys-pressed #{}))))

(comment
(let [keydown (event-chan js/window "keydown")]
  (go (while true
        (let [key (<! keydown)
              keycode (.-keyCode key)]
          (swap! app-state assoc :keys-pressed
                 (conj (:keys-pressed @app-state) keycode))
          (prn "keydown")
          (prn app-state)))))

(let [keyup (event-chan js/window "keyup")]
  (go (while true
        (let [key (<! keyup)
              keycode (.-keyCode key)]
          (swap! app-state assoc :keys-pressed
                 (disj (:keys-pressed @app-state) keycode))
          (prn "keyup")
          (prn app-state)))))
  )

(go
  (prn "hello")
  (<! (timeout 1000))
  (prn "async")
  (<! (timeout 1000))
  (prn "world"))

(def c (chan))

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

(defn render [q]
  (apply str
         (for [p (reverse q)]
           (str "<div class='proc-" p "'>Process " p "</div>"))))

(defn chanmap [f in]
  (let [c (chan)]
    (go (loop []
          (if-let [v (<! in)]
            (do (>! c (f v))
                (recur))
            (close! c))))
    c))

(go (while true (<! (timeout 250)) (>! c 1)))
(go (while true (<! (timeout 1000)) (>! c 2)))
(go (while true (<! (timeout 1500)) (>! c 3)))

(defn peekn [v n]
  (if (> (count v) n)
    (subvec v (- (count v) n))
    v))

(let [el (by-id "ex0")
      out (by-id "ex0-out")]
  (go (loop [q []]
        (set-html! out (render q))
        (recur (-> (conj q (<! c)) (peekn 10))))))

(defn location [el]
  (let [[left top] (cljs.core/map int (offset el))]
    (fn [e]
      {:x (+ (.-offsetX e) left)
       :y (+ (.-offsetY e) top)})))

(let [el (by-id "ex1")
      outm (by-id "ex1-mouse")
      outk (by-id "ex1-key")
      mc (chanmap (location el) (listen el "mousemove"))
      kc (listen js/window "keydown")]
  (go (while true
        (let [[v c] (alts! [mc kc])]
          (condp = c
            mc (set-html! outm (str (:x v) ", " (:y v)))
            kc (set-html! outk (handle-key-event v)))))))

(defn simple-component [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/h2 nil "CSP examples (core.async)"))))

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
