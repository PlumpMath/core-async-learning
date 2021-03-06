(ns keyboard-driven-om.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! >! alts! timeout close!]]
            [cljs-http.client :as http]
            [cljs.reader :as reader]
            [goog.events :as events]
            [goog.style :as style])
  (:import [goog.events EventType]))

(enable-console-print!)

(def app-state
  (atom {:title "CSP Examples (core.async)"
         :keys-pressed #{}
         :processes []
         :tiles []
         :fqjn ""
         :map {}
         :repos []
         :focused {}
         :fielddef {}}))

(extend-type string
  ICloneable
  (-clone [s] (js/String. s)))

(extend-type js/String
  ICloneable
  (-clone [s] (js/String. s))
  om/IValue
  (-value [s] (str s)))

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

(defn GET [url k]
  (go (let [response (<! (http/get url {:with-credentials? false}))]
        (prn (:status response))
        ;(prn (:body response))
        (swap! app-state assoc k (:body response)))))

(defn handle-key-event [event]
  (let [k (.-keyCode event)]
    (condp = k
      37 "left"
      39 "right"
      38 "up"
      40 "down"
      34 "pagedown"
      32 "space"
      13 "enter"
      16 "shift"
      17 "control"
      18 "alt"
      27 "esc"
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
        (prn "navigating away from page"))))
        ;(js/confirm "are you sure you want to close?"))))

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
             (dom/h3 nil "AppState")
             (apply dom/ul nil
                    ;(map #(dom/li nil (str %)) app)))))
                    ;; prevent printing of that massive map
                    (map #(when (not= :map (first %)) (dom/li nil (str %))) app)))))

(om/root app-state-watcher app-state
  {:target (by-id "state-area")})

(defn process-view [proc owner]
  (om/component
    (dom/li nil (str "Process " proc))))

(defn process-area [app owner]
  (om/component
    (dom/div nil
             (dom/h3 nil "Process Area")
             (apply dom/ul nil
                    (om/build-all process-view (:processes @app-state))))))

;(om/root process-area app-state
  ;{:target (by-id "process-area")})

(go (loop [q []]
      (let [procs (-> (conj q (<! c)) (peekn 10))]
        (swap! app-state assoc :processes (reverse procs))
        (recur procs))))

(defn location [el]
  (let [[left top] (cljs.core/map int (offset el))]
    (fn [e]
      {:x (+ (.-offsetX e) left)
       :y (+ (.-offsetY e) top)})))

;; XTL Utils =======================
(defn get-docdef-atts [xtl]
  (-> xtl :children first :atts))

(defn build-fqjn-index-xtl [xtl]
  )

(defn build-fqjn-index [xnet]
  (set (concat (-> xnet :input build-fqjn-index-xtl)
               (-> xnet :output build-fqjn-index-xtl))))

;; Keyboard Shortcut funcs =========
(defn clear-keys []
  (prn "clearing keys")
  (swap! app-state assoc :keys-pressed #{}))

(defn get-tiling-workspace-size []
  )

(defn resize-all-tiles []
  )

(defn add-tile []
  (swap! app-state assoc :tiles
         (let [tiles (:tiles @app-state)
               n (count tiles)]
           (if (> n 29)
             tiles
             (conj tiles {:num (inc n)}))))
  (resize-all-tiles))

(defn remove-tile []
  (swap! app-state assoc :tiles
         (let [tiles (:tiles @app-state)]
           (if (empty? tiles)
             tiles
             (pop tiles))))
  (resize-all-tiles))

(defn focus-tile [n]
  (let [tiles (:tiles @app-state)]
        (swap! app-state assoc :focused
               (if (> (count tiles) n) (nth tiles n) (:focused @app-state)))))

(def xdapi "http://localhost:5000/xd/")

(defn update-fqjn [path]
  (GET (str xdapi "fqjn/" path) :fqjn))

(defn load-map [path]
  (prn (str "loaded map: " path))
  (GET (str xdapi "map/" path) :map))

(defn set-fielddef []
  (let [fielddef (-> @app-state :map :input :children first :children
                     rest rest rest rest first :children first)]
    (swap! app-state assoc :fielddef fielddef)))

(defn clear-map []
  (prn "cleared map")
  (swap! app-state assoc :map {}))

(defn set-docdef-focused-tile []
  (swap! app-state assoc :focused (conj (:focused @app-state)
                                  {:docdef-atts (get-docdef-atts
                                                  (-> @app-state :map :input))})))

(defn mutate-tree []
  (let [current-map (:map @app-state)
        modified-map (assoc-in current-map [:input :children 0 :children
                                            4 :children 0 :atts :javaName]
                               "NEW BREAKING JAVANAME")]
    (swap! app-state assoc :map modified-map)))

(defn publish-map []
  (prn (str "publishing map: "
            (-> @app-state :map :output get-docdef-atts :fullyQualifiedJavaName))))

(defn refresh-repos []
  (GET (str xdapi "list") :repos))

(defn undo []
  (prn "undoing"))

(defn prefix [ks]
  (conj #{16 17} ks))

(defn handle-key-chord [chord-set]
  (condp = chord-set
    #{16} (prn "you're pressing shift!")
    #{16 17} (prn "holding the prefix")
    (prefix 49)  (focus-tile 0)
    (prefix 50)  (focus-tile 1)
    (prefix 51)  (focus-tile 2)
    (prefix 52)  (focus-tile 3)
    (prefix 53)  (focus-tile 4)
    (prefix 54)  (focus-tile 5)
    (prefix 55)  (focus-tile 6)
    (prefix 56)  (focus-tile 7)
    (prefix 57)  (focus-tile 8)
    (prefix 69)  (prn "conjed shortcut 69")
    (prefix 70)  (update-fqjn "examples/master/attributed_xml/poCustWrite.xtl")
    (prefix 71)  (clear-keys)
    (prefix 74)  (prn "J")
    (prefix 75)  (clear-map)
    (prefix 76)  (load-map "examples/master/attributed_xml/poCustWrite.xtl")
    (prefix 80)  (prn "P")
    (prefix 186) (prn "focus the minibuffer")
    (prefix 220) (mutate-tree)
    (prefix 222) (set-fielddef)
    (prefix 68)  (remove-tile)
    (prefix 83)  (publish-map)
    (prefix 84)  (add-tile)
    (prefix 86)  (set-docdef-focused-tile)
    (prefix 90)  (undo)
    #{17 18 84}  (add-tile)
    #{17 18 68}  (remove-tile)
    #{17 18 82}  (refresh-repos)
    (prn "not bound")))

(let [el (by-id "ex1")
      outm (by-id "ex1-mouse")
      outk (by-id "ex1-key")
      mc (chanmap (location el) (listen el "mousemove"))
      kc (listen js/window "keydown")]
  (go (while true
        (let [[v c] (alts! [mc kc])]
          (-> @app-state :keys-pressed handle-key-chord)
          ;(condp = c
            ;mc (set-html! outm (str (:x v) ", " (:y v)))
            ;kc (set-html! outk (handle-key-event v)))))))
          ))))

(defn title-component [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/h3 nil (:title app)))))

;(om/root title-component app-state
  ;{:target (by-id "app")})

(defn display-keys-component [app owner]
  (reify
    om/IRender
    (render [_]
      (let [pressed (:keys-pressed app)]
        (dom/p nil (str pressed))))))

;(om/root display-keys-component app-state
  ;{:target (by-id "map")})

(defn get-tile-style [color width height]
  #js {:border (str "2px solid " color) :overflow "auto"
       :float "left" :resize "both" :margin "1px"
       :width (str width "px") :height (str height "px")})

(defn tile-view [tile owner]
  (om/component
    (let [focus-me #(swap! app-state assoc :focused tile)]
      (dom/div #js {:style (get-tile-style "black" 200 354)
                    :onMouseOver focus-me
                    :onClick focus-me}
               (clj->js tile)))))
               ;(om/build xtl-view (-> @app-state :map :input))))))

(defn tile-pane-component [app owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:style #js {:border "2px solid black" :overflow "auto"
                                :height "360px"}}
               (apply dom/div nil
                      (om/build-all tile-view (:tiles app)))))))

(defn tile-pane-holder [app state]
  (om/component
    (dom/div nil
             (dom/h3 nil "Tile Workspace")
             (om/build tile-pane-component app))))

(om/root tile-pane-component app-state
  {:target (by-id "tile-area")})

(defn commit-change [text owner]
  (om/set-state! owner :editing false))

(defn handle-change [e text owner]
  (om/transact! text (fn [_] (.. e -target -value))))

(defn display [show]
  (if show
    #js {}
    #js {:display "none"}))

(defn editable [text owner]
  (reify
    om/IInitState
    (init-state [_]
      {:editing false})
    om/IRenderState
    (render-state [_ {:keys [editing]}]
      ;(dom/li nil
      (dom/div nil
              (dom/span #js {:style (display (not editing))} (str text))
              (dom/input
                #js {:style (display editing)
                     :value (om/value text)
                     :onChange #(handle-change % text owner)
                     ;:onClick #(om/set-state! owner :editing true)
                     :onKeyDown #(when (= (.-key %) "Enter")
                                   (commit-change text owner))
                     :onBlur (fn [e] (commit-change text owner))})
              (dom/button
                #js {:style (display (not editing))
                     :onClick #(om/set-state! owner :editing true)}
                "Edit")))))

(defn att-view [att owner]
  (om/component
    ;(dom/li nil (str (-> att first name) ": " (last att)))))
    (dom/div nil (-> att first name)
             (om/build editable (second att)))))

(defn atts-view [atts owner]
  (om/component
    (dom/div nil
             (apply dom/ul nil (om/build-all att-view atts)))))
             ;(apply dom/ul nil (om/build-all editable (vals atts))))))
             ;(apply dom/ul nil (om/build-all editable atts)))))

(defn node-view [node owner]
  (om/component
    (dom/div nil
             (:name node)
             (dom/br nil nil)
             (-> node :text str)
             ;(om/build editable (:text node))
             (om/build atts-view (:atts node))
             (apply dom/div nil
                    (om/build-all node-view (:children node))))))

(defn xtl-view [xtl owner]
  (om/component
    (dom/div #js {:style #js {:border "2px solid black" :margin "1px"
                              :overflow "auto"}}
             (dom/h4 nil (-> xtl get-docdef-atts :fullyQualifiedJavaName))
             (om/build node-view xtl))))

(defn map-view [app owner]
  (om/component
    (dom/div nil
             (dom/h3 nil "Map Area")
             (om/build xtl-view (-> app :map :input))
             (om/build xtl-view (-> app :map :output)))))

(om/root map-view app-state
  {:target (by-id "map-area")})
