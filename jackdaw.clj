(ns jackdaw
  (:import (javax.swing JFrame JTextField JLabel JButton JOptionPane JPanel)
           (java.awt GridLayout BasicStroke Rectangle Font)
           (java.awt.geom Point2D)
           (java.awt.font LineBreakMeasurer FontRenderContext)
           (java.text AttributedString)
           (java.awt.font TextAttribute)
           (java.awt RenderingHints Toolkit GraphicsEnvironment Graphics2D Color)))

; Config management
(def app-defaults
  { :name         "Jackdaw"
    :width        300
    :height       300
    :commands     []
    :stroke-color (Color. 0 0 0)
    :fill-color   (Color. 255 255 255) })

(def fill-color)

(def config (ref app-defaults))

(defn set-config [vals]
  (dosync (ref-set config (merge @config vals))))

(defn add-cmd [seq]
  (set-config { :commands (merge (@config :commands) seq) }))

(defn current-style []
  { :fill-color   (@config :fill-color),
    :stroke-color (@config :stroke-color) })

(defn current-font-style []
  { :color (@config :fill-color),
    :size  14
    :font  "Monospaced" })

; Drawing structs
(defstruct shape-2d :type :x :y :width :height :style)
(defstruct text :type :x :y :style :body)
(defstruct layout :type :width :height)

; Drawing
(defmulti draw (fn [object g l] (:type object)))

(defmethod draw ::jackdaw/Rect [r g l]
  (doto g
    (.setColor ((r :style) :fill-color))
    (.fillRect (r :x) (r :y) (r :width) (r :height))
    (.setColor ((r :style) :stroke-color))
    (.drawRect (r :x) (r :y) (r :width) (r :height))))

(defmethod draw ::jackdaw/Oval [e g l]
  (doto g
    (.setColor ((e :style) :fill-color))
    (.fillOval (e :x) (e :y) (e :width) (e :height))
    (.setColor ((e :style) :stroke-color))
    (.drawOval (e :x) (e :y) (e :width) (e :height))))

(defmethod draw ::jackdaw/Para [t g l]
  (doto g
    (.setRenderingHint (. RenderingHints KEY_ANTIALIASING)
                       (. RenderingHints VALUE_ANTIALIAS_ON)))
  (let [body (AttributedString. (t :body))]
    (doto body
      (.addAttribute (.. TextAttribute FONT)
                     (Font. ((t :style) :font) (. Font PLAIN) ((t :style) :size)))
      (.addAttribute (.. TextAttribute SIZE) ((t :style) :size))
      (.addAttribute (.. TextAttribute FOREGROUND) ((t :style) :color)))
    (let [width (l :width)
          x 10
          start_y 300
          ; LineBreakMeasurer does the real work
          measure (LineBreakMeasurer.
                  (.. body getIterator)
                  (.. g getFontRenderContext))]
      (loop [layout (.. measure (nextLayout width))
             position (.. measure getPosition)
             y start_y]
        (. layout draw g x y)
        (if (zero? (- (.length (t :body)) position))
          ()
          (recur 
            (.. measure (nextLayout width))
            (.. measure getPosition)
            (+ y (. layout getAscent) (. layout getDescent) (. layout getLeading))))))))

(defn default-flow []
  (ref (struct layout ::Flow (@config :width) (@config :height))))

(defn draw-all []
  (let [current-layout (default-flow)
        r (doto (proxy [JPanel] [] (paint [g]
    (doall (for [cmd (@config :commands)]
      (if (= ::jackdaw/Stack (cmd :type))
        (dosync (ref-set current-layout cmd))
        (draw cmd g @current-layout)))))))]
    (doto (@config :active-frame)
      (.add r)
      (.setVisible true))))

; Windowing
(defn create-window [name width height]
  (set-config { :width width })
  (set-config { :height height })
  (set-config { :active-frame (JFrame. name) })
    (doto (config :active-frame) 
       (.setLayout (GridLayout. 1 1))
       (.setSize width height)
       (.setResizable false)))

; Interface
(defmacro app [name options & cmds]
  `(do (create-window ~name (~options :width) (~options :height)) ~@cmds (draw-all)))

(defn fill [r g b]
  (set-config { :fill-color (Color. r g b) }))

(defn stroke [r g b]
  (set-config { :stroke-color (Color. r g b) }))

(defn rect [x y width height]
  (add-cmd (struct shape-2d ::Rect x y width height (current-style))))

(defn oval [x y width height]
  (add-cmd (struct shape-2d ::Oval x y width height (current-style))))

(defn para [body]
  (add-cmd (struct text ::Para 10 300 (current-font-style) body)))

(defn stack []
  (add-cmd (struct layout ::Stack (@config :width) (@config :height))))

(defn flow []
  (add-cmd (struct layout ::Flow (@config :width) (@config :height))))
