(ns maze.core)
(set! *warn-on-reflection* true)

(defn oristr [ori] (case ori
                     :left  "<"
                     :up    "^"
                     :right ">"
                     :down  "v" ))
(defn righter [ori] (case ori
                     :left  :up
                     :up    :right
                     :right :down
                     :down  :left ))
(defn lefter [ori] (case ori
                     :left  :down
                     :up    :left
                     :right :up
                     :down  :right ))
(defn oripos [ori] (case ori
                     :left  [ 0 -1]
                     :up    [-1  0]
                     :right [ 0  1]
                     :down  [ 1  0] ))
(defn posinc [[px py] v] 
  (let [[vx vy] (oripos v)]  [(+ px vx) (+ py vy)]) )

; ori: :left, :up, :right, :down ;orientation
; pos: [x y] i.e. (row, col)     ;position
(defrecord Monster [pos ori])

(defn turn-right [m] (update-in m [:ori] righter))
(defn turn-left [m] (update-in m [:ori] lefter))
(defn step [m] (update-in m [:pos] #(posinc % (:ori m)) ))
 
;st : set of walls (world:field)
(defn is-wall [st {pos :pos, ori :ori} f] (contains? st (posinc pos (f ori)))) 
(defn is-right-wall [st m] (is-wall st m righter))
(defn is-front-wall [st m] (is-wall st m identity))

;field: Set<[x y]> (is-in == Wall)
(defrecord World [field monster halfstep size]) ; set<X>, m, bool, [w h]

(defn make-world [w h strs] (let [m (atom (->Monster [-1 -1] :invalid))]
  (->World (set (mapcat (fn [s i] ;walk rows
             (mapcat (fn [c j] (if (= c \X) ;walk line (cols)
                                 [[i j]]
                                 (when (contains? (set "<>v^") c) 
                                   (do 
                                     (swap! m #(assoc % 
                                                 :pos [i j] 
                                                 :ori (case c
                                                        \< :left
                                                        \> :right
                                                        \^ :up
                                                        \v :down
                                                        ))) ;/swap
                                     () ) ;return empty seq to cat
                                   ))) s (range w)) ;/fn[c j] /mapcat cols
             ) strs (range h))) ;/mapcat rows  
    @m false [w h]) ))

(defn wstep "AI algorithm" [{f :field, m :monster, hs :halfstep,  :as w}] 
  (let [f-half #(assoc w :halfstep (not hs), :monster (% m))]
    (if hs (f-half step)
      (if (is-right-wall f m)
        (update-in w [:monster] (if (is-front-wall f m) turn-left step))
        (f-half turn-right) ))
    ))

(defn str-world [{f :field, {pos :pos, ori :ori}:monster, [w h]:size}] 
  (apply str 
    (for [i (range h) ;rows
          j (range (inc w))]; +1 for \newline
      (if (= j w) \newline 
        (if (= [i j] pos) (oristr ori) ;where monster is
          (if (contains? f [i j]) \X \.) ))
      )))

(defn walk-world [steps] 
  (doseq [:let [read-int #(Integer/parseInt (read-line))]
          w (->> (make-world (read-int) (read-int) (repeatedly read-line)) ; w h map
              (iterate wstep) (rest) (map str-world) (take steps) )]
    (println w)))

 ; (buffering output* == about 30% increase)(* print with-out-str)
(with-in-str 
  "8
4
XXXXXXXX
XvXX...X
X....X.X
XXXXXXXX" (time (walk-world 20)))


