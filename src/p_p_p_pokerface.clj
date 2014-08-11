(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [card]
  (let [[fst _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst) (Integer/valueOf (str fst))
                                (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn- hand->rank [hand]
  (map rank hand))

(defn- vals-by-rank [hand]
  (vals (frequencies (map rank hand))))

(defn- max-same-rank [hand]
  (apply max (vals-by-rank hand)))

(defn pair? [hand]
  (= 2 (max-same-rank hand)))

(defn three-of-a-kind? [hand]
  (= 3 (max-same-rank hand)))

(defn four-of-a-kind? [hand]
  (= 4 (max-same-rank hand)))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= [2 3] (sort (vals-by-rank hand))))

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort (vals-by-rank hand)))
      (four-of-a-kind? hand)))

(defn- straight-seq? [hand]
  (let [lowest (apply min hand)
        cards  (count hand)]
     (= (range lowest (+ lowest cards)) (sort hand))))

(defn straight? [hand]
  (let [ace-high   (hand->rank hand)
        ace-low    (replace {14 1} ace-high)]
    (or (straight-seq? ace-high) (straight-seq? ace-low))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]   [pair? 1]
                   [two-pairs? 2]   [three-of-a-kind? 3]
                   [straight? 4]    [flush? 5]
                   [full-house? 6]  [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matches (filter #((first %) hand) checkers)
        scores  (map second matches)]
    (apply max scores)))
