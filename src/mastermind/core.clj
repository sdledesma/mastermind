(ns mastermind.core)

(def colors #{:black :white :yellow :green :blue :red})
(def colors-seq (seq colors))
(def move-size 4)

(defn valid-color? [color]
  (contains? colors color))

(defn valid-move? [move]
  (= move-size
     (count move)
     (count (filter valid-color? move))))

(defn generate-random-combination []
  (loop [rc ()]
    (if (= move-size (count rc))
      (apply vector rc)
      (recur (cons (rand-nth colors-seq) rc)))))

(defn remove-indices [source indices-to-remove]
  (let [indices (apply hash-set indices-to-remove)]
    (loop [idx 0
           result []]
      (if (= idx
             (count source))
        result
        (if (contains? indices idx)
          (recur (inc idx)
                 result)
          (recur (inc idx)
                 (conj result (source idx))))))))

(defn array-to-count-map [ar]
  (loop [idx 0
         res {}]
    (if (= idx
           (count ar))
      res
      (let [k      (ar idx)
            newres (if (contains? res k)
                     (update res k inc)
                     (assoc res k 1))]
        (recur (inc idx) newres)))))

(defn count-partial-matches [secret-as-count-map guess]
  (loop [idx 0
         sacm secret-as-count-map
         res 0]
    (if (= idx
           (count guess))
      res
      (let [k     (guess idx)
            newcm (if (contains? sacm k)
                    (if (= (sacm k) 1)
                      (dissoc sacm k)
                      (update sacm k dec))
                    sacm)
            newres (if (= newcm sacm)
                     res
                     (inc res))]
        (recur (inc idx) newcm newres)))))

(defn identify-full-matches [secret player-move]
  (loop [idx 0
         res []]
    (if (= idx
           (count player-move))
      res
      (if (= (player-move idx)
             (secret idx))
        (recur (inc idx)
               (conj res idx))
        (recur (inc idx)
               res)))))

(defn check-move-secret [secret guess]
  (let [correct-indices (identify-full-matches secret guess)
        leftover-secret (remove-indices secret correct-indices)
        leftover-guess  (remove-indices guess correct-indices)
        white-pegs      (count correct-indices)
        black-pegs      (count-partial-matches (array-to-count-map leftover-secret) leftover-guess)]
    {:white-pegs white-pegs
     :black-pegs black-pegs}))
