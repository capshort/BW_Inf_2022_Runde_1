(ns sudokopie.transformations
  (:require
   [clojure.string :as str]))


(defn- get-sudoku-struktur
  [struktur sudoku-coll]
  (->> sudoku-coll
       (map (partial into (empty struktur)))
       (into (empty struktur))))

(defn- get-zeilenbloecke
  [sudoku]
  (->> sudoku
       (partition 3)
       (get-sudoku-struktur (empty sudoku))))

(defn- remove-zeilenbloecke
  [sudoku]
  (->> sudoku
       (flatten)
       (partition 9)
       (get-sudoku-struktur (empty sudoku))))

(def ^:private permutationen
  [[0 1 2]
   [0 2 1]
   [1 0 2]
   [1 2 0]
   [2 0 1]
   [2 1 0]])

(defn- apply-permutation-im-block
  [block permutation]
  (->> block
       (map-indexed list)
       (sort-by #(.indexOf permutation (first %)))
       (map second)
       (into (empty block))))

(defn- rotate-90-grad-uhrzeigersinn
  [sudoku]
  (get-sudoku-struktur
   (empty sudoku)
   (for [i (range 9)]
     (->> sudoku
          (flatten)
          (drop i)
          (take-nth 9)
          (reverse)))))

#_(defn- get-zirkel
  [anzahl-punkte]
  (for ))

#_(defn- umbenennungen-in-anzahl
  [anzahl]
  (for [zirkel (get-zirkel anzahl)])) ; Set aus maps zurückgeben
; 0 -> [{}]
; 1 -> [{}] Sonderfall
; 2 -> [ {1 2 2 1}
;        {1 3 3 1}
;        #_...
;        {2 3 3 2}
;        #_...
;        {7 8 8 7}
;        {7 9 9 7}
;        {8 9 9 8} ]

#_(def ^:private umbenennungen
  (for [anzahl-umbenennungen (range 10)]
    (umbenennungen-in-anzahl anzahl-umbenennungen))
  #_[{1 2
    2 1}
   {1 3
    3 1}
   {1 4
    4 1}
   #_...
   {1 2
    2 3
    3 1}])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API

(def sudoku1
  [[6 0 0 0 8 0 0 0 1]
   [0 9 0 4 0 0 6 0 0]
   [0 0 0 0 9 0 0 0 2]
   [0 0 0 0 0 1 0 0 0]
   [0 0 0 6 0 0 0 0 5]
   [3 2 7 5 0 0 0 0 8]
   [0 0 0 0 7 0 0 0 0]
   [0 0 6 8 0 3 9 7 0]
   [0 0 0 0 0 0 0 8 0]])

(def sudoku2
  [[0 0 9 0 0 4 0 0 6]
   [0 6 0 8 0 0 1 0 0]
   [0 0 0 9 0 0 2 0 0]
   [0 0 0 0 1 0 0 0 0]
   [0 0 0 0 0 6 5 0 0]
   [7 3 2 0 0 5 8 0 0]
   [0 0 0 7 0 0 0 0 0]
   [0 0 0 0 0 0 0 8 0]
   [6 0 0 0 3 8 0 7 9]])

(defn rotate-uhrzeigersinn
  [sudoku anzahl-90-grad-drehungen]
  (if (<= anzahl-90-grad-drehungen 0)
    sudoku
    (rotate-uhrzeigersinn
     (rotate-90-grad-uhrzeigersinn sudoku)
     (dec anzahl-90-grad-drehungen))))

(defn permutate-im-zeilenblock
  [sudoku zeilenblock-nr permutation]
  (letfn [(permutiere-oder-behalte
            [indexed-blocks]
            (map (fn [indexed-block]
                   (if (= zeilenblock-nr (first indexed-block))
                     (apply-permutation-im-block (second indexed-block) permutation)
                     (second indexed-block)))
                 indexed-blocks))]
    (->> sudoku
         (get-zeilenbloecke)
         (map-indexed list)
         (permutiere-oder-behalte)
         (get-sudoku-struktur (empty sudoku))
         (remove-zeilenbloecke))))

(defn permutate-im-spaltenblock
  [sudoku spaltenblock-nr permutation]
  (-> sudoku
      (rotate-uhrzeigersinn 1)
      (permutate-im-zeilenblock spaltenblock-nr permutation)
      (rotate-uhrzeigersinn 3)))

(defn permutate-zeilenbloecke
  [sudoku permutation]
  (-> sudoku
      (get-zeilenbloecke)
      (apply-permutation-im-block permutation)
      (remove-zeilenbloecke)))

(defn permutate-spaltenbloecke
  [sudoku permutation]
  (-> sudoku
      (rotate-uhrzeigersinn 1)
      (permutate-zeilenbloecke permutation)
      (rotate-uhrzeigersinn 3)))

(defn rename-ziffern
  [sudoku renamings]
  (letfn [(rename-ziffer
            [renaming]
            (let [startziffer (key renaming)
                  zielziffer (val renaming)]
              (->> sudoku
                   (flatten)
                   (map (fn [eintrag]
                          (if (= eintrag startziffer)
                            (str "->" zielziffer)
                            eintrag))))))
          (apply-renaming
           [current-result sudoku-renamed-ziffer]
           (->> current-result
                (map vector sudoku-renamed-ziffer)
                (map (fn [[zu-verarbeitender-eintrag bisheriger-eintrag]]
                       (if (str/starts-with? zu-verarbeitender-eintrag "->")
                         (Integer. (str/replace zu-verarbeitender-eintrag "->" ""))
                         bisheriger-eintrag)))))]
    (->> renamings
         (seq)
         (map rename-ziffer)
         (reduce apply-renaming (flatten sudoku))
         (partition 9)
         (get-sudoku-struktur (empty sudoku)))))

(def rotations
  (for [anzahl-drehungen (range 1 4)]
    (fn [sudoku]
      (rotate-uhrzeigersinn sudoku anzahl-drehungen))))

(def permutationen-in-zeilenbloecken
  (for [permutation permutationen
        zeilenblock-nr [0 1 2]]
    (fn [sudoku]
      (permutate-im-zeilenblock sudoku zeilenblock-nr permutation))))

(def permutationen-in-spaltenbloecken
  (for [permutation permutationen
        spaltenblock-nr [0 1 2]]
    (fn [sudoku]
      (permutate-im-zeilenblock sudoku spaltenblock-nr permutation))))

(def permutationen-der-zeilenbloecke
  (for [permutation permutationen]
    (fn [sudoku]
      (permutate-zeilenbloecke sudoku permutation))))

(def permutationen-der-spaltenbloecke
  (for [permutation permutationen]
    (fn [sudoku]
      (permutate-spaltenbloecke sudoku permutation))))

(comment

  sudoku1
  ;; [[6 0 0 0 8 0 0 0 1]
  ;;  [0 9 0 4 0 0 6 0 0]
  ;;  [0 0 0 0 9 0 0 0 2]
  ;;  [0 0 0 0 0 1 0 0 0]
  ;;  [0 0 0 6 0 0 0 0 5]
  ;;  [3 2 7 5 0 0 0 0 8]
  ;;  [0 0 0 0 7 0 0 0 0]
  ;;  [0 0 6 8 0 3 9 7 0]
  ;;  [0 0 0 0 0 0 0 8 0]]

  (apply-permutation-im-block (first (get-zeilenbloecke sudoku1)) [0 2 1])
  ;; =>
  ;; [[6 0 0 0 8 0 0 0 1]
  ;;  [0 0 0 0 9 0 0 0 2]
  ;;  [0 9 0 4 0 0 6 0 0]]

  (permutate-im-zeilenblock sudoku1 0 [0 2 1])
  ;; =>
  ;; [[6 0 0 0 8 0 0 0 1]
  ;;  [0 0 0 0 9 0 0 0 2]
  ;;  [0 9 0 4 0 0 6 0 0]
  ;;  [0 0 0 0 0 1 0 0 0]
  ;;  [0 0 0 6 0 0 0 0 5]
  ;;  [3 2 7 5 0 0 0 0 8]
  ;;  [0 0 0 0 7 0 0 0 0]
  ;;  [0 0 6 8 0 3 9 7 0]
  ;;  [0 0 0 0 0 0 0 8 0]]

  (permutate-zeilenbloecke sudoku1 [0 2 1])
  ;; =>
  ;; [[6 0 0 0 8 0 0 0 1]
  ;;  [0 9 0 4 0 0 6 0 0]
  ;;  [0 0 0 0 9 0 0 0 2]
  ;;  [0 0 0 0 7 0 0 0 0]
  ;;  [0 0 6 8 0 3 9 7 0]
  ;;  [0 0 0 0 0 0 0 8 0]
  ;;  [0 0 0 0 0 1 0 0 0]
  ;;  [0 0 0 6 0 0 0 0 5]
  ;;  [3 2 7 5 0 0 0 0 8]]

  (rotate-90-grad-uhrzeigersinn sudoku1)
  ;; =>
  ;; [[0 0 0 3 0 0 0 0 6]
  ;;  [0 0 0 2 0 0 0 9 0]
  ;;  [0 6 0 7 0 0 0 0 0]
  ;;  [0 8 0 5 6 0 0 4 0]
  ;;  [0 0 7 0 0 0 9 0 8]
  ;;  [0 3 0 0 0 1 0 0 0]
  ;;  [0 9 0 0 0 0 0 6 0]
  ;;  [8 7 0 0 0 0 0 0 0]
  ;;  [0 0 0 8 5 0 2 0 1]]

  (let [renamings {1 3
                   3 5
                   5 1}]
    (rename-ziffern sudoku1 renamings))
  ;; =>
  ;; [[6 0 0 0 8 0 0 0 3]
  ;;  [0 9 0 4 0 0 6 0 0]
  ;;  [0 0 0 0 9 0 0 0 2]
  ;;  [0 0 0 0 0 3 0 0 0]
  ;;  [0 0 0 6 0 0 0 0 1]
  ;;  [5 2 7 1 0 0 0 0 8]
  ;;  [0 0 0 0 7 0 0 0 0]
  ;;  [0 0 6 8 0 5 9 7 0]
  ;;  [0 0 0 0 0 0 0 8 0]]
  )
