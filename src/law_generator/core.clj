(ns law-generator.core
  (:gen-class))

(def join-space (partial clojure.string/join " "))

(defn rand-int-range
  "Accepts starting and ending integers (both inclusive) and generates a random integer in that range"
  [start end]
  (+ start (rand-int (- (inc end) start))))

(defn functionator [item]
  (if (vector? item)
    (fn [] (apply (first item) (rest item)))
    (fn [] item)))


(def foo (functionator [rand-int-range 1 54]))

(foo)


(def test1 [rand-int-range 1 54])

(def ralias (fn [] (apply (first test1) (rest test1))))

(defn joiner [& args]
  (let [pieces (map functionator args)]
    (fn [] (join-space (map (fn [i] (i)) pieces)))))

(def test1 (joiner "blah" [rand-int-range 1 50]))

(test1)

(defn total-weight
  "Expects a seq of maps with a numerical :weight"
  [items]
  (reduce + (map :weight items)))

(def law-subs [{:name "Article" :weight 1}
               {:name "Subsection" :weight 1}
               {:name "Clause" :weight 1}
               {:name "Paragraph" :weight 1}
               {:name "Appendix" :weight 1}])

(defn law-sub
  [] 
  (let [subs law-subs
        totals (total-weight subs)
        target (rand totals)]
    (loop [[sub & remaining] subs
           accumulated-weight (:weight sub)]
      (if (> accumulated-weight target)
        (:name sub)
        (recur remaining (+ accumulated-weight (:weight (first remaining))))))))

(def law-sources ["Guildpact"
                 "Articles of Founding"
                 "Precinct Statute"
                 "Civic Preservation Code"
                 "Decalateral Compact"])

(defn law-source [] (rand-nth law-sources))

(def law-forms [(joiner  "Title" [rand-int-range 1 99] [law-sub] [rand-int-range 1 999] [law-sub] [rand-int-range 1 999] "of the" [law-source]) 
                (joiner [law-source] [law-sub] [rand-int-range 1 100] [law-sub] [rand-int-range 1 100])])

(defn has-dupes? [law]
  (let [splt (clojure.string/split law #" ")]
    (not (= (count (distinct splt)) (count splt)))))

(defn generate-law []
  (let [law 
        ((rand-nth law-forms))]
    (if (has-dupes? law)
      (generate-law)
      law)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (for [x (range 20)]
        (generate-law)))


