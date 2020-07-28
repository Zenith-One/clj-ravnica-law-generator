(ns law-generator.core
  (:require [clojure.string :refer [join] :as string])
  (:gen-class))

(def law-subs ["Title"
               "Article"
               "Section"
               "Subsection"
               "Clause"
               "Paragraph"])

(def law-sources ["Guildpact"
                  "Articles of Founding"
                  "Precinct Statute"
                  "Civic Preservation Code"
                  "Decalateral Compact"])

(def law-forms [(fn [p] (format "%s, %s" (rand-nth law-sources) p))
            (fn [p] (format "%s of the %s" p (rand-nth law-sources)))])

(defn genlaw
  []
  (let [depth (max 2 (rand-int (count law-subs)))
        law-path
        (map vector
             law-subs
             (repeatedly depth  #(rand-int 100)))]
    (->> law-path
         (map (fn [[p n]] (str p " " n)))
         (join ", ")
         ((rand-nth law-forms)))))

(defn -main
  "Generate 20 random laws."
  [& args]
  (repeatedly 20 genlaw))
