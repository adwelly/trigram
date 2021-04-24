(ns trigram.core
  (:require [clojure.java.io :as io]
            [opennlp.nlp :refer :all]
            [opennlp.treebank :refer :all]))

;; This is practically idiomatic these days:
(defn lazy-open [uri]
  (let [line-count (atom 0)]
    (defn helper [rdr]
      (lazy-seq 
         (if-let [line (.readLine rdr)]
           (do (swap! line-count inc) (cons (str line " ") (helper rdr)))
           (.close rdr)))))
  (lazy-seq (helper (io/reader uri))))

(def get-sentences (make-sentence-detector "en-sent.bin"))

(def tokenize (make-tokenizer "en-token.bin"))

(defn longsnozzle-sample [] ;; 596 max
  (get-sentences (apply str (lazy-open "https://www.gutenberg.org/files/65137/65137-0.txt"))))

(defn edwin-drood-sample [] ;; 5094 max
 (get-sentences (apply str (lazy-open "http://www.gutenberg.org/files/564/564-0.txt"))))

(defn go-long []
  (map tokenize (longsnozzle-sample)))

(defn chop [l] ; l is list of strings
  (partition 3 1 nil l))

(defn pair [l] ; l is a list of things
  false)