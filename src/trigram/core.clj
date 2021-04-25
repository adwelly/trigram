(ns trigram.core
  (:require [clojure.java.io :as io]
            [opennlp.nlp :refer :all]
            [opennlp.treebank :refer :all]))

;; This is practically idiomatic these days:
(defn lazy-open [uri]
    (defn helper [rdr]
      (lazy-seq 
         (if-let [line (.readLine rdr)]
           (do (cons (str line " ") (helper rdr)))
           (.close rdr))))
  (lazy-seq (helper (io/reader uri))))

(def get-sentences (make-sentence-detector "en-sent.bin"))

(def tokenize (make-tokenizer "en-token.bin"))

(defn longsnozzle-sample [] ;; 596 max
  (get-sentences (apply str (lazy-open "https://www.gutenberg.org/files/65137/65137-0.txt"))))

(defn edwin-drood-sample [] ;; 5094 max
 (get-sentences (apply str (lazy-open "http://www.gutenberg.org/files/564/564-0.txt"))))

(defn chop [l] ; l is list of strings
  (partition 3 1 nil l))

(defn pair [[f s & t]] ; l is a list of things. This works properly with lists of 3 and 2 but chops longer
    (if (nil? t) [[f] [s]] [[f s] [(first t)]]))

(defn conjoin-pair [m [k v]]
  (if (contains? m k) (assoc m k (conj (get m k) (first v))) (assoc m k v)))


(defn sentence-trigrams [l] ; l is a list of strings
  (reduce conjoin-pair {} (map pair (chop l))))

(defn conjoin-maps [m0 m1]
  (merge-with #(vec (distinct (into %1 %2))) m0 m1))

(defn go-long []
  (reduce conjoin-maps (map sentence-trigrams (map tokenize (longsnozzle-sample)))))

(defn go-drood []
  (reduce conjoin-maps (map sentence-trigrams (map tokenize (edwin-drood-sample)))))