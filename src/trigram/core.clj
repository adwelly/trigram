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

(def start-token "XXX-START-XXX")

(defn longsnozzle-sample [] ;; 596 max
  (map #(str start-token " " %) (drop 7 (get-sentences (apply str (lazy-open "https://www.gutenberg.org/files/65137/65137-0.txt"))))))

(defn edwin-drood-sample [] ;; 5094 max
 (drop 5 (get-sentences (apply str (lazy-open "http://www.gutenberg.org/files/564/564-0.txt")))))

(defn chop [l] ; l is list of strings
  (partition 3 1 nil l))

(defn pair [[f s & t]] ; l is a list of things. This works properly with lists of 3 and 2 but chops longer
    (if (nil? t) [[f] [s]] [[f s] [(first t)]]))

(defn conjoin-pair [m [k v]]
  (if (contains? m k) (assoc m k (conj (get m k) (first v))) (assoc m k v)))


(defn sentence-trigrams [l] ; l is a list of strings
  (reduce conjoin-pair {} (map pair (chop l))))

;slightly controversially - removing dups
(defn conjoin-maps [m0 m1]
  (merge-with #(vec (distinct (into %1 %2))) m0 m1))

; Room for a transducer here?
(defn go-long []
  (reduce conjoin-maps (map sentence-trigrams (map tokenize (longsnozzle-sample)))))

(defn go-drood []
  (reduce conjoin-maps (map sentence-trigrams (map tokenize (edwin-drood-sample)))))

;; Find keys beginning with a token
(defn begins [m t]
  (filter #(= (first %) t) (keys m)))

;; Find starting points in a trigram map
(defn starts [m]
  (begins m start-token))

(defn step [acc token tm strategy end-condp]
  (let [chosen-key (strategy acc token tm end-condp)
       ; _ (println "Chosen key: " chosen-key)
        value (get tm chosen-key)
        ;_ (println "value: " value)
        next-token (first (shuffle value))
        ;_ (println "next-token: " next-token)
        next-acc (into [] (concat acc chosen-key))
        ;_ (println "next-acc: " next-acc)
        potential-final-acc (conj next-acc next-token) ]
    (if (end-condp potential-final-acc) 
      {:acc potential-final-acc :token nil}
      {:acc next-acc :token next-token})))

;; Walk the trigram map until some condition (p) is fulfilled. The condition
;; p will be applied at each step to the growing accumulator of results
(defn walk [tm strategy end-condp]
  (loop [the-acc [] the-token start-token]
    (if (nil? the-token)
      the-acc
      (let [{:keys [acc token]} (step the-acc the-token tm strategy end-condp)]
        (recur acc token)))))