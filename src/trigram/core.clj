(ns trigram.core
  (:require [clojure.java.io :as io]
            [opennlp.nlp :refer :all]
            [opennlp.treebank :refer :all]))

;; Load the sample(s)
;; ========================

;; This is practically idiomatic these days:
(defn lazy-open [uri]
    (defn helper [rdr]
      (lazy-seq 
         (if-let [line (.readLine rdr)]
           (do (cons (str line " ") (helper rdr)))
           (.close rdr))))
  (lazy-seq (helper (io/reader uri))))

;; These two defs have been lifted from Dakrones nlp library...

(def get-sentences (make-sentence-detector "en-sent.bin"))

(def tokenize (make-tokenizer "en-token.bin"))

;; Mark trigrams that start sentences with this token. This preserves a little of the 
;; sentence structure.
(def start-token "XXX-START-XXX")

;; The Longsnozzle Event. Gutenberg header and footer dropped
(defn longsnozzle-sample [] ;; 596 max
  (map #(str start-token " " %) (drop-last 116 (drop 7 (get-sentences (apply str (lazy-open "https://www.gutenberg.org/files/65137/65137-0.txt")))))))

;; The Mystery of Edwin Drood. Gutenberg header and footer dropped
(defn edwin-drood-sample [] ;; 5094 max
   (map #(str start-token " " %) (drop-last 116 (drop 6 (get-sentences (apply str (lazy-open "http://www.gutenberg.org/files/564/564-0.txt")))))))

;; Make the trigram map
;; ====================
(defn chop [l] ; l is list of strings
  (partition 3 1 nil l))

(defn pair [[f s & t]] ; l is a list of things. This works properly with lists of 3 and 2 but chops longer
    (if (nil? t) [[f] [s]] [[f s] [(first t)]]))

(defn conjoin-pair [m [k v]]
  (if (contains? m k) (assoc m k (conj (get m k) (first v))) (assoc m k v)))

(defn sentence-trigrams [l] ; l is a list of strings
  (reduce conjoin-pair {} (map pair (chop l))))

;slightly controversially - removing dups (this does fritz the frequency a bit)
(defn conjoin-maps [m0 m1]
  (merge-with #(vec (distinct (into %1 %2))) m0 m1))

; Opportunity for a transducer here?
(defn go-long []
  (reduce conjoin-maps (map sentence-trigrams (map tokenize (longsnozzle-sample)))))

; Transducers again
(defn go-drood []
  (reduce conjoin-maps (map sentence-trigrams (map tokenize (edwin-drood-sample)))))

;; Find trigram-map keys beginning with a token
(defn begins [tm token]
  (filter #(= (first %) token) (keys tm)))

;; Find starting points in a trigram map
(defn starts [m]
  (begins m start-token))
 
;; This is the heart of it; step takes an accumulator that starts as empty, a token
;; that is the next token to be searched for and a strategy and end condition. The 
;; strategy is intended to be the technique by which the next candidate trigram is
;; chosen. The end condition is used by step to signal that it has met the conditions.
;; Because we are dealing with random walks, it's possible that step runs into a dead 
;; end on the graph. This shows up in the same way with the next-token being nil.

(defn step [acc token tm strategy end-condp]
  (let [chosen-key (strategy acc token tm end-condp)
        value (get tm chosen-key)
        next-token (first (shuffle value))
        next-acc (into [] (concat acc chosen-key))
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

;; Turn a list of tokens into a nice string, dropping the start-token
(defn ->str [c]
  (apply str (interpose " " (drop 1 c)))) ;; drop the start-token

;; Repeat a walk that fails to meet the end condition (because the walk entered 
;; a cul-de-sac)
(defn repeated-walk [tm strategy end-condp]
  (loop []
    (let [candidate (walk tm strategy end-condp)]
      (if (end-condp candidate)
        (->str candidate)
        (do (print ".") (recur))))))

;; Strategies and end conditions....
;; =================================
;;
;; The insight is that once you have a trigram map you can define various ways of moving
;; around the graph it represents by having a distinct strategy for picking the next
;; trigram (e.g totally random, favouring interior points of the graph etc. etc), as well
;; as different end conditions for halting the exploration.
;; This approach makes testing a little easier as you can write deterministic strategies.
;; See the core_test for a few examples.
;;
;; As an example, Keep-going tries to generate as long a sequence as possible by sticking to interior
;; parts of the graph represented by the trigram map

(defn keep-going [acc token tm end-condp]
  (let [candidate-values (begins tm token)
        [interior terminal] (split-with #(< 1 (count %)) candidate-values) ; by a quirk of construction, terminal nodes have only one token in value
        interior-choice (first (shuffle interior))] ; pick a random interior if available
    (if (some? interior-choice)
      interior-choice
      (first (shuffle terminal))))) ; otherwise pick a random terminal

(defn punctuation-mark? [s]
  (contains? #{"." "," "\"" "'" ";" "--" "-" "?" "!"} s))

(defn more-than-50? [final-acc]
  (< 50 (count (remove punctuation-mark? final-acc))))

(defn ends-with-stop? [final-acc]
  (contains? #{"." "?" "!"} (last final-acc)))

;; Some answers
;; ============

(defn seq-50-long []
  (repeated-walk (go-long) keep-going more-than-50?))

(defn seq-50-drood []
(repeated-walk (go-drood) keep-going more-than-50?))

;; This is not quite what was asked for as I'm priviliging idiomatic end of sentence punctuation
(defn sentence [tm]
  (repeated-walk tm keep-going ends-with-stop?))

(defn ten-sentences []
  (let [tm (go-drood)]
    (doall (for [n (range 10)] (sentence tm)))))

;; The approach to a 'sentence containing a word' is to create a backwards step function
;; that runs over a trigram-map in reverse to the usual LR order, and starting with the 
;; desired word, work back to the beginning of sentence. Then use the existing code to generate 
;; the second half of the sentence, and join the two together.
