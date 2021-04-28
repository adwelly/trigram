(ns trigram.core-test
  (:require [midje.sweet :refer :all]
            [trigram.core :refer :all]
            [ubergraph.core :as uber]))

(def test-sample0
  ["The" "cat" "sat" "on" "the" "other" "cat" "on" "the" "mat" "on" "the" "floor."])

(def test-sample1
  ["Now" "is" "the" "winter" "of" "our" "discontent" "made" "summer" "by" "this" "glorious" "sun" "of" "York."])

(fact "Trigramize breaks lists into threes and twos, ones are eliminated"
      (chop test-sample0) => [["The" "cat" "sat"]
                              ["cat" "sat" "on"]
                              ["sat" "on" "the"]
                              ["on" "the" "other"]
                              ["the" "other" "cat"]
                              ["other" "cat" "on"]
                              ["cat" "on" "the"]
                              ["on" "the" "mat"]
                              ["the" "mat" "on"]
                              ["mat" "on" "the"]
                              ["on" "the" "floor."]
                              ["the" "floor."]]

     (chop test-sample1) => [["Now" "is" "the"] 
                             ["is" "the" "winter"] 
                             ["the" "winter" "of"] 
                             ["winter" "of" "our"] 
                             ["of" "our" "discontent"] 
                             ["our" "discontent" "made"] 
                             ["discontent" "made" "summer"] 
                             ["made" "summer" "by"] 
                             ["summer" "by" "this"] 
                             ["by" "this" "glorious"] 
                             ["this" "glorious" "sun"] 
                             ["glorious" "sun" "of"] 
                             ["sun" "of" "York."] 
                             ["of" "York."]])

(facts "about pair"
       (fact "pair converts a list of three things into a pair of a list of two and one thing"
             (pair '(1 2 3)) => [[1 2] [3]])
        (fact "pair converts a list of two things into a list with one thing and the other thing"
              (pair '(1 2)) => [[1] [2]]))

(facts "sentence-trigrams creates a map for a single sentence" 
       (sentence-trigrams [1 2 3 4 5]) => 
       {[1 2] [3]
        [2 3] [4]
        [3 4] [5]
        [4] [5]}
       (sentence-trigrams test-sample0) =>
       {["the" "other"] ["cat"]
        ["mat" "on"] ["the"]
        ["cat" "on"] ["the"]
        ["cat" "sat"] ["on"]
        ["The" "cat"] ["sat"]
        ["the"] ["floor."]
        ["the" "mat"] ["on"]
        ["on" "the"] ["other" "mat" "floor."]
        ["sat" "on"] ["the"]
        ["other" "cat"] ["on"]})

(facts "about conjoin-pair"
       (fact "if the map does not contain the key, the key and value are added" 
             (conjoin-pair {} [[1 2] [3]]) => {[1 2] [3]})
       (fact "if the map contains the key, the values are appended"
             (conjoin-pair {[1 2] [3]} [[1 2] [4]]) => {[1 2] [3 4]}))

(fact "Conjoin maps takes two maps of trigrams and produces one. Duplications in values are removed"
      (conjoin-maps {:a [1] :b [2 3]} {:b [4 5] :c [6]}) =>
      {:a [1] :b [2 3 4 5] :c [6]}
      (conjoin-maps 
         {["the" "other"] ["cat"]
          ["the" "mat"] ["on"]
          ["on" "the"] ["other"]}
         {["on" "the"] ["other" "mat" "floor."]
          ["sat" "on"] ["the"]
          ["other" "cat"] ["on"]}) =>
        {["the" "other"] ["cat"]
         ["the" "mat"] ["on"]
         ["on" "the"] ["other" "mat" "floor."]
         ["sat" "on"] ["the"]
         ["other" "cat"] ["on"]})

(fact "starts finds elements in the map that are starting tokens"
      (starts {["XXX-START-XXX" 3] [1]
               [2 3] [4]
               ["XXX-START-XXX" 4] [19]}) =>
               [["XXX-START-XXX" 3]
                ["XXX-START-XXX" 4]])

(defn constantly-true [l] true)

(defn more-than-5-tokens? [l]
  (< 5 (count l)))

(defn pick-first-value [acc token tm end-condp]
  (first (sort (begins tm token))))

(facts "about step"
       (fact "step will return the accumulater, with the final token and nil as the next token if the end condition is met"
             (step [1 2 3 4] 5 {[5 6] [7]} pick-first-value more-than-5-tokens?) =>
               {:acc [1 2 3 4 5 6 7] :token nil})
       (fact "step will return the current accumulator and the next token if the end condition is not met"
             (step [1] 2 {[2 3] [4]
                          [5 6] [7]}
                   pick-first-value more-than-5-tokens?) =>
             {:acc [1 2 3] :token 4}))

(facts "about walk"
       (fact "walk will keep calling step until step returns nil in the place of next token"
             (walk {[start-token 6] [7]} pick-first-value constantly-true) =>
             ["XXX-START-XXX" 6 7]
             (walk {[start-token 1] [0]
                    [0 1] [0]} pick-first-value more-than-5-tokens?) =>
            ["XXX-START-XXX" 1 0 1 0 1 0]
             (walk (merge {[start-token "The"] ["cat"]} (sentence-trigrams test-sample0)) pick-first-value more-than-5-tokens?) =>
            ["XXX-START-XXX" "The" "cat" "on" "the" "floor."])) 

;(facts "about ubergraph"
      ;(fact "vectors in nodes are acceptable"
            ;(ubergraph.core/graph [[1 2] [2 3]] [[1 2] [4 5]] [[4 5] [6 7]] [[2 3] [6 7]]) => 
            ;false))
      

(facts "about all-all-keys-starting-with"
       (all-keys-starting-with {["the" "other"] ["cat"]
                                ["the" "mat"] ["on"]
                                ["on" "the"] ["other"]} "the") => [["the" "other"] ["the" "mat"]])

(facts "about find-all-nodes"
       (find-all-nodes {["and" "with"] ["the" "my"]
                        ["the" "other"] ["cat"]
                        ["the" "mat"] ["on"]
                        ["my" "foot"] ["on"]
                        ["on" "the"] ["other"]} ["and" "with"]) => 
        [[["and" "with"] ["the" "other"]]
         [["and" "with"] ["the" "mat"]]
         [["and" "with"] ["my" "foot"]]])

(facts "about tm->graph"
       (tm->graph {["and" "with"] ["the" "my"]
                        ["the" "other"] ["cat"]
                        ["the" "mat"] ["on"]
                        ["my" "foot"] ["on"]
                        ["on" "the"] ["other"]}) => (contains {:node-map anything}))