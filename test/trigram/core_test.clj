(ns trigram.core-test
  (:require [midje.sweet :refer :all]
            [trigram.core :refer :all]))

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


