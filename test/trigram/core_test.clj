(ns trigram.core-test
  (:require [midje.sweet :refer :all]
            [trigram.core :refer :all]))

(def test-sample0
  ["The" "cat" "sat" "on" "the" "other" "cat" "on" "the" "wall."])

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
                              ["on" "the" "wall."]
                              ["the" "wall."]]

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

(pending-facts "about pair"
       (fact "pair converts a list of three things into a pair of a list of two and one thing"
             (pair '(1 2 3)) => [[1 2] 3]))
