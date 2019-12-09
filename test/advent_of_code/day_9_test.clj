(ns advent-of-code.day-9-test
  "Unit tests for day 9."
  (:require [clojure.test :as test]
            [advent-of-code-2019.day-9 :as sut]))

(test/deftest example-program-1
  (let [program [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]]
    (test/is (= program (sut/intcode program [])))))

(test/deftest example-program-2
  (test/is (= [1219070632396864] (sut/intcode [1102,34915192,34915192,7,4,7,99,0] []))))

(test/deftest example-program-3
  (test/is (= [1125899906842624] (sut/intcode [104,1125899906842624,99] []))))

(test/deftest answer-1
  (test/is (= [2955820355] (sut/intcode [1]))))

(test/deftest answer-2
  (test/is (= [46643] (sut/intcode [2]))))

(test/deftest answer-1-async-wrapped
  (test/is (= [2955820355] (sut/intcode-async-wrapped [1]))))

(test/deftest answer-2-async-wrapped
  (test/is (= [46643] (sut/intcode-async-wrapped [2]))))
