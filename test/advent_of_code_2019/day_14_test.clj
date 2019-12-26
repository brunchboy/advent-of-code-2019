(ns advent-of-code-2019.day-14-test
  "Unit tests for day 14."
  (:require [clojure.test :as test]
            [advent-of-code-2019.day-14 :as sut]))

(def sample-reactions-1
  "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL
")

(test/deftest read-sample-1
  (test/is (= {"A"    {:yield 10, :inputs [[10 "ORE"]]},
               "B"    {:yield 1, :inputs [[1 "ORE"]]},
               "C"    {:yield 1, :inputs [[7 "A"] [1 "B"]]},
               "D"    {:yield 1, :inputs [[7 "A"] [1 "C"]]},
               "E"    {:yield 1, :inputs [[7 "A"] [1 "D"]]},
               "FUEL" {:yield 1, :inputs [[7 "A"] [1 "E"]]}}
              (sut/read-reactions sample-reactions-1))))

(test/deftest sample-1-details
  (test/is (= [31 {"FUEL" 0, "A" 2, "ORE" 0, "E" 0, "D" 0, "C" 0, "B" 0}]
              (sut/ore-needed 1 "FUEL" (sut/read-reactions sample-reactions-1) {}))))

(test/deftest sample-1
  (test/is (= 31 (sut/make-fuel (sut/read-reactions sample-reactions-1)))))

(def sample-reactions-2
  "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL")

(test/deftest read-sample-2
  (test/is (= {"A"    {:yield 2, :inputs [[9 "ORE"]]},
               "B"    {:yield 3, :inputs [[8 "ORE"]]},
               "C"    {:yield 5, :inputs [[7 "ORE"]]},
               "AB"   {:yield 1, :inputs [[3 "A"] [4 "B"]]},
               "BC"   {:yield 1, :inputs [[5 "B"] [7 "C"]]},
               "CA"   {:yield 1, :inputs [[4 "C"] [1 "A"]]},
               "FUEL" {:yield 1, :inputs [[2 "AB"] [3 "BC"] [4 "CA"]]}}
              (sut/read-reactions sample-reactions-2))))

(test/deftest sample-2-details
  (test/is (= [165 {"FUEL" 0, "AB" 0, "A" 0, "ORE" 0, "B" 1, "BC" 0, "C" 3, "CA" 0}]
              (sut/ore-needed 1 "FUEL" (sut/read-reactions sample-reactions-2) {}))))

(test/deftest sample-2
  (test/is (= 165 (sut/make-fuel (sut/read-reactions sample-reactions-2)))))

(def sample-reactions-3
  "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")

(test/deftest sample-3
  (test/is (= 13312 (sut/make-fuel (sut/read-reactions sample-reactions-3)))))

(test/deftest sample-3-part-2
  (test/is (= 82892753 (sut/fuel-potential (sut/read-reactions sample-reactions-3)))))

(def sample-reactions-4
  "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF")

(test/deftest sample-4
  (test/is (= 180697 (sut/make-fuel (sut/read-reactions sample-reactions-4)))))

(test/deftest sample-4-part-2
  (test/is (= 5586022 (sut/fuel-potential (sut/read-reactions sample-reactions-4)))))

(def sample-reactions-5
  "171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX")

(test/deftest sample-5
  (test/is (= 2210736 (sut/make-fuel (sut/read-reactions sample-reactions-5)))))

(test/deftest part-1
  (test/is (= 136771 (sut/part-1))))

(test/deftest sample-5-part-2
  (test/is (= 460664 (sut/fuel-potential (sut/read-reactions sample-reactions-5)))))

(test/deftest part-2
  (test/is (= 8193614 (sut/fuel-potential))))
