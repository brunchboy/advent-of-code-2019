(ns advent-of-code-2019.day-1
  "Solutions to the Day 1 problems"
  (:require [clojure.repl :refer :all]))

(def module-masses
  "The masses of all the modules that need to be launched."
  [113481
   140620
   123826
   86474
   71091
   126880
   103784
   140154
   124024
   54281
   80810
   109441
   68828
   144207
   99151
   136876
   99398
   138555
   118619
   133215
   139302
   137780
   136649
   83358
   63027
   75067
   73974
   90158
   94691
   86847
   61466
   81184
   86043
   119923
   116576
   131380
   102136
   143364
   124421
   123141
   138131
   73274
   84598
   61410
   67240
   136186
   63878
   135804
   73599
   84526
   116178
   114587
   58606
   79162
   124031
   120329
   61270
   89887
   54859
   67618
   96669
   56796
   55725
   96105
   68833
   52417
   72249
   53930
   139995
   86217
   131618
   137145
   54944
   76456
   82141
   69754
   102656
   57461
   108747
   79510
   105715
   98046
   116903
   139339
   127451
   135374
   88468
   69524
   76112
   110928
   99160
   137229
   121433
   65951
   56267
   117209
   61358
   73659
   69633
   149274])

(defn fuel
  "Calculate fuel requred to launch a module of the given mass."
  [mass]
  (-> mass
      (/ 3)
      int
      (- 2)))

(defn answer
  "Calculate the sum of fuel requirements for launching all modules."
  []
  (apply + (map fuel module-masses)))

(defn fuel-recursive
  "Calculate the fuel required to launch a module of the given mass,
  taking in account the mass of the fuel itself, recursively, until
  the additional amount is no longer positive."
  [mass]
  (loop [base (fuel mass)
         added (fuel base)]
    (if (pos? added)
      (recur (+ base added) (fuel added))
      base)))

(defn answer-2
  "Calculate the sum of fuel requirements for launching all modules,
  including the fuel."
  []
  (apply + (map fuel-recursive module-masses)))
