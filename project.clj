(defproject advent-of-code-2019 "0.1.0-SNAPSHOT"
  :description "Solutions to the 2019 Advent of Code problems"
  :url "https://github.com/brunchboy/advent-of-code-2019"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/core.async "0.6.532"]
                 [clojure-lanterna "0.9.4"]
                 [jline/jline "2.14.2"]
                 [com.taoensso/timbre "4.10.0"]]
  :main advent-of-code-2019.day-25
  :repl-options {:init-ns advent-of-code-2019.core}
  :jvm-opts [#_"-Xmx30g" #_"-Xss512m"])
