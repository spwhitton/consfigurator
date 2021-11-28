(in-package :consfigurator/tests)
(named-readtables:in-readtable :consfigurator)
(in-consfig "consfigurator/tests")

(deftest version<.1 (version< "1.0.1" "1.0.2") t)

(deftest version<=.1 (version<= "1.0.1" "1.0.2") t)

(deftest version<.2 (version< "1.0.1" "1.0.1") nil)

(deftest version<=.2 (version<= "1.0.1" "1.0.1") t)

(deftest version<.3 (version< "1.1" "1.0.1") nil)

(deftest version<=.3 (version<= "1.1" "1.0.1") nil)

(deftest version<.4 (version< "1.a.1" "1.1") t)

(deftest version<.5 (version< "1..1" "1.1") t)
