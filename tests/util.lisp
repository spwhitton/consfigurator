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

;; without domain
(deftest valid-hostname-p.1 (valid-hostname-p "localhost") t)

(deftest valid-hostname-p.2 (valid-hostname-p "host.example.com") t)

;; case insensitive check
(deftest valid-hostname-p.3 (valid-hostname-p "host.Example.Com") t)

;; total length too long
(deftest valid-hostname-p.4
    (valid-hostname-p (format nil "~127@{a.~}a" nil))
  nil)

;; label too long
(deftest valid-hostname-p.5
    (valid-hostname-p (format nil "~64@{a~}a" nil))
  nil)

;; valid use of `-'
(deftest valid-hostname-p.6 (valid-hostname-p "host-name.example.com") t)

;; invalid use of `-'
(deftest valid-hostname-p.7 (valid-hostname-p "-hostname.example.com") nil)

;; invalid character
(deftest valid-hostname-p.8 (valid-hostname-p "_hostname.example.com") nil)

;; invalid character 2
(deftest valid-hostname-p.9 (valid-hostname-p "foo/bar") nil)
