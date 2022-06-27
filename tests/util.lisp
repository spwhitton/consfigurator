(in-package :consfigurator/tests)
(named-readtables:in-readtable :consfigurator)
(in-consfig "consfigurator/tests")

(deftest multiple-value-mapcan.1
    (multiple-value-mapcan
     (lambda (car1 car2 car3)
       (values (list car1 car2 car3) (list car3 car2 car1)))
     '(1 2 3 4) '(1 2 3) '(5 4 3 2 1))
  (1 1 5 2 2 4 3 3 3) (5 1 1 4 2 2 3 3 3))

(deftest multiple-value-mapcan.2
    (multiple-value-mapcan #'list '(1 2 3 4 5))
  (1 2 3 4 5))

(deftest multiple-value-mapcan.3
    (let ((n 0))
      (multiple-value-mapcan
       (lambda (car1 car2 car3)
         (if (> (incf n) 2)
             (list car2)
             (values (list car1 car2 car3) (list car3))))
       '(1 2 3 4) '(1 2 3) '(5 4 3 2 1)))
  (1 1 5 2 2 4 3) (5 4))

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

;; valid use of '-'
(deftest valid-hostname-p.6 (valid-hostname-p "host-name.example.com") t)

;; invalid use of '-'
(deftest valid-hostname-p.7 (valid-hostname-p "-hostname.example.com") nil)

;; invalid character
(deftest valid-hostname-p.8 (valid-hostname-p "_hostname.example.com") nil)

;; invalid character 2
(deftest valid-hostname-p.9 (valid-hostname-p "foo/bar") nil)
