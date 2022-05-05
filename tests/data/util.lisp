(in-package :consfigurator/tests)
(named-readtables:in-readtable :consfigurator)
(in-consfig "consfigurator/tests")

;; relative parts
(deftest literal-data-pathname.1
    (unix-namestring (literal-data-pathname "/home/user/data/" "foo" "bar"))
  "/home/user/data/foo/bar")

;; missing trailing / on part 1
(deftest literal-data-pathname.2
    (unix-namestring (literal-data-pathname "/home/user/data" "foo" "bar"))
  "/home/user/data/foo/bar")

;; absolute part 2
(deftest literal-data-pathname.3
    (unix-namestring (literal-data-pathname "/home/user/data/" "/foo" "bar"))
  "/home/user/data/foo/bar")

;; relative part 2, "_"
(deftest literal-data-pathname.4
    (unix-namestring (literal-data-pathname "/home/user/data/" "_foo" "bar"))
  "/home/user/data/_foo/bar")

;; absolute part 3
(deftest literal-data-pathname.5
    (unix-namestring (literal-data-pathname "/home/user/" "/data" "/foo/bar"))
  "/home/user/data/foo/bar")

;; with type
(deftest literal-data-pathname.6
    (unix-namestring
     (literal-data-pathname "/home/user/" "/data" "/foo/bar" :type "txt"))
  "/home/user/data/foo/bar.txt")

;; base-path is pathname

(deftest literal-data-pathname.7
    (unix-namestring (literal-data-pathname #P"/home/user/data/" "foo" "bar"))
  "/home/user/data/foo/bar")

;; base-path not a directory
(deftest literal-data-pathname.8
    (handler-case
        (literal-data-pathname #P"/home/user/data" "foo" "bar")
      (simple-program-error (c) "fail"))
  "fail")

;; extra '/' at end
(deftest literal-data-pathname.9
    (unix-namestring (literal-data-pathname "/home/user/data//" "foo" "bar"))
  "/home/user/data/foo/bar")

;; extra '/' in middle
(deftest literal-data-pathname.10
    (unix-namestring (literal-data-pathname "/home/user//data/" "foo" "bar"))
  "/home/user/data/foo/bar")

;; extra '/' part 2
(deftest literal-data-pathname.11
    (unix-namestring (literal-data-pathname "/home/user/data/" "foo//" "bar"))
  "/home/user/data/foo/bar")

;; extra '/' part 3
(deftest literal-data-pathname.12
    (unix-namestring (literal-data-pathname "/home/user/data/" "foo" "//bar"))
  "/home/user/data/foo/bar")
