(in-package :consfigurator.data.pgp)

;;;; Simple PGP-encrypted file source of prerequisite data

;; We provide an implementation of REGISTER-DATA-SOURCE and functions for the
;; user to call at the REPL to add pieces of data, see what's there, etc.  (a
;; prerequisite data source which was some sort of external file-generating or
;; secrets storage database might not provide any functions for the REPL).
