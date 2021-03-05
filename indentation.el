;; if you eval these forms in Emacs you'll get correct indentation in
;; unevaluated property application specifications

;; actual properties
(put 'file:has-content 'common-lisp-indent-function '(2 2))

;; actual properties but suffixed with a period
(put 'deploys. 'common-lisp-indent-function '(4 4 2))
(put 'deploys-these. 'common-lisp-indent-function '(4 4 2))
(put 'chroot:os-bootstrapped. 'common-lisp-indent-function '(4 4 2))
