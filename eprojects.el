(define-project-type c_cpp (generic-git)
 (look-for "Makefile")
 :relevant-files ("\\.c$" "\\.cpp$" "\\.h$" "\\.inl$" "\\.inc$")
)

(define-project-type python (generic-git)
  (look-for "setup.py")
  :relevant-files ("\\.py$" "\\.rst$" "\\.js$" "\\.html$" "\\.c$" "\\.h$")
  :irrelevant-files ("\\.py[co]$"))

(provide 'eprojects)
