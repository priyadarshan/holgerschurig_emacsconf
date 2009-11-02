;; Package: eproject

(load "elisp/eproject/eproject") ;; 'noerror 'nomessage)
(load "elisp/eproject/eproject-extras") ;; 'noerror 'nomessage)

;; Silence compiler
(require 'cl)
(flet ((look-for (expr &optional (expr-type :filename)) )))

(define-project-type 'libertas (generic)
  (look-for "persistcfg.c")
  ;;:relevant-files ("\\*.c$" "\\.h" "Makefile" "Kconfig")
  )

(add-hook 'libertas-project-file-visit-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 "make -C /usr/src/linux-wl SUBDIRS=drivers/net/wireless/libertas")))

(define-project-type lisp (generic)
  (eproject--scan-parents-for file
			      (lambda (directory)
				(let ((dname (file-name-nondirectory directory)))
				  (file-exists-p (format "%s/%s.asd" directory dname)))))
  ;;:relevant-files ("\\.lisp$" "\\.asd$")
  )
