EMACS ?= emacs
PWD := $(shell pwd)

.PHONY:: all
all::


# Checkout profile-emacs
all:: git/profile-dotemacs/.git
git/profile-dotemacs/.git:
	git submodule init
	git submodule update
# Update profile-emacs
.PHONY:: update
update::
	git submodule update


# Checkout org
all:: elisp/org-mode/.git/config
elisp/org-mode/.git/config:
	cd elisp; \
	git clone git://orgmode.org/org-mode.git
# Update org
update::
	cd elisp/org-mode; \
	git pull; \
	git log --reverse -p ORIG_HEAD..HEAD | tee ../tmp/org-mode.log; \
	test -s ../tmp/org-mode.log || rm -f ../tmp/org-mode.log


.PHONY:: run
run:
	$(EMACS) -Q -l init.el

.PHONY:: profile
profile:
	$(EMACS) -Q -l git/profile-dotemacs/profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
        (setq load-file-name \"$(abspath init.el)\"))" \
	-f profile-dotemacs

