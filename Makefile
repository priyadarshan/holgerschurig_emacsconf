EMACS ?= emacs

all: packages update

.PHONY:: packages
packages:
	$(EMACS) -batch -l packages.el

.PHONY:: update
update:
	git submodule init
	git submodule update

.PHONY:: run
run:
	$(EMACS) -Q -l init.el

.PHONY:: profile
profile:
	$(EMACS) -Q -l git/profile-dotemacs/profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
        (setq load-file-name \"$(abspath init.el)\"))" \
	-f profile-dotemacs

