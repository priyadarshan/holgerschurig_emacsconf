EMACS ?= emacs
PWD := $(shell pwd)

all:: packages update

.PHONY:: packages
packages:
	$(EMACS) -batch -l packages.el

.PHONY:: update
update:
	git submodule init
	git submodule update
	cd git/mu; git pull
	touch --no-create git/mu/autogen.sh

git/mu/autogen.sh:
	git clone https://github.com/djcb/mu git/mu

git/mu/INSTALL: git/mu/autogen.sh
	cd git/mu; \
	autoreconf -i

git/mu/Makefile: git/mu/INSTALL
	cd git/mu; \
	./configure \
		--disable-webkit \
		--disable-guile \
		--prefix=$(PWD)/git/mu/dist

git/mu/mu/mu: git/mu/Makefile
	$(MAKE) -C git/mu

all:: git/mu/dist/bin/mu
git/mu/dist/bin/mu: git/mu/mu/mu
	$(MAKE) -C git/mu install

cleanmu:
	cd git/mu; \
	git clean -fdx

.PHONY:: run
run:
	$(EMACS) -Q -l init.el

.PHONY:: profile
profile:
	$(EMACS) -Q -l git/profile-dotemacs/profile-dotemacs.el \
	--eval "(setq profile-dotemacs-file \
        (setq load-file-name \"$(abspath init.el)\"))" \
	-f profile-dotemacs

