#!/usr/bin/python
f = open("holger.el")

inSource = False
for s in f.readlines():
	s = s.rstrip()
	if not s:
		print s
		continue
	if s.startswith(";;"):
		if inSource:
			inSource = False
			print "#+END_SRC"
			print
		if s.startswith(";;;_ "):
			print
			print s[5:]
			continue

		if len(s)>2 and s[2] == " ":
			print s[3:]
		else:
			print s[2:]
		continue
	if not inSource:
		inSource = True
		print
		print "#+BEGIN_SRC emacs-lisp"
	print s

