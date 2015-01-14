#!/usr/bin/python
f = open("holger.org")

insrc = False
incmt = False
for s in f.readlines():
	if incmt:
		if s.startswith("# END_SRC"):
			incmt = False
			print
			continue
		print ";;", s,
	elif insrc:
		if s.startswith("#+END_SRC"):
			insrc = False
			print
			continue
		print s,
	else:
		if s.startswith("#+BEGIN_SRC emacs-lisp"):
			insrc = True
			continue

		if s.startswith("# BEGIN_SRC"):
			incmt = True
			continue

		if s.startswith("#+"):
			continue

		if s.startswith("*"):
			if s.startswith("* "):
				print "\n\n\n"
				print ";" * 77
				print
			print ";;;;", s,
			continue
		print ";; ",s,
