# author: Keith Irwin <keith.irwin@gmail.com>

version = 0.1.0
app-name = cl-stomp
dist-dir = $(app-name)-$(version)
dist-files = *.lisp *.asd
dist-doc = $(dist-dir)/doc
doc-files = *.html *.txt

tarchive = $(dist-dir).tar
tarball = $(tarchive).gz

detritus := *.fasl *~ $(dist-dir) $(tarball)

#.PHONY: dist
dist:
	mkdir -p $(dist-dir)
	mkdir -p $(dist-doc)
	cp $(dist-files) $(dist-dir)
	cp $(doc-files) $(dist-doc)
	tar cvf $(tarchive) $(dist-dir)
	gzip $(tarchive)

#.PHONY: clean
clean:
	rm -rf $(detritus)
