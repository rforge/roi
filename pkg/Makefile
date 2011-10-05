PACKAGE=ROI
R=/home/theussl/bin/R-devel

all: pkg plugins

ROI: pkg

## FIXME: roxygen should be installed for R-devel as well
pkg: roxclean
	R CMD roxygen -s $(PACKAGE)
	$(R) CMD build $(PACKAGE).roxygen

check:
	$(R) CMD check $(PACKAGE)_*.tar.gz

install:
	$(R) CMD INSTALL $(PACKAGE)_*.tar.gz

roxclean:
	rm -rf $(PACKAGE).roxygen

clean: roxclean
	rm -rf $(PACKAGE)*.Rcheck
	rm -f  $(PACKAGE)*.tar.gz

