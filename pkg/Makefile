PACKAGE=ROI
R=$(shell which R-devel)

all: pkg plugins

ROI: pkg

## FIXME: roxygen should be installed for R-devel as well
pkg: roxclean
	R CMD roxygen -s $(PACKAGE)
	$(R) CMD build $(PACKAGE).roxygen

plugins:
	$(R) CMD build $(PACKAGE).plugin.glpk
	$(R) CMD build $(PACKAGE).plugin.symphony
	$(R) CMD build $(PACKAGE).plugin.quadprog

check:
	$(R) CMD check $(PACKAGE)*.tar.gz
	./check_results
	cat SUMMARY

install:
	$(R) CMD INSTALL $(PACKAGE)*.tar.gz

roxclean:
	rm -rf $(PACKAGE).roxygen

clean: roxclean
	rm -rf $(PACKAGE)*.Rcheck
	rm -f  $(PACKAGE)*.tar.gz
	rm -f check.csv
	rm -f SUMMARY

submit: clean all install check
	echo 'scp $(PACKAGE)*.tar.gz cran.wu.ac.at:/srv/ftp/incoming/'
	echo 'ssh cran.wu.ac.at "chgrp cranadmin /srv/ftp/incoming/$(PACKAGE)*"'
	echo 'ssh cran.wu.ac.at "chmod g+w /srv/ftp/incoming/$(PACKAGE)*"'
