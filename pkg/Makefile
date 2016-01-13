PACKAGE=ROI
R=$(shell which R-devel)
##R=R

all: pkg plugins

ROI: pkg

## FIXME: roxygen should be installed for R-devel as well
pkg:
	echo "require(roxygen2); roxygenize(package.dir = '$(PACKAGE)', clean = TRUE, roclets = c('namespace', 'rd'))" | $(R) --vanilla
	$(R) CMD build $(PACKAGE)

plugins:
	$(R) CMD build $(PACKAGE).plugin.glpk
	$(R) CMD build $(PACKAGE).plugin.symphony
	$(R) CMD build $(PACKAGE).plugin.quadprog

models:
	echo "require(roxygen2); roxygenize(package.dir = '$(PACKAGE).models.finance', roclets = c('namespace', 'rd'))" | $(R) --vanilla
	$(R) CMD build $(PACKAGE).models.finance

check:
	$(R) CMD check $(PACKAGE)*.tar.gz
	./check_results
	cat SUMMARY

install:
	$(R) CMD INSTALL $(PACKAGE)*.tar.gz

clean: roxclean
	rm -rf $(PACKAGE)*.Rcheck
	rm -f  $(PACKAGE)*.tar.gz
	rm -f check.csv
	rm -f SUMMARY

submit: clean all install check
	echo 'scp $(PACKAGE)*.tar.gz cran.wu.ac.at:/srv/ftp/incoming/'
	echo 'ssh cran.wu.ac.at "chgrp cranadmin /srv/ftp/incoming/$(PACKAGE)*"'
	echo 'ssh cran.wu.ac.at "chmod g+w /srv/ftp/incoming/$(PACKAGE)*"'
