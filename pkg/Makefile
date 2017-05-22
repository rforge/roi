PACKAGE=ROI
#R=$(shell which R-devel)
R=R
RR=/home/florian/bin/R_dev/bin/R

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

clean:
	rm -rf $(PACKAGE)*.Rcheck
	rm -f  $(PACKAGE)*.tar.gz
	rm -f check.csv
	rm -f SUMMARY

submit: clean all install check
	echo 'scp $(PACKAGE)*.tar.gz cran.wu.ac.at:/srv/ftp/incoming/'
	echo 'ssh cran.wu.ac.at "chgrp cranadmin /srv/ftp/incoming/$(PACKAGE)*"'
	echo 'ssh cran.wu.ac.at "chmod g+w /srv/ftp/incoming/$(PACKAGE)*"'

######

## archive
archive.plugins:
	mv ROI.plugin*.tar.gz archive/

clean.Rchecks:
	rm -rf *.Rcheck

## ROI
## ===
ROI.namespace:
	export ROI_LOAD_PLUGINS="FALSE"
	$(RR) -e "library(roxygen2); roxygenise(package.dir='ROI', roclets=c('rd', 'namespace'))"

ROI.build:
	export ROI_LOAD_PLUGINS="FALSE"
	$(RR) -e "library(roxygen2); roxygenise(package.dir='ROI', roclets=c('rd', 'namespace'))"
	$(RR) CMD build ROI

ROI.check: ROI.build
	$(RR) CMD check ROI_*.tar.gz \\
	cp ROI.Rcheck/ROI-manual.pdf ROI-manual.pdf

ROI.install: ROI.build 
	$(RR) CMD INSTALL ROI_*.tar.gz

ROI.all: ROI.namespace ROI.install


## alabama
## =======
alabama.build:
	$(RR) CMD build ROI.plugin.alabama

alabama.check: alabama.build 
	$(RR) CMD check ROI.plugin.alabama*.tar.gz

alabama.install: alabama.build
	$(RR) CMD INSTALL ROI.plugin.alabama*.tar.gz

## deoptim
## =======
deoptim.build:
	$(RR) CMD build ROI.plugin.deoptim

deoptim.check: deoptim.build 
	$(RR) CMD check ROI.plugin.deoptim*.tar.gz

deoptim.install: deoptim.build
	$(RR) CMD INSTALL ROI.plugin.deoptim*.tar.gz

## ecos
## ====
ecos.build:
	$(RR) CMD build ROI.plugin.ecos

ecos.check: ecos.build 
	$(RR) CMD check ROI.plugin.ecos*.tar.gz

ecos.install: ecos.build
	$(RR) CMD INSTALL ROI.plugin.ecos*.tar.gz

## glpk
## ====
glpk.build:
	$(RR) CMD build ROI.plugin.glpk

glpk.check: glpk.build 
	$(RR) CMD check ROI.plugin.glpk*.tar.gz

glpk.install: glpk.build
	$(RR) CMD INSTALL ROI.plugin.glpk*.tar.gz

## gurobi
## ======
gurobi.build:
	$(RR) CMD build ROI.plugin.gurobi

gurobi.check: gurobi.build 
	$(RR) CMD check ROI.plugin.gurobi*.tar.gz

gurobi.install: gurobi.build
	$(RR) CMD INSTALL ROI.plugin.gurobi*.tar.gz

## ipop
## ====
ipop.build:
	$(RR) CMD build ROI.plugin.ipop

ipop.check: ipop.build 
	$(RR) CMD check ROI.plugin.ipop*.tar.gz

ipop.install: ipop.build
	$(RR) CMD INSTALL ROI.plugin.ipop*.tar.gz

## lpsolve
## =======
lpsolve.build:
	$(RR) CMD build ROI.plugin.lpsolve

lpsolve.check: lpsolve.build 
	$(RR) CMD check ROI.plugin.lpsolve*.tar.gz

lpsolve.install: lpsolve.build
	$(RR) CMD INSTALL ROI.plugin.lpsolve*.tar.gz

## miplib
## ======
miplib.docs:
	$(RR) -e "library(roxygen2); roxygenise(package.dir = 'ROI.models.MIPLIB', roclets=c('rd', 'namespace'))"

miplib.build:
	$(RR) CMD build ROI.plugin.miplib

miplib.check: miplib.build 
	$(RR) CMD check ROI.plugin.miplib*.tar.gz

miplib.install: miplib.build
	$(RR) CMD INSTALL ROI.plugin.miplib*.tar.gz

## mosek
## =====
mosek.build:
	$(RR) CMD build ROI.plugin.mosek

mosek.check: mosek.build 
	$(RR) CMD check ROI.plugin.mosek*.tar.gz

mosek.install: mosek.build
	$(RR) CMD INSTALL ROI.plugin.mosek*.tar.gz

## msbinlp
## =======
msbinlp.build:
	$(RR) CMD build ROI.plugin.msbinlp

msbinlp.check: msbinlp.build 
	$(RR) CMD check ROI.plugin.msbinlp*.tar.gz

msbinlp.install: msbinlp.build
	$(RR) CMD INSTALL ROI.plugin.msbinlp*.tar.gz

## nloptr
## ======
nloptr.build:
	$(RR) CMD build ROI.plugin.nloptr

nloptr.check: nloptr.build
	$(RR) CMD check ROI.plugin.nloptr*.tar.gz

nloptr.install: nloptr.build 
	$(RR) CMD INSTALL ROI.plugin.nloptr*.tar.gz

## optimx
## ======
optimx.build:
	$(RR) CMD build ROI.plugin.optimx

optimx.check: optimx.build
	$(RR) CMD check ROI.plugin.optimx*.tar.gz

optimx.install: optimx.build 
	$(RR) CMD INSTALL ROI.plugin.optimx*.tar.gz

## quadprog
## ========
quadprog.build:
	$(RR) CMD build ROI.plugin.quadprog

quadprog.check: quadprog.build 
	$(RR) CMD check ROI.plugin.quadprog*.tar.gz

quadprog.install: quadprog.build
	$(RR) CMD INSTALL ROI.plugin.quadprog*.tar.gz

## scs
## ===
scs.build:
	$(RR) CMD build ROI.plugin.scs

scs.check: scs.build
	$(RR) CMD check ROI.plugin.scs*.tar.gz

scs.install: scs.build
	$(RR) CMD INSTALL ROI.plugin.scs*.tar.gz

## symphony
## ========
symphony.build:
	$(RR) CMD build ROI.plugin.symphony

symphony.check: symphony.build
	$(RR) CMD check ROI.plugin.symphony*.tar.gz

symphony.install: symphony.build 
	$(RR) CMD INSTALL ROI.plugin.symphony*.tar.gz

check.plugins: clean.Rchecks ecos.check glpk.check ipop.check nloptr.check quadprog.check scs.check symphony.check


## Testing
## =======
test.cplex:
	$(RR) CMD check ROI.plugin.cplex

test.ecos:
	$(RR) CMD check ROI.plugin.ecos

test.glpk: glpk.install
	$(RR) -e "setwd('ROI.plugin.glpk/tests'); source('run-all.R')"

test.ipop: ipop.install
	$(RR) -e "setwd('ROI.plugin.ipop/tests'); source('run-all.R')"

test.mosek:
	$(RR) CMD check ROI.plugin.mosek

test.nloptr:
	$(RR) CMD check ROI.plugin.nloptr

test.quadprog: quadprog.install
	$(RR) -e "setwd('ROI.plugin.quadprog/tests'); source('run-all.R')"

test.scs:
	$(RR) CMD check ROI.plugin.scs

test.symphony: symphony.install
	$(RR) -e "setwd('ROI.plugin.symphony/tests'); source('run-all.R')"

## not all but all available
test.all: test.ecos test.glpk test.ipop test.nloptr test.quadprog test.scs test.symphony

