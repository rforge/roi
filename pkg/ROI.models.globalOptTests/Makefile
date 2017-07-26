Rdevel=/home/florian/bin/R_dev/bin/R

docs:
	$(Rdevel) -e "Sys.setenv(ROI_LOAD_PLUGINS = FALSE); library(roxygen2); roxygenise(roclets=c('rd'))"
