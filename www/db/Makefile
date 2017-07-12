RR=/home/florian/bin/R_dev/bin/R

all: solver_db_cran solver_db_rforge solver_db_github solver_db_merge

solver_db_merge:
	$(RR) --vanilla < create_solver_db_merge.R

solver_db_cran:
	$(RR) --vanilla < create_solver_db_cran.R 

solver_db_rforge:
	$(RR) --vanilla < create_solver_db_rforge.R 

solver_db_github:
	$(RR) --vanilla < create_solver_db_github.R 

