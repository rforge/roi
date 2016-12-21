library(ROI)
library(slam)

simple_triplet_matrix(3, 1, 1)

## ---------------------------
## Objective
## ---------------------------
test.L_objective <- function() {
	L_objective(1:3)
	L_objective(1:3, LETTERS[1:3])
	L_objective(t(as.simple_triplet_matrix(1:5)))
	L_objective(t(as.simple_triplet_matrix(1:5)), LETTERS[1:3])
}

test.Q_objective <- function() {
	Q_objective(diag(2))
	Q_objective(diag(2), 1:2)
	Q_objective(diag(2), 1:3)
}

test.F_objective <- function() {
	F_objective(sum, 3)
}

## ---------------------------
## Constraints
## ---------------------------
test.NO_constraint <- function() {
	NO_constraint(3)
}

## ---------------------------
## Types
## ---------------------------


## ---------------------------
## Bounds
## ---------------------------

## ---------------------------
## R-Methods
## ---------------------------
## rbind

## c

## as

## is

## G

## J

## variable.names


## test 
file = Sys.getenv("ROI_TEST_LOG_FILE")
ROI_TEST_ERRORS <- 0L
rt <- function(expr, silent = FALSE) {
	err <- try(expr, silent = silent)
	if ( inherits(err, "try-error") ) 
		ROI_TEST_ERRORS <<- ROI_TEST_ERRORS + 1L
	err
}

cat("# Constructors\n", file=file)
cat("## Objective\n", file=file)
cat("### L_objective\n", file=file)
rt( test.L_objective() )

cat("### Q_objective\n", file=file)
rt( test.Q_objective() )

cat("### F_objective\n", file=file)
rt( test.F_objective() )

cat("## Constraints\n", file=file)

cat("## Types\n", file=file)

cat("## Bounds\n", file=file)



str(OP)