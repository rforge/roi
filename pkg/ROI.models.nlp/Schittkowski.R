
test_start <- function(x) isTRUE(abs(x$objective(x$start) - x$f_start) < 1e-5)
test_solution <- function(x) isTRUE(abs(x$objective(x$solution$solution) - x$solution$objval) < 1e-5)
test_problem <- function(x) isTRUE(test_start(x) & test_solution(x))
test_problems <- function(x) sapply(x, test_problem)

fstart <- function(x) x$objective(x$start)
fobj <- function(x) x$objective(x$solution$solution)

X[[2]]$objective(X[[2]]$solution$sol)
X <- list()

X[[1]] <- list(
    problem = 1L,
    classification = "PBR-T1-1",
    source = "Betts [8]",
    number_of_variables = 2,
    number_of_constraints = list(inequality=0, equality=0, variable_bounds=0),
    objective = function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2,
    constraints = NULL,
    bounds = list(li=2, ui=NULL, lb=-1.5, ub=NULL),
    start = c(-2, 1),
    f_start = 909,
    solution = list(solution=c(1, 1), objval = 0)
)

X[[2]] <- list(
    problem = 2L,
    classification = "PBR-T1-2",
    source = "Betts [8]",
    number_of_variables = 2,
    number_of_constraints = list(inequality=0, equality=0, variable_bounds=1),
    objective = function(x) 100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2,
    constraints = NULL,
    bounds = list(li=2, ui=NULL, lb=1.5, ub=NULL),
    start = c(-2, 1),
    f_start = 909,
    solution = list(solution=c(2*sqrt(598/1200) * cos(1/3*acos(1/(400*(sqrt(598/1200)^3)))), 1.5), 
                    objval = 0.0504261879)
)

X[[3]] <- list(
    problem = 3L,
    classification = "QBR-T1-1",
    source = "Schuldt [56]",
    number_of_variables = 2,
    number_of_constraints = list(inequality=0, equality=0, variable_bounds=1),
    objective = function(x) x[2] + 10^(-5) * (x[2] - x[1])^2,
    constraints = NULL,
    bounds = list(li=2, ui=NULL, lb=0, ub=NULL),
    start = c(10, 1),
    f_start = 1.00081,
    solution = list(solution=c(0, 0), 
                    objval = 0)
)

X[[4]] <- list(
    problem = 4L,
    classification = "PBR-T1-3",
    source = "Asaadi [1]",
    number_of_variables = 2,
    number_of_constraints = list(inequality=0, equality=0, variable_bounds=2),
    objective = function(x) 1/3 * (x[1] + 1)^3 + x[2],
    constraints = NULL,
    bounds = list(li=c(1, 2), ui=NULL, lb=c(0, 1), ub=NULL),
    start = c(1.125, 0.125),
    f_start = 3.323568,
    solution = list(solution=c(1, 0), 
                    objval = 8/3)
)

X[[5]] <- list(
    problem = 5L,
    classification = "GBR-T1-1",
    source = "McCormick [41]",
    number_of_variables = 2,
    number_of_constraints = list(inequality=0, equality=0, variable_bounds=4),
    objective = function(x) (sin(x[1] + x[2]) + (x[1] - x[2])^2 - 1.5 * x[1] + 2.5 * x[2] + 1),
    constraints = NULL,
    bounds = list(li=c(1, 2), ui=c(1, 2), lb=c(-1.5, -3), ub=c(4, 3)),
    start = c(0, 0),
    f_start = 1,
    solution = list(solution=c(-pi/3 + 1/2, -pi/3 - 1/2),
                    objval = -1/2*sqrt(3) - pi/3)
)


test_problems(X)

fobj(X[[4]])
fstart(X[[4]])
8/3
