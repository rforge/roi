q("no")
R

if (!require(globalOptTests)) install.packages("globalOptTests"); library(globalOptTests)

tests <- c("Ackleys", "AluffiPentini", "BeckerLago", "Bohachevsky1", "Bohachevsky2", 
           "Branin", "Camel3", "Camel6", "CosMix2", "CosMix4", "DekkersAarts", 
           "Easom", "EMichalewicz", "Expo", "GoldPrice", "Griewank", "Gulf", "Hartman3", 
           "Hartman6", "Hosaki", "Kowalik", "LM1", "LM2n10", "LM2n5", "McCormic",
           "MeyerRoth", "MieleCantrell", "Modlangerman", "ModRosenbrock", "MultiGauss", 
           "Neumaier2", "Neumaier3", "Paviani", "Periodic", "PowellQ", "PriceTransistor", 
           "Rastrigin", "Rosenbrock", "Salomon", "Schaffer1", "Schaffer2", "Schubert", 
           "Schwefel", "Shekel10", "Shekel5", "Shekel7", "Shekelfox5", "Wood", "Zeldasine10",
           "Zeldasine20")


test_name <- tests[1]
goTest(1:10, test_name)

library(ROI)

build_ROI_object <- function(test_name) {
    n <- getProblemDimen(test_name)
    db <- getDefaultBounds(test_name)
    vb <- V_bound(li=seq_along(db$lower), ui=seq_along(db$upper), 
                  lb=db$lower, ub=db$upper)
    fobj <- F_objective(F=function(x) goTest(x, test_name), n=n)
    OP(objective=fobj, bounds=vb)
}


op <- build_ROI_object(test_name)
op

system.time(Global_Optimization_Tests <- lapply(tests, build_ROI_object))

