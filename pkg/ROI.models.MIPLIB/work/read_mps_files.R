require("Rglpk")
require("ROI")

path <- "./miplib2010-benchmark"

files <- dir(path)
nm <- make.names(gsub(".mps$", "", files))

as.OP.MILP <- function(x){
    obj <- t(x$objective)
    rownames(obj) <- attr(x, "objective_name")
    colnames(obj) <- attr(x, "objective_vars_names")
    mat <- x$constraints[[1]]
    rownames(mat) <- attr(x, "constraint_names")
    OP( objective = L_objective( obj ),
        constraints = L_constraint(mat, x$constraints[[2]], x$constraints[[3]]),
        bounds = V_bound( li = x$bounds$lower$ind,
                          ui = x$bounds$upper$ind,
                          lb = x$bounds$lower$val,
                          ub = x$bounds$upper$val,
                          nobj = attr(x,"n_objective_vars") ),
        types = x$types,
        maximum = x$maximum )
}

for( i in seq_along(files) ){
    writeLines( sprintf("Reading file %s ...", files[i]) )
    prob <- tryCatch( as.OP(Rglpk_read_file(file.path(path, files[i]), type = "MPS_free")), error = identity )
    if( inherits(prob, "error") )
        writeLines( sprintf("Reading/converting instance %s failed!", nm[i]) )
    else{
        assign(nm[i], prob)
        save( list = nm[i], file = file.path("..", "data", paste(nm[i], "rda", sep = ".")), compress = "bzip2" )
        writeLines("Done.")
    }
}

