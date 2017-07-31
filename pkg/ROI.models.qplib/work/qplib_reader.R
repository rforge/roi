
library(slam)

strip <- function(x) gsub("(^\\s+|\\s+$)", "", x)

qplib_reader <- function(file) {
    x <- readLines(file)
    
    ## extract comments    
    comments <- character(length(x))
    i <- grep("#", x, fixed = TRUE)
    comments[i] <- gsub(".*#\\s*", "", x[i])
    x[i] <- gsub("\\s*#.*", "", x[i])

    b <- nchar(x) > 0
    x <- strip(x[b])
    comments <- comments[b]

    names(x)[1:5] <- c("name", "type", "sense", "n", "m")
    op_nrow <- as.integer(x["m"])
    op_ncol <- as.integer(x["n"])

    type_objective <- substr(x["type"], 1, 1)   ## L (linear) or Q ()
    type_variables <- substr(x["type"], 2, 2)   ## B, I, M, G (binary, integer, mixed, general)
    type_constraints <- substr(x["type"], 3, 3) ## 

    if ( type_objective == "L" ) {
        names(x)[6:7] <- c("bd", "nb") ## default value of b, number of non-default values
        iter <- 7 + as.integer(x["nb"]) 
        b <- rep.int(as.double(x["bd"]), x["m"])
        tmp <- strsplit(x[7 + seq_len(x["nb"])], "\\s+")
        bi <- as.integer(lapply(tmp, "[[", 1))
        bv <- as.double(lapply(tmp, "[[", 2))
        b[bi] <- bv
    } else {

    }

    ## q0
    q0 <- as.double(x[(iter <- iter + 1L)])

    ## Q_i
    nQi <- as.integer(x[(iter <- iter + 1L)])
    tmp <- strsplit(x[iter + seq_len(nQi)], "\\s+")
    iter <- iter + nQi
    Q <- data.frame(i = as.integer(lapply(tmp, "[[", 1)),
                    h = as.integer(lapply(tmp, "[[", 2)),
                    k = as.integer(lapply(tmp, "[[", 3)),
                    v = as.double(lapply(tmp, "[[", 4)))
    Qi <- unique(Q$i)
    to_slam <- function(i, Q, nvariables) {
        x <- Q[(i == Q$i),]
        simple_triplet_matrix(x$h, x$k, x$v, nrow = nvariables, ncol = nvariables)
    }
    QL <- lapply(Qi, to_slam, Q = Q, nvariables = op_ncol)

    ## a_i
    nai <- as.integer(x[(iter <- iter + 1L)])
    tmp <- strsplit(x[iter + seq_len(nai)], "\\s+")
    iter <- iter + nai
    A <- simple_triplet_matrix(i = as.integer(lapply(tmp, "[[", 1)),
                               j = as.integer(lapply(tmp, "[[", 2)),
                               v = as.double(lapply(tmp, "[[", 3)),
                               nrow = op_nrow, ncol = op_ncol)

    ## c_\infty
    infty <- as.double(strip(x[(iter <- iter + 1L)]))
    ## default value for entries in cl
    default_cl <- as.double(strip(x[(iter <- iter + 1L)]))

    ## 

    x[iter + 1L]
    x[iter + 2L]

    head(a)
    tmp

    iter <- iter - 1L

    op_nrow
    op_ncol

}

dir()
x <- qplib_reader("QPLIB_3562.qplib")

file <- "QPLIB_3562.qplib"

Sys.setenv(ROI_LOAD_PLUGINS = FALSE)
library(ROI)
str(Q_constraint(diag(2), 1:2, "<=", 0))

