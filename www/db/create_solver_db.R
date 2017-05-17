
setwd("/home/florian/work/Optimization/ROI/ROI_R-Forge/www/db")

CRAN <- "https://cran.r-project.org/"
RFORGE <- "http://r-forge.r-project.org/"

plugins_cran <- available.packages(repos = CRAN, cont)
plugins_rforge <- available.packages(repos = RFORGE)

head(plugins_cran, 2)
head(plugins_rforge, 2)

pattern <- "^ROI.plugin"
plugins_cran <- plugins_cran[grep(pattern, plugins_cran[,'Package']),]
plugins_rforge <- plugins_rforge[grep(pattern, plugins_rforge[,'Package']),]

setdiff(colnames(plugins_cran), colnames(plugins_rforge))
setdiff(colnames(plugins_rforge), colnames(plugins_cran))

all(colnames(plugins_cran) == colnames(plugins_rforge))

local.roi.plugins <- function() {
    lib.loc <- .libPaths()
    pkgs <- grep( "ROI.plugin", unlist(lapply(lib.loc, dir)), value = TRUE )
    pkgs
}

get_rforge_plugins <- function() {
    x <- system("svn list svn://svn.r-forge.r-project.org/svnroot/roi/pkg", intern = TRUE)
    gsub("/", "", grep("^ROI.plugin", x, value = TRUE), fixed = TRUE)
}

## lapply(.libPaths(), function(lib) try(remove.packages("ROI.plugin.solnp", lib)))

plugins_rforge_svn <- get_rforge_plugins()
plugins_rforge_not_compiled <- intersect(local.roi.plugins(), setdiff(plugins_rforge_svn, plugins_rforge))

parse_description <- function(pkg) {
    a <- as.data.frame(setNames(rep(list(as.character(NA)), length(cnames)), cnames), 
                       stringsAsFactors = FALSE)
    lib_locs <- .libPaths()
    for (lib_loc in lib_locs) {
        suppressWarnings(pkg_descr <- packageDescription(pkg, lib.loc = lib_loc))
        if ( !is.na(pkg_descr[1L]) )
            break
    }
    b <- as.data.frame(unclass(pkg_descr), stringsAsFactors = FALSE)
    stopifnot(nrow(b) == 1L)
    j <- intersect(colnames(a), colnames(b))
    a[1, j] <- b[1, j]
    a[1, "Repository"] <- "http://R-Forge.R-project.org"
    a
}

parse_descriptions <- function(pkgs) {
    cnames <- colnames(available.packages())
    do.call(rbind, lapply(pkgs, parse_description))        
}

plugins_rforge2 <- parse_descriptions(plugins_rforge_not_compiled)

as.df <- function(x) as.data.frame(x, stringsAsFactors = FALSE)

plugins <- rbind(as.df(plugins_cran), as.df(plugins_rforge), plugins_rforge2)

library(ROI)

pkgs <- gsub("ROI.plugin.", "", plugins$Package, fixed = TRUE)

pkgs

sign <- make_MILP_signatures()
class(sign)
signature_entries <- colnames(sign)

entries <- ROI:::solver_db$get_entries()

extract_signature <- function(x) {
    as.df(x[c("solver", signature_entries)])
}

solver_signatures <- do.call(rbind, lapply(entries, extract_signature))
rownames(solver_signatures) <- NULL

fun <- function(solver) solver_signatures[solver_signatures$solver == solver, -1L]
usolver <- unique(solver_signatures$solver)
solver_signatures <- setNames(lapply(usolver, fun), usolver)

str(solver_signatures)

head(plugins)

i <- 1L

plugins$Signature <- list(NA)
for ( i in seq_len(nrow(plugins)) ) {
    solver_name <- gsub("ROI.plugin.", "", plugins$Package[i], fixed = TRUE)

    if ( solver_name %in% names(solver_signatures) ) {
        solver_signature <- solver_signatures[[solver_name]]
        rownames(solver_signature) <- NULL
        plugins$Signature[[i]] <- solver_signature
    } else {
        cat(solver_name, "not available!\n")
    }
}

head(plugins$Signature)
plugins$Signature[[1L]]
plugins$Signature[[2L]]
head(plugins$Signature[[3L]])

getwd()

saveRDS(plugins, "SOLVERS.rds")
