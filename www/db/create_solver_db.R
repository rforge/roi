
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

saveRDS(plugins, "SOLVERS.rds")
