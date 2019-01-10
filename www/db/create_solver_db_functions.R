##
## NOTE: I cloud also create my own cran server for ROI packages
##       the problem is kind of that R-Forge is not really reliable.

as.df <- function(x) as.data.frame(x, stringsAsFactors = FALSE)

install_dependencies <- function() {
    lib.loc <- head(.libPaths(), 1)
    pkgs <- c("remotes", "lattice", "Matrix", "slam", "ROI", "Rsymphony", "Rglpk", "DEoptim", "DEoptimR")
    for (pkg in pkgs) {
        if ( !length(dir(lib.loc, pattern = pkg)) ) {
            install.packages(pkg, lib = lib.loc)
        }
    }
}

install_package <- function(pkg, repos, lib.loc, r_version, type = "source") {
    cmd <- sprintf('%s --slave -e "install.packages(%s, lib = %s, repos = %s, type = %s)"', 
                  r_version, shQuote(pkg), shQuote(lib.loc), shQuote(repos), shQuote(type))
    out <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    status_error <- length(grep("error", out, ignore.case = TRUE))
    status_dir <- !length(dir(lib.loc, pattern = pkg))
    status_error + status_dir
}

install_package_rforge <- function(pkg, url, repos, lib.loc, r_version, type = "source") {
    status <- install_package(pkg, repos, lib.loc, r_version, type)
    if (!status) {
        cmd <- sprintf('%s --slave -e "remotes:::install_svn(%s, lib = %s)"', 
                       r_version, shQuote(url), shQuote(lib.loc))
        out <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
        status_error <- length(grep("error", out, ignore.case = TRUE))
        status_dir <- !length(dir(lib.loc, pattern = pkg))        
        status_error + status_dir
    }
    status
}



install_package_github <- function(repo, lib.loc, r_version) {
    ## for now assume that the repo name and the package name are
    ## the same
    pkg <- gsub(".*/", "", repo)
    cmd <- sprintf('%s --slave -e "remotes:::install_github(%s, lib = %s)"', 
                   r_version, shQuote(repo), shQuote(lib.loc))
    out <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    status_error <- length(grep("error", out, ignore.case = TRUE))
    status_dir <- !length(dir(lib.loc, pattern = pkg))
    status_error + status_dir
}

get_signature_entries <- function() {
    x <- ROI_plugin_make_signature( objective = "L",
                                    constraints = c("X", "L"),
                                    types = c("C", "I", "B", "CI", "CB", "IB", "CIB"),
                                    bounds = c("X", "V"),
                                    cones = c("X"),
                                    maximum = c(TRUE, FALSE) )
    colnames(x)
}

parse_description <- function(pkg, lib.loc, cnames) {
    a <- as.data.frame(setNames(rep(list(as.character(NA)), length(cnames)), cnames), 
                       stringsAsFactors = FALSE)

    suppressWarnings(pkg_descr <- packageDescription(pkg, lib.loc = lib.loc))
    b <- as.data.frame(unclass(pkg_descr), stringsAsFactors = FALSE)
    stopifnot(nrow(b) == 1L)
    j <- intersect(colnames(a), colnames(b))
    a[1, j] <- b[1, j]
    a[1, "Repository"] <- "http://R-Forge.R-project.org"
    a
}

extract_signature <- function(plugin) {
    signature_entries <- get_signature_entries()
    entries <- ROI:::solver_db$get_entries()
    .extract_signature <- function(x) as.df(x[c("plugin", "solver", signature_entries)])
    solver_signatures <- do.call(rbind, lapply(entries, .extract_signature))    
    i <- which(solver_signatures$plugin == plugin)
    solver_signatures <- solver_signatures[i, -1L]    
    rownames(solver_signatures) <- NULL
    solver_signatures
}

register_roi_plugin <- function(pkgname) {
    getNamespace(pkgname)
}

get_roi_solver_cran <- function(repos) {
    plugins_cran <- available.packages(repos = repos)
    pattern <- "^ROI.plugin"
    plugins_cran <- plugins_cran[grep(pattern, plugins_cran[,'Package']),]
    plugins_cran
}

get_roi_solver_rforge <- function() {
    ## we list all the plugins in the r-forge folder and 
    ## additional user defined (when it becomes necessary)
    svn_url <- "svn://svn.r-forge.r-project.org/svnroot/roi/pkg"
    x <- system(sprintf("svn list %s", svn_url), intern = TRUE)
    solver_names <- gsub("/", "", grep("^ROI.plugin", x, value = TRUE), fixed = TRUE)
    data.frame(name = solver_names, 
               svn = file.path(svn_url, solver_names),
               stringsAsFactors = FALSE)
}

create_solver_db_cran <- function(r_version, lib.loc, repos = "https://cran.r-project.org") {
    
    ## .libPaths(c(lib.loc, .libPaths()))  
    install_dependencies()
    Sys.setenv(ROI_LOAD_PLUGINS = FALSE)
    library(ROI)
    
    ## CRAN
    roi_solver_cran <- as.df(get_roi_solver_cran(repos))
    roi_solver_cran$Signature <- list(NA)
    for (i in seq_len(nrow(roi_solver_cran))) {
        pkg <- roi_solver_cran[i, 'Package']
        status <- install_package(pkg, repos, lib.loc, r_version)
        if (!status) {
            suppressMessages( do.call(require, list(pkg)) )
            plugin <- gsub("^ROI\\.plugin\\.", "", pkg)
            roi_solver_cran[[i, 'Signature']] <- extract_signature(plugin)
            ## try(remove.packages(pkg, lib.loc), silent = TRUE)
        } else {
            cat("NOTE: package '", pkg, "' could not be installed from '", repos, "'!\n", sep = "")
        }
    }
    roi_solver_cran
}

create_solver_db_rforge <- function(r_version, lib.loc, repos = "http://R-Forge.R-project.org") {
    
    install_dependencies()
    Sys.setenv(ROI_LOAD_PLUGINS = FALSE)
    library(ROI)

    cnames <- c(colnames(available.packages(repos=repos)), "Signature")
  
    ## R-Forge
    roi_solver_rforge_info <- get_roi_solver_rforge()
    roi_solver_rforge_names <- roi_solver_rforge_info$name
    roi_solver_rforge <- vector("list", length(roi_solver_rforge_names))
    names(roi_solver_rforge) <- roi_solver_rforge_names
    for (i in seq_along(roi_solver_rforge)) {
        pkg <- roi_solver_rforge_names[i]
        url <- roi_solver_rforge_info$svn[i]
        status <- install_package_rforge(pkg, url, repos, lib.loc, r_version)
        if (!status) {
            roi_solver_rforge[[i]] <- parse_description(pkg, lib.loc, cnames)
            suppressMessages( do.call(require, list(pkg)) )
            plugin <- gsub("^ROI\\.plugin\\.", "", pkg)
            roi_solver_rforge[[i]]$Signature <- list(extract_signature(plugin))
        } else {
            cat("NOTE: package '", pkg, "' could not be installed from '", repos, "'!\n", sep = "")
        }
    }
    roi_solver_rforge <- do.call(rbind, roi_solver_rforge)
    roi_solver_rforge
}

create_solver_db_github <- function(r_version, lib.loc, repos, cran) {
    
    install_dependencies()
    Sys.setenv(ROI_LOAD_PLUGINS = FALSE)
    library(ROI)
    library(RCurl)

    cnames <- c(colnames(available.packages(repos=cran)), "Signature")
  
    ## GITHUB
    roi_solver_github <- vector("list", length(repos))
    
    for (i in seq_along(repos)) {
        repo <- repos[i]
        pkg <- gsub(".*/", "", repo)
        status <- install_package_github(repo, lib.loc, r_version)
        if (!status) {
            roi_solver_github[[i]] <- parse_description(pkg, lib.loc, cnames)
            suppressMessages( do.call(require, list(pkg)) )
            plugin <- gsub("^ROI\\.plugin\\.", "", pkg)
            roi_solver_github[[i]]$Signature <- list(extract_signature(plugin))
            git_repo <- file.path("https://github.com", repo)
            roi_solver_github[[i]]$Repository <- sub("/ROI.plugin.*", "", git_repo)
        } else {
            cat("NOTE: package '", pkg, "' could not be installed from '", repo, "'!\n", sep = "")
        }
    }
    roi_solver_github <- do.call(rbind, roi_solver_github)
    ##roi_solver_github$Repository <- "https://github.com"
    roi_solver_github
}

