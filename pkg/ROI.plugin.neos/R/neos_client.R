
##
## The idea is to create less traffic for neos we save request results
## in an in-memory database.
##
db <- new.env(parent = emptyenv())
db$help <- NULL
db$emailHelp <- NULL
db$welcome <- NULL
db$version <- NULL
db$listAllSolvers <- NULL
db$listCategories <- NULL

neos_get_db <- function(key) {
    db <- getNamespace("ROI.plugin.neos")$db
    if ( missing(key) ) return(db)
    db[[key]]
}

neos_set_db <- function(key, value) {
    db <- getNamespace("ROI.plugin.neos")$db
    db[[key]] <- value
    invisible(NULL)
}

neos_url <- function() "https://www.neos-server.org"

neos_simple_call <- function(method) xmlrpc(neos_url(), method)

neos_lazy_simple_call <- function(method) {
    if ( is.null(neos_get_db(method)) ) {
        neos_set_db(method, xmlrpc(neos_url(), method))
    } else {
    }
    neos_get_db(method)
}

neos_help <- function() writeLines(neos_lazy_simple_call("help"))
neos_emailHelp <- function() writeLines(neos_lazy_simple_call("emailHelp"))
neos_welcome <- function() writeLines(neos_lazy_simple_call("welcome"))
neos_version <- function() neos_lazy_simple_call("version")
neos_ping <- function() neos_simple_call("ping")
neos_ls_solvers <- function() neos_lazy_simple_call("listAllSolvers")
neos_ls_categories <- function() neos_lazy_simple_call("listCategories")

neos_queue <- function(as_character = FALSE) {
    queue <- neos_simple_call("printQueue")
    if (as_character) queue else writeLines(queue)
}

neos_solver_template <- function(category, solver_name, input_method) {
    key <- paste(clean(c(category, solver_name, input_method)), collapse = ":")
    if ( is.null(neos_get_db(key)) ) {
        params <- list(category = category, solvername = solver_name, 
                       inputMethod = input_method)
        template <- xmlrpc(neos_url(), "getSolverTemplate", params = params)
        neos_set_db(key, read_xml(template))
    }
    xml_copy(neos_get_db(key))
}

set_templanete_parameters <- function(xml_template, params) {
    template_params <- xml_children(xml_template)
    template_params_names <- as.character(lapply(template_params, xml_name))
    for (i in seq_along(params)) {
        key <- names(params)[i]
        k <- which(key == template_params_names)
        if ( length(k) ) {
            xml_replace(template_params[[k]], new_xml_node(key, params[[key]]))
        } else {
            xml_add_sibling(template_params[[length(template_params)]],
                new_xml_node(key, params[[key]]))
        }
        
    }
    as.character(xml_template)
}

neos_submit_job <- function(xmlstring, user = NULL, password = NULL) {
    if ( is.null(user) | is.null(password) ) {
        response <- xmlrpc(neos_url(), "submitJob", params = list(xmlstring = xmlstring))
    } else {
        params <- list(xmlstring = xmlstring, user = user, password = password)
        response <- xmlrpc(neos_url(), "submitJob", params = params)
    }
    neos_job(response[[1]], response[[2]])
}

neos_job <- function(job_number, password) {
    job <- list()
    class(job) <- "neos_job"
    job$job_number <- job_number
    job$password <- password
    job$status <- function() {
        self <- parent.env(environment())$job
        xmlrpc(neos_url(), "getJobStatus", 
               params = list(jobnumber = self$job_number, password = self$password))
    }
    job$info <- function() {
        self <- parent.env(environment())$job
        xmlrpc(neos_url(), "getJobInfo", 
               params = list(jobnumber = self$job_number, password = self$password))
    }
    job$final_results <- function() {
        self <- parent.env(environment())$job
        response <- xmlrpc(neos_url(), "getFinalResults",
                           params = list(jobnumber = self$job_number, password = self$password))
        rawToChar(response)
    }
    job$output_file <- function(file_name) {
        self <- parent.env(environment())$job
        params <- list(jobNumber = self$job_number, password = self$password, 
                       fileName = file_name)
        response <- xmlrpc(neos_url(), "getOutputFile", params = params)
        rawToChar(response)
    }
    job
}

neos_get_results <- function(job) {
    url <- "https://www.neos-server.org"
    fn <- 'results.txt'
    params <- list(jobNumber = job@jobnumber, password = job@password, fileName = fn)
    resp <- xmlrpc(url, "getOutputFile", params = params)
    rawToChar(resp)
}

neos_client <- function() {
    neos <- new.env(parent = emptyenv())
    class(neos) <- "neos_client"
    neos$url <- "https://www.neos-server.org"
    neos$simple_call <- function(method) {
        self <- parent.env(environment())$neos
        xmlrpc(self$url, method)
    }
    neos$help <- function() {
        self <- parent.env(environment())$neos
        writeLines(self$simple_call("help"))
    }
    neos$emailHelp <- function() {
        self <- parent.env(environment())$neos
        writeLines(self$simple_call("emailHelp"))
    }
    neos$welcome <- function() {
        self <- parent.env(environment())$neos
        writeLines(self$simple_call("welcome"))
    }
    neos$version <- function() {
        self <- parent.env(environment())$neos
        self$simple_call("version")
    }
    neos$ping <- function() {
        self <- parent.env(environment())$neos
        self$simple_call("ping")
    }
    neos$printQueue <- function() {
        self <- parent.env(environment())$neos
        writeLines(self$simple_call("printQueue"))
    }
    neos$listAllSolvers <- function() {
        self <- parent.env(environment())$neos
        self$simple_call("listAllSolvers")
    }
    neos$listCategories <- function() {
        self <- parent.env(environment())$neos
        self$simple_call("listCategories")
    }
    neos$getSolverTemplate <- function(category, solver_name, input_method) {
        self <- parent.env(environment())$neos
        params <- list(category = category, solvername = solver_name, 
                       inputMethod = input_method)
        xmlrpc(self$url, "getSolverTemplate", params = params)
    }

    neos    
}
