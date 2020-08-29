
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
db$solvers_in_category <- list()

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
    if ( is.null(x <- neos_get_db(method)) ) {
        x <- xmlrpc(neos_url(), method)
        neos_set_db(method, x)
    }
    x
}

neos_help <- function() writeLines(neos_lazy_simple_call("help"))
neos_emailHelp <- function() writeLines(neos_lazy_simple_call("emailHelp"))
neos_welcome <- function() writeLines(neos_lazy_simple_call("welcome"))
neos_version <- function() neos_lazy_simple_call("version")
neos_ping <- function() neos_simple_call("ping")
neos_ls_categories <- function() neos_lazy_simple_call("listCategories")

neos_ls_solvers <- function(category = NULL) {
    if ( is.null(category) ) {
        neos_lazy_simple_call("listAllSolvers")
    } else {
        db <- getNamespace("ROI.plugin.neos")$db
        if ( !category %in% names(db$solvers_in_category) ) {
            params <- list(category = category)
            x <- xmlrpc(neos_url(), "listSolversInCategory", params = params)
            db$solvers_in_category[[category]] <- x
        }
        db$solvers_in_category[[category]]
    }
}

neos_queue <- function(verbose = TRUE) {
    queue <- neos_simple_call("printQueue")
    if (verbose) {
        writeLines(queue)
        invisible(queue)
    } else {
        queue
    }
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

set_template_parameters <- function(xml_template, params) {
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

neos_submit_job <- function(x, xmlstring, user = "", password = "") {
    if ( (nchar(user) == 0L) | (nchar(password) == 0L) ) {
        response <- xmlrpc(neos_url(), "submitJob", params = list(xmlstring = xmlstring))
    } else {
        params <- list(xmlstring = xmlstring, user = user, password = password)
        response <- xmlrpc(neos_url(), "authenticatedSubmitJob", params = params)
    }
    neos_job(response[[1]], response[[2]], x)
}

neos_job <- function(job_number, password, x) {
    job <- list()
    class(job) <- "neos_job"
    job$job_number <- job_number
    job$password <- password
    job$status <- function() {
        self <- parent.env(environment())$job
        xmlrpc(neos_url(), "getJobStatus", 
               params = list(jobNumber = self$job_number, password = self$password))
    }
    job$completion_code <- function() {
        self <- parent.env(environment())$job
        params <- list(jobNumber = self$job_number, password = self$password)
        xmlrpc(neos_url(), "getCompletionCode", params = params)
    }
    job$info <- function() {
        self <- parent.env(environment())$job
        params <- list(jobNumber = self$job_number, password = self$password)
        xmlrpc(neos_url(), "getJobInfo", params = params)
    }
    job$kill_job <- function(killmsg="") {
        self <- parent.env(environment())$job
        params <- list(jobNumber = self$job_number, password = self$password, killmsg = killmsg)
        xmlrpc(neos_url(), "killJob", params = params)
    }
    job$final_results <- function(wait = FALSE) {
        self <- parent.env(environment())$job
        params <- list(jobNumber = self$job_number, password = self$password)
        neos_method <- if (wait) "getFinalResults" else "getFinalResultsNonBlocking"
        response <- xmlrpc(neos_url(), neos_method, params = params)
        rawToChar(response)
    }
    job$output_file <- function(file_name) {
        self <- parent.env(environment())$job
        params <- list(jobNumber = self$job_number, password = self$password, fileName = file_name)
        response <- xmlrpc(neos_url(), "getOutputFile", params = params)
        response
    }
    job$objective_function <- objective(x)
    job$solution <- function(wait = FALSE) {
        self <- parent.env(environment())$job
        if ( isTRUE(self$status() != "Done") & !wait ) {
            message("job not finished yet")
            return(NULL)
        }
        neos_message <- self$final_results(wait)
        if ( !neos_message_indicates_success(neos_message) ) {
            stop(neos_message)
        }
        output_file <- sprintf("%s-%s-solver-output.zip", self$job_number, self$password)
        response <- self$output_file(output_file)
        tmpfi <- tempfile(fileext="-neos.zip")
        on.exit(unlink(tmpfi))
        writeBin(response, tmpfi)
        results_con <- unz(tmpfi, "results.txt")
        on.exit(close(results_con))
        neos_result_txt <- readLines(results_con)
        neos_results <- extract_results(neos_result_txt)
        objval <- tryCatch(unname(self$objective_function(neos_results$solution)), error = function(e) NA_real_)
        status <- generate_status_code(neos_results$solver_status, neos_results$model_status)
        neos_results$message <- neos_message
    
        ROI_plugin_canonicalize_solution(solution = neos_results$solution, 
                                         optimum = objval, status = status,
                                         solver = "neos", message = neos_results)
    }
    job
}

neos_read_result_file <- function(file, neos_message, objective_function) {
    neos_results <- extract_results(readLines(file))
    objval <- tryCatch(unname(objective_function(neos_results$solution)), 
                       error = function(e) NA_real_)
    status <- generate_status_code(neos_results$solver_status, neos_results$model_status)
    neos_results$message <- neos_message
    
    ROI_plugin_canonicalize_solution(solution = neos_results$solution, 
                                     optimum = objval, status = status,
                                     solver = "neos", message = neos_results)
}

# NOT used currently since with caching the data in the package environment
# I get a cache for each R session.
neos_client <- function() {
    neos <- new.env(parent = emptyenv())
    class(neos) <- "neos_client"
    neos$help <- neos_help
    neos$emailHelp <- neos_emailHelp
    neos$welcome <- neos_welcome
    neos$version <- neos_version
    neos$ping <- neos_ping
    neos$printQueue <- neos_queue
    neos$listAllSolvers <- function() {
        neos_ls_solvers()
    }
    neos$listSolversInCategory <- function(category) {
        neos_ls_solvers(category)
    }
    neos$listCategories <- neos_ls_categories
    neos$getSolverTemplate <- neos_solver_template
    neos    
}



