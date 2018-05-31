
## NOTE: Gurobi
##       The Gurobi license for NEOS does not permit connections via XML-RPC. 
##       If you look at a solver page for Gurobi, for example, 
##       https://neos-server.org/neos/solvers/milp:Gurobi/AMPL.html, you will 
##       see that the XML-RPC interface is disabled. The license only allows 
##       submissions via the web interface. If you were able to submit jobs via 
##       XML-RPC previously, that was a mistake on our part introduced during an 
##       update to the NEOS codebase.

write_gams <- function(x, file, ...) {
    model <- to_gams(x)
    writeLines(model, con = file)
}

set_default_control_values <- function(x) {
    default_cntrl <- list(interface = "", id = 0, user = "rneos")
    modifyList(x, default_cntrl[!names(default_cntrl) %in% names(x)])
}

select_method <- function(model_type, is_mip) {
    if ( is_mip ) {       
        if ( model_type == "lp" ) "mosek" else "Knitro"
    } else {
        if ( model_type == "lp" ) "mosek" else "Knitro"
    }
}

match_solver <- function(method) {
    solver_method <- neos_solver_mapping()[clean(method)]
    if ( is.na(solver_method) )
        stop("unknown solver: ", shQuote(method), call. = FALSE)
    solver_method
}

select_neos_solver <- function(method, is_mip, model_type) {
    if ( is.null(method) ) {
        method <- select_method(model_type, is_mip)
        warning("no method provided set to ", shQuote(method), call. = TRUE)
        return(method)
    }
    match_solver(method)
}

neos_xml_call <- function(model, solver_name, email) {
    cate <- unname(match_category(solver_name))
    template <- neos_solver_template(category = cate, solver_name = solver_name, 
                                     input_method = "GAMS")
    argslist <- c(email = email, list(model = model, options = "", gdx = "", 
                                      wantgdx = "", wantlog = "", comments = ""))
    xml <- set_templanete_parameters(xml_template = template, params = argslist)
    ## some solvers need a working email address not supported by rneos therefore
    ## we inject it
    ## if ( !is.null(email) ) {
    ##     email_insertion <- sprintf("<email>%s</email>\n <model>", email)
    ##     xml <- gsub("<model>", email_insertion, xml, fixed = TRUE)
    ## }
    xml
}

## c(email = "flo", list(model = "model", options = ""))

raise_licence_error <- function(password) {
    stop(paste(password, collapse = "\n"), 
         " In some cases the solver licence does not permit connections via", 
         " XML-RPC. Therefore these solvers can not be accessed by 'ROI.plugin.neos'",
         " directly. An alternative option is to write the problem out via",
         " write.op(model, 'my_op.gms', 'gams') and commit it via the web-interface.",
         " Or just use a alternative solver.")
}

if (FALSE) {
    library(xml2)
    library(xmlrpc2)
    attach(getNamespace("ROI.plugin.neos"), name = "package:ROI.plugin.neos")
    control <- list()
}

solve_OP <- function(x, control = list()) {
    control <- set_default_control_values(control)

    if ( inherits(constraints(x), "NO_constraint") ) {
        L <- matrix(0, nrow = 0, ncol = length(objective(x)))
        constraints(x) <- L_constraint(L = L , dir = character(), rhs = double())
    }
    
    model_type <- which_model_type(x)
    is_mip <- any(types(x) %in% c("B", "I"))
    
    model <- to_gams(x)

    solver_name <- unname(select_neos_solver(control$method, is_mip, model_type))

    xml <- neos_xml_call(model, solver_name, control$email)
    solver_call <- list(neos_submit_job, xmlstring = xml, user = control$user, 
                        password = control$password)
    mode(solver_call) <- "call"
    if ( isTRUE(control$dry_run) )
        return(solver_call)

    job <- eval(solver_call)

    if ( any(grep("Error", job$password, ignore.case = TRUE)) )
        raise_licence_error(job$password)

    if ( (is.logical(control$wait) & !isTRUE(control$wait)) )
        return(job)

    neos_message <- job$final_results()
    if ( !neos_message_indicates_success(neos_message) ) stop(neos_message)
    neos_results <- extract_results(job$output_file("results.txt"))
    objval <- tryCatch(objective(x)(neos_results$solution), error = function(e) NA_real_)
    status <- generate_status_code(neos_results$solver_status, neos_results$model_status)
    neos_results$message <- neos_message
    
    ROI_plugin_canonicalize_solution(solution = neos_results$solution, 
                                     optimum = objval, status = status,
                                     solver = "neos", message = neos_results)
}

neos_message_indicates_success <- function(x) {
    if ( !is.character(x) ) return(FALSE)
    any(grepl("---BEGIN.SOLUTION---", x, fixed = TRUE))
}

neos_get_results <- function(job) {
    url <- "https://www.neos-server.org"
    fn <- 'results.txt'
    params <- list(jobNumber = job@jobnumber, password = job@password, fileName = fn)
    resp <- xmlrpc(url, "getOutputFile", params = params)
    rawToChar(resp)
}

extract_results <- function(neos_results) {
    res <- strip(unlist(strsplit(neos_results, "\n", fixed = TRUE)))
    i <- cumsum(grepl(":", res, fixed = TRUE))
    b <- duplicated(i)
    setNames(split(as.double(res[b]), i[b]), gsub(":", "", res[!b], fixed = TRUE))
}

