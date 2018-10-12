
x <- readLines("MessageHandling_mod.hpp")

symbol <- gsub("(,.*|\\s.*)", "", x)
id <- seq_along(x) - 2L
msg <- x
msg[grep("**<", msg, fixed = TRUE, invert = TRUE)] <- ""
msg <- gsub(".*\\/\\*\\*<\\s*", "", msg)
msg <- gsub("\\s*\\*\\/\\s*", "", msg)

messages <- list()
for (i in seq_along(symbol)) {
    messages[[i]] <- list(symbol = symbol[i], id = id[i], msg = msg[i])
}


fun <- function() {
    cat("\n\n\n\n\n\n\n")
    cat('qpoases_status_codes <- list(\n    ')
    line_end <- ",\n    "
    for (i in seq_along(symbol)) {
        if ( length(symbol) == i )
            line_end <- ")"
        line <- sprintf("list(symbol = %s, id = %i, msg = %s)%s",
                        shQuote(symbol[i]), id[i], shQuote(msg[i]), line_end)
        cat(line)
    }
    cat("\n\n")
}

fun()

