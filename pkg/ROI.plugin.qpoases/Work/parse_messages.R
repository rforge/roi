
fp <- ".."
fp <- file.path(fp, "src", "qpoases", "include", "qpOASES", "MessageHandling.hpp")
file.exists(fp)

x <- readLines(fp)
x <- paste(x, collapse = "\n")

pattern <- "enum returnValue.+?\\}"
m <- gregexpr(pattern, x)
x <- unlist(regmatches(x, m))

strip <- function(x) gsub("(^\\s+|\\s+$)", "", x)

y <- gsub("\\/\\*.*?\\*\\/", "", x)
y <- gsub("[,\\}\\{]", "", y)
y <- gsub("enum returnValue", "", y)
y <- unlist(strsplit(y, "\n"))
y <- gsub("\\s", " ", y)
y <- strip(y)
y <- y[nchar(y) > 0]

head(y)
cat(y)

z <- strsplit(y, "\\s*=\\s*")

key <- sapply(z, "[[", 1)
value <- sapply(z, function(x) if (length(x) > 1) as.integer(x[2L]) else NA_integer_)

stopifnot(all(value[1:2] == c(-1, 0)), all(is.na(value[-c(1:2)])))

value[-c(1:2)] <- seq_along(value[-c(1:2)])

is.na(value)
head(z)

value

key
value


cat(deparse(key))
cat(deparse(value))


source("../R/status_codes.R")

status_codes
