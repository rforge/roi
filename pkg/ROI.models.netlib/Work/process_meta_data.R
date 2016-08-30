
download_meta_data <- function(url="http://www.netlib.org/lp/data/readme") {
    suppressWarnings(x <- try(readLines(url), silent=TRUE))
    if ( class(x) == "try-error") {
        print("TODO: Error Handling")
    }
    header_line <- grep("Name\\s+Rows\\s+Cols\\s+Nonzeros", x)
    x <- x[-seq_len(header_line-1)]
    footer_line <- grep("BOUND-TYPE\\s+TABLE", x)
    x <- x[-(length(x) - seq_len(length(x)-footer_line))]
    empty_rows <- grepl("^\\s*$", x)
    x <- x[!empty_rows]
    header <- strsplit(x[1], "\\s+")
    x <- x[-1]
    x <- gsub("(see NOTES)", "  (see NOTES)  ", x, fixed=TRUE)
    y <- strsplit(x, "\\s{2,}")
    fix <- function(w)  {
        if ( length(w) == 7 ) return(w)
        return(c(w[-length(w)], "", tail(w, 1)))
    }
    return(do.call(rbind, lapply(y, fix)))    
}

##' Start downloading
download.file("http://www.netlib.org/lp/data/readme", "README.txt")

x <- readLines("README.txt")

header_line <- grep("Name\\s+Rows\\s+Cols\\s+Nonzeros", x)
x <- x[-seq_len(header_line-1)]
footer_line <- grep("BOUND-TYPE\\s+TABLE", x)
x <- x[-(length(x) - seq_len(length(x)-footer_line))]
empty_rows <- grepl("^\\s*$", x)
x <- x[!empty_rows]
header <- strsplit(x[1], "\\s{2,}")
header <- header[[1]]
x <- x[-1]
x <- gsub("(see NOTES)", "  (see NOTES)  ", x, fixed=TRUE)
y <- strsplit(x, "\\s{2,}")
fix <- function(w)  {
    if ( length(w) == 7 ) return(w)
    return(c(w[-length(w)], "", tail(w, 1)))
}

z <- do.call(rbind, lapply(y, fix))
colnames(z) <- header

z <- as.data.frame(z, stringsAsFactors = FALSE)
colnames(z) <- c("name", "rows", "cols", "non-zeros", "bytes", "br", "optimal_value")
head(z)

strip <- function(x) gsub("(^\\s*|\\s*$)", "", x)

z$opt <- strip(z$opt)
z$opt <- gsub("\\s+\\*+", "", z$opt)
z$opt <- gsub("(see NOTES)", "", z$opt, fixed=TRUE)
z$opt <- as.numeric(z$opt)

rownames(z) <- tolower(make.names(z$name))
rownames(z)

saveRDS(z, "meta.rds")

meta <- z
save(meta, file="../data/meta.rda")

