netlib <- function(x=c("all", "metainfo", "adlittle", "afiro", "agg", "agg2", 
                       "agg3",  "bandm", "beaconfd", "blend", "bnl1", "bnl2", 
                       "boeing1", "boeing2", "bore3d", "brandy", "capri",  "cycle", 
                       "czprob", "d2q06c", "d6cube", "degen2",  "degen3", "dfl001", 
                       "e226", "etamacro", "fffff800",  "finnis", "fit1d", "fit1p", 
                       "fit2d", "fit2p",  "forplan", "ganges", "gfrd.pnc", "greenbea", 
                       "greenbeb", "grow15", "grow22", "grow7", "israel", "kb2", 
                       "lotfi", "maros.r7", "maros", "meta",  "modszk1", "nesm", 
                       "perold", "pilot.ja", "pilot",  "pilot.we", "pilot4", "pilot87", 
                       "pilotnov",  "recipe", "sc105", "sc205", "sc50a", "sc50b", 
                       "scagr25", "scagr7", "scfxm1", "scfxm2", "scfxm3", "scorpion", 
                       "scrs8", "scsd1", "scsd6", "scsd8",  "sctap1", "sctap2", "sctap3", 
                       "seba", "share1b",  "share2b", "shell", "ship04l", "ship04s", "ship08l", 
                       "ship08s", "ship12l", "ship12s", "sierra", "stair", 
                       "standata", "standmps", "stocfor1", "stocfor2", "stocfor3", 
                       "truss", "tuff", "vtp.base", "wood1p",  "woodw","x25fv47", "x80bau3b")) {
    folder <- system.file("data", package = "ROI.models.netlib")
    if ( missing(x) )
        return( setdiff(dir(folder), "metainfo.rds") )
    
    problem_name <- match.arg(x)
    if ( x == "all" ) {
        pnames <- c("adlittle", "afiro", "agg", "agg2", "agg3", "bandm", "beaconfd", 
                    "blend", "bnl1", "bnl2", "boeing1", "boeing2", "bore3d", "brandy", 
                    "capri", "cycle", "czprob", "d2q06c", "d6cube", "degen2", "degen3", 
                    "dfl001", "e226", "etamacro", "fffff800", "finnis", "fit1d", "fit1p", 
                    "fit2d", "fit2p", "forplan", "ganges", "gfrd.pnc", "greenbea", 
                    "greenbeb", "grow15", "grow22", "grow7", "israel", "kb2", "lotfi", 
                    "maros.r7", "maros", "modszk1", "nesm", "perold", "pilot.ja", 
                    "pilot", "pilot.we", "pilot4", "pilot87", "pilotnov", "recipe", "sc105", 
                    "sc205", "sc50a", "sc50b", "scagr25", "scagr7", "scfxm1", "scfxm2",
                    "scfxm3", "scorpion", "scrs8", "scsd1", "scsd6", "scsd8", "sctap1",
                    "sctap2", "sctap3", "seba", "share1b", "share2b", "shell", "ship04l", 
                    "ship04s", "ship08l", "ship08s", "ship12l", "ship12s", "sierra",
                    "stair", "standata", "standmps", "stocfor1", "stocfor2", "stocfor3",
                    "truss", "tuff", "vtp.base", "wood1p", "woodw", "x25fv47", "x80bau3b")
        ppath <- file.path(folder, sprintf("%s.rds", pnames))
        prob <- lapply(ppath, readRDS)
    } else {
        prob <- readRDS(file.path(folder, x))
    }
    prob
}

