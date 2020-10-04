## This function will read an simple .Rmd file, strip all of the .Rmd specific code indicators (which the R console can't interpret) and replace the beginning of each non-code line with the “#'” indicator
## sourced from http://rstudio-pubs-static.s3.amazonaws.com/12734_0a38887f19a34d92b7311a2c9cb15022.html
## added ability to attach slide number based on the slide separator, (i.e. pattern = "========================================================")

rmd2rscript <- function(inFile, slideNumber = TRUE, removeSlideText = TRUE){
    # read the file
    fileIn <- readLines(inFile)
    # identify the start of code blocks
    cdStrt <- which(grepl(fileIn, pattern = "```{r*", perl = TRUE))
    # identify the end of code blocks
    cdEnd <- sapply(cdStrt, function(x){
        preidx <- which(grepl(fileIn[-(1:x)], pattern = "```", perl = TRUE))[1]
        return(preidx + x)
    })

    # identify where slide indiciators are
    if(slideNumber){
      slides <- which(grepl(fileIn, pattern = "========================================================", perl = TRUE))
      # identify the end of code blocks
      slideNo <- 1:length(slides)
      fileIn[slides] <- paste("# * Slide", slideNo)
    }
    

    # define an expansion function
    # strip code block indacators
    fileIn[c(cdStrt, cdEnd)] <- ""
    expFun <- function(strt, End){
        strt <- strt+1
        End <- End-1
        return(strt:End)
    }
    idx <- unlist(mapply(FUN = expFun, strt = cdStrt, End = cdEnd, 
                         SIMPLIFY = FALSE))
    # add comments to all lines except code blocks
    comIdx <- 1:length(fileIn)
    comIdx <- comIdx[-idx]
    for(i in comIdx){
        fileIn[i] <- paste("#' ", fileIn[i], sep = "")

    }

    fileIn <- gsub(fileIn, pattern = "#' # \\* ", replacement = "############################################### ")

    if(removeSlideText) {
      toMatch <- c("^#' ")

      fileIn <- fileIn[which(!grepl(fileIn, pattern = paste(toMatch,collapse="|")))]
    }

    # remove image links
    fileIn <- fileIn[!grepl(fileIn, pattern = "knitr::include_graphics|options\\(width|useFancyQuotes|scipen", perl = TRUE)]
    
    # create an output file
    nm <- strsplit(inFile, split = "\\.")[[1]][1]
    flOut <- file(paste(nm, "_Script.R", sep = ""), "w")
    for(i in 1:length(fileIn)){
        cat(fileIn[i], "\n", file = flOut, sep = "\t")
    }

    close(flOut)
}

setwd("/Users/teachingaccount/Desktop/GIT/RStudio_TeachingSample")
rmd2rscript("index.Rmd")
