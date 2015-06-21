## Filename: createSyntaxR.R
## Purpose: Create an R syntax file from a template file to read data
##
## To run in terminal use:  R CMD BATCH --vanilla createSyntaxR.R

## Created at:  Sat 2015-04-25 at 01:57:17
## Author:      Peter Baker
## Hostname:    sph-ph-428-04p.sph.uq.edu.au
## Directory:   /home/pete/Data/R.workflow/Rpackage-201501/src/R/
## Licence:     GPLv3 see <http://www.gnu.org/licenses/>
##
## Change Log: 2015-05-01 at 17:47:14
##             Added dependencies and generalised to several process.types
##

##' Create \code{R} syntax for reading, cleaning, summarising and analysing
##'
##' Function is used internally by \code{\link{createProjectSkeleton}} and
##' \code{\link{addFile}} to produce individual \code{.R} template
##' syntax files for cleaning, summarising and analysing data files in
##' a data analysis project
##' 
##' @param dir.project directory name for project
##' @param filesAndDFs object of S3 class \code{fileAndDataName}
##' containing relevant filenames, directories for setting up syntax
##' and Makefiles
##' @param template name of template text file as string. Default:
##' \code{NULL} for predefined template from \code{\link{dryworkflow}}
##' package.
##' @param project.steps steps to be carried out in project, specified
##' as a vector of strings. Options are \dQuote{read} to read data
##' (always assumed present), \dQuote{clean} clean data,
##' \dQuote{summary} summary statistics and basic plots,
##' \dQuote{analyse} perform statistical analysis, \dQuote{compare}
##' compare datasets and in particular different versions of the same
##' data set, \dQuote{mergeAll} merge data sets of more than one; and
##' \dQuote{reportRmd} or \dQuote{reportRnw} produce reports using
##' \code{\link{rmarkdown}} and/or\code{\link{Sweave}} and
##' \dQuote{presentRmd} or \dQuote{beamerRnw} produce presentations
##' using \code{\link{rmarkdown}} and/or\code{\link{Sweave}}
##' @param makefile.depends files to be used as dependencies in
##' addition to the syntax file for targets. Default: worked out from
##' project structure (\code{NULL})
##' @param makefile.targets strings with file extensions for targets
##' in makefiles Default: c(\dQuote{Rout}, \dQuote{pdf}) using
##' \code{R BATCH} and \code{stitch} via \code{rmarkdown}, respectively
##' @param myFunction.files character vector of own homegrown function
##' file names to be sourced not currently put in to a package
##' @param libraries character vector of library statements to be
##' added near top of \code{R} syntax file. Default: load
##' \code{dryworkflow} \code{Hmisc}, \code{foreign}
##' @param template.dir directory containing template. Default:
##' \code{\link{dryworkflow}} package directory
##' @param print.mismatches print mismatches when creating syntax or
##' markdown files from templates. Default: \code{FALSE}
##' @param overwrite.file logical whether or not to allow overwrite of
##' existing file. Default: FALSE
##' @param string.prefix string of characters for start of strings to
##' be substituted from template file. Default \sQuote{@@@@}
##' @param string.suffix string of characters for end of strings to be
##' substituted from template file. Default: same as
##' \code{string.prefix}
##' @param delete.start lines between and including those containing
##' the \code{delete.start} and \code{delete.end} patterns will be
##' removed. Default: \dQuote{---- START: DELETE THIS SECTION ----}
##' @param delete.end Default: \dQuote{---- END: DELETE THIS SECTION ----}
##' @param extras list of extra options to be passed to function for
##' substitution in template. Default: licence, author and
##' title.project obtained from global options
##' @param AUTHOR string containing Author's name for \code{R} and
##' markdown syntax files
##' @param TITLE.PROJECT string containing title of project for log
##' files and reports
##' @return Lines to be included in Makefile for reading files as
##' character vector
##' @author Peter Baker  \email{pete@@petebaker.id.au}
createSyntaxR <-
  function(
    dir.project, filesAndDFs, template,
    project.steps = c("read", "clean", "summary", "analyse", "mergeAll",
      "codebook", "reportRmd", "reportRnw", "presentRmd", "beamerRmd",
      "beamerRnw"),
    makefile.depends = NULL, makefile.targets = c("Rout", "pdf"),
    myFunction.files = NULL,  libraries = NULL,
    template.dir = NULL, print.mismatches = FALSE,
    overwrite.file = FALSE,
    string.prefix = "@@", string.suffix = string.prefix,
    delete.start = "-- START: DELETE THIS SECTION --",
    delete.end = "-- END: DELETE THIS SECTION --",
    extras = list(LICENCE = options()$dryworkflow$licence,
      AUTHOR = options()$dryworkflow$author,
      TITLE.PROJECT = options()$dryworkflow$title.project)
  )
{

  ##  targets = c("clean", "summary", "analysis"),
  ##  depends = c("read", "codebook", "clean", "summary", "analysis"),
  ##  projectDirs, filesAndDFs,
  ## gsub(".R$", ".Rout", basename(syntax.file)),
  ## makefile.pdf = gsub(".R$", ".pdf", basename(syntax.file)),

  ## should I just write a separate one for each task and get fancier
  ## later - guessing this is pretty easy to do it this way as so much
  ## in common


  ## project.steps arguments
  project.steps <- match.arg(project.steps)     # there can only be one
  makefile.targets <-
    match.arg(makefile.targets, c("Rout", "pdf", "docx", "html"),
              several.ok = TRUE)
  if (project.steps %in% c("reportRmd", "reportRnw", "presentRmd",
                           "beamerRmd", "beamerRnw")){
    reportFile <- TRUE
    makefile.targets <- setdiff(makefile.targets, "Rout")
    syntax.ext <-
      ifelse(project.steps %in% c("reportRmd", "presentRmd", "beamerRmd"),
                         "Rmd", "Rnw")
  } else {
    reportFile <- FALSE
    syntax.ext <- "R"
  }
  
  ## project.steps options ------------------------------------------------
  ## not good test - need some sort of project configuration
  ## maybe not so needed
  if (mode(dir.project) != "character") stop("dir.project wrong")
  ## check correct classes
  ## if (class(projectDirs) != "drywDestinationDirs")
  ##   stop("'projectDirs' not of class 'drywDestinationDirs'")
  if (class(filesAndDFs) != "fileAndDataNames")
    stop("'filesAndDFs' not of class 'fileAndDataNames'")
  if (mode(extras) != "list")
    stop("'extras' not of mode 'list'")
  ## if (mode(data.dir) != "list")
  ##   stop("'data.dir' not of mode 'list'")
  
  ## libraries --------------------------------------------------
  if (is.null(libraries)){
    libraries <-
      c("library(dryworkflow) # Some of these libraries load others too",
        "library(plyr)", "library(reshape2)",
        "library(lubridate)", "library(stringr)",
        "library(Hmisc)", "library(car)", "library(compare)") 
  } else {
    libraries <- ""
  }

  if (is.null(myFunction.files)) myFunction.files <- ""

  ## dependencies --------------------------------------------- ?????????
  reportDeps <- c("clean", "summary", "analyse")
  if (is.null(makefile.depends)){
    makefileDepends <- list(read = "data", clean = "read", mergeAll = "clean",
                            summary = "clean", analyse = "clean",
                            codebook = "codebook",
                            reportRmd = reportDeps, reportRnw = reportDeps,
                            presentRmd = reportDeps,
                            beamerRmd = reportDeps, beamerRnw = reportDeps)
  }

  ## specific read in data commands
  readDataCommands <-
    switch(project.steps,
           read = whichReadCommand(filesAndDFs[["dataFiles"]]),
           codebook = "readCodeBook",
           "load")
  ## if (project.steps == "read"){
  ##   readDataCommands <- whichReadCommand(filesAndDFs[["dataFiles"]])
  ## } else {
  ##   if (project.steps == "codebook"){
  ##     readDataCommands <- "readCodeBook"
  ##   } else {
  ##     readDataCommands <- "load"
  ##   }
  ## }

  ## simple substitutions --------------------------------------------
  ## input/output directories and reading data in
  dataInputDir <- switch(project.steps,
                         read = filesAndDFs$directories["dataOrig"],
                         codebook = filesAndDFs$directories["dataCodebook"],
                         filesAndDFs$directories["dataDeriv"])
  ## data input files and data frame
  dataframe.read <-
    switch(project.steps, read = paste(filesAndDFs$dataFrames[["read"]], "<- "),
           codebook = paste(filesAndDFs$dataFrames[["codebook"]], "<- "), NULL)
  if (reportFile){
    dataFileInput <- filesAndDFs$RDataFiles[["clean"]]
  } else {
    dataFileInput <-
      switch(project.steps,
             read = filesAndDFs$dataFiles,
             codebook = filesAndDFs$codebookFiles,
             filesAndDFs$RDataFiles[[makefileDepends[[project.steps]]]])
  }
  ##   if (project.steps == "read"){
  ##   dataFileInput <- filesAndDFs$dataFiles
  ## } else {
  ##   if (project.steps == "codebook"){
  ##     dataFileInput <- filesAndDFs$codebookFiles
  ##   } else {
  ##     dataFileInput <-
  ##       filesAndDFs$RDataFiles[[makefileDepends[[project.steps]]]]
  ##   }
  ## }      
  inputFileCommands <-
    paste0(dataframe.read,
          stringr::str_c(readDataCommands, '("', dataInputDir, '/',
                         dataFileInput, '")'))
  

  ## data frames - input/output
  if (reportFile){
    dataFrameIn <- filesAndDFs$dataFrames[["clean"]]
    dataFrameSum <- filesAndDFs$dataFrames[["summary"]]
    dataFrameAna <- filesAndDFs$dataFrames[["analyse"]]
    rdataSum <- filesAndDFs$RDataFiles[["summary"]]
    rdataAna <- filesAndDFs$RDataFiles[["analyse"]]
  } else {
    dataFrameIn <- 
      switch(project.steps, read = filesAndDFs$dataFrames[["read"]],
             codebook = filesAndDFs$dataFrames[["codebook"]],
             filesAndDFs$dataFrames[[makefileDepends[[project.steps]]]])
  }
  ## dataFrameSaved <- switch(reportFile, NULL,
  ##                          filesAndDFs$dataFrames[[project.steps]])
  if (reportFile){
    dataFrameSaved <- dataOutputDir <- dataFileOutput <- NULL
  } else{
    dataFrameSaved <- filesAndDFs$dataFrames[[project.steps]]
    ## always put derived data in this directory
    dataOutputDir <- filesAndDFs$directories["dataDeriv"]
    dataFileOutput <- filesAndDFs$RDataFiles[[project.steps]]
  }
  
  ## same for all R syntax so need to be processed for all R syntax
  ## files
  if(!reportFile) ps2 <- project.steps else ps2 <- NULL
  common.strings <- c(list(
    PROJECT.STEP = ps2,
    DATE.CREATED = date(),
    DIR.DATA.INPUT = dataInputDir,
    DIR.DATA.SAVED = dataOutputDir,
    MYLIB.FILES = myFunction.files,
    LIBRARIES = libraries), extras)

  ## destination directories for R syntax ----------------------------
  ## rSyntaxDirs <- list(read = projectDirs$working.dirs$readMerge,
  ##                     clean = projectDirs$working.dirs$readMerge,
  ##                     codebook = projectDirs$working.dirs$readMerge,
  ##                     compare = projectDirs$working.dirs$readMerge,
  ##                     mergeAll = projectDirs$working.dirs$readMerge,
  ##                     summary = projectDirs$working.dirs$work,
  ##                     analyse = projectDirs$working.dirs$work,
  ##                     reportRmd = projectDirs$working.dirs$report,
  ##                     reportRnw = projectDirs$working.dirs$report,
  ##                     presentRmd = projectDirs$working.dirs$report,
  ##                     beamerRmd = projectDirs$working.dirs$report,
  ##                     beamerRnw = projectDirs$working.dirs$report)

  ## rsyntax directories
  dataDirs <- project.steps %in% c("dataDeriv", "dataOrig", "dataCodebook")
  rSyntaxDirs <- filesAndDFs$directories[!dataDirs]
  rSyntaxDirs <- lapply(rSyntaxDirs, stringr::str_replace,
                        pattern = stringr::str_c("^..", .Platform$file.sep),
                        replacement = "")

  ## mergeAll - need inputs as a string instead of separate strings
  if (project.steps == "mergeAll"){
    dataFrames4merge <-
      paste0("c('", paste(dataFrameIn, collapse = "', '"), "')")
    dataFrameIn <- dataFrames4merge
    rdataFiles4merge <-
      paste0("c('", paste(dataFileInput, collapse = "', '"), "')")
    dataFileInput <- rdataFiles4merge
    dataFiles4merge <- 
      paste0("c('", paste(filesAndDFs$dataFiles, collapse = "', '"), "')")
  }

  ## syntax file ------------------------------------------------------
  if(reportFile){
    RsyntaxFiles <- filesAndDFs$reportFiles[[project.steps]]
  } else {
    RsyntaxFiles <- filesAndDFs$RsyntaxFiles[[project.steps]]
  }
  syntax.files <-
    file.path(dir.project,
              dir.x <- rSyntaxDirs[[project.steps]], RsyntaxFiles)

  ## specific strings to substitute - one for each R syntax file ------
  subst.strings1 <-
    list(DATAFILE = switch(project.steps,
           codebook = filesAndDFs$codebookFiles,
           mergeAll = dataFiles4merge,  filesAndDFs$dataFiles),
         INPUT.COMMANDS = inputFileCommands,
         RDATA.INPUT = dataFileInput,
         RDATA.SAVED = dataFileOutput,
         DATAFRAME.INPUT = dataFrameIn,
         DATAFRAME.SAVED = dataFrameSaved)
  if (!reportFile)
     subst.strings1 <- c(subst.strings1, list(SYNTAX.FILE = RsyntaxFiles))

  if (project.steps == "mergeAll"){ # want loading files on separate
                                    # lines not in loop for each file
    common.strings <-
      c(common.strings,
        list(INPUT.COMMANDS = subst.strings1$INPUT.COMMANDS))
    subst.strings1$INPUT.COMMANDS <- NULL
  }
  
  if (reportFile){
    common.strings <- c(common.strings, list(SYNTAX.FILE = RsyntaxFiles))
    names(subst.strings1)[names(subst.strings1) == "DATAFRAME.INPUT"] <- 
      "DATAFRAME.CLEAN"
    names(subst.strings1)[names(subst.strings1) == "RDATA.INPUT"] <- 
      "RDATA.CLEAN.SAVED"
    subst.strings1 <- c(subst.strings1,
                        list(RDATA.SUMMARY.SAVED = rdataSum,
                             RDATA.ANALYSIS.SAVED = rdataAna,
                             DATAFRAME.SUMMARY = dataFrameSum,
                             DATAFRAME.ANALYSIS = dataFrameAna))
    ## drop NULLs
    subst.strings1[names(subst.strings1[sapply(subst.strings1,
                                               function(x) (is.null(x)))])] <-
                                                 NULL
    common.strings[names(common.strings[sapply(common.strings,
                                               function(x) (is.null(x)))])] <-
                                                 NULL
  }

  ## Check for consistency - all same length for writing in loop
  if (project.steps != "compare"){
    if (! length( unique( sapply(subst.strings1, length))) == 1){
      print(subst.strings1)
      stop("not all elements of 'subst.string1' have same length")
    }
  }
  
  makefileLines <-
    c("",
      switch(project.steps,
             read = "## Read data and store for cleaning and analysis",
             summary = "## Summaries and analyses", NULL))

  ## apply the subst list - seems easiest in a loop ----------------------
  ## template <- ifelse(reportFile, template.files[project.steps],
  ##                    template.files[paste0(project.steps, "R")])
  previous.step <- makefileDepends[[project.steps]]
  if (length(previous.step) == 1) {
    whichDirs <- switch(previous.step, data = "dataOrig",
                        codebook = "dataCodebook", previous.step)
  } else {
    whichDirs <- previous.step
  }
  previousDirectory <- filesAndDFs$directories[whichDirs]
  
  needRecursive <- 
    (previousDirectory != filesAndDFs$directories[[project.steps]])
  if (all(needRecursive)){
    make.dep.dir <- paste0(previousDirectory, "/")
  } else {
    make.dep.dir <- NULL
  }

  targets <- NULL
  
  for (J in 1:length(RsyntaxFiles)){ # J <- 1
    subst.Str <- c(lapply(subst.strings1, function(x) x[J]),
                       common.strings)
    createFromTemplate(
      syntax.files[J], subst.strings = subst.Str,
      template = template, template.dir = template.dir,
      print.mismatches = print.mismatches,
      overwrite.file = overwrite.file,
      string.prefix = string.prefix, string.suffix = string.suffix,
      delete.start = delete.start, delete.end = delete.end)
    ## Rout for dependency for Makefile
    ## same directory?
    if (!reportFile){ # R syntax needs previous step .Rout as a dependency  
      if (project.steps != "mergeAll"){
        make.dep.file <- 
          switch(previous.step,
                 data = dataFileInput[J],
                 codebook = dataFileInput[J],
                 stringr::str_replace(filesAndDFs$RsyntaxFiles[[previous.step]][J],
                                      ".R$", ".Rout"))
      } else {
        make.dep.file <-
          paste(stringr::str_replace(filesAndDFs$RsyntaxFiles[[previous.step]],
                                     ".R$", ".Rout"), collapse = " ")
      }
      make.dep <-
        stringr::str_c(make.dep.dir, make.dep.file)
    } else {  # reports/ presentations need clean, summary and analysis as dependendencies
      if (filesAndDFs$report.which == "first"){
        make.dep.file <-
          paste(stringr::str_replace(sapply(filesAndDFs$RsyntaxFiles[previous.step],
                                            function(x) x[J]), ".R$", ".Rout"))
        make.dep <- paste(paste0(make.dep.dir, make.dep.file), collapse =" ")
      }
    }
    ## write make file lines ----------------------------------------------
    TARGETEXT <- stringr::str_c(".", syntax.ext, "$")
    for (EXT in makefile.targets){
      t1 <- stringr::str_c(stringr::str_replace(basename(syntax.files[J]),
                                                TARGETEXT, paste0(".", EXT)))
      targets <- c(targets, t1)
      makefileLines <-
        c(makefileLines,
          stringr::str_c(t1, ": ${@:.", EXT, "=.", syntax.ext,"} ", make.dep))
    } 
  }

  ## for makefiles
  list(targets = targets, makefileLines = makefileLines)
  
}



## function(x, dir.project = dir.project, mnemonic = MNEMONIC,
##          readMerge = file.path(dir.project,
##            projectDirs$working.dirs$readMerge),
##          data.orig = file.path(projectDirs$working.dirs$dataOrig),
##          data.deriv = file.path(projectDirs$working.dirs$dataDeriv),
##          data.codebook = file.path(projectDirs$working.dirs$codebook),
##          codebook = NULL)

## ## for debugging
## dir.project
## filesAndDFs
## myFunction.files
## template = template.files["reportRmd"]
## template.dir
## makefile.depends = NULL
## overwrite.file = FALSE
## file.string = "FILENAME"
## string.prefix = "@@"
## string.suffix = string.prefix
## delete.start = "-- START: DELETE THIS SECTION --"
## delete.end = "-- END: DELETE THIS SECTION --"
## extras = list(LICENCE = licence)
## type.project = c("normal", "simple")
## libraries = NULL

## ##makefile.targets <- c("Rout", "pdf")
## ##project.steps <- "read"
## makefile.targets <- c("pdf", "html")
## project.steps <- "reportRmd"

## ## how to choose read/clean etc dests - only a couple but
## rSyntaxDirs <- list(read = projectDirs$working.dirs$readMerge,
##                     clean = projectDirs$working.dirs$readMerge,
##                     codebook = projectDirs$working.dirs$readMerge,
##                     summary = projectDirs$working.dirs$work,
##                     analyse = projectDirs$working.dirs$work)

## syntax.file <- file.path(dir.project, rSyntaxDirs[["clean"]],
##                          filesAndDFs$RsyntaxFiles[["clean"]][1])
## ## if dir.project NULL then use a "."
## ##syntax.file <- file.path(".", rSyntaxDirs[["clean"]],
## ##                         filesAndDFs$RsyntaxFiles[["clean"]][1])
## ## syntax.file
## ## > dirname(syntax.file)
## ## [1] "myRproject/readMergeData"
## ## > basename(syntax.file)
## ## [1] "clean_data1_csv.R"

## createSyntaxR(dir.project = dir.project,
##               syntax.file = syntax.file,
              
##   projectDirs = projectDirs,
##   filesAndDFs = filesAndDFs,
##   rdata.dir = data.dir$DIR.DERIVED,
##   myFunction.files = myFunction.files, 
##   template = template.files[["readR"]],
##   template.dir = template.dir)
## }
