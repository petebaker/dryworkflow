## Filename: setUpDirectoryStructure.R
## Purpose: Create directory structure for dryWorkflow project
##
## To run in terminal use:  R CMD BATCH --vanilla setUpDirectoryStructure.R

## Created at:  Mon Mar 23 23:38:52 2015
## Author:      Peter Baker
## Hostname:    clearwell2.fritz.box
## Directory:   /home/pete/Data/R.workflow/Rpackage-201501/src/R/
## Licence:     GPLv3 see <http://www.gnu.org/licenses/>
##
## Change Log: 
##

## perhaps this shouldn't be called directly so but may need to for
## custom setup

##' Set up directories for data analysis project
##' 
##' @param style style for directory and file names (\code{unix} or
##' \code{windows}), Default: \code{unix}
##' @param type.project type of project: normal, simple or custom
##' (custom Not Yet Implemented). The style of directory structure
##' for the project. Default: \dQuote{normal}
##' @param destinations \code{list} of destination directories where
##' original (and added) files will be moved. This is a \code{list}
##' with named components \sQuote{data}, \sQuote{doc},
##' \sQuote{codebook} and \sQuote{lib} for data files, documents,
##' codebooks and R functions, respectively. Default: unix directory
##' names will be \code{list(data = "data/original", doc = "doc/original", codebook = "data/codebook", lib = "lib")} and
##' Windows will be of similar form.
##' @param extra extra directories additional to standard setup
##' @param extra.destinations extra destination directories additional
##' to standard setup
##' @param ... extra arguments passed to specific functions
##' @return \code{list} of directories including destinations for
##' initial files of class \sQuote{drywDestinationDirs}. Named
##' components are \code{directories} and \code{destinations}.
setUpDirectoryStructure <-
  function(style = NULL, type.project = NULL, destinations = NULL,
           extra = NULL, extra.destinations = NULL, ...)
{

  ## I think that these directories should be taken out of
  ## .setDRYWOptions() and put in separate function but somehow it
  ## must match up with the destination directories

  ## keywords internal - may reinstate

  ## HERE IT IS - some deleting of comments required b4 git commit!!!

  ## custom could just accept extra and all others set

  ## basically need these destinations: - ignored if unix or windows
  ## data files: data/original
  ## doc files: doc/original
  ## code books: doc/codebook
  ## libs: lib
  ## extra customised directories to be added to standard ones

  ## 'style' arg ---------------------------------------------
  
  allStyles <- c("unix", "windows", "custom")

  if (length(style) == 0){
    STYLE <- getOption("dryworkflow")$style
  } else {
    STYLE <- match.arg(style, allStyles)
  }
  ## how to test - is this OK
  if (STYLE == "custom"){
    stop("Error: 'custom' style not yet implemented")
  }

  ## 'type.project' arg ----------------------------------------

  allTypes <- c("normal", "simple", "custom")
  if (length(type.project) == 0){
    TYPE.PROJECT <- getOption("dryworkflow")$type.project
  } else {
    TYPE.PROJECT <- match.arg(type.project, allTypes)
  }
  ## how to test - is this OK
  if (TYPE.PROJECT == "custom"){
    stop("Error: 'custom' 'type.project' not yet implemented")
  }


  
  ## set up directory names depending on filename style
  ## simple just has a subset of these and 'custom' - not yet implemented
  window.dirs <- list(top = c("Administration", "Backups",
                        "Datasets", "Documents", "Extra", "Library_Functions",
                        "Original", "Posted", "Reading",
                        "Read_And_Merge_Data", "Reports", "Source",
                        "Testing", "Working"),
                      admin = paste0("Administration/",
                        c("Budget", "Correspondence")),
                      data = paste0("Datasets/",
                        c("Code_Books", "Derived", "Original")),
                      doc = paste0("Documents/", c("Original", "Reading")),
                      extra = extra,
                      posted = paste0("Posted/",
                        c("Code_Books", "Data", "Documents", "Reports")))

  unix.dirs <- list(top = c("admin", "backups", "data", "doc", "extra", "lib",
                      "posted", "readMergeData", "reports",
                      "src", "test", "work"),
                    admin = paste0("admin/", c("budget", "correspondence")),
                    data = paste0("data/", c("codebook", "derived", "original")),
                    doc = paste0("doc/", c("original", "reading")),
                    extra = extra,
                    posted = paste0("posted/",
                      c("codebook", "data", "doc", "reports")))
  
  ## set up directory structure --------------------------

  if (TYPE.PROJECT== "normal"){
    DIRS <- switch(STYLE, unix = unix.dirs, windows = window.dirs)
  } else { # simple - need to think about custom later
    if (STYLE == "unix"){
      DIRS <- c("admin", "data", "doc", "posted")
    } else { # windows
      DIRS <- c("Administration", "Data", "Documents", "Posted")
    }
  }
  
  ## if 'extra' directories then set them here no error checking yet -
  ## should be list of character vetors that does not conflict which
  ## general structure just set up
  if (!is.null(extra)) {
    DIRS$extra <- extra
  }
  
  ## set up destination directories ------------------------------
  ## NB: codebooks need to come before data files in this list
  ## otherwise they will get moved to 'data/original' and not be
  ## present to move to 'data/codebook'

  DESTINATION <- list()
  if (TYPE.PROJECT== "normal"){
    DESTINATION$codebook <- switch(STYLE, unix = "data/codebook",
                                   windows = "Data/Code_Books")
    DESTINATION$data <- switch(STYLE, unix = "data/original",
                               windows = "Data/Original")
    DESTINATION$doc <- switch(STYLE, unix = "doc/original",
                              windows = "Documents/Original")
    DESTINATION$lib <- switch(STYLE, unix = "lib",
                              windows = "Library_Functions")
  } else { # simple - need to think about custom later
    DESTINATION$codebook <- switch(STYLE, unix = "data", windows = "Data")
    DESTINATION$data <- switch(STYLE, unix = "data", windows = "Data")
    DESTINATION$doc <- switch(STYLE, unix = "doc", windows = "Documents")
    DESTINATION$lib <- "."
  }

  if (!is.null(extra.destinations)) {
    DESTINATION$extra <- extra.destinations
  }

  ## set up directories for RsyntaxFiles, analysis, etc

  WORKING <- list()
  WORKING$readMerge <- ifelse(TYPE.PROJECT == "normal", DIRS$top[8], ".")
  WORKING$work <- ifelse(TYPE.PROJECT == "normal", DIRS$top[12], ".")
  WORKING$reports <- ifelse(TYPE.PROJECT == "normal", DIRS$top[9], ".")
  WORKING$dataDeriv <- ifelse(TYPE.PROJECT == "normal", DIRS$data[2], ".")
  WORKING$dataOrig <- DESTINATION$data
  WORKING$codebook <- DESTINATION$codebook

  ## return directory lists of class 'drywDestinationDirs''
  
  destDirs <- list(directories = DIRS, destinations = DESTINATION,
                   working.dirs = WORKING)
  class(destDirs) <- "drywDestinationDirs"
  destDirs
}
