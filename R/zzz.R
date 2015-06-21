## Filename: zzz.R
## Purpose: .onLoad function for dryWorkflow package
##

## Created at:  Wed Mar 18 14:47:27 2015
## Author:      Peter Baker
## Hostname:    sph-ph-428-04p.sph.uq.edu.au
## Directory:   /home/pete/Data/R.workflow/Rpackage-201501/src/R/
## Licence:     GPLv3 see <http://www.gnu.org/licenses/>
##
## Change Log: 
##

## https://stat.ethz.ch/pipermail/r-devel/2011-June/061279.html

##' Read template file into a string
##'
##' Internal function used by functions when checking or creating
##' syntax and markdown files
##'
##' Note that lines between (and including) lines containing the
##' patterns \dQuote{---- START: DELETE THIS SECTION ----} and
##' \dQuote{---- END: DELETE THIS SECTION ----} will be deleted from
##' the template
##' 
##' @param template name of template text file as string. Default:
##' \code{NULL} for predefined template from \code{\link{dryworkflow}}
##' package.
##' @param template.dir directory containing template. Default:
##' \code{\link{dryworkflow}} package directory
##' @param delete.start lines between and including those containing
##' the \code{delete.start} and \code{delete.end} patterns will be
##' removed. Default: \dQuote{---- START: DELETE THIS SECTION ----}
##' @param delete.end Default: \dQuote{---- END: DELETE THIS SECTION
##' ----}
##' @keywords internal
##' @return vector string containing complete \code{template} with a
##' string for each line of the template file
##' @author Peter Baker \email{pete@@petebaker.id.au}
readTemplate <- function(
  template, template.dir = NULL,
  delete.start = "-- START: DELETE THIS SECTION --",
  delete.end = "-- END: DELETE THIS SECTION --"){

  ## template --------------------------------------------------
  if (!is.character(template)){
    stop(paste0("'template' must be a character string"))
  }
  if (length(grep(" ", template)) > 0)
    warning(paste0("Filename '", template, "' should not contain spaces"))
  ## process template.dir -------------------------------------------------
  if (is.null(template.dir)) {
    template.dir <- file.path(system.file(package="dryworkflow"), "templates")
  } else {
    if (!file.exists(template.dir)) stop(paste("Directory", template.dir,
                                               "not found"))
  }
  TEMPLATE <- file.path(template.dir, template)
  if (!file.exists(TEMPLATE))
    stop(paste0("Template file '", TEMPLATE, "' not found"))

  ## read template file ----------------------------------------------
  template.txt <- readLines(TEMPLATE)
  
  ## drop any lines specified in template -----------------------------
  preamble.start <- grep(delete.start, template.txt)
  preamble.end <- grep(delete.end, template.txt)
  for (I in 1:length(preamble.start)){        # drop preamble
    template.txt <- template.txt[-c(preamble.start[I]:preamble.end[I])]
  }
  
  template.txt

}

## ## for debugging
## template <- readTemplate("template_readR.txt", template.dir = "templates")

##' extract strings for substitution from template for checking etc
##'
##' Given a template vector \code{template} read in from a template
##' file and string format via \code{string.prefix} and
##' \code{string.sufffix}, extract a vector of all strings to be
##' substituted.
##' 
##' @param template character vector where each element is the lines
##' of a template file
##' @param string.prefix string of characters for start of strings to
##' be substituted. Default \sQuote{@@@@}
##' @param string.suffix string of characters for end of strings to be
##' substituted. Default: same as \code{string.prefix}
##' @keywords internal
##' @return vector of strings for substitution
##' @seealso \code{\link{readTemplate}} reads a template file into a
##' string, \code{\link{createFromTemplate}} creates a new file from a template
extractSubstStrings <- function(
  template, string.prefix = "@@", string.suffix = string.prefix){

  ## process arguments ------------------------------------------------
  if (mode(template) != "character")
    stop(paste0("Template '", template, "' should be a character vector"))

  if (string.suffix == string.prefix){
    splitLines <-
      stringr::str_split(template, string.suffix)
  } else {
    warning("largely untested with different 'string.prefix' and 'string.suffix'")
    splitLines <-
      stringr::str_split(template, stringr::str_c(string.prefix, "|",
                                                  string.suffix))
  }
  splitLines <-
    splitLines[sapply(splitLines, function(y) length(y) >1)] # drop if no @@
  possibleStrings <- # pull out every even no. string
    unique(unlist(lapply(splitLines, function(y) y[1:length(y) %% 2 == 0])))

  possibleStrings
}


##' Provide appropriate read commands for reading data files
##'
##' Using the filename extensions, such as \code{.csv} or \code{.dta},
##' a best match is determined for inclusion in \code{R} syntax files
##' for reading data. Suggested commands can be changed or extras
##' added by using the \code{other} option.
##' 
##' @param data.files character vector of file names
##' @param commands list of strings to be substituted into R syntax
##' files to read each file in \code{data.files}
##' @param other list of either alternate or extra commands for
##' reading data files
##' @param extra.args extra arguments that will be be added to the
##' read command produced
##' @param unsupported string to use if filetype not supported 
##' @keywords internal
##' @return named vector of characters to substitute for read commands
##' in \code{R} syntax file. The \dQuote{extraLibs} attribute is a
##' character vector of library commands which can be incorporated
##' into syntax files although this is unnecessary because functions
##' are called directly
##' @author Peter Baker
whichReadCommand <- function(
  data.files,
  commands = list(tsv = "read.delim", csv = "read.csv",
    dta = "foreign::read.dta",
    sas7bdat = "sas7bdat::read.sas7bdat",
    xport = "foreign::read.xport",
    sav = "foreign::spss.get",
    rdata = "load", rda = "load",
    xls = "readxl::read_excel", xlsx = "readxl::read_excel"),
  other = NULL,
  extra.args = list(tsv = NA, csv = "na.strings = c(\"\", \"NA\", \".\")",
    dta = NA,
    sas7bdat = NA,
    xport = NA,
    sav = NA,
    rdata = NA, rda = NA,
    xls = NA, xlsx = NA),
  unsupported = "UNSUPPORTED:PUT_READ_FUNCTION_HERE"){

  ## process other and user defined read commands -----------------
  if (is.null(other)){
    overwrite.commands <- intersect(names(other), names(commands))
    commands[overwrite.commands] <- other[overwrite.commands]
    add.commands <- setdiff(names(other), names(commands))
    commands <- c(commands, other[add.commands])
  }

  ## process file extensions present  -------------------------
  TYPES <- unique(dataFile.type <- tolower(tools::file_ext(data.files)))
  READ.COMMANDS <- unlist(commands[TYPES])
  ## find any libraries present
  extraLibs <- unlist(lapply(strsplit(unlist(READ.COMMANDS), split = "::"),
                             function(x) if(length(x) > 1)
                                           paste0("library(", x[1], ")")))

  ## process extra arguments if present
  EXTRA.ARGS <- unlist(extra.args[TYPES])
  extraArgs <- unlist(lapply(EXTRA.ARGS,
                             function(y) ifelse(is.na(y), "",
                                                stringr::str_c(", ", y))))
  
  ## set commands for each file ------------------------------
  readDataExts <- unlist(commands[TYPES])
  read.data.command <- readDataExts[dataFile.type]
  names(read.data.command) <- data.files
  
  attr(read.data.command, "extraLibs") <- unlist(extraLibs)
  attr(read.data.command, "extraArgs") <- extraArgs
  read.data.command
}

## for debugging
## template <- readTemplate("template_readR.txt", template.dir = "templates")
## string.prefix = "@@"; string.suffix = string.prefix
## extractSubstStrings(template)

## whichReadCommand(filesAndDFs[["dataFiles"]])
## test <- whichReadCommand(filesAndDFs[["dataFiles"]])
## data.files <- filesAndDFs[["dataFiles"]]



## Filename: init.R
## Purpose: initialise dryworkflow package based in xcms in Bioconductor
##          However, the function arguments are not used at all - fix?
##
## To run in terminal use:  R CMD BATCH --vanilla init.R

## Created at:  Fri Sep 26 23:43:55 2014
## Author:      Peter Baker
## Hostname:    clearwell2.fritz.box
## Directory:   /home/pete/Data/R.workflow/Rpackage-201501/src/R/
## Updated from:   /home/pete/Data/R.workflow/ideas/
## Licence:     GPLv3 see <http://www.gnu.org/licenses/>
##
## Change Log: 2015-01-14 at 14:30:07: Updated to new S3 package coding
##                                     in Rpackage-201501
##

## NB: instead of cat and maybe print to output messages you should
## probably use message which can then be supressed with
## suppressPackageStartupMessages and suppressMessages - see ?message


##' Internal function for setting global dryworkflow options
##' @keywords internal
.setDRYWOptions <- function() { # ??? should be less!

  ## Neither argument used - but this is default for any package
  
  ## TODO -----------------------------------------------------------

  ## set defaults if not already set --------------------------------------
  
  ## NB: can be set in .Rprofile and overridden in options to
  ##     createProjectSkeleton

  ## current.opts <- options()
  ## options("dryworkflow")  
  ## .setDRYWOptions()
  ## options("dryworkflow")
  ## options(current.opts)

  current.opts <- options()

  ## hardwired defaults ---------------------------------------------
  
  DIR.PROJECT <- "myRproject" # directory name for project - pass1??
  NAME.PROJECT <- "My Data Analysis Project" # name of project on output
  TITLE.PROJECT <- "My Data Analysis Project" # title for reports
  TYPE.PROJECT <- "normal" # normal, simple, custom (custom NYI)
  STYLE <- "unix"    # c("unix", "windows") style for directory and filenames
  ## data types and data source directories
  DATA.SRC <- "."    # source directory for data files - multiple OK
  DATA.EXT <- c(".xls", ".xlsx", ".csv", ".dta", ".sav", ".xpt", ".RData", ".rda")
  DATA.MV <- TRUE  # default move rather than copy
  ## document types and source directories
  DOC.SRC <- "."    # source directory for doc files - multiple OK
  DOC.EXT <- c(".doc", ".docx", ".odt", ".tex")
  DOC.MV <- TRUE
  ## codebook types and source directories  
  CODE_BK.SRC <- "."    # source directory for codebook files - multiple OK
  CODE_BK.EXT <- paste0("_codebook.", c("csv", "xls", "xlsx"))
  CODE_BK.MV <- TRUE
  LIB.SRC <- "."    # library files - R syntax directory - multiple OK
  LIB.EXT <- c(".R")
  LIB.MV <- TRUE # default move but should it be copy rather than move
  FORCE <- FALSE # whether to force move/copy new files if old files exist
  ## vector of files that will not be moved from current directory
  DONTMOVE <- "setupProject.R"
  LOG.EXT <- ".txt" # extension of log file(s) orgmode or .txt file(s)
  ## report template currently produced for first data file only
  REPORT.MARKDOWN <- c("Rmd", "Rnw") # output styles html, doc, org,
                                     # Rmd, tex - vector
  REPORT.TYPES <- "all" # output styles html, docx, pdf - vector
  REPORT.WHICH <- "first" # c("first", "merge", "all") which data
                          # files to produce reports for. Default: first
  AUTHOR <- "-- Insert author here --" # author for reports
  ADDRESS <- "-- Insert address here --" # address for reports
  TABLE1 <- FALSE              # logical: produce table 1 reports
  CUSTOM.DIRS <- "custom"       # list of custom directories - user defined
  MNEMONIC <- ""               # three or four letter mnemonic for naming
  EXTRA <- "extra"             # extra directories additional to standard setup
  COMMON.MK.DIR <- switch(STYLE,         # common library for make & functions 
                          unix = "~/lib", 
                          windows = "${HOME}/Library")
  COMMON.MK.FILE <- "common.mk"         # name of common make file for
                                        # inclusion in each Makefile
  LICENCE <- "GPL3 see <http://www.gnu.org/licenses/>"
  TEMPLATE.DIR <- file.path(system.file(package="dryworkflow"), "templates")
  template.choices <- c("readR", "cleanR", "summaryR", "analyseR",
                        "codebookR", "mergeAllR", "compareR",
                        "reportRmd", "reportRnw", "presentRmd", "beamerRmd",
                        "beamerRnw", "make", "custom")
  TEMPLATES <- stringr::str_c("template_", template.choices, ".txt")
  names(TEMPLATES) <- template.choices
  PRINT.MISMATCHES <- FALSE # print mismatches when creating syntax or
                            # markdown files from templates
  
  ## put all options above into character vector ------------------
  
  OPTIONS <- c("DIR.PROJECT", "NAME.PROJECT", "TITLE.PROJECT",
               "TYPE.PROJECT", "STYLE", "FORCE", "DONTMOVE", "LOG.EXT",
               "REPORT.MARKDOWN", "REPORT.TYPES", "REPORT.WHICH",
               "AUTHOR", "ADDRESS", "TABLE1", "CUSTOM.DIRS",
               "MNEMONIC", "EXTRA", "COMMON.MK", "LICENCE",
               "TEMPLATE.DIR", "TEMPLATES", "PRINT.MISMATCHES")

  INITIAL.FILES <- c("CODE_BK.SRC", "CODE_BK.EXT", "DATA.SRC", "DATA.EXT",
                     "DOC.SRC", "DOC.EXT", "LIB.SRC", "LIB.EXT")
  IFILES.DEFAULT <-
    list(codebook =
             list(src = CODE_BK.SRC, ext = CODE_BK.EXT, move = CODE_BK.MV),
         data = list(src = DATA.SRC, ext = DATA.EXT, move = DATA.MV),
         doc = list(src = DOC.SRC, ext = DOC.EXT, move = DOC.MV),
         lib = list(src = LIB.SRC, ext = LIB.EXT, move = LIB.MV))

  COMMON.MK <- list(dir = COMMON.MK.DIR, file = COMMON.MK.FILE)

  ## directory structure - two level --------------------------

  ## I really think this should be implemented when setting up project
  ## skeleton and not as options

  ## set up directory names depending on filename style
  ## simple just has a subset of these and 'custom' - not yet implemented
  ## window.dirs <- list(top = c("Administration", "Backups",
  ##                       "Datasets", "Documents", "Extra", "Library_Functions",
  ##                       "Original", "Posted", "Reading",
  ##                       "Read_And_Merge_Data", "Reports", "Source",
  ##                       "Testing", "Working"),
  ##                     admin = paste("Administration/",
  ##                       c("Budget", "Correspondence"), sep=""),
  ##                     data = paste("Datasets/",
  ##                       c("Code_Books", "Derived", "Original"), sep=""),
  ##                     doc = paste("Documents/", c("Original", "Reading"),
  ##                       sep=""),
  ##                     custom = CUSTOM.DIRS,
  ##                     posted = paste("Posted/",
  ##                       c("Code_Books", "Data", "Documents", "Reports"),
  ##                       sep=""))

  ## unix.dirs <- list(top = c("admin", "backups", "data", "doc", "extra", "lib",
  ##                     "posted", "readMergeData", "reports",
  ##                     "src", "test", "work"),
  ##                   admin = paste("admin/", c("budget", "correspondence"),
  ##                     sep=""),
  ##                   data = paste("data/", c("codebook", "derived", "original"),
  ##                     sep=""),
  ##                   doc = paste("doc/", c("original", "reading"), sep=""),
  ##                   custom = CUSTOM.DIRS,
  ##                   posted = paste("posted/",
  ##                     c("codebook", "data", "doc", "reports"), sep=""))

  ## simple project ---------------------------------------------------

  ## doc, data (original) and everything else in root directory ??
  
  ## not used just yet - possibly don't need this as only have one package
  ## but potentially good to check if already set in .Rprofile
  ## if (! any(is.na(dryw.opt))) {
  ##   if (class(dryw.opt) != "drywPkg")
  ##     stop("obviously invalid package options !")
    
  ##   dryw <- getOption("dryworkflow")
  ##   dryw$dryworkflow <- dryw.opt
  ##   options("dryworkflow"=dryw)
  ##   return()
  ## }

  ## add dryworkflow specific options - this initialises list!
  ## (not unlike what is done in 'affy')
  ## NB: Could use current.opts$dryworkflow too
  if (is.null(getOption("dryworkflow"))) {
    dryw <- list()
    class(dryw) <- "drywOptions"
    options("dryworkflow"=dryw)
  }

  ## get options for dryw -------------------------
  ##   only set defaults if not already set

  DryW <- getOption("dryworkflow")

  for (O in OPTIONS){ # O <- DIR.PROJECT
    if (is.null(OPT <- getOption("dryworkflow")[[tolower(O)]])) {
      DryW[[tolower(O)]] <- get(O)
    } ##else {
      ##DryW[[tolower(O)]] <- OPT # surely if its there than you don't need this
    ##}
  }

  ## initial file characteristics --------------------------

  if (is.null(IFILES <- getOption("dryworkflow")[["inital.files"]])) {
    DryW[["inital.files"]] <- IFILES.DEFAULT
  } else {
    for (I in names(IFILES.DEFAULT)){
      for (J in names(IFILES.DEFAULT[[1]])){
        if(is.null(DryW[["inital.files"]][[I]][[J]])){
          DryW[["inital.files"]][[I]][[J]] <- IFILES.DEFAULT[[I]][[J]]
        }
      }
    }
  }

  ## check that git is set up on system ---------------------------------
  DryW[["git"]] <- list(present = NULL, user.name = NULL, user.email = NULL)
  if (length(system("git --version", intern = TRUE))<=0){
    warning("git not found on system. Please put install git or fix PATH")
    DryW[["git"]][["present"]] <- FALSE
  } else {
    DryW[["git"]][["present"]] <- TRUE    
  }

  ## if (DryW[["type.project"]] == "normal"){
  ##   if (DryW[["style"]] == "unix"){
  ##     DryW[["dir.structure"]] <- unix.dirs
  ##   } else {
  ##     DryW[["dir.structure"]] <- window.dirs
  ##   }
  ## } else { # simple - need to think about custom
  ##   if (DryW[["style"]] == "unix"){
  ##     DryW[["dir.structure"]] <- c("admin", "doc", "posted")
  ##   } else {
  ##     DryW[["dir.structure"]] <- c("Administration", "Documents", "Posted")
  ##   }
  ## }

  ## destination directories - needs to be set up after style & type

  ## set up a default dir for doc, data, .R, under all scenarios
  ## maybe DESTINATION.DOC, DESTINATION.DATA, DESTINATION.LIB and
  ## anything else thats required so that when directory structure
  ## defined these are too
  ## NB: if not defined then work out below form the TYPE.PROJECT,
  ##     STYLE, and directories

  ## DESTINATION.DOC <- ifelse(STYLE=="unix", "doc/original", "Documents/Original")
  ## DESTINATION.DATA <- ifelse(STYLE=="unix", "data/original", "Data/Original")
  ## DESTINATION.CODEBOOK <- ifelse(STYLE=="unix", "data/codebook",
  ##                                "Data/Code_Books")
  ## DESTINATION.LIB <- ifelse(STYLE=="unix", "lib", "Library_Functions")
  
  ## DESTINATION <- c("DESTINATION.DOC", "DESTINATION.DATA",
  ##                  "DESTINATION.CODEBOOK", "DESTINATION.LIB")
  
  ## for (D in DESTINATION){ # D <- DESTINATION.DOC
  ##   if (is.null(DEST <- getOption("dryworkflow")[[D]])) {
  ##     DryW[[tolower(D)]] <- get(D)
  ##   } else {
  ##     DryW[[tolower(D)]] <- DEST
  ##   }
  ## }
    
  ## set all options globally
  
  options("dryworkflow"=DryW)
  
  ## use this as
  ## if(as.logical(getOption("dryw$type", TRUE)))
  ##   cat("do cleanup\n")
}

## getOption("dryworkflow")

## which to use
## Usage:

##      .onLoad(libname, pkgname)
##      .onAttach(libname, pkgname)
##      .onUnload(libpath)
##      .onDetach(libpath)
##      .Last.lib(libpath)

## I think this is OK but now moved to zzz.R (and somewhat different too)

##' Internal function called when library is loaded to set options
##' @keywords internal
.onLoad <- function(libname, pkgname){
  .setDRYWOptions()
  invisible()
}

## Probably best to write a little message here which is writen to
## console when package attached
##' Internal function called when library is attached
##' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to dryworkflow Version ",
                        utils::packageVersion("dryworkflow"))
}

## .onAttach("dryWorkflow") # or are these in NAMESPACE and auto called ???

