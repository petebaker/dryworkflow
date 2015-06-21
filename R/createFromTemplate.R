## Filename: createFromTemplate.R
## Purpose: Create an R, Rmd, Rnw, ... file from a template
##
## To run in terminal use:  R CMD BATCH --vanilla createFromTemplate.R

## Created at:  Tue Apr 14 12:25:20 2015
## Author:      Peter Baker
## Hostname:    sph-ph-428-04p.sph.uq.edu.au
## Directory:   /home/pete/Data/R.workflow/Rpackage-201501/src/R/
## Licence:     GPLv3 see <http://www.gnu.org/licenses/>
##
## Change Log: 
##

## First like could be put in as AT title

##' Create an R, Rmd or Rnw file from a template
##'
##' Creates and writes an \code{R} syntax or report file from a
##' template by substituting for specified strings. By default, these
##' templates are provided as part of the \code{\link{dryworkflow}}
##' package. In order to customise these files to a particular project
##' or style of working, alternate templates and string formats can be
##' provided.
##'
##' By default, strings in the template file that look like
##' @@@@SYNTAX.FILE@@@@ and @@@@DATA.FILE@@@@ are substituted with
##' strings provided as elements in a list with named components
##' SYNTAX.FILE, DATA.FILE and so on provided as the \code{subst.strings}
##' argument. The string prefix and suffix can be changed but must be
##' the same throughout the template.
##'
##' @param file.name full filename of file to be written provided as a
##' string and including directory if necessary
##' @param subst.strings named list of string substitutions
##' @param template name of template text file as string. Default:
##' \code{NULL} for predefined template from \code{\link{dryworkflow}}
##' package.
##' @param template.dir directory containing template. Default:
##' \code{\link{dryworkflow}} package directory
##' @param overwrite.file logical whether or not to allow overwrite of
##' existing file. Default: \code{FALSE}
##' @param string.prefix string of characters for start of strings to
##' be substituted from template file. Default \sQuote{@@@@}
##' @param string.suffix string of characters for end of strings to be
##' substituted from template file. Default: same as
##' \code{string.prefix}
##' @param delete.start lines between and including those containing
##' the \code{delete.start} and \code{delete.end} patterns will be
##' removed. Default: \dQuote{---- START: DELETE THIS SECTION ----}
##' @param delete.end Default: \dQuote{---- END: DELETE THIS SECTION ----}
##' @param print.mismatches logical to declare wther to print warnings
##' about unused and undefined strings. Default: \code{FALSE}
##' @return None
##' @author Peter Baker \email{pete@@petebaker.id.au}
createFromTemplate <- function(
  file.name, subst.strings, template, template.dir,
  overwrite.file = FALSE,
  string.prefix = "@@", string.suffix = string.prefix,
  delete.start = "-- START: DELETE THIS SECTION --",
  delete.end = "-- END: DELETE THIS SECTION --",
  print.mismatches = FALSE){

  ## -------------------------------------------------------------
  ## process function arguments ----------------------------------
  ## -------------------------------------------------------------

  ## extract filename and directory
  x <- basename(file.name)
  dir.x <- dirname(file.name)
  
  ## x - filename for writing ----------------------------------------------
  if (!is.character(x))
    stop(paste0("Filename '", x, "' should be a character string"))
  if (length(grep(" ", template)) > 0)
    stop(paste0("Filename '", x, "' should not contain spaces"))

  ## dir.x - for writing file x ------------------------------------------
  if (is.null(dir.x)){
    stop("dir.x' must be specified")
  } else {
    if (!file.exists(dir.x))
      stop(paste0("destination directory '", dir.x, "'for '", x, "' not found"))
  }
  
  ## subst.strings ---------------------------------------------
  if (!is.list(subst.strings)) stop("'subst.strings' must be a list")
  if (!all(sapply(subst.strings, is.character)))
    stop("All components of 'subst.strings' must be character strings")

  ## overwrite.file ------------------------------------------------------
  if (overwrite.file){
    if (file.exists(file.name)) file.remove(file.name)
  } else{
    if (file.exists(file.name))
      stop("file '", x, "' found in '", dir.x, "'. Remove first?")
  }

  ## ## file.string -------------------------------------------------------
  ## if (!is.character(file.string)){
  ##   stop(paste0("'file.string' must be a character string"))
  ## }
  ## ## check that 'file.string' is not in 'subst.strings' but if it is,
  ## ## make sure its sensible
  ## if (file.string %in% names(subst.strings)){
  ##   warning("'file.string' also found in 'subst.strings'")
  ##   if (subst.strings[[file.string]] != x)
  ##     stop("Inconsistent definitions of file to be written\n",
  ##          paste0("File to be written 'x' = ", x),
  ##          paste0("'subst.strings file' = ", subst.strings[[file.string]]))
  ## } else{ # otherwise set it for 'x'
  ##   subst.strings[[file.string]] <- x
  ## }
  
  ## ------------------------------------------------------------------
  ## process template file --------------------------------------------
  ## ------------------------------------------------------------------
  
  ## read template file ----------------------------------------
  template.txt <-
    readTemplate(template = template, template.dir = template.dir,
                 delete.start = delete.start, delete.end = delete.end)
  
  ## find strings in file -------------------------------------------------
  strings4change <-
    extractSubstStrings(template.txt,
                        string.prefix = string.prefix,
                        string.suffix = string.suffix)

  ## Warn about mismatches - filename already extracted from x  -----------
  if (print.mismatches){
    missing.strings <- setdiff(strings4change, names(subst.strings))
    if (length(missing.strings)>0){
      warning("These strings missing and so will need to be set manually: ",
              paste(missing.strings, collapse = ", "))
    }
    extra.strings <- setdiff(names(subst.strings), strings4change)
    if (length(extra.strings)>0){
      warning("Extra strings defined but will not be used: ",
              paste(extra.strings, collapse = ", "))
    }
  }
  
  ## replace filename - but now just incorporated into subst.strings
  ## template.txt <- 
  ##   stringr::str_replace_all(
  ##     template.txt,
  ##     stringr::str_c(string.prefix, file.string, string.suffix), x)
  
  for (y in names(subst.strings)){
    sub.name <- stringr::str_c(string.prefix, y, string.suffix)
    noStrings <- length(subst.strings[[y]])
    if (noStrings == 1){
      template.txt <-
        stringr::str_replace_all(template.txt, sub.name, subst.strings[[y]])
    } else { ## multiline replacement! roll your own # assume full line
      while(length(toReplace <- grep(y, template.txt))>0){
        template.txt <-
          c(template.txt[c(1:(toReplace-1))], subst.strings[[y]],
            template.txt[c((toReplace+1):length(template.txt))])
      }
    }
  }

  ## ------------------------------------------------------------------
  ## write new file ---------------------------------------------------
  ## ------------------------------------------------------------------

  file2write <- file(file.name, "wt")
  writeLines(template.txt, file2write)
  close(file2write)

  cat(stringr::str_c("File: '", file.name, "' written\n"))
}

## debugging
## x <- "read-data1_csv.R"
## dir.x <- "myRproject/readMergeData"
## lib.dir <- "../lib"
## my.lib.files <- c("prestend.R", "tttt.R")
## my.libs <- stringr::str_c("source(file.path('", lib.dir,
##                           "', '", my.lib.files, "')")                   
## libs <- c("require(dryworkflow) # Hmisc etc will be auto loaded",
##           "require(foreign)", "require(Hmisc)")
## subst.strings <- list(DATE.CREATED = date(), ## LICENCE MISSING!
##                       DATAFILE = "data1.csv",
##                       DIR.DATAFILE = "../data/original",
##                       RDATA.FILE = "../data/derived/data1_csv.RData",
##                       LIBRARIES = libs,
##                       DATAFRAME = "data1",
##                       READ.DATA.COMMAND = "read.csv",
##                       READ.CODEBOOK = "### Code book not used",
##                       MYLIB.DIR = "../lib",
##                       MYLIB.FILES= my.libs)
## ## type <- "readR" # not used in this function but outside
## template <- "template_readR.txt"
## template.dir <- "templates"
## file.string <- "FILENAME"
## delete.start <- "-- START: DELETE THIS SECTION --"
## delete.end <- "-- END: DELETE THIS SECTION --"
## overwrite.file <- FALSE

## createFromTemplate("read_data1_csv.R", dir.x = "myRproject/readMergeData",
##                    subst.strings = subst.strings,
##                    template = "template_readR.txt", template.dir = template.dir)
