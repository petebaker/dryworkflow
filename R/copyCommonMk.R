##' Copy dryworkflow package common.mk file to specified destination
##'
##' The file \code{common.mk} contains pattern rules to process
##' \code{R}, \code{Rmd} and \code{Rnw} files to output a range of
##' output files including \code{Rout}, \code{pdf}, \code{html} and
##' \code{docx} files
##'
##' The \code{common.mk} file is supplied with the \code{dryworkflow}
##' package. Ideally, this file should be placed in a specific
##' directory used for all data analysis projects. In \code{linux}
##' this would usually be something like \code{~/lib}. The definitions
##' and rules can then be used for any project by including the
##' contents with an \code{include} command placed near the end of a
##' \code{Makefile}. Individual definitions or rules can be overridden
##' by redefining them after the \code{include} statement.  The latest
##' version of \code{common.mk} is always available at
##' \url{https://github.com/petebaker/r-makefile-definitions}. Once a
##' basic Makefile is set up (usually by
##' \code{\link{createProjectSkeleton}}) then type \code{make help}
##' for more details.
##'
##' @param destination string containing directory name for copying
##'     \code{common.mk}. Default: "~/lib" for unix style set ups and
##'     $HOME/Library for windows style set ups
##' @param overwriteFile logical indicating whether to overwrite
##'     existing \code{common.mk} file: Default: FALSE
##' @param createDir whether to create destination directory if it
##'     doesn't exist: Default = FALSE
##' @return None
##' @author Peter Baker \email{pete@@petebaker.id.au}
##' @examples
##'   copyCommonMk("testMake", createDir = TRUE)
##' @export
copyCommonMk <- function(
  destination = NULL, overwriteFile = FALSE, createDir = FALSE){

  ## filename - perhaps could be a argument to function
  common.mk <- "common.mk"

  ## find common.mk in dryworkflow package -----------------------------
  source.dir <- file.path(system.file(package="dryworkflow"), "makefile.common")
  if (!file.exists(source.dir)) stop(paste("Directory", source.dir,
                                           "not found"))
  source.file <- file.path(source.dir, common.mk)
  
  ## process destination -------------------------------------------------
  config <- FALSE
  if (is.null(destination)) {
    HOME <- Sys.getenv("HOME")
    ## is there a configuration file here (or below). If so use that,
    ## otherwise use drywoptions to dset destination directory
    if (file.exists("configFile.rds")) {
      config <- TRUE
      projectConfig <- readRDS("configFile.rds")
    }
    if (file.exists(fp <- file.path("..", "configFile.rds"))) {
      config <- TRUE
      projectConfig <- readRDS(fp)      
    }
    if(config){
      if (class(projectConfig) != "drywProjectConfig"){
        warning("('projectConfig' not of class 'drywProjectConfig'")
        config <- FALSE
        style <- getOption("dryworkflow")$style
        cat("Using global 'dryworkflow' options\n")
      } else {
        style <- projectConfig$style
        config <- TRUE
        cat("Using options for project:", projectConfig$name.project, "\n")
      }
    }
    if (length(grep(" ", destination)) > 0)
      warning(paste0("Directory '", destination, "' should not contain spaces"))

    
    destination <- file.path(HOME, ifelse(style == "unix", "lib", "Library"))
    cat("Destination set to:", destination)
  }

  ## copy file --------------------------------------------------
  if (!dir.exists(destination) & createDir)
    dir.create(destination)
  commonFile <- file.path(destination, common.mk)

  if (file.exists(commonFile) & !overwriteFile)
    stop ("File", commonFile, "exists, specify 'overwriteFile' if needs be")

  file.copy(source.file, commonFile, overwrite = overwriteFile,
            copy.date = TRUE)
  cat(stringr::str_c("+++ File successfully copied to '", destination,"'\n"))

  cat("NB: The newest version of 'common.mk' is always available at\n",
      "    'https://github.com/petebaker/r-makefile-definitions'\n")
}
