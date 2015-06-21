##' Copy dryworkflow package template files to specified destination
##'
##' Copies all template \code{.txt} files from package
##' \code{dryworkflow} to a directory so that they can be modified and
##' reused. Files can be customised and then used by
##' \code{\link{createProjectSkeleton}} by specifying an alternative
##' \code{template.dir}. Note that if the directory specified already
##' exists then an error will be produced.
##'
##' Files have names like \code{template_cleanR.txt},
##' \code{template_readR.txt}, \code{template_analyseR.txt} and so
##' on. Their use should be obvious from the file name. Strings which
##' may be changed are described at the top of the template file and
##' the description will be removed from the syntax file which is
##' produced using \code{\link{createProjectSkeleton}}.
##'
##' @param destination string containing directory name for copying
##' template files. Default: a new directory "templates" in the
##' current directory
##' @return None
##' @author Peter Baker \email{pete@@petebaker.id.au}
##' @examples
##'   copyTemplates()
##' @export
copyTemplates <- function(destination = "templates"){
  
  ## find common.mk in dryworkflow package -----------------------------
  source.dir <- file.path(system.file(package="dryworkflow"), "templates")
  if (!file.exists(source.dir)) stop(paste("Directory", source.dir,
                                           "not found"))
  
  source.files <- list.files(path = source.dir,
                             pattern = ".txt$", all.files = TRUE)
  cat("Template files:\n")
  print(source.files)
  
  ## process destination -------------------------------------------------

  ## create directroy but exit if present
  if (dir.exists(destination))
    stop(stringr::str_c("Directory: '", destination, "' exists. Remove first"))
  dir.create(destination)

  ## copy files to destination
  cat("\n+++++ Template files being copied to '", destination, "'\n", sep="")
  successful.logical <-
    file.copy(file.path(source.dir, source.files), destination,
              overwrite = FALSE)

  if (all(successful.logical)) cat("All files successfully copied\n")
  
}
