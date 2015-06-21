##' Copy dryworkflow package file .gitignore to specified destination
##'
##' The file \code{.gitignore} contains patterns and file names
##' indicating which files are not to be tracked by \code{git}. This
##' is copied to a destination directory or the current directory if
##' not specified.
##' 
##' The file \code{.gitignore} is supplied with the \code{dryworkflow}
##' package. The file is called \code{DOTgitignore} and, by default,
##' renamed automatically to \code{.gitignore} and copied to the
##' current directory. Various output from \code{R}, intermediate
##' files from \code{latex} and \code{knitr} are specified as files
##' which \code{git} does not track. Note that on operating systems
##' like \code{linux}, files beginning with a dot (.) are hidden and so
##' to unhide \code{.gitignore} the file name is modified to start
##' with the letters \code{DOT}. However, to work effectively with
##' \code{git}, the file must be named \code{.gitignore}.
##'
##' @param destination string containing directory name for copying
##'     \code{gitignore}. Default: current directory
##' @param overwriteFile logical indicating whether to overwrite
##'     existing \code{.gitignore} file: Default: FALSE
##' @param createDir whether to create destination directory if it
##'     doesn't exist: Default = FALSE
##' @param renameDotGitignore logical, whether to rename
##'     \code{DOTgitignore} to \code{.gitignore}. Default: TRUE
##' @return None
##' @author Peter Baker \email{pete@@petebaker.id.au}
##' @examples
##'   copyGitIgnore("testGit", createDir = TRUE, renameDotGitignore = FALSE)
##' @export
copyGitIgnore <- function(
  destination = NULL, overwriteFile = FALSE, createDir = FALSE,
  renameDotGitignore = TRUE){

  if (is.null(destination)) destination <- "."
    
  ## filename - perhaps could be a argument to function
  gitignoreSrc <- gitignore <- "DOTgitignore"
  if (renameDotGitignore){
    gitignore <- ".gitignore"
  }
    
  ## find gitignore in dryworkflow package -----------------------------
  source.dir <- file.path(system.file(package="dryworkflow"), "git")
  if (!file.exists(source.dir)) stop(paste("Directory", source.dir,
                                           "not found"))
  source.file <- file.path(source.dir, gitignoreSrc)
  if (!file.exists(source.file)) stop(paste("File:", source.file,
                                           "not found"))
  
  ## process destination -------------------------------------------------
  config <- FALSE
  if (length(grep(" ", destination)) > 0){
    warning(paste0("Directory '", destination, "' should not contain spaces"))
    cat("Destination is:", destination)
  }

  ## copy file --------------------------------------------------
  if (!dir.exists(destination) & createDir)
    dir.create(destination)
  gitFile <- file.path(destination, gitignore)

  if (file.exists(gitFile) & !overwriteFile)
    stop ("File", gitFile, "exists, specify 'overwriteFile' if needs be")

  file.copy(source.file, gitFile, overwrite = overwriteFile,
            copy.date = TRUE)
  cat(stringr::str_c("+++ File '", gitignoreSrc, "' successfully copied to '",
                       destination, "' as '", gitignore, "'\n"))

  ## cat("NB: The newest version of 'gitignore' is always available at\n",
  ##     "    'https://github.com/petebaker/r-makefile-definitions'\n")
}
