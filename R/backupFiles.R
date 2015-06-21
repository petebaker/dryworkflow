##' Backup original, R files, R report and posted files to allow regenerating
##'
##' Zips all relevant files to allow work to be reproduced although,
##' for safety, it is best to make your own backup.
##'
##' While \code{backupFiles} provides a way to backup all relevant
##' files to enable work to be reproduced, it is best to have a more
##' comprehensive personalised strategy tailored to your own
##' circumstances. In particular, it may be more prudent to modify
##' \code{Makefile} to backup off site by using make. See Makefile and
##' type \code{make help rsync} at the command or shell prompt to help
##' automate remote backup. Note that this function should be called
##' from the main project directory not a work/reporting sub directory
##'
##' @param zipFile string containing name for zip file. Default:
##' \code{NULL} whereby name is derived from today's date and the
##' project directory name
##' @return None
##' @author Peter Baker \email{pete@@petebaker.id.au}
backupFiles <- function(zipFile = NULL){

  ## somehow put this in somewhere - testhat??
  ## @examples
  ##   backupFiles()
  
  ## check to see if project configuration is available ----------------
  if (file.exists("configFile.rds")){
    projectConfig <- readRDS("configFile.rds")
  } else {
    stop("backupFiles can only be called from the main project directory")
  }
  
  cat("Important files for", projectConfig$project.dir, "will be zipped\n",
      "NB: Do not rely on this. Always make you own backup!\n")

  dir.project <- projectConfig$settings$dir.project
  zipFile <-
    stringr::str_c(dir.project, "_", as.character(lubridate::today()), ".zip")
    ## stringr::str_c(dir.project, lubridate::today(), ".zip")
  cat("+++ Files will be zipped to", zipFile, "\n\n")

  ## files to backup -------------------------------------------------
  pc <- projectConfig
  full.backed.up <- c(pc$projectDirs$working.dirs$dataOrig,
                      pc$projectDirs$working.dirs$codebook,
                      pc$projectDirs$destinations$lib,
                      pc$projectDirs$directories$posted,
                      pc$projectDirs$directories$doc)
  dirs.syntax.R <- c(pc$projectDirs$working.dirs$readMerge,
                     pc$projectDirs$working.dirs$work)
  dirs.syntax.Rmd <- c(pc$projectDirs$working.dirs$reports)

  allFiles <- c(list.dirs(full.backed.up),
                list.files(dirs.syntax.R, pattern = ".R$|Makefile",
                           full.names = TRUE),
                list.files(dirs.syntax.Rmd, pattern = ".Rmd$|.Rnw$|Makefile",
                           full.names = TRUE), "Makefile", "configFile.rds")


  cat("+++ Files to be backed up:\n")
  print(allFiles)
  
  zipTest <- zip(file.path("..", zipFile), allFiles)
  if (zipTest == 0) cat("All files successfully zipped\n")
  
}
