##' Get dryworkflow configuration from configFile.rds file
##'
##' Configuration file \code{configFile.rds} is created with
##' \code{\link{createProjectSkeleton}} when a project is created. It
##' contains details like directory structures and various parameters
##' for a project.
##' 
##' @param projectDir base directory of dryWorkflow project
##' @param checkAbove check directory above current directory for
##' presence of configuration file
##' @param checkSubDirs check directories below current directory for
##' presence of configuration file(s). If there is more than one then
##' print locations. If there is only one, print a message indicating
##' which project directory is present and use that.
##'
##' @return object of class \dQuote{drywProjectConfig} else FALSE if
##' file \code{configFile.rds} is not found or if object not of
##' correct class
getProjectConfig <- function(
  projectDir = ".",
  checkAbove = FALSE,
  checkSubDirs = FALSE)
{

  if (!(dir.exists(projectDir)))
    stop("Project directory:", projectDir, "not found")

  ## check project directory ----------------------------------------
  if (projectDir =="."){
    fp <- "configFile.rds"
  } else {
    fp <- file.path(projectDir, "configFile.rds")
  }

  if (checkAbove & checkSubDirs){
    stop("Please specify only one of 'checkAbove' or 'checkSubDirs'")
  }
  
  ## check directory above if specified -----------------------------
  if (checkAbove){
    fp <- file.path("..", "configFile.rds")
  }

  ## check directory(s) below if specified -----------------------------
  if (checkSubDirs){
    checkDirs <- setdiff(list.dirs(), ".") # all subdirectories not "."
    if (length(checkDirs) > 0){
      allConfigs <-
        list.files(checkDirs, "configFile.rds", full.names = TRUE)
    }
    if (length(allConfigs) > 1){
      cat("Multiple possibilities for configuration files:\n")
      print(allConfigs)
      stop("Please specify only one configuration file")
    } else {
      fp <- allConfigs
    }
  }
  
  ## tidy up and return config object ---------------------------------
  projectConfig <- readRDS(fp)
  
  if (class(projectConfig) != "drywProjectConfig"){
    warning("('projectConfig' not of class 'drywProjectConfig'")
    projectConfig <- FALSE
  }

  ## return project configuration
  projectConfig
}
