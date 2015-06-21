##' add a new code book, data file, document or R file to project
##'
##' add a code book, data or document file to a \code{dryworkflow}
##' project. If appropriate, relevant R files will be created and
##' Makefiles, log files and git repository updated
##'
##' @param file.name name of file to add to project as a string
##' @param move whether to move instead of copy file Default: TRUE
##' @param projectDir base directory of \code{dryworkflow} project
##' directory. Default: \dQuote{myProject}
##' @param replace logical, if file exists then replace. Default: FALSE
##' @return logical TRUE/FALSE indicating success of adding file
addFile <- function(
  file.name,
  move = TRUE,
  projectDir = "myProject",
  replace = FALSE)
{

  ## is there a configuration file here If so use that, else exit
  ## but in future could make it more flexible

  ## check options  ---------------------------------------------
  if (!file.exists(file.name)) stop("File not found: ", file.name)
  if (!dir.exists(projectDir)) stop("Directory not found: ", projectDir)
  if (!is.logical(move)) stop("move must be TRUE or FALSE")
  if (replace)
    stop("Option not yet implemented for safety reasons\n       Please remove file manually")

  ## could modify this later rewrite of not in top level directory
  baseDir <- projectDir
  
  ## determine project config -------------------------------------------
  ## use project configuration if found - otherwise use global options??
  ## needs some more thought
  ##projectConfig <- getProjectConfig(projectDir = projectDir, checkAbove = TRUE)
  projectConfig <- getProjectConfig(projectDir = projectDir)

  cat("++++ Project Configuration Details:\n")
  print(comment.pc <- comment(projectConfig))
  
  ## determine log file to add details of file added -------------------
  setup.log <- stringr::str_c("setup_", projectConfig$settings$dir.project,
                              projectConfig$settings$log.ext)
  if (file.exists(setup.log)){
    addToSetup <- TRUE
  } else {
    stop("Please start 'addFile' in directory containing Project Directory: ",
         projectConfig$settings$dir.project)  
  }
  ## Obsolete?
  ## if (!projectConfig){
  ##   projectConfig <- getOption("dryworkflow")
  ##   cat("Using global 'dryworkflow' options\n")
  ## } else {
  ##   cat("Using options for project:", projectConfig$name.project, "\n")
  ## }
  style <- projectConfig$style

  ## determine type of file --------------------------------------
  file.ext <- tolower(tools::file_ext(file.name))
  file.ext1 <- stringr::str_c(".", file.ext)

  file.type <- NULL
  if (file.ext1 %in% projectConfig$settings$inital.files$doc$ext)
    file.type <- "doc"
  if (file.ext1 %in% projectConfig$settings$inital.files$codebook$ext)
    file.type <- "codebook"
  if (file.ext1 %in% projectConfig$settings$inital.files$data$ext)
    file.type <- "data"
  
  if (is.null(file.type)){
    stop("Error: file must be a data, code book or document file")
  } else {
    cat("File of type:", file.type, "\n\n")
  }
  
  ## determine destination ----------------------------------------
  destinationSub <-
    switch(file.type,
           doc = projectConfig$projectDirs$destinations$doc,
           data = projectConfig$projectDirs$working.dirs$dataOrig,
           codebook =  projectConfig$projectDirs$working.dirs$codebook)
  destination <- file.path(baseDir, destinationSub)

  ## add to setup logfile
  sink(setup.log, append = TRUE, split = TRUE)
  ## NB: should make this more flexible in future
  cat("\n++++++++++++++++++++++++++++++++++++++++++++++++\n")
  cat(paste0("Details of adding file: '", file.name, "'\n"))
  cat("++++++++++++++++++++++++++++++++++++++++++++++++\n\n")

  ## move file --------------------------------------------------------
  cat(paste0("File: '", file.name, "' will be moved to '", destination, "'\n"))

  dest.file <- file.path(destination, file.name)
  if (file.exists(dest.file)){
    stop("Destination file:", dest.file, "exists. Remove first?")
  }
  MV <- ifelse(move, "moved", "copied")
  if (move){
    moved.success <-
      file.rename(file.name, dest.file)
  } else {
    moved.success <-
      file.copy(file.name, dest.file, copy.date = TRUE)
    ##            overwrite = force, copy.date = TRUE)
  }
  if (moved.success)
    cat(stringr::str_c("File: '", file.name, "' ", MV, " successfully\n\n"))

  ## if doc then update projectConfig and git repo and exit -------------
  if (file.type == "doc"){
    wd <- getwd(); on.exit(setwd(wd)) # jump back when finished
    sink()  ## stop sink'ing until after chamge to project directory
    setwd(projectDir)

    ## Needs more checking but looks reasonable !!!!!!!!!!!!!!!!!!!!!!!!
    sink(flog <- file.path("..", setup.log), append = TRUE, split = TRUE) 

    ## update project config
    comment(projectConfig) <-
      c(comment.pc, stringr::str_c("File: '", file.name, "' added at ", date()))
    saveRDS(projectConfig, file = projectConfig$configFile)
    cat("+++ Project configuration updated\n\n")
    ## update git
    cat("+++ Updating git repository\n")
    sgit.1 <-
      system2("git", stringr::str_c("add ",
                                    file.path(destinationSub, file.name)),
                      stdout = TRUE, stderr = TRUE)
    if (length(sgit.1)) print(sgit.1)
    sgit.2 <-
      system2("git", stringr::str_c("commit -m \"Added file ',", file.name,
                                    "'\" -a"), stdout = TRUE, stderr = TRUE)
    if (length(sgit.2)) print(sgit.2)

    ## clean up and exit
    setwd(wd)
    sink()    
    return(TRUE)
  }

  ## Construct R files, makefiles, .Rmd files etc  -------------------
  ## message("\n+++ Creating R Syntax templates for reading/analysing data ...")
  cat("\n+++ Creating R Syntax templates for reading/analysing data ...\n")

  ## Use template files for make, .R syntax, .Rmd etc etc etc etc 
  template.choices <-
    list(data = c("readR", "cleanR", "summaryR", "analyseR"),
         codebook = "codebookR")
  template.dir <- projectConfig$settings$template.dir
  templates <- template.files <- projectConfig$settings$templates

  Rsyntax.types <- template.choices[[file.type]]
  Rsyntax.types <- gsub("R$", "", Rsyntax.types) # drop last R
  
  MakefileLines <- vector(mode = "list", length = length(Rsyntax.types))
  names(MakefileLines) <- c(Rsyntax.types)

  ## set up file names and data frame names
  newFilesAndDFs <- list(file.name)
  names(newFilesAndDFs) <-
    ifelse(file.type == "data", "dataFiles", "codebookFiles")
  newFilesAndDFs$dataFrames <-
    lapply(Rsyntax.types,
           function(x)
             stringr::str_c(x, "_",
                            stringr::str_replace(file.name, "\\.", "_")))
  names(newFilesAndDFs$dataFrames) <- Rsyntax.types
  newFilesAndDFs$RsyntaxFiles <-
    lapply(newFilesAndDFs$dataFrames, function(x) stringr::str_c(x, ".R"))
  newFilesAndDFs$RoutFiles <-
    lapply(newFilesAndDFs$dataFrames, function(x) stringr::str_c(x, ".Rout"))
  newFilesAndDFs$RDataFiles <-
    lapply(newFilesAndDFs$dataFrames, function(x) stringr::str_c(x, ".RData"))
  newFilesAndDFs$availableCodeBooks <- projectConfig$availableCodeBooks
  newFilesAndDFs$directories <- projectConfig$filesAndDFs$directories
  class(newFilesAndDFs) <- "fileAndDataNames" ## cheating

  ## loop through R syntax files
  cat("R syntax files:\n")
  for (RS in Rsyntax.types){ # RS <- "read"
    if (length(newFilesAndDFs$RsyntaxFiles[[RS]]) > 0) {
      MakefileLines[[RS]] <- 
        createSyntaxR(dir.project = projectDir, filesAndDFs = newFilesAndDFs,
                      project.steps = RS, template.dir = template.dir,
                      print.mismatches =
                          projectConfig$settings$print.mismatches,
                      ## myFunction.files = myFunction.files, 
                      template = template.files[paste0(RS, "R")])
    }
  }
    
  ## add new lines to Makefiles ---------------------------------------
  ## message("\n+++ Appending new file to Makefiles ...")
  cat("\n+++ Appending new files to Makefiles ...\n")

  makeTypes <- c("readMerge", "work")
  makeDirs <-
    unlist(projectConfig$projectDirs$working.dirs)[makeTypes]
  makeFiles <- file.path(baseDir, makeDirs, "Makefile")
  names(makeFiles) <- makeTypes

  ## read/clean
  mkfiler <- file(mfname1r <- makeFiles["readMerge"], open = "rt")
  mf1 <- readLines(mkfiler)
  close(mkfiler)
  unlink(mkfiler)

  ## add targets to all line
  allLine <- grep("^all:", mf1)
  mf1[allLine] <- paste(mf1[allLine], paste(MakefileLines$clean$targets,
                                            collapse = " "))
  ## add extra lines to makefile
  mf1 <- c(mf1, "", paste0("## Extra R syntax for file: '", file.name,
                           "' added at ", date()),
           MakefileLines$read$makefileLines, MakefileLines$clean$makefileLines)

  mkfiler <- file(mfname1r, open = "wt")
  writeLines(mf1, mkfiler)
  close(mkfiler)
  unlink(mkfiler)
  cat("Wrote file:", mfname1r, "\n")
  
  ## summary/analysis
  mkfiler <- file(mfname1r <- makeFiles["work"], open = "rt")
  mf1 <- readLines(mkfiler)
  close(mkfiler)
  unlink(mkfiler)

  ## add targets to all line
  allLine <- grep("^all:", mf1)
  mf1[allLine] <- paste(mf1[allLine],
                        paste(MakefileLines$summary$targets, collapse = " "),
                        paste(MakefileLines$analyse$targets, collapse = " "))
  ## add extra lines to makefile
  mf1 <- c(mf1, "", paste0("## Extra R syntax for file: '", file.name,
                           "' added at ", date()),
           MakefileLines$summary$makefileLines,
           MakefileLines$analyse$makefileLines)

  mkfiler <- file(mfname1r, open = "wt")
  writeLines(mf1, mkfiler)
  close(mkfiler)
  unlink(mkfiler)
  cat("Wrote file:", mfname1r, "\n")

  ## add file and new syntax files to git then update  ---------------

  ## seems that writing Makefiles messes up sink()
  ## cat("sink.number\n")
  ## print(sink.number())
  sink()  ## stop sink'ing until after chamge to project directory
  
  ## only set up directory for the appropriate directory -----------
  wd <- getwd(); on.exit(setwd(wd)) # jump back when finished
  setwd(projectDir)

  ## Needs checking but looks reasonable !!!!!!!!!!!!!!!!!!!!!!!!
  sink(flog <- file.path("..", setup.log), append = TRUE, split = TRUE) 

  ## update project config
  comment(projectConfig) <-
    c(comment.pc, stringr::str_c("File: '", file.name, "' added at ", date()))
  saveRDS(projectConfig, file = projectConfig$configFile)
  cat("+++ Project configuration updated\n\n")

  ## add the file - but this produces no output
  sgit.1 <-
      system2("git", stringr::str_c("add ",
                                    file.path(destinationSub, file.name)),
                      stdout = TRUE, stderr = TRUE)
  ## if (length(sgit.1)) print(sgit.1)

  rsFiles <- 
    file.path(newFilesAndDFs$directories[c("read", "clean", "analyse",
                                           "summary")],
              newFilesAndDFs$RsyntaxFiles)
  rsFiles <- gsub("\\.\\.", "\\.", rsFiles)
  rsFiles <- paste(rsFiles, collapse = " ")
  sgit.2 <-
      system2("git", stringr::str_c("add ", rsFiles),
              stdout = TRUE, stderr = TRUE)
  ## no output produced
  ## if (length(sgit.2)) print(sgit.2)

  sgit.3 <-
    system2("git", stringr::str_c("commit -m \"Added file '", file.name,
                                "'\" -a"), stdout = TRUE, stderr = TRUE)
  if (length(sgit.3)) print(sgit.3)

  ## return to start dir, finish off log file and exit 
  setwd(wd)
  sink()  
    
  return(TRUE)

}
  
## attr(addFile,"ex") <- function(){
## internal function only
##CUT##\dontrun{addFile("myNewDat.dta")}

