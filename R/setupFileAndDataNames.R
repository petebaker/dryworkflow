## Filename: setupFileAndDataNames.R
## Purpose: Set up filenames for R syntax and markdown along with object names
##
## To run in terminal use:  R CMD BATCH --vanilla setupFilenamesAndDataFrames.R

## Created at:  Fri Apr 17 15:52:39 2015
## Author:      Peter Baker
## Hostname:    clearwell2.fritz.box
## Directory:   /home/pete/Data/R.workflow/Rpackage-201501/src/R/
## Licence:     GPLv3 see <http://www.gnu.org/licenses/>
##
## Change Log: 
##

## NB: work with 'initialFiles' first then use it fpr addFile (single file)

## food for thought - this could even create read.code etc but that
## might be too messy



##' Internal: Create syntax filenames and object names for processing
##'
##' This is an internal \code{\link{dryworkflow-package}} function. It
##' is primarily designed to be called by
##' \code{\link{createProjectSkeleton}} and \code{\link{addFile}} to
##' set up file and object names for processing. Given a list of data
##' filenames and optionally, a list of project steps, various names
##' are created for use with templates, makefiles and git for version
##' control.
##' 
##' @param dir.project dir.project directory name for project. Default:
##' \dQuote{myRproject}
##' @param destinations \code{list} of destination directories where
##' original (and added) files will be moved. This is a \code{list}
##' with named components \sQuote{data}, \sQuote{doc},
##' \sQuote{codebook} and \sQuote{lib} for data files, documents,
##' codebooks and R functions, respectively. Default: unix directory
##' names will be \code{list(data = "data/original", doc =
##' "doc/original", codebook = "data/codebook", lib = "lib")} and
##' Windows will be of similar form but capitalised
##' with longer form names.
##' @param projectConfig project configuration stored at project
##' creation and updated when files added. Format similar to similar
##' to getOptions(\dQuote{dryworkflow})
##' @param projectDirs directory structure of project of class
##' 'drywDestinationDirs'
##' @param filenames filenames for added files but not set for new
##' project.
##' @param initial.files initial file sources and extensions
##' @param mnemonic three or four letter mnemonic to aid remembering
##' and for succinct naming \code{R}, \code{Rmd} and \code{Rnw} files
##' and project directory. Default: \code{NULL} for none
##' @param project.steps steps to be carried out in project, specified
##' as a vector of strings. Options are \dQuote{read} to read data
##' (always assumed present), \dQuote{clean} clean data,
##' \dQuote{summary} summary statistics and basic plots,
##' \dQuote{analyse} perform statistical analysis, \dQuote{compare}
##' compare datasets and in particular different versions of the same
##' data set,  \dQuote{mergeAll} merge data sets of more than one; and
##' \dQuote{reportRmd} or \dQuote{reportRnw} produce reports using
##' \code{\link{rmarkdown}} and/or\code{\link{Sweave}} and
##' \dQuote{presentRmd} or \dQuote{beamerRnw} produce presentations
##' using \code{\link{rmarkdown}} and/or\code{\link{Sweave}}
##' @param report.markdown vector of markdown file types to be
##' employed to produce reports such as \dQuote{.org}, \dQuote{.Rmd}
##' and \dQuote{.Rnw}. Default: \dQuote{.Rmd} and \dQuote{.Rnw}.
##' @param report.which which data files to produce reports
##' for. Choices: \dQuote{first}, \dQuote{merge}, \dQuote{all})
##' Default: \dQuote{first}
##' @return an S3 object of class \code{fileAndDataName} 
##' 
setupFileAndDataNames <-
  function(dir.project, destinations, projectConfig, projectDirs, 
           filenames = NULL,
           initial.files = NULL, mnemonic = "",
           project.steps = c("read", "codebook", "clean", "summary", "analyse",
             "compare", "mergeAll", "reportRmd", "reportRnw", "presentRmd",
             "beamerRmd", "beamerRnw"),
           report.markdown = c("Rmd", "Rnw"),
           report.which = c("first", "merge", "all"))
{

  ## NB: need to extract projectDirs from projectConfig when it gets written

  ## Q: Do I really need Rout files - I think not
  ##    can just do the old
  ##    makefile.target = gsub(".R$", ".Rout", basename(syntax.file)),
  ##     makefile.pdf = gsub(".R$", ".pdf", basename(syntax.file)),

  ## keywords internal - may reinstate this or not

  ## check inputs (some of them) ----------------------------------
  if (class(projectDirs) != "drywDestinationDirs")
    stop("'projectDirs' should be of class 'drywDestinationDirs'")
  
  
  cat("\n++++ Setting up file names and object names\n\n")
  cat("Creating names of R syntax, markdown file names and object names for\n")
  if (is.null(filenames)) {
    ## createProjectSkeleton assuming all files have just been moved
    ## so only works for successfully moved files
    PROJ.SKEL <- TRUE
    cat(" new project\n")
  } else {
    ## addFiles need to check for consistency, existence etc LATER!!
    ## of course need to specify filenames but dir.project and
    ## destinations should from config
    cat("existing project '", projectConfig$name.project, "'\n", sep ="")
    PROJ.SKEL <- FALSE
    print(filenames)
  }

  ## project types and report types -------------------------------
  if (PROJ.SKEL){  # new project - perhaps most of this can be reused!
    project.steps <-
      match.arg(project.steps,
                c("read", "codebook", "clean", "summary", "analyse",
                  "compare", "mergeAll", "reportRmd", "reportRnw",
                  "presentRmd", "beamerRmd", "beamerRnw"), several.ok = TRUE)
    report.markdown <-
      match.arg(report.markdown, c("Rmd", "Rnw"), several.ok = TRUE)
    report.which <- match.arg(report.which)
  
    ## names of data types and codebooks
    ## set up data types for moved/copied files based on extension
    ## and find matching codebook if any
    
    ## MOST of thise needs to be done for (PROJ.SKEL === TRUE) or both may be OK
    dataFiles <- list.files(stringr::str_c(dir.project, "/",
                                           destinations[["data"]]),
                            all.files = TRUE, ignore.case = TRUE)[-c(1:2)]
    ## possibleDataTypes <- gsub("\\.", "", tolower(data.ext))

## UP TO HERE BUT THINK THIS SHOULD BE SOMEWHERE ELSE - MORE GENERAL IN CASE ADD NEW DATA TYPES _ DEFEINITELY SHOULD BE AN ARGUMENT
    
  data.types <- tolower(tools::file_ext(dataFiles))
  data.files <- data.frame(ID = tolower(tools::file_path_sans_ext(dataFiles)),
                           dataType = data.types, dataFile = dataFiles,
                           stringsAsFactors = FALSE)
  codebookFiles <- list.files(stringr::str_c(dir.project, "/",
                                     destinations[["codebook"]]),
                              all.files = TRUE, ignore.case = TRUE)[-c(1:2)]
  codebook.types <- tolower(tools::file_ext(codebookFiles))
  codebookBase <- tools::file_path_sans_ext(codebookFiles)
  codebookBase <- gsub("_codebook$", "", codebookBase)
  codebook.files <- data.frame(ID = tolower(codebookBase),
                               codebook = codebookFiles,
                               codebookType = codebook.types,
                               stringsAsFactors = FALSE)

  ## matching data/codebooks NB: can have same codebook for several data files
    codebookMatches <- merge(data.files, codebook.files, all = TRUE)
  }
  ## set up names for R and report files  -----------------------------------
  
  report.steps <- project.steps[grep("report|present|beamer", project.steps)]
  allRsyntax <- setdiff(project.steps, c(report.steps, "compare", "mergeAll"))

  ## filenames for R syntax -------------------------------------------
  RsyntaxFiles <-
    lapply(allRsyntax,
           function(y) stringr::str_c(mnemonic, y, "_",
                                      gsub("\\.", "_", dataFiles), ".R",
                                      sep=""))
  names(RsyntaxFiles) <- allRsyntax
  ## if more than 1 data file then create and add in merge syntax
  if ("mergeAll" %in% project.steps & length(dataFiles) > 1){
    RsyntaxFiles$mergeAll <- "mergeAll.R"
  }

  ## codebook R syntax file to read codebooks and store/compare if available
  if (length(codebookFiles) > 0){   # correct test?? 
    RsyntaxFiles$codebook <-
      stringr::str_c(mnemonic, "read_codebook_", codebookBase, ".R")
  }

  ## filenames for reports/presentations ---------------------------------
  rStepSplits <- stringr::str_split_fixed(report.steps, "R", 2)
  rownames(rStepSplits) <- report.steps
  rStepSplits[,2] <- stringr::str_c(".R",rStepSplits[,2]) 
  reportFiles <-
    lapply(report.steps,
           function(y)
             stringr::str_c(mnemonic, rStepSplits[y,1], "_",
                            gsub("\\.", "_", dataFiles), rStepSplits[y,2]))
  names(reportFiles) <- report.steps
  if (report.which == "merge") stop("Sorry - not yet implemented")
  if (report.which == "first"){
    for (II in 1:length(reportFiles))
      reportFiles[[II]] <- reportFiles[[II]][1]
  }

  ## create Rout filenames for make -------------------------------
  RoutFiles <-
    lapply(RsyntaxFiles, function (x) gsub("\\.R$", "\\.Rout", x))
  
  ## create RData filenames for make/R ---------------------------
  RDataFiles <-
    lapply(RsyntaxFiles,
           function (x) gsub("read-", "", gsub("\\.R$", "\\.RData", x)))
  ## codebooks if present
  if (length(codebookFiles) > 0){   # correct test?? 
    RDataFiles$codebook <-
      gsub("^read_codebook", "codebook", gsub("\\.R$", "\\.RData",
                                             RsyntaxFiles$codebook))}
  ## RData files - replace read with orig
  RDataFiles <-
    lapply(RDataFiles,
           function(y){
             stringr::str_replace(y, "^read", "orig")})

  ## data frame names ---------------------------------------------
  suffix <- c("_orig", "_cl", "_sum", "_anly") # best to have untouched orig
  names(suffix) <- c("read", "clean", "summary", "analyse")
  dataFrames <-
    lapply(allRsyntax, function(y){
      stringr::str_c(stringr::str_replace( dataFiles, "\\.", "_"),
                    suffix[y])})
  names(dataFrames) <- allRsyntax

  ## add data frame name for merged
  if ("mergeAll" %in% names(RsyntaxFiles)){
    dataFrames$mergeAll <- "mergedData" 
  }
  ## add codebooks
  if (length(codebookFiles) > 0){   # correct test?? 
    dataFrames$codebook <- stringr::str_c("codebook_", codebookBase)}

  ## add destination directories
  directories <- file.path("..",
                        c(projectDirs$working.dirs$readMerge,
                          projectDirs$working.dirs$readMerge,
                          projectDirs$working.dirs$readMerge,
                          projectDirs$working.dirs$work,
                          projectDirs$working.dirs$work,
                          projectDirs$working.dirs$readMerge,
                          projectDirs$working.dirs$readMerge,
                          projectDirs$working.dirs$reports,
                          projectDirs$working.dirs$reports,
                          projectDirs$working.dirs$reports,
                          projectDirs$working.dirs$reports,
                          projectDirs$working.dirs$reports,
                          projectDirs$working.dirs$dataDeriv,
                          projectDirs$working.dirs$dataOrig,
                          projectDirs$working.dirs$codebook))
  names(directories) <- c(project.steps, "dataDeriv", "dataOrig",
                          "dataCodebook")
  
  ## return "fileAndDataNames" S3 object
  fileAndDataNames <- list(dataFiles = dataFiles,
                           codebookFiles = codebookFiles,
                           RsyntaxFiles = RsyntaxFiles,
                           reportFiles = reportFiles,
                           report.which = report.which,
                           RoutFiles = RoutFiles,
                           RDataFiles = RDataFiles,
                           dataFrames = dataFrames,
                           availableCodeBooks = codebookMatches,
                           directories = directories)
  class(fileAndDataNames) <- "fileAndDataNames"

  fileAndDataNames
  
}
## could ouput very similar to whats need for templates but perhaps it
## already is


## ## for debugging
## dir.project
## destinations
## projectConfig <- drywOptions # only used for unset options so far in
##                              # calling function - hopefully no
##                              # mistakes made - better strategy?
## filenames <- NULL
## project.steps <- c("read", "clean", "summary", "analyse",
##                    "compare", "mergeAll", "report")
## report.markdown <- c("Rmd", "Rnw")
## mnemonic <- NULL
