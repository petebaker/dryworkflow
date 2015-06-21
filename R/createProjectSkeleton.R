## Filename: createProjectSkeleton.R
## Purpose: Create a new project using data, documents and R libraries
##

## Created at:  Mon Mar 23 00:20:32 2015
## Author:      Peter Baker
## Hostname:    clearwell2.fritz.box
## Directory:   /home/pete/Data/R.workflow/Rpackage-201501/src/R/
## Licence:     GPLv3 see <http://www.gnu.org/licenses/>
##
## Change Log: 
##
## need library(tools) for filename stuff

##' Create dryWorkflow data analysis project skeleton
##' 
##' Create skeleton directory structure, R syntax templates, report
##' templates, log file stubs and also move (or copy) data and doc
##' files to appropriate directories.  Also, create project
##' configuration file, Makefiles and then initialise logs, notes and
##' version control using \code{git}.
##' 
##' Note that option parameters are either set as an argument to the
##' function or automatically via global options using
##' \code{getOptions("dryw")}. For further information about
##' setting these options globally via \code{setOptions}, please see
##' \code{\link{dryworkflow-package}}.
##'
##' @param dir.project directory name for project. Default:
##' \dQuote{myRproject}
##' @param name.project name of project (for output documents, comments
##' etc). Default: \dQuote{My Data Analysis Project}
##' @param title.project title string for project reports
##' (for output documents, comments etc). Default: \dQuote{My Data Analysis Project}
##' @param type.project type of project: \dQuote{normal},
##' \dQuote{simple} or \dQuote{custom} (custom NYI). The style of
##' directory structure for the project.  Default \dQuote{normal}
##' @param style style for directory and filenames \dQuote{unix} or
##' dQuote{windows}, Default: \dQuote{unix}
##' @param data.src source directory for data files, multiple
##' OK. Default: current directory \dQuote{.}
##' @param data.ext possible data file extensions. Default:
##' c(\dQuote{.xls}, \dQuote{.xlsx}, \dQuote{.csv}, \dQuote{.dta}, \dQuote{.sav}, \dQuote{.xpt}, \dQuote{.RData}, \dQuote{.rda})
##' @param data.mv whether to move (TRUE) or copy (FALSE) data files
##' to destination directory. Default: TRUE which will move files
##' @param doc.src source directory for documents, multiple
##' OK. Default: current directory \dQuote{.}
##' @param doc.ext possible data file extensions. Default:
##' c(\dQuote{doc}, \dQuote{docx}, \dQuote{odt}, \dQuote{tex})
##' @param doc.mv whether to move (TRUE) or copy (FALSE) document files
##' to destination directory. Default: TRUE which will move files
##' @param codebook.src source directory for codebook files - multiple
##' OK. Default: current directory \dQuote{.}
##' @param codebook.ext possible code book file extensions. Default:
##' c(\dQuote{_codebook.csv}, \dQuote{_codebook.xls}, \dQuote{_codebook.xlsx})
##' @param codebook.mv whether to move (TRUE) or copy (FALSE) codebook
##' files to destination directory. Default: TRUE which will move
##' files
##' @param lib.src source directory for library files - multiple
##' OK. Default: current directory  \dQuote{.}
##' @param lib.ext possible data file extensions. Default: \dQuote{R}
##' @param lib.mv whether to move (TRUE) or copy (FALSE)
##' library/function files to destination directory. Default: TRUE
##' which will move files
##' @param force \code{logical} Whether to force creation of project
##' directory. Default: \code{FALSE} do not overwite existing directory
##' @param dontmove character vector of files that will not be moved
##' or copied from source directories. Default:
##' \dQuote{setupProject.R}
##' @param log.ext extension of log and README file(s) which are either
##' plain text (\dQuote{.txt}) or orgmode (\dQuote{.org})
##' text file(s) Default: \dQuote{.txt}
##' @param report.types vector of output templates to be produced
##' including \dQuote{.html}, \dQuote{.docx} and
##' \dQuote{.pdf}. Default: \dQuote{all}
##' @param author author name for reports. Default:
##' \dQuote{Insert author name here}
##' @param address address for reports. Default: \dQuote{Insert address here}
##' @param table1 logical: produce table 1 style summary statistics for
##' reports.  Default: \code{FALSE}
##' @param custom.dirs list of extra directories to be
##' created. Default: \dQuote{extra}
##' @param mnemonic three or four letter mnemonic to aid remembering
##' and for succinct naming \code{R}, \code{Rmd} and \code{Rnw} files
##' and project directory. Default: \code{NULL} for none
##' @param extra extra directories additional to standard
##' setup. Default: \code{NULL} for none
##' @param common.mk list with components \code{dir} the location of
##' common.mk and other library files. Default: \dQuote{~/lib} for
##' \code{unix} and \dQuote{$HOME/Library} for
##' \code{Windows}. However, if a global directory is not found then
##' the project specific directory will be used. The second component
##' is \code{file} the filename containing makefile rules for \code{R},
##' \code{Rmd} etc files for inclusion into Makefiles. Default:
##' \dQuote{common.mk}
##' @param template.dir directory name containing template
##' files. Default: templates provided with the \code{dryworkflow}
##' package
##' @param templates names list of template files. See the templates
##' provided with the \code{dryworkflow} package for details
##' @param print.mismatches print mismatches when creating syntax or
##' markdown files from templates. Default: \code{FALSE}
##' @param report.which which data files to produce reports
##' for. Choices: \dQuote{first}, \dQuote{merge}, \dQuote{all})
##' Default: \dQuote{first}
##' @param report.markdown vector of markdown file types to be
##' employed to produce reports. Default: \dQuote{.org},
##' \dQuote{.Rmd}, \dQuote{.Rnw}. Default: \dQuote{.Rmd} and
##' \dQuote{.Rnw}
##' @param licence Licence for syntax files. Could be a string such as
##' \sQuote{Copyright J Smith 2015} Default: \dQuote{GPL3 see <http://www.gnu.org/licenses/>}
##' @param ... extra parameters passed to safe directory creation
##' @return invisible or does it??? what about monitoring??
##' @author Peter Baker  \email{pete@@petebaker.id.au}
##' @examples
##' ## A project with all default settings
##'
##' ## copy .csv file and codebook from dryWorkflow package
##' file.copy(system.file('demoFiles', 'small2.csv', package='dryworkflow'),
##'           'small2.csv')
##' file.copy(system.file('demoFiles', 'small2_codebook.csv',
##'                       package='dryworkflow'), 'small2_codebook.csv')
##'
##' ## NB: In practice, always check directories, R syntax  etc
##' ##     before using 'make'
##' createProjectSkeleton(dir.proj = "testProject",
##'                       name.project = "Experiment 1",
##'                       dontmove = "dryworkflow-Ex.R")
##' @export
createProjectSkeleton <-function(
  dir.project = NULL, name.project = NULL, title.project = NULL,
  type.project = NULL, style = NULL,
  data.src = NULL, data.ext = NULL, data.mv = NULL,
  doc.src = NULL, doc.ext = NULL, doc.mv = NULL,
  codebook.src = NULL, codebook.ext = NULL, codebook.mv = NULL,
  lib.src = NULL, lib.ext = NULL, lib.mv = NULL,
  force = NULL, dontmove = NULL,
  log.ext = NULL, report.types = NULL,
  author = NULL, address = NULL,
  table1 = NULL, custom.dirs = NULL,
  mnemonic = NULL, extra = NULL,
  common.mk = NULL,
  template.dir = NULL, templates = NULL, print.mismatches = NULL,
  report.which = NULL,  report.markdown = NULL, licence = NULL, ...) {
  
  ## For testing:
  ##   dir.project = NULL; name.project = NULL; type.project = NULL; style = NULL; data.src = NULL; data.ext = NULL; doc.src = NULL; doc.ext = NULL; codebook.src = NULL; codebook.ext = NULL; lib.src = NULL; lib.ext = NULL; force = NULL; dontmove = NULL; log.ext = NULL; report.markdown = NULL; report.types = NULL; author = NULL; address = NULL; table1 = NULL; custom.dirs = NULL;  mnemonic = NULL; extra = NULL; data.mv = NULL; doc.mv = NULL; codebook.mv = NULL; lib.mv = NULL; common.lib = NULL; common.mk = NULL; licence = NULL; template.dir = NULL; templates = NULL; report.which = NULL; title.project = NULL
  
  ## potential extra function arguments
  ## strata = NULL,##<< stratum variable for report tables (needs work)
  ## strata.label = NULL,##<< stratum variable label for report tables (needs work)
  ## table1 = FALSE,##<< logical: produce table 1 reports 
  ## ... ##<< extra arguments mostly to file handling functions. eg 'recursive = TRUE' will find data and document files recursively

  ## ---------------------------------------------------------------
  ## process options -----------------------------------------------
  ## ---------------------------------------------------------------
  
  ## check all options, set where required and exit on errors
  drywOptions <- getOption("dryworkflow")

  ## project configuration setings
  projectConfig <- list(settings = drywOptions)
  
  ## dir.project -------------------------
  if (is.null(dir.project)) { # set if specified
    dir.project <- drywOptions$dir.project
  } else {
    projectConfig$settings$dir.project <- dir.project
  }
  if (length(grep(" ", dir.project)))
    stop("Error: Directory name for 'dir.project' should not contain spaces")

  ## name.project -------------------------
  if (is.null(name.project)) { # set if not specified
    name.project <- drywOptions$name.project
  } else {
    projectConfig$settings$name.project <- name.project
  }
  
  ## logging create project ------------------------------------------------
  ## need to test this better and perhaps specialise for orgMode too
  if (is.null(log.ext)){
    log.ext <-  drywOptions$log.ext
    LOG.EXT <- match.arg(log.ext, c(".txt", ".org", ".log"))
    ## message("Extension for log files set to: ", log.ext)
    cat("Extension for log files set to: ", log.ext, "\n")
  } else {
    projectConfig$settings$log.ext <- log.ext
  }
  
  setup.log <- stringr::str_c("setup_", dir.project, log.ext)
  setup.dir <- getwd()
  sink(setup.log)
  cat("'dryworkflow::createProjectSkeleton' log: started at", date(), "\n")
  cat("Default 'dryworkflow' options:\n")
  print(drywOptions)
  cat("'createProjectSkeleton' call:\n")
  match.call()
  sink()
  sink(setup.log, append = TRUE, split = TRUE) 
  
  ## type.project -------------------------
  if (is.null(type.project)) { # set if not specified
    type.project <- drywOptions$type.project
    PROJ.TYPE <- match.arg(type.project, c("normal", "simple", "custom"))
    if (PROJ.TYPE == "custom")
      stop("Error: My apologies but type 'custom' not yet supported.")
  }
  
  ## style --------------------------------------
  if (is.null(style)) { # set if specified
    style <- drywOptions$style
  } else {
    STYLE <- match.arg(style, c("unix", "windows"))
    projectConfig$settings$style <- STYLE
  }
  
  ## common makefile definitions
  if (is.null(common.mk)){
    common.mk <- drywOptions$common.mk
  } else {
    projectConfig$settings$common.mk <- common.mk
  }
  if (is.null(author)){
    author <- drywOptions$author
  } else {
    projectConfig$settings$author <- author
  }

  ## check that git is set up ---------------------------------
  if (drywOptions$git$present){
    GIT.DEFAULT <-
      list(present = TRUE,
           user.name = system("git config user.name", intern = TRUE),
           user.email = system("git config user.email", intern = TRUE))
    if (GIT.DEFAULT$user.name == "" | GIT.DEFAULT$user.email == ""){
      if (GIT.DEFAULT$user.name == "")
        warning("git user name not set. Please set with 'git config user.name'\n - see 'git config --help'")
      if (GIT.DEFAULT$user.email == "")
        warning("git user email not set. Please set with 'git config user.email'\n - see 'git config --help'")
      stop("Please set global option(s) and try again")
    } else {
      drywOptions$git$user.name <- GIT.DEFAULT$user.name
      drywOptions$git$user.email <- GIT.DEFAULT$user.email
    }
    projectConfig$settings$git <- GIT.DEFAULT

    if (projectConfig$settings$author == "-- Insert author here --"){
      projectConfig$settings$author <-
        stringr::str_c(GIT.DEFAULT$user.name, " <",
                       GIT.DEFAULT$user.email, ">")
    }
  } else {
    warning("git not found on system. Please put install git or fix PATH")
  }

  ## set destination directories -----------------------------------
  projectDirs <- setUpDirectoryStructure(style) # custom NYI
  if (class(projectDirs) != "drywDestinationDirs"){
    cat("Project Directories:\n")
    print(projectDirs)
    stop("Something wrong with project directory structure")
  }
  
  destinations <- projectDirs$destinations
  fileCategories <- names(destinations)
  
  ## create destination directories --------------------------------
  
  ## function to safely create directory - "borrowed" from package.skeleton
  safe.dir.create <- function(path, ...) {
    dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir
    if (!dirTest(path) && !dir.create(path, ...)) 
      stop(gettextf("cannot create directory '%s'", path), 
           domain = NA)
  }
  
  if(is.null(force)) force <- drywOptions$force # force directory creation?
  
  ## create main project directory
  if (file.exists(dir.project) && !force) 
    stop(gettextf("directory '%s' already exists", dir.project), 
         domain = NA)
  safe.dir.create(dir.project)        # create project directory but not
                                        # if there already - no message
  
  ## create project directories
  cat("\n+++ Creating project directories:\n")
  TMP <- lapply(paste(dir.project, unlist(projectDirs$directories), sep="/"),
                function(x){
                  safe.dir.create(x, recursive = TRUE)
                  cat("Created directory:", x, "\n")
                  invisible()
                })
  
  if (force){
    stop("Not Implemented Yet: Need to have back up option - think a zip/tgz would be OK\n")
  }
  
  ## process source file extensions and source directories  --------------

  ## file wildcards and set up for moving files to directories
  ## message("\nMoving data, codebooks, documents and R functions ...")
  cat("\nMoving data, codebooks, documents and R functions ...\n")
  
  ## initial file sources and extensions -----------------------------
  ## no custom available yet
  initialFiles <- drywOptions$inital.files

  ## overide initial source directories/extensions if they are set
  SRC.DIRS <- paste0(fileCategories, ".src")
  SRC.EXTS <- paste0(fileCategories, ".ext")
  for(FCAT in fileCategories){
    for (TYPE in c("src", "ext")){
      OPT.SETTING <- get(OPT <- paste0(FCAT, ".", TYPE))
      if (is.null(OPT.SETTING)){
        assign(OPT, initialFiles[[FCAT]][[TYPE]])
      } else {
        initialFiles[[FCAT]][[TYPE]] <-  OPT.SETTING
      }
    }
  }

  ## check to see only one destination per filetype category
  if (max(Ndests <- sapply(destinations, length)) > 1){
    stop("More than one destination spcified for",
         paste(names(Ndests)[Ndests>1], collapse = ","))
  }

  ## check consistency if source and destinations for filetypes
  if (! identical(names(initialFiles), names(destinations))){
    cat("Initial Files\n")
    print(initialFiles)
    cat("Destination Directories\n")
    print(destinations)
    stop("Inconsistencies detected between initial file specification and destinations")
  }
  
  ## move the files (if they exist)

  ## 2 Qs:
  ## 1 need to have upper/lower case extensions for windows users
  ##   BUT NYI !!!! needs exact now
  ## 2 need to move/copy - just revamp what I did b4
  ## also record somewhere - to logs - maybe config but not so sure

  ## for checking
  ## (x <- names(initial.files)[4])

  files2move <- lapply(fileCategories,
         function(x){
             Sys.glob(file.path(initialFiles[[x]]$src,
                                paste0("*", unlist(initialFiles[[x]]$ext))))
         })
  names(files2move) <- fileCategories

  ## make sure that codebooks not moved to data directory
  files2move$data <- setdiff(files2move$data, files2move$codebook)

  ## drop files that shouldn't be moved to new directories. This is
  ## usually the syntax to set up the project but sometimes user may
  ## wish to keep other files from being moved too

  ## move files but not dontmove!
  if (is.null(dontmove)) { # set if specified
    dontmove <- drywOptions$dontmove
  }  else {
    projectConfig$settings$dontmove <- dontmove
  }

  ## for debugging - comment or delete once debugged
  ## dontmove <- c("dryWorkflow-package.R", "setUpDirectoryStructure.R",
  ##               "createProjectSkeleton.R", "init.R", "zzz.R",
  ##               "applyCodeBook.R", "createMergeR.R",
  ##               "readCodeBook.R", "createFromTemplate.R", "createSyntaxR.R",
  ##               "setupFileAndDataNames.R",
  ##               "setupDevelopment.R")

  ## Mainly for debugging - this should be set in options if required
  ## initialFiles[["lib"]]$move <- FALSE   # copy lib files just in case 1 missed
  
  ## may need better grep and need better file glob case insensitive
  ## above although hope that under windows it doesn't matter as
  ## windows may do what you want re case insensitive!

  ## drop from file list as don't move
  filesNotMoved <- unlist(lapply(unlist(files2move),
                                 function(y) lapply(dontmove,
                                                    function(z) y[grep(z, y)])))
  files2movePLUSnotMv <- files2move
  files2move <- lapply(files2movePLUSnotMv,
                       function(z) setdiff(z, filesNotMoved)) 

  ## move (or copy) files to destination directories ----------------

  cat("\n+++ Files will now be moved (or copied) to destination directories\n")

  moved <- notMoved <- vector("list", length(destinations))
  names(moved) <- names(notMoved) <- fileCategories

  if (length(files2move) > 0) {  # Fcat <- "data"   Fcat <- "lib"
    for (Fcat in fileCategories){
      MV <- ifelse(mv_not_copy <-  initialFiles[[Fcat]]$move, "moved", "copied")
      cat("Files to be", MV, "to directory:", destinations[[Fcat]], "\n")
      if (length(files2move[[Fcat]]) == 0){
        cat("No files to be", MV, "\n")
      } else {
        print(files2move[[Fcat]])
        destinationFilesFcat <-  file.path(dir.project, destinations[[Fcat]],
                                           files2move[[Fcat]])
        ## if (force){
        if (probFiles <- any(file.exists(destinationFilesFcat))) {
          ## need a backup option first before implementing this!!!!
          cat("Warning: Overwriting files is not currently available for moving or copying files\n")
          cat("Please delete destination files manually or move them to safety\n")
          cat("These file(s) would have been overwritten:")
          print(probFiles)
        }
        ## }
        
        if (mv_not_copy){
          moved.success <-
            file.rename(files2move[[Fcat]], destinationFilesFcat)
        } else {
           moved.success <-
             file.copy(files2move[[Fcat]], destinationFilesFcat,
                       copy.date = TRUE)
           ##            overwrite = force, copy.date = TRUE)
        }
        moved[[Fcat]] <- files2move[[Fcat]]
        if (all(moved.success)){
          cat("All files", MV, "successfully\n\n")
        } else {
          ok.moved <- moved[[Fcat]][moved.success]
          not.ok.moved <- moved[[Fcat]][!moved.success]
          if (length(ok.moved)>0) moved[[Fcat]] <- ok.moved
          if (length(not.ok.moved)>0) notMoved[[Fcat]] <- not.ok.moved
          cat("Warning: Not all files", MV, "successfully!\n")
          cat("         Could it be that some files were already present at destination?\n")
          cat("Warning: these files", MV, "successfully:\n")
          print(ok.moved)
          cat("Warning: these files NOT", MV, "successfully:\n")
          print(not.ok.moved)
        }
      }
    }
  }

  ## Construct R files, makefiles, .Rmd files etc  -------------------
  
  ## message("\n+++ Creating R Syntax templates for reading/analysing data ...")
  cat("\n+++ Creating R Syntax templates for reading/analysing data ...\n")

  ## get data file names, extensions and codebook names ----------------
  ## NB can get basename, ext using utiliies in tools package

  ## process mnemonic, data and codebook options -----------------------
  ## check neumonics and use if necessary -------------------------------
  if (is.null(mnemonic)) { # set if not specified
    mnemonic <- drywOptions$mnemonic
  } else {
    projectConfig$settings$mnemonic <- mnemonic
  }
  if (length(grep(" ", mnemonic))) stop("mnemonic should not contain spaces")
  if (length(mnemonic)>5) warning("mnemonic should be shorter than 6 letters")
  if (mnemonic != "") {
    MNEMONIC <- stringr::str_c(mnemonic, "-")
  } else {
    MNEMONIC <- ""
  }
  ##if (is.null(data.ext)) { # set if unspecified
  ##  data.ext <- drywOptions$data.ext
  ##}

  workingDirs <- projectDirs$working.dirs
  syntaxDirs <- list(readMerge = c("read", "clean", "compare", "mergeAll",
                                   "codebook"),
                     work = c("summary", "analyse"),
                     reports = "report")
  if (! all(names(syntaxDirs) %in% names(workingDirs)))
    stop("wrong directories specified for creating R syntax")

  
  if (is.null(report.markdown)) { # set if unspecified
    report.markdown <- drywOptions$report.markdown
  } else {
    projectConfig$settings$report.markdown <- report.markdown
  }
  
  if (is.null(report.types)) { # set if unspecified
    report.types <- drywOptions$report.types
  } else {
    projectConfig$settings$report.types <- report.types
  }

  if (is.null(report.which)){
    report.which <- drywOptions$report.which
  } else {
    report.which <- match.arg(report.which, c("first", "merge", "all"))
  }
  projectConfig$settings$report.which <- report.which

  filesAndDFs <-
    setupFileAndDataNames(dir.project = dir.project,
                          destinations = destinations,
                          projectConfig = drywOptions,
                          projectDirs = projectDirs,
                          initial.files = initialFiles,
                          report.markdown = report.markdown,
                          report.which = report.which)

  ## if unset use standard options for creating suntax/report files
  if (is.null(licence)) { # set if unspecified
    licence <- drywOptions$licence
  } else {
    projectConfig$settings$licence <- licence
  }

  ## type.project and resulting settings ------------------------------
  ##type.choices <- c("normal", "simple")
  ##type.project <- match.arg(type.project, choices = type.choices) #see above
  BASE <- ifelse(type.project == "normal", "..", "")
  deriv.data.dir <-
    file.path(BASE, projectDirs$working.dirs$dataDeriv)
  orig.data.dir <-
    file.path(BASE, projectDirs$working.dirs$dataOrig)
  codebook.data.dir <-
      file.path(BASE, projectDirs$working.dirs$codebook)  
  data.dir <- list(DIR.DATAFILE = orig.data.dir, DIR.DERIVED = deriv.data.dir,
                   DIR.CODEBOOK = codebook.data.dir)

  ## library files used by all syntax -----------------------
  myFunction.files <-
    stringr::str_c('source(file.path("', BASE, '", "', destinations[["lib"]],
                   '", "', files2move[["lib"]], '"))')                   

  ## get templates for R etc -----------------------------------
  ## Use template files for make, .R syntax, .Rmd etc etc etc etc 
  template.choices <- c("readR", "cleanR", "summaryR", "analyseR",
                        "codebookR", "mergeAllR", "compareR",
                        "reportRmd", "reportRnw", "presentRmd", "beamerRnw",
                        "make", "custom")
  ## template.dir <- "~/Data/A_Projects/R.workflow/package" # REPLACE !!!!
  ## template.dir <- "./templates"
  if (is.null(template.dir)) { # set if unspecified - NEEDS FIXING
    template.dir <- drywOptions$template.dir
  }
  ## BUT HERE NEED ONES UNDER DEVELOPMENT - options not working
  ## because package not yet installed - OK at moment but needs checking
  ## template.dir <- "./templates"
  if (is.null(templates)) { # set if unspecified
    template.files <- drywOptions$templates
  } else {
    projectConfig$settings$templates <- templates
  }
  if (is.null(print.mismatches)) { # set if unspecified
    print.mismatches <- drywOptions$print.mismatches
  } else {
    projectConfig$settings$print.mismatches <- print.mismatches
  }
  if (is.null(address)){
    address <- drywOptions$address
  } else {
    projectConfig$settings$address <- address
  }
  if (is.null(title.project)){
    title.project <- drywOptions$title.project
  } else {
    projectConfig$settings$title.project <- title.project
  }

  extras <- list(AUTHOR = author, ADDRESS = address, LICENCE = licence,
                 TITLE.PROJECT = title.project)

  ## R syntax and report files  -------------------------------------
  
  ## create R syntax files ---------------------------------------------

  Rsyntax.types <- template.choices[grep("R$", template.choices)]
  ## should only use 'compare' for updates
  Rsyntax.types <- setdiff(Rsyntax.types, "compareR")
  if (length(filesAndDFs$dataFiles) < 2)  # produce mergeAll.R if >2 data files
    Rsyntax.types <- setdiff(Rsyntax.types, "mergeAllR")

  Report.types <- template.choices[grep("Rnw$|Rmd$", template.choices)]

  Rsyntax.types <- gsub("R$", "", Rsyntax.types) # drop last R

  MakefileLines <- vector(mode = "list",
                          length = length(Rsyntax.types) + length(Report.types))
  names(MakefileLines) <- c(Rsyntax.types, Report.types)

  ## loop through R syntax files
  cat("R syntax files:\n")
  for (RS in Rsyntax.types){ # RS <- "read"
    if (length(filesAndDFs$RsyntaxFiles[[RS]]) > 0) {
      MakefileLines[[RS]] <- 
        createSyntaxR(dir.project = dir.project, filesAndDFs = filesAndDFs,
                      project.steps = RS, template.dir = template.dir,
                      print.mismatches = print.mismatches,
                      myFunction.files = myFunction.files, 
                      template = template.files[paste0(RS, "R")],
                      extras = extras)
    }
  }

  ## create report files -------------------------------------
  
  ## loop through report/presentations
  cat("R markdown and presentation files:\n")
  for (RT in Report.types){
    ## print(RT)
    ## print(template.files[RT])
    MakefileLines[[RT]] <- 
      createSyntaxR(dir.project = dir.project, filesAndDFs = filesAndDFs,
                    project.steps = RT, template.dir = template.dir,
                    print.mismatches = print.mismatches,
                    myFunction.files = myFunction.files, 
                    template = template.files[RT],
                    extras = extras)
  }
  
  ## leave compare until later - really its just compare from compare
  ## package so probably best as a specific addon/function when you
  ## receive a new version of file - eg option to addFile 

  ## message("\n+++ Creating Makefiles for reading/analysing data ...")
  cat("\n+++ Creating Makefiles for reading/analysing data ...\n")
  
  mkfileHeader <- function(mk.extra = NULL){
    mf.head <-
      c(paste("## Makefile created by 'dryworkflow' at",date()), "")
    ## "## NB: do not remove/alter this line (used for updates)", 
    if (length(mk.extra)>0){
      mf.head <- c(mf.head, mk.extra)
    }
    mf.head
  }

  ## Makefile in (top) project directory
  mkfile <- file(mfname1 <- file.path(dir.project, "Makefile"), "wt")#, raw=TRUE)
  makeDirs <- unlist(workingDirs[c("readMerge", "work", "reports")])
  makeDirs.proj <- file.path(dir.project, makeDirs)
  names(makeDirs.proj) <- c("readMerge", "work", "reports")
  
  ## write Makefile in home directory - need to do simple too
  
  if (type.project == "normal") {
    ## TEMPLATE.DIR<-file.path(system.file(package="dryWorkflow"), "templates")
    ## copy templates - this should be in package using 'copyTemplate' or above
    mf <- c(mkfileHeader(),
            readLines(file.path(template.dir, "Makefile_base.txt")),
            "", file.path(common.mk$dir, common.mk$file))
    mf <- gsub("@@READ.DIR@@", makeDirs[["readMerge"]], mf)
    mf <- gsub("@@WORK.DIR@@", makeDirs[["work"]], mf)
    mf <- gsub("@@REPORT.DIR@@", makeDirs[["reports"]], mf)
    writeLines(mf, mkfile)
    close(mkfile)
    cat("Wrote file:", mfname1, "\n")

    ## read/clean
    mkfiler <-
      file(mfname1r <- file.path(makeDirs.proj["readMerge"], "Makefile"), "wt")
    mk.targ <- c(".PHONY: all",
                 paste("all:",
                       paste(MakefileLines$clean$targets, collapse = " ")))
    mf <- c(mkfileHeader(), mk.targ, MakefileLines$read$makefileLines,
            MakefileLines$clean$makefileLines, "",
            file.path(common.mk$dir, common.mk$file))
    writeLines(mf, mkfiler)
    close(mkfiler)
    cat("Wrote file:", mfname1r, "\n")

    ## analyse/summary
    mkfiler <-
      file(mfname1r <- file.path(makeDirs.proj["work"], "Makefile"), "wt")
    mk.targ <- c(".PHONY: all",
                 paste("all:",
                       paste(MakefileLines$analyse$targets,
                             MakefileLines$summary$targets,
                             collapse = " ")))
    mf <- c(mkfileHeader(), mk.targ, MakefileLines$summary$makefileLines,
            MakefileLines$analyse$makefileLines, "",
            file.path(common.mk$dir, common.mk$file))
    writeLines(mf, mkfiler)
    close(mkfiler)
    cat("Wrote file:", mfname1r, "\n")

    ## reports - need to restrict to what is specified
    ## Report.types
    targets <- sapply(MakefileLines[Report.types], function(x) x$targets)
    mklines <- sapply(MakefileLines[Report.types], function(x) x$makefileLines)
    mkfiler <- 
      file(mfname1r <- file.path(makeDirs.proj["reports"], "Makefile"), "wt")
    mk.targ <- c(".PHONY: all",
                 paste("all:", paste(targets, collapse = " ")))
    mf <- c(mkfileHeader(), mk.targ, mklines, "",
            file.path(common.mk$dir, common.mk$file))
    writeLines(mf, mkfiler)
    close(mkfiler)
    cat("Wrote file:", mfname1r, "\n")
  }

  ## NB: Could set if(projectGonfig$git$present) set up git repo else
  ## not but here want to force git installation
  
  ## git -------------------------------------------------------

  ## seems that writing Makefiles messes up sink()
  ## cat("sink.number\n")
  ## print(sink.number())
  sink()  ## stop sink'ing until after chamge to project directory
  ## cat("sink.number\n")
  ## print(sink.number())

  ## only set up directory for the appropriate directory -----------
  wd <- getwd(); on.exit(setwd(wd)) # jump back when finished
  setwd(dir.project)
  
  sink(flog <- file.path(setup.dir, setup.log), append = TRUE, split = TRUE) 
  ##cat("sink.number\n")
  ##print(sink.number())
  ##print(flog)

  ## finalise and save projectConfig --------------------------------------

  ## message("\n+++ creating projectConfig ...\n")
  cat("\n+++ creating projectConfig ...\n")

  projectConfig$projectDirs <-  projectDirs
  projectConfig$filesAndDFs <- filesAndDFs
  ## configFile <- file.path(dir.project, "configFile.RData")
  configFile <- "configFile.rds"
  projectConfig$configFile <- configFile
  class(projectConfig) <- "drywProjectConfig"
  comment(projectConfig) <-
    paste0("'dryworkflow' configuration for '", dir.project, "' saved at ",
           date())
  saveRDS(projectConfig, file = configFile)

  cat("File: ", configFile, "\n")
  print(comment(projectConfig))
  
  ## message("\n+++ Creating git repository and initialising ...\n")
  cat("\n+++ Creating git repository and initialising ...\n")

  ## initialise git and commit
  sgit.1 <- system2("git", "init", stdout = TRUE, stderr = TRUE)
  print(sgit.1, quote = FALSE)
  sgit.2 <- system2("git", "add .", stdout = TRUE, stderr = TRUE)
  ## print(sgit.2)
  ## system2("git", paste("git commit -m 'Intial commit for ",
  ##                      name.project, "'", sep=""))
  sgit.3 <- system2("git",
                    stringr::str_c("commit -m 'Intial commit for ",
                                   name.project, "'"),
                    stdout = TRUE, stderr = TRUE)
  print(sgit.3, quote = FALSE)

  ## create .gitignore file in project file base directory
  copyGitIgnore()

  ## and what about first pass of git?? if .git doesn't exist

  ## message("Done.")
  cat("Done.\n")

  sink()
  
  ## save project configuration - args(projectSkeleton)
  setwd(wd)

  ## do I need to add something like this - plus run with make
  ##message(gettextf("Further steps are described in '%s'.", 
  ##                 file.path(dir, "Read-and-delete-me")), domain = NA)

  ## stop sink'ing log file
  ##sink()
  
}
