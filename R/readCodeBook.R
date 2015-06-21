##' Read a code book in standard format as a csv file
##'
##' Reads a code book stored as a \code{csv} file for either checking
##' against a data file or relabelling factor levels or labelling
##' variables.
##'
##' Often, data dictionaries or code books are provided with data
##' files. Rather than a \code{word} \code{doc} or \code{pdf} files,
##' the format required here is in a very specific format stored as a
##' \code{csv} file. Once read in, attributes such as factor
##' labels/levels and variable labels can be added to the
##' \code{data.frame} and/or also used to check factor labels and
##' variable names are consistent with the code book. Note that while
##' various methods may be available which attempt to convert word
##' docs or pdf's to a spreadsheet and/or csv file, extreme care
##' should be taken as these are far from perfect.
##' 
##' @param x filename of codebook to parse
##' @param codebook.directory directory containing codebook
##' @param col.names named character vector containing column names in
##' Code Book file. The vector contains components \dQuote{var.name} =
##' variable name, \dQuote{var.orig} = original name (if changed),
##' \dQuote{label} for printing/plotting, \dQuote{level} = factor
##' levels, \dQuote{min} and \dQuote{max} for continuous measurements,
##' \dQuote{comments} = comments about the variable which may include
##' the measurement instrument or references about the measurement
##' @param non.standard named list of non-standard names of columns
##' with names \code{c("var.name", "var.orig", "var.label", "levels", "min", "max")}
##' @param na.strings a character vector of strings which are to be
##' interpreted as \sQuote{NA} values.  Blank fields are also
##' considered to be missing values in logical, integer, numeric and
##' complex fields. Default: \code{c("", "NA", ".", " ")}
##' @return S3 object of type class \dQuote{codebook}
##' @author Peter Baker  \email{pete@@petebaker.id.au}
##' @examples
##' file.copy(system.file('demoFiles', 'data1_codebook.csv',
##'                       package='dryworkflow'), 'data1_codebook.csv')
##' data1_codebook <- readCodeBook("data1_codebook.csv",
##'          non.standard = list(levels = "Factor.Levels",
##'                              var.orig = "Old.Variable"))
##' @export
readCodeBook <-
  function(
    x, codebook.directory = NULL, 
    col.names = c(var.name = "Variable", var.orig = "Original.Name",
      var.label = "Label", levels = "Levels", min = "Min", max = "Max"),
    non.standard = NULL,
    na.strings = c("", "NA", ".", " "))
{
  
  if (is.null(codebook.directory)) {
    codebook.directory <- "."
  }

  ## set up column names for processing -----------------------------
  col.names <- match.arg(col.names, several.ok = TRUE)
  which.names <- c(var.name = NA, var.orig = NA, var.label = NA,
                   levels = NA, min = NA, max = NA)
  names.set <- names(which.names)

  if (!is.null(non.standard)){
    if (! all(names(non.standard) %in% names.set)){
      cat("User provided names for 'non.standard':\n")
      print(names(non.standard))
      cat("Should be in:\n")
      print(names.set)
      stop("Please provide correct names.")
    } else {
      col.names[names(non.standard)] <- non.standard
    }
  }
  
  ## read in codebook ----------------------------------------
  cat("\nFunction 'readCodeBook' largely untested: beware!\n\n")
  code.file <- file.path(codebook.directory, x)
  xCodes <- read.csv(code.file, na.strings = na.strings,
                     stringsAsFactors = FALSE)
  fileName <- deparse(substitute(x))
  colNames <- names(xCodes)

  ## check names present and not -------------------------------------------
  definedNames <- col.names %in% colNames # are these present
  presentNames <- unlist(col.names[definedNames])  # names that are present
  absentNames <- which.names[!(names(which.names) %in% names(presentNames))]
  
 
  ## are variables names same as specified and if not make suggestions
  if (!(all(colNames %in% col.names))){
    cat(stringr::str_c("File: '", fileName, "'"), "\n")
    cat("Column Names:\n")
    print(colNames)
    cat("Not all column names properly defined. Not defined:\n")
    print(absentNames)
    cat("\nVariables present:\n")
    print(presentNames)
    varNames <- tolower(colNames)
    possible <- list(var.name = "variable|var", var.orig = "orig|old",
                     var.label = "lab", levels = "lev", min = "min",
                     max = "max")
    cat("\nPotential variable names (see ?readCodeBook):\n")
    ptest <- function(y)
      {
        if (length(gg <- grep(possible[[y]], tolower(colNames)))>0)
          colNames[gg]
        else NA
      }
    poss <- lapply(names(possible), ptest)
    names(poss) <- names(possible)
    print(poss)                 
  }
  if ("levels" %in% names(absentNames)){
    cat("Warning: factor levels column not found.\n This should be set if possible\n")
    isFactorLevels <- FALSE
  } else {
    isFactorLevels <- TRUE    
  }
  
  ## variable labels ------------------------------------------------
  if (length(presentNames["var.label"]) > 0){
    vNames <- !is.na(xCodes[ ,presentNames["var.name"]])
    varLabels <- xCodes[ ,presentNames["var.label"]][vNames]
    names(varLabels) <- xCodes[ ,presentNames["var.name"]][vNames]
  }
  
  ## renamed variables: --------------------------------------------------
  ## if variable renamed then construct table with old, new name
  if ("var.orig" %in% names(presentNames)){
    ## extract old/new variable names
    renamedVars <- xCodes[,c(presentNames["var.name"],
                             presentNames["var.orig"])]
    ## drop wissings which are result of info re factor levels etc
    renamedVars <- renamedVars[!is.na(renamedVars[ ,presentNames["var.name"]]),]
  } else {
    renamedVars <- NA
  }

  ## set factor levels ------------------------------------------------
  if (isFactorLevels){
    xCodes$var.name.filled <-
      as.character(zoo::na.locf(xCodes[,presentNames["var.name"]]))
    ## appears more than twice then is a factor
    factors <- rle(xCodes$var.name.filled)
    n.levels <- factors$lengths
    factors <- factors$values[factors$lengths>1]
    n.levels <- n.levels[n.levels > 1]
    names(n.levels) <- factors
    
    factor.info <- xCodes[xCodes[, "var.name.filled"] %in% factors, ]
    tmp <- strsplit(factor.info[, presentNames["levels"]], "=")
    factor.info$fac.level <- sapply(tmp, function(y) y[1])
    factor.info$fac.label <- sapply(tmp, function(y) y[2])
    factor.info$Factors <- factor.info$var.name.filled
    ## hadley doesn't like dots so var.name.filled messes up VNF ok
    ## plyr::dlply(factor.info, #.(factor.info$Factors),
    ##              FACTOR,
    ##              function(y) list(fac.level = y$fac.level,
    ##                               fac.label = y$fac.label))
    ## but really weird plyr interaction is driving me mad - use by instead
    factorLevels <- 
      by(factor.info, factor.info$Factors, function(y)
        list(fac.level = y$fac.level, fac.label = y$fac.label))
    ## min and max for continuous -------------------------------------
    contVars <- xCodes[grep("[Cc]ont", xCodes[,presentNames["levels"]]),
                       presentNames["var.name"]]
    contVars <- unique(contVars)
    minMaxVars <- data.frame(var.name = contVars, min = NA, max = NA)
    
    if ("min" %in% names(presentNames)){
      for (C in contVars)
        minMaxVars[minMaxVars$var.name == C, "min"] <-
          xCodes[xCodes[, presentNames["var.name"]] == C, presentNames["min"]]
    }
    if ("max" %in% names(presentNames)){
      for (C in contVars)
        minMaxVars[minMaxVars$var.name == C, "max"] <-
          xCodes[xCodes[, presentNames["var.name"]] == C, presentNames["max"]]
    }
  } else {
    factorLevels <- factor.info <- factors <- contVars <- minMaxVars <- NA
  }
  
  ## store all codebook data away in a S3 "codebook" class
  code.book <- list(codeBook = xCodes,
                    varNames = names(varLabels),
                    varLabels = varLabels,
                    factorNames = factors,
                    factorLevels = factorLevels,
                    minMaxVars = minMaxVars,
                    factorInfo = factor.info,
                    renamedVars = renamedVars,
                    otherInfo = list(presentNames = presentNames,
                      absentNames = absentNames,
                      contNames = contVars))
  class(code.book) <- "codebook"
  comment(code.book) <- paste0("Codebook read from '", code.file,
                               "' at ", date())
  code.book
}

## codebook.directory <-  "../inst/demoFiles"
## x <- "data1_codebook.csv"
## col.names <- c(var.name = "Variable", var.orig = "Old.Variable",
##                var.label = "Label", levels = "Factor.Levels")

