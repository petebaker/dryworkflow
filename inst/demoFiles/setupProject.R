## File:    setupProject.R
## Purpose: use 'dryworkflow' library to set up initial project structure

library(dryworkflow)

## Place relavent data, document (and perhaps R function) files in
## current directory. The next command sets up the project
createProjectSkeleton("test1")

## Once you are happy with created files, run this command to run R
## files and make
makeProject("test1")

## ideally run make from inside RStudio, emacs, command line or
## programmer's editor

## For testing run this to retrieve data/docs and remove
##removeProject("test1", ask=FALSE)

