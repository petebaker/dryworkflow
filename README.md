# dryworkflow

The information below is also available in package help.

Also see [the blog site](http://www.petebaker.id.au/r-package-dryworkflow "Peter Baker's R blog") for details.

## Installation

In *R* as Administrator, to install please use the following:

```{r}
library(devtools)  # available on CRAN (or github)
devtools::install_github("petebaker/dryworkflow")
```

### Prerequisites

To use Makefile definitions and version control using git, you need to install
- GNU Make  [http://www.gnu.org/software/make/](http://www.gnu.org/software/make/)
- R         [http://www.r-project.org/](http://www.r-project.org/)
- latexmk   [http://www.ctan.org/pkg/latexmk/](http://www.ctan.org/pkg/latexmk/)
- R packages on CRAN: rmarkdown, knitr

Note that **Windows** users can install Rtools (available from CRAN) to get a working version of *make* and may also need to install *pandoc* and *latex* to produce pdf files if they haven't already. Miktex is recommended although texlive will also work well.
- Rtools   [http://cran.r-project.org/bin/windows/Rtools/](http://cran.r-project.org/bin/windows/Rtools/)
- pandoc   [http://johnmacfarlane.net/pandoc/](http://johnmacfarlane.net/pandoc/)
- miktex   [http://miktex.org/](http://miktex.org/)

**MACOSX** users should install a recent version of *Xcode CLT* (Xcode command line tools) and *Homebrew* in order to install *make* and *git*. Unfortunately, I don't yet know much about Macs as my brand new (and first) MacBook Pro is still in for repairs. For some hints try [http://www.moncefbelyamani.com/how-to-install-xcode-homebrew-git-rvm-ruby-on-mac/](http://www.moncefbelyamani.com/how-to-install-xcode-homebrew-git-rvm-ruby-on-mac/). Finally, to produce pdf reports *MacTex* [https://tug.org/mactex/](https://tug.org/mactex/) is recommended.

In **linux**, if they aren't already installed, simply install these
packages using the system package manager.

The easiest way to install *git* an *pandoc* on all platforms is to
install *RStudio*. If you don't have a favourite programmer's editor
that you already use for **R** then this is the best way to use **R**
as well.  Install **RStudio** from http://rstudio.org. Note that you
may need to put he file containing *RStudio* etc in the *PATH*.

You can check that *make*, *git* and *pandoc* are installed by typing

```{bash}
git --version
make --version
pandoc --version
```

Finally, check that latex is available with

```{bash}
pdflatex --version
```

## Using the dryworkflow package

The **dryworkflow** package produces a project skeleton for data
analysis including *R* syntax files, report and Makefiles. Given data
files and documents, the skeleton is generated with initial
directories, template log files, template *R* syntax for data checking
and initial analysis, makefiles and a *git* repository is initialised.

### Templates

*R* syntax templates for reading, cleaning, merging, summarising and
analysing data and *Rmarkdown* and *Sweave* templates for reports. The
function *copyTemplates* may be used to get copies of these templates
which can then be modified for use when creating a project skeleton.

### Make and definitions

Makefiles are generated. The file *common.mk* provides pattern rules
to produce *.Rout* and *.pdf* files from *R* syntax files and *.html*,
*.pdf* and *.docx* files from *.Rmd* R markdown and *.Rnw* files.  The
function *copyCommonMk* may be used to get a copy the *common.mk* file
used by the installed version of the *dryworkflow* package. The latest
version of *common.mk* can always be found at
[https://github.com/petebaker/r-makefile-definitions](https://github.com/petebaker/r-makefile-definitions)

### .gitignore

A *.gitignore* file is created in the base project directory to
indicate files not to be tracked by *git*.  The function
*copyGitIgnore* may be used to get a copy the *.gitignore* file used
by the installed version of the *dryworkflow* package. The latest
version of *.gitignore* can always be found at
[https://github.com/petebaker/r-gitignore](https://github.com/petebaker/r-gitignore)

### Project Options

Note that option parameters are either set as an argument to the
function *createProjectSkeleton* or automatically via global options
using *getOption("dryworkflow")*. Customised options may be set in
*.Rprofile* using global options and these will be set automatically
when *dryworkflow* is loaded.

### Examples

#### setting global options or put these in .Rprofile

```{r}
current.opts <- options()
options("dryworkflow" = list(git = list(user.name = "My Name", user.email = "myname@email.com")))
library(dryworkflow)
options("dryworkflow")
```

#### A project with all default settings

```{r}
## copy .csv file and codebook from dryWorkflow package
## noting that normally you just place files in current directory
## and then run 'createProjectSkeleton'
file.copy(system.file('demoFiles', 'small2.csv', package='dryworkflow'),
          'small2.csv')
file.copy(system.file('demoFiles', 'small2_codebook.csv',
                      package='dryworkflow'), 'small2_codebook.csv')

## NB: In practice, always check directories, R syntax  etc
##     before using 'make'
createProjectSkeleton(dir.proj = "testProject2",
                      name.project = "Experiment 1",
                      dontmove = "dryworkflow-Ex.R")
```

