\name{blue}
\alias{blue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
blueprint
}
\description{
Use blueprint-file to import a subset of variables from several data files, optionally
transform them by specified functions (e.g. to recode values) and recombine them into a new wide data.frame.

}
\usage{
blueprint(blueprint =
"/Users/eur/Documents/140_Datenaufbereitung/pisa.xlsx",
logfile=determinde.by.blueprint, , waves
= 1, out_file=NULL)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{blueprint}{
A meta-data file that contains specifications what to do. The file
format is taken from the suffix as defined in the
\link{package::rio}{rio}-package. See the vignette for details of the structure.

}
  \item{waves}{
A numeric vector specifying the waves that shall be included from the
  blueprint file.
}
  \item{logfile}{
Path to a logfile that shall be used for logging
}
  \item{fun=TRUE}{
Logical vector wheter the functions from fun should be applied on the specified variables.
}
  \item{exended}{
Logical vector wheter extended logging (including descriptive
  statistics shall be written to the logfile. This by now takes
  considerably more time.
}
  \item{out_file}{
Path to file the data is written after merging. The suffix determines
  the file type.
}

  \item{debug}{
Will be removed when alpha.
}
  \item{...}{
Optionally commands given to import. Note the which command to
  specify sheets when loading XSLX files.
}

}
\details{

}
\value{
Returns a new merged data.frame according to the blueprint  / \link{dplyr::tibble}. 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Marc Schwenzer <m.schwenzer@uni-tuebingen.de>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## blueprint('/path/to/file')
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ merging }% use one of  RShowDoc("KEYWORDS")
