\name{blueprint}
\alias{blueprint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
blueprint
}
\description{
Use blueprint-file to import a subset of variables from several data files, optionally
transform them by specified functions (e.g. to recode values) and recombine them into a new wide data.frame.

}
\usage{
blueprint(blueprint = "/Users/eur/Documents/140_Datenaufbereitung/pisa.xlsx", sheet = 1, waves = 1, debug = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{blueprint}{
A meta-data file that contains specifications what to do. The file
format is taken from the suffix as defined in the \link{package::rio}{rio}-package.

}
  \item{...}{
Optionally commands given to 
}
  \item{waves}{
%%     ~~Describe \code{waves} here~~
}
  \item{debug}{
Will be removed when alpha.
}
}
\details{

}
\value{
Returns a new data.frame / \link{dplyr::tibble}. 
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
"blueprint"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
