\name{load.blue}
\alias{load.blue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
load.blue
}
\description{
Creates or loads blueprint-file.

}
\usage{
open.blue(blueprint = paste0(getwd(),'/blueprint.xlsx'), waves = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{blueprint}{
A meta-data file that contains specifications what to do. The file
format is taken from the suffix as defined in the
\link{package::rio}{rio}-package. See the vignette for details of the structure.

}
  \item{waves}{
A number specifying the number of waves that shall be included in the
  new blueprint file. This can be changed later manually.
}
}
\details{
}
\value{
No Value. It will just generate or open the specified blueprint file.
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
## blue.laod('/path/to/file.xlsx')
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ merging }% use one of  RShowDoc("KEYWORDS")
