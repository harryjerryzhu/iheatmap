\name{get_iheatmap}
\alias{get_iheatmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get the interactive heatmap
}
\description{
%%  ~~ show the interactive heatmap ~~
}
\usage{
get_iheatmap(iheatmap_object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  iheatmap_object

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- return an interactive heatmap plot ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
IHeatMap_object<-IHeatMap(expression=expression,profile=profile)
iheatmap_object<-create_iheatmap (IHeatMap_object)
get_iheatmap (iheatmap_object)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
