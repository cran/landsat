\name{relnorm}
\alias{relnorm}
\title{
	Relative normalization of an image
}
\description{
	Use regression methods to adjust distribution of values in image tofix to match those in the master image. 
}
\usage{
relnorm(master, tofix, mask, method = "MA", nperm = 1000)
}
\arguments{
  \item{master}{
	The target image, in SpatialGridDataFrame, data frame, matrix or vector format.
}
  \item{tofix}{
	The image to be normalized, in any format.
}
  \item{mask}{
	Areas to be omitted, if any, such as a cloud mask. Only NA values within the mask will be used.
}
  \item{method}{
	Regression method to be used. OLS: Ordinary Least Squares; MA: Major Axis (recommended); SMA: Standard Major Axis.
}
  \item{nperm}{
	Number of permutations to use for significance testing.
}
}
\details{
	The regression coefficients from tofix ~ master will be used to match the distribution of values of tofix to those in the master image.
}
\value{
	\item{regression.results }{The regression results from lmodel2}
	\item{newimage }{The transformed image, in the same format in which tofix was provided.}
}
\author{
	Sarah Goslee
}

\seealso{
\code{\link{histmatch}}
}
\examples{
\dontrun{
	data(nov3)
	data(july3)
	par(mfrow=c(2,2))
	image(nov3)
	image(july3)

	nov3.newR <- relnorm(master=july3, tofix=nov3)
	image(nov3.newR$newimage)

	nov3.newH <- histmatch(master=july3, tofix=nov3)
	image(nov3.newH$newimage)
}
}
\keyword{ stats }
\keyword{ image }
