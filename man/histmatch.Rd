\name{histmatch}
\alias{histmatch}
\title{
	Histogram matching of an image
}
\description{
	Force image x to match target image by matching their histograms.
}
\usage{
histmatch(master, tofix, mask, minval = 0, maxval = 255, by = 1)
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
  \item{minval}{
	Lower bound of the possible range of values in target and tofix images.
}
  \item{maxval}{
	Upper bound of the possible range of values in target and tofix images.
}
  \item{by}{
	Step size to use in constructing histograms. Should be appropriate for minval and maxval of the images.
}
}
\details{
	The histogram of the tofix image will be forced to match that of the target image.
}
\value{
	\item{recode }{The transformation table used to match the histograms.}
	\item{newimage }{The transformed image, in the same format in which tofix was provided.}
}
\author{
	Sarah Goslee
}

\seealso{
	\code{\link{relnorm}}
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
