\name{movingwindow}
\alias{movingwindow}
\title{
	Simple moving window function.
}
\description{
	Very simple function to apply a kernel to a matrix across a moving window.
}
\usage{
movingwindow(x, kernel)
}

\arguments{
  \item{x}{
	A matrix.
}
  \item{kernel}{
	The kernel to be applied to the matrix, for example a Sobel kernel.
}
}
\details{
	This function is used in the calculation of slope and aspect by slopeasp().
}
\value{
	Returns the transformed matrix.
}
\author{
	Sarah Goslee
}
\note{
	Should be rewritten in C for greater efficiency.
}
\seealso{
	\code{\link{slopeasp}}
}

\examples{
	data(dem)
	dem.smoothed <- movingwindow(dem, matrix(c(1,1,1,1,0,1,1,1,1), 3, 3)/8)

	par(mfrow=c(1,2))
	image(dem)
	image(dem.smoothed)
}
\keyword{ stats }

