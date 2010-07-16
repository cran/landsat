\name{thermalband}
\alias{thermalband}
\title{
	Thermal band to temperature conversion.
}
\description{
	Converts Landsat thermal band DN (TM or ETM+ band 6-1 and 6-2) to temperature using default coefficients from Chander et al. 2009.
}
\usage{
thermalband(x, band)
}
\arguments{
  \item{x}{
	Landsat band 6 Digital Number (DN) in matrix, data frame or SpatialGridDataFrame format.
}
  \item{band}{
	6 for TM; 61 or 62 for the appropriate ETM+ bands. Any other value will fail.	
}
}
\value{
	Returns a temperature image in the same format as x.
}
\references{
	Coefficients from Chander, G., Markham, B.L., Helder, D.L. 2009. Summary of current radiometric calibration coefficients for Landsat MSS, TM, ETM+, and EO-1 ALI sensors. Remote Sensing of Environment 113:893-903.
}
\author{
	Sarah Goslee
}
\examples{
	data(nov61)
	nov.temp1 <- thermalband(nov61, 61)
	image(nov.temp1)
}
\keyword{ spatial }

