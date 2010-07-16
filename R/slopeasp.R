slopeasp <-
function(x, EWres, NSres, EWkernel, NSkernel, smoothing=1)
{

    ## calculate slope, aspect, illumination from elevation data
    ## uses a 3x3 window
    ## x contains elevation data
    ## res is cell size (resolution) in the same units as elevation
    ## for EW and NS directions (cells need not be square)

    ## smoothing: smoothing coefficient for calculating slope

    # works whether x is a SpatialGridDataFrame or a dataframe
    # maintain the original x so we can return the same class of result
    xmat <- as.matrix(x)

    if(missing(EWres) & class(x) == "SpatialGridDataFrame") {
        EWres <- x@grid@cellsize[1]
    }
    else {
        stop("EWres must be specified if x is not a SpatialGridDataFrame.\n")
    }
    if(missing(NSres) & class(x) == "SpatialGridDataFrame") {
        NSres <- x@grid@cellsize[2]
    }
    else {
        stop("NSres must be specified if x is not a SpatialGridDataFrame.\n")
    }

    # basic 3x3 kernel by default, but can specify others
    if(missing(EWkernel)) {
        EWkernel <- matrix(
        c(-1/8, 0, 1/8,
          -2/8, 0, 2/8,
          -1/8, 0, 1/8), 
        ncol=3, nrow=3, byrow=TRUE)
    }
    EW.mat <- movingwindow(xmat, EWkernel) / EWres

    if(missing(NSkernel)) {
        NSkernel <- matrix(
        c(1/8,   2/8,  1/8,
             0,    0,    0,
          -1/8, -2/8, -1/8), 
        ncol=3, nrow=3, byrow=TRUE)
    }
    NS.mat <- movingwindow(xmat, NSkernel) / NSres

    ## calculate slope
    ## smoothing correction for slope, useful for some image topographic corrections

    slope <- atan(sqrt(EW.mat^2 + NS.mat^2)/smoothing)
    slope <- (180/pi) * slope # convert to degrees

    # want to end up with 0 < aspect <= 360 where terrain is sloped; 0 where slope is flat
    # 0 == N; aspect goes clockwise from N

##    aspect <- matrix(NA, nrow=nrow(xmat), ncol=ncol(xmat))
##    aspect[EW.mat < 0 & !is.na(EW.mat) & NS.mat < 0 & !is.na(NS.mat)] <- atan(EW.mat/NS.mat)[EW.mat < 0 & !is.na(EW.mat) & NS.mat < 0 & !is.na(NS.mat)]
##    aspect[NS.mat >= 0 & !is.na(NS.mat)] <- pi + atan(EW.mat/NS.mat)[NS.mat >= 0 & !is.na(NS.mat)]
##    aspect[EW.mat >= 0 & !is.na(EW.mat) & NS.mat < 0 & !is.na(NS.mat)] <- 2*pi + atan(EW.mat/NS.mat)[EW.mat >= 0 & !is.na(EW.mat) & NS.mat < 0 & !is.na(NS.mat)]
##    aspect <- (180/pi) * aspect # convert to degrees

    # CL2007 provided better equation
    aspect <- 180 - (180/pi) * atan(NS.mat / EW.mat) + 90 * (EW.mat / abs(EW.mat))

    aspect[slope == 0] <- 0

    # return results as SpatialGridDataFrame if input was
    if(class(x) == "SpatialGridDataFrame") {
        temp <- x
        temp@data[,1] <- as.vector(aspect)
        aspect <- temp
        temp@data[,1] <- as.vector(slope)
        slope <- temp
    }

    list(slope=slope, aspect=aspect)
}

