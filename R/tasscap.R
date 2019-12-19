tasscap <-
function(basename, sat=7)

{
    # basename is the name of the band data files, which will have the band number appended
    # should be in at-sensor reflectance (rc1)
    # sat: 5 = Landsat 5 (TM), 7 = Landsat 7 (ETM+), 8 = Landsat 8 (OLI)
        
        # original papers
        # Kauth and Thomas
        # Crist and Cicone
        # Baig et al

    if(sat %in% c(5, 7)) {
        banda <- get(paste(basename, "1", sep=""))
        bandb <- get(paste(basename, "2", sep=""))
        bandc <- get(paste(basename, "3", sep=""))
        bandd <- get(paste(basename, "4", sep=""))
        bande <- get(paste(basename, "5", sep=""))
        bandf <- get(paste(basename, "7", sep=""))
    }

    if(sat == 8) {
        banda <- get(paste(basename, "2", sep=""))
        bandb <- get(paste(basename, "3", sep=""))
        bandc <- get(paste(basename, "4", sep=""))
        bandd <- get(paste(basename, "5", sep=""))
        bande <- get(paste(basename, "6", sep=""))
        bandf <- get(paste(basename, "7", sep=""))
    }

    if(is(banda, "SpatialGridDataFrame")) {
    	output.sgdf <- banda
	    use.sgdf <- TRUE
    	banda <- banda@data[,1]
    	bandb <- bandb@data[,1]
    	bandc <- bandc@data[,1]
    	bandd <- bandd@data[,1]
    	bande <- bande@data[,1]
    	bandf <- bandf@data[,1]
    }

    all.bands <- cbind(banda, bandb, bandc, bandd, bande, bandf)

        
    if(sat == 7) {
        tc.coef <- matrix(c(
        # Tasseled cap coefficients for Landsat 7 ETM+ at-satellite reflectance from HWY+2002
    # Band 1     Band 2       Band 3     Band 4     Band 5        Band 7     Index
     0.3561,     0.3972,      0.3904,    0.6966,    0.2286,       0.1596,    #  Brightness       
    -0.3344,    -0.3544,     -0.4556,    0.6966,   -0.0242,      -0.2630,    #  Greenness       
     0.2626,     0.2141,      0.0926,    0.0656,   -0.7629,      -0.5388,    #  Wetness          
     0.0805,    -0.0498,      0.1950,   -0.1327,    0.5752,      -0.7775,    #  Fourth           
    -0.7252,    -0.0202,      0.6683,    0.0631,   -0.1494,      -0.0274,    #  Fifth           
     0.4000,    -0.8172,      0.3832,    0.0602,   -0.1095,       0.0985     #  Sixth            
    ), ncol=6, byrow=TRUE)
    } else if(sat == 5) {
        tc.coef <- matrix(c(
        # TM Tasseled Cap Equivalent Transformation Matrix for Band Reflectance Factor from Crist1985
    # Band 1     Band 2       Band 3     Band 4     Band 5        Band 7     Index
     0.2043,     0.4158,      0.5524,    0.5741,     0.3124,       0.2303,    #  Brightness
    -0.1603,    -0.2819,     -0.4934,    0.7940,    -0.0002,      -0.1446,    #  Greenness
     0.0315,     0.2021,      0.3102,    0.1594,    -0.6806,      -0.6109,    #  Wetness
    -0.2117,    -0.0284,      0.1302,   -0.1007,     0.6529,      -0.7078,    #  Fourth
    -0.8669,    -0.1835,      0.3856,    0.0408,    -0.1132,       0.2272,    #  Fifth
     0.3677,    -0.8200,      0.4354,    0.0518,    -0.0066,      -0.0104     #  Sixth
    ), ncol=6, byrow=TRUE)
    } else if(sat == 8) {
        tc.coef <- matrix(c(
        # OLI Tasseled Cap Transformation coefficients for Landsat 8 at-satellite reflectance. 
    # Band 2     Band 3      Band 4      Band 5      Band 6      Band 7     Index
	 0.3029,	 0.2786,	 0.4733,	 0.5599,	 0.5080,	 0.1872,    #  Brightness
	-0.2941,	-0.2430,	-0.5424,	 0.7276,	 0.0713,	-0.1608,    #  Greenness
	 0.1511,	 0.1973,	 0.3283,	 0.3407,	-0.7117,	-0.4559,    #  Wetness
	-0.8239,	 0.0849,	 0.4396,	-0.0580,	 0.2013,	-0.2773,    #  Fourth
	-0.3294,	 0.0557,	 0.1056,	 0.1855,	-0.4349,	 0.8085,    #  Fifth
	 0.1079,	-0.9023,	 0.4119,	 0.0575,	-0.0259,	 0.0252     #  Sixth
    ), ncol=6, byrow=TRUE)
    } else {
        stop("sat not recognized.\n")
    }

    colnames(tc.coef) <- c("banda", "bandb", "bandc", "bandd", "bande", "bandf")
    rownames(tc.coef) <- c("Brightness", "Greenness", "Wetness", "Fourth", "Fifth", "Sixth")
    tc.coef <- t(tc.coef)

    output <- all.bands %*% tc.coef
    output <- as.data.frame(output[,1:3])

    if(use.sgdf) {
    	Brightness <- output.sgdf
	Brightness@data[,1] <- output[, "Brightness"]
    	Greenness <- output.sgdf
	Greenness@data[,1] <- output[, "Greenness"]
    	Wetness <- output.sgdf
	Wetness@data[,1] <- output[, "Wetness"]
	output <- list(Brightness=Brightness, Greenness=Greenness, Wetness=Wetness)
    }

    output
}

