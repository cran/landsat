lssub <- function(filename, outname, centerx, centery, centerepsg, widthx, widthy)
{

### subset a landsat image 

    ## get information about the landsat image
    ## requires gdalinfo
    lsinfo <- system(paste("gdalinfo ", filename, sep=""), intern=TRUE)

    # image origin
    lsorigin <- lsinfo[grepl("Origin", lsinfo)]
    lsorigin <- strsplit(lsorigin, "[()]")[[1]][2]
    lsorigin <- as.numeric(strsplit(lsorigin, ",")[[1]])

    lspixelsize <- lsinfo[grepl("Pixel Size", lsinfo)]
    lspixelsize <- strsplit(lspixelsize, "[()]")[[1]][2]
    lspixelsize <- as.numeric(strsplit(lspixelsize, ",")[[1]])
    lspixelsize <- abs(lspixelsize)

    lsepsg <- lsinfo[grepl("EPSG.*]]$", lsinfo)]
    lsepsg <- strsplit(lsepsg, '"')[[1]][4]

    # reproject target point if necessary
    if(!missing(centerepsg)) {
        if(centerepsg != lsepsg) {
            cat("reprojecting...\n")
            newcenter <- system(paste("echo ", centerx, " ", centery, " | gdaltransform -s_srs EPSG:", centerepsg, " -t_srs EPSG:", lsepsg, 
                sep=""), intern=TRUE)
            newcenter <- as.numeric(strsplit(newcenter, " ")[[1]][1:2])
            centerx <- newcenter[1]
            centery <- newcenter[2]
        }
    }

    # shift target point the minimum necessary to match pixel boundaries
    shiftval <- centerx - lsorigin[1]
    shiftval <- ((shiftval / lspixelsize[1]) - floor(shiftval / lspixelsize[1])) * lspixelsize[1]
    if(shiftval != 0) {
        #    if(shiftval < lspixelsize[1]) shiftval <- shiftval - lspixelsize[1]
        centerx <- centerx - shiftval
    }
    shiftval <- centery - lsorigin[2]
    shiftval <- ((shiftval / lspixelsize[2]) - floor(shiftval / lspixelsize[2])) * lspixelsize[2]
    if(shiftval != 0) {
        #    if(shiftval < lspixelsize[2]) shiftval <- shiftval - lspixelsize[2]
        centery <- centery - shiftval
    }

    system(paste("gdal_translate -projwin ", centerx - (lspixelsize[1]*widthx/2), " ", centery + (lspixelsize[2]*widthy/2), " ", centerx + (lspixelsize[1]*widthx/2), " ", centery - (lspixelsize[2]*widthy/2), " ", filename, " ", outname, sep=""), intern=TRUE)

    invisible()

}

