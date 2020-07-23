# function for reading GrADS precipitation data sets and building time series
pFromGrADS <- function(coordFile, gridsPath, outFile, 
                       bbox, nx, ny, naValue, recordSize,
                       p4str, dateTimeSep, returnRasterList){

  library(sp)
  library(raster)

  # read coordinates file
  if(!file.exists(coordFile))
    stop('file ', coordFile, ' not found')
  coords <- read.table(coordFile, 
                       sep=';', 
                       header=TRUE, 
                       dec='.',
                       stringsAsFactors=FALSE)
  
  # list GrADS files
  if(!dir.exists(gridsPath))
    stop('folder ', gridsPath, ' not found')
  grdFiles <- list.files(gridsPath, pattern="*.grd")
  
  # prepare output data.frame
  rain <- data.frame(dateTime=character())
  for(i in 1:nrow(coords)){rain <- cbind(rain, character())}
  names(rain) <- c("dateTime", coords$label)
  
  # write empty output data.frame to disk
  if(file.exists(outFile))
    stop('file ', outFile, ' already exists; rename old file or use different name')
  write.table(rain, outFile, quote=FALSE, row.names=FALSE, sep=",")
  
  # list for storing resulting raster objects
  if(returnRasterList){rList <- list()}
  
  # progress bar
  i=0 
  pb <- txtProgressBar(min=0, max=length(grdFiles), style=3)
  
  # loop over GrADS files
  for(grdi in grdFiles){
    
    # update counter
    i=i+1
    
    # read GrADS data set as stream; records have size = recordSize (bytes)
    grdname = paste(gridsPath, grdi, sep="")
    grdsize = nx*ny
    grdconn = file(grdname, "rb")
    xx = readBin(con=grdconn, what="numeric", n=grdsize, size=recordSize)
    close(grdconn)
    
    # get dateTime from GrADS file name
    tt <- sub(pattern=".grd", 
              replacement="", 
              x=strsplit(x=grdi, 
                         split=dateTimeSep)[[1]][2])
    
    # convert GrADS file contents to raster object
    mm <- matrix(xx, nrow=ny, byrow=TRUE)[rev(seq_len(ny)), ]
    mm[mm == naValue] <- NA_real_
    rr <- raster::raster(mm, 
                         xmn=bbox['xmin'], xmx=bbox['xmax'], 
                         ymn=bbox['ymin'], ymx=bbox['ymax'],
                         crs=p4str)
    
    
    # store resulting raster in rList
    if(returnRasterList){rList[[tt]] <- rr}

    # extract values at coordinates and add them to data.frame
    row <- paste(tt, 
               paste(raster::extract(rr, cbind(coords$lon, coords$lat)),
                     collapse=","),
               sep=",")
    
    # append line to results file
    write(row, file=outFile, append=TRUE)
    
    # update progress bar and counter
    setTxtProgressBar(pb, i)
  }
  
  if(returnRasterList)
    return(rList)
}
