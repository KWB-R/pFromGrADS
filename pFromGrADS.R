# function for reading GrADS precipitation data sets and building time series
pFromGrADS <- function(coordFile, gridsPath, outFile, 
                       bbox, nx, ny, naValue, recordSize,
                       p4str, dateTimeSep, returnRasterList){

  library(sp)
  library(raster)

  # read coordinates file
  coords <- readCoordinates(coordFile)
  
  # list GrADS files
  grdFiles <- listGradsFiles(gridsPath)
  
  # prepare output data.frame
  rain <- initRainData(columns = coords$label)
  
  # write empty output data.frame to disk
  writeRainData(rain, outFile)

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
    xx <- readGradsData(grdname = paste0(gridsPath, grdi), 
                        grdsize = nx * ny,
                        recordSize = recordSize)
    
    # get dateTime from GrADS file name
    tt <- getDateTimeFromFilename(grdi, sep = dateTimeSep)

    # convert GrADS file contents to raster object
    mm <- valuesToMatrix(xx, nrow = ny, naValue = naValue)
    rr <- convertToRasterObject(mm, bbox, p4str)

    # store resulting raster in rList
    if(returnRasterList){rList[[tt]] <- rr}
    
    # extract values at coordinates and add them to data.frame
    row <- extractRowFromRaster(tt, rr, lon = coords$lon, lat = coords$lat)

    # append line to results file
    write(row, file=outFile, append=TRUE)
    
    # update progress bar and counter
    setTxtProgressBar(pb, i)
  }
  
  if(returnRasterList)
    return(rList)
}

# readCoordinates --------------------------------------------------------------
readCoordinates <- function(coordFile)
{
  if (! file.exists(coordFile))
    stop('file ', coordFile, ' not found')
  
  read.table(coordFile, 
             sep = ';', 
             header = TRUE, 
             dec = '.',
             stringsAsFactors = FALSE)
}

# listGradsFiles ---------------------------------------------------------------
listGradsFiles <- function(gridsPath)
{
  if (! dir.exists(gridsPath))
    stop('folder ', gridsPath, ' not found')
  
  list.files(gridsPath, pattern = "*.grd")
}

# initRainData -----------------------------------------------------------------
initRainData <- function(columns)
{
  columns <- stats::setNames(nm = c("dateTime", columns))

  do.call(data.frame, lapply(columns, function(i) character()))
}

# writeRainData ----------------------------------------------------------------
writeRainData <- function(rain, outFile)
{
  if (file.exists(outFile))
    stop('file ', outFile, ' already exists; rename old file or use different name')
  
  write.table(rain, outFile, quote = FALSE, row.names = FALSE, sep = ",")
}

# readGradsData ----------------------------------------------------------------
readGradsData <- function(grdname, grdsize, recordSize)
{
  grdconn = file(grdname, "rb")
  xx = readBin(con=grdconn, what="numeric", n=grdsize, size=recordSize)
  close(grdconn)
  xx
}

# getDateTimeFromFilename ------------------------------------------------------
getDateTimeFromFilename <- function(grdi, sep)
{
  sub(pattern = ".grd", 
      replacement = "", 
      x = strsplit(x = grdi, 
                   split = sep)[[1]][2])
}

# valuesToMatrix ---------------------------------------------------------------
valuesToMatrix <- function(x, nrow, naValue)
{
  # Arrange the values in a matrix, fill the matrix row by row
  m <- matrix(x, nrow = nrow, byrow = TRUE)
  
  # Replace NA indicating value with a proper NA value
  m[m == naValue] <- NA_real_
  
  # Revert the row order
  m[rev(seq_len(nrow)), ]
}
                             
# convertToRasterObject --------------------------------------------------------
convertToRasterObject <- function(mm, bbox, p4str)
{
  raster::raster(mm, 
                 xmn = bbox['xmin'], xmx = bbox['xmax'], 
                 ymn = bbox['ymin'], ymx = bbox['ymax'],
                 crs = p4str)
}

# extractRowFromRaster ---------------------------------------------------------
extractRowFromRaster <- function(tt, rr, lon, lat)
{
  paste(tt, 
        paste(raster::extract(rr, cbind(lon, lat)),
              collapse = ","),
        sep = ",")
}
