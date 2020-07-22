# function for reading GrADS precipitation data sets and building time series
pFromGrADS <- function(coordFile, gridsPath, outFile, 
                       bbox, nx, ny, naValue, p4str,
                       dateTimeSep) {

  library(sp)
  library(raster)

  # read coordinates file
  coords <- readCoordinates(coordFile)
  
  # list GrADS files
  grdFiles <- listGridFiles(gridsPath)
  
  # prepare output data.frame
  rain <- initRainDataFrame(columns = coords$label)
  
  # write empty output data.frame to disk
  writeRainData(rain, outFile)

  # list for storing resulting raster objects
  rList <- list()
  
  # progress bar
  i <- 0 
  pb <- txtProgressBar(min = 0, max = length(grdFiles), style = 3)
  
  # loop over GrADS files
  for (grdi in grdFiles) {
    
    # update counter
    i <- i + 1
    
    # read GrADS data values
    xx <- readGradsData(file = paste0(gridsPath, grdi), nx, ny)

    # get dateTime from GrADS file name
    tt <- getDateTimeFromFilename(grdi, sep = dateTimeSep)

    # convert GrADS file contents to a matrix
    mm <- valuesToMatrix(xx, nx, ny, naValue)
    
    # convert matrix to raster object
    rr <- convertToRasterObject(mm, bbox, p4str)

    # store resulting raster in rList
    rList[[tt]] <- rr

    # extract values at coordinates and add them to data.frame
    row <- extractRowFromRaster(tt, rr, lon = coords$lon, lat = coords$lat)

    # append line to results file
    write(row, file = outFile, append = TRUE)
    
    # update progress bar and counter
    setTxtProgressBar(pb, i)
  }
  
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

# listGridFiles ----------------------------------------------------------------
listGridFiles <- function(gridsPath)
{
  if (! dir.exists(gridsPath))
    stop('folder ', gridsPath, ' not found')
  
  list.files(gridsPath, pattern = "*.grd")
}

# initRainDataFrame ------------------------------------------------------------
initRainDataFrame <- function(columns)
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
readGradsData <- function(file, nx, ny)
{
  # Open the file as a binary stram. Close the file when this function is quit
  # (either regularly or due to an error)
  con <- file(file, "rb")
  on.exit(close(con))
  
  # Records have a size of 4 bytes. Read one more than required for the grid.
  # Directly remove the first record by indexing with [-1]
  readBin(con, what = "numeric", n = nx * ny + 1L, size = 4L)[-1L]
}

# getDateTimeFromFilename ------------------------------------------------------
getDateTimeFromFilename <- function(grdi, sep)
{
  sub(pattern = ".grd", 
      replacement = "", 
      x = strsplit(x = grdi, 
                   split = sep)[[1]][2])
}

# convertToRasterObject --------------------------------------------------------
convertToRasterObject <- function(mm, bbox, p4str)
{
  raster::raster(mm, 
                 xmn = bbox['xmin'], xmx = bbox['xmax'], 
                 ymn = bbox['ymin'], ymx = bbox['ymax'],
                 crs = p4str)
}

# valuesToMatrix ---------------------------------------------------------------
valuesToMatrix <- function(x, nx, ny, naValue)
{
  m <- matrix(NA, nrow = ny, ncol = nx)
  
  for (j in 1:ny) {
    end <- j * nx
    start <- end - nx + 1
    m[ny - j, ] <- x[start:end]
  }
  
  m[m == naValue] <- NA_real_
  
  m
}

# extractRowFromRaster ---------------------------------------------------------
extractRowFromRaster <- function(tt, rr, lon, lat)
{
  paste(tt, 
        paste(raster::extract(rr, cbind(lon, lat)),
              collapse = ","),
        sep = ",")
}
