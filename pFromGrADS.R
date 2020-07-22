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

    # convert GrADS file contents to raster object
    rr <- convertToRasterObject(xx, nx, ny, bbox, p4str)

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
  grdsize <- nx * ny
  
  grdconn <- file(file, "rb")
  
  # Read GrADS data set as stream skipping one record at beginning and end.
  # Records have a size of 4 bytes
  skip <- readBin(con = grdconn, what = "numeric", n = 1, size = 4)
  xx <- readBin(con = grdconn, what = "numeric", n = grdsize, size = 4)
  skip <- readBin(con = grdconn, what = "numeric", n = 1, size = 4)
  
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

# convertToRasterObject --------------------------------------------------------
convertToRasterObject <- function(xx, nx, ny, bbox, p4str)
{
  mm <- matrix(NA, nrow = ny, ncol = nx)

  for (j in 1:ny) {
    end <- j * nx
    start <- end - nx + 1
    mm[ny - j, ] <- xx[start:end]
  }
  
  rr <- raster::raster(mm, 
                       xmn = bbox['xmin'], xmx = bbox['xmax'], 
                       ymn = bbox['ymin'], ymx = bbox['ymax'],
                       crs = p4str)

  rr[rr == naValue] <- NA_real_
  
  rr
}

# extractRowFromRaster ---------------------------------------------------------
extractRowFromRaster <- function(tt, rr, lon, lat)
{
  paste(tt, 
        paste(raster::extract(rr, cbind(lon, lat)),
              collapse = ","),
        sep = ",")
}
