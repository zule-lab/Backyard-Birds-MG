download_file <- function(url, dest){
  if (extension == 'csv') {
    download_csv(url, dest)
  } else if (extension == 'tif') {
    download_tif(url, dest)
  } else if (extension == 'shp') {
    download_shp(url, dest)
  } else if (extension == 'zip'){
    
  }
}


download_tif <- function(url, dest){
  temp <- tempfile()
  download.file(url, dest, mode = "wb")
  star <- read_stars(file.path("/vsizip", dest), proxy = TRUE)
  return(star)
}


download_csv <- function(url, sub){
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  master <- as.character(unzip(temp, list = TRUE)$Name)
  df <- read.csv(unz(temp, master[sub]))
  return(df)
}


download_shp <- function(url, dest){
  temp <- tempfile()
  download.file(url, dest, mode = "wb")
  shp <- st_read(file.path("/vsizip", dest))
  return(shp)
}