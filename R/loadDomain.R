#' loadDomain
#'
#' @param domain A character vector that contains the acronym for the domain
#' @param dir The directory name containng the input files
#'
#' @return data frame containing the data from the input file
#' @export
#'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC
#' @examples
#' domain<-'BW'
#' bw<-loadDomain(domain, dir='/inputdir')
#'
loadDomain <- function(domain, dir=dir){
  if (!methods::hasArg(dir)) {dir<-dir <- dirname(file.choose())}
  stopifnot(is.character(domain), length(domain) ==1)
  # pattern<-paste0('^SEND_',domain,"-.*.csv")
  pattern<-paste0('^',domain,"_BASF_.*.CSV")
  files <- list.files(dir, pattern=pattern,ignore.case = T, full.names=TRUE)
  df <- lapply(files, data.table::fread, dec='.')
  return(df)
}
