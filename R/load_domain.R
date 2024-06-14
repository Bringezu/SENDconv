#' Title
#'
#' @param load_domain 
#' @param dir - character containg the directory name for the source files
#' @param domain - character containing the abbreviation for the domain (e.g. BW for body weight etc.) 
#'
#' @return data frame of the csv export file
#' @export data frame in SEND like structure
#'
#' @examples bw<-load_domain('BW', dir='input_dir')
load_domain <- function(domain, dir=dir){
  if (!hasArg(dir)) {dir <- dirname(file.choose())}
  stopifnot(is.character(domain), length(domain) ==1)
  pattern<-paste0("SEND_",domain,"-.*.csv")
  files <- list.files(indir, pattern=pattern, full.names=TRUE)
  df <- lapply(files, fread)
  return(df)
}