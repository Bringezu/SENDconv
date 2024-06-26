#' loadDomain
#'
#' @param domain A character vector that contains teh acronym for the domain
#' @param dir The directory name containng the input files
#'
#' @return data frame containing the data from the input file
#' @export
#'
#' @examples
#' domain<-'BW'
#' bw<-loadDomain(domain, dir='/inputdir')
#'
loadDomain <- function(domain, dir=dir){
  if (!methods::hasArg(dir)) {dir<-dir <- dirname(file.choose())}
  stopifnot(is.character(domain), length(domain) ==1)
  pattern<-paste0("SEND_",domain,"-.*.csv")
  files <- list.files(dir, pattern=pattern, full.names=TRUE)
  df <- lapply(files, data.table::fread)
  return(df)
}
