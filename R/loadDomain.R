#' loadDomain
#'
#' @param domain A character vector that contains the acronym for the domain
#' @param dir The directory name containing the input files
#'
#' @return data frame containing the data from the input file
#' @examples
#' domain<-'BW'
#' bw<-loadDomain(domain, dir='/inputdir', pattern=pattern, dec=dec)
#' @export
loadDomain <- function(domain, dir=dir, pattern=pattern, dec=dec){
  if (!methods::hasArg(domain)) {return('please provide a Domain acronyme, e.g. BW for Bodyweight')} 
  # If no dir was provided, select a  directory 
  if (!methods::hasArg(dir)) {dir<- dirname(file.choose())} # 
  # Define a default pattern
  if (!methods::hasArg(pattern)) {pattern<- paste0('SEND_',domain,'-.*.csv')}  
  # Define a default pattern
  if (!methods::hasArg(dec)) {dec<- ','}  
  
  stopifnot(is.character(domain), length(domain) ==1)
  
  # pattern<-paste0('^SEND_',domain,"-.*.csv")
  # pattern<-paste0('^',domain,"_BASF_.*.CSV")
  # files <- list.files(dir, pattern=pattern,ignore.case = T, full.names=TRUE)
  
  # filter files per date (only files not older than 7 days)
  md<-14
  files <- list.files(dir,pattern=pattern,ignore.case = T, full.names=TRUE) %>% 
    file.info() %>%
    tibble::rownames_to_column() %>% 
    dplyr::filter(as.Date(ctime) > Sys.Date()-md)
  if (nrow(files)>0) {df <- lapply(files$rowname, data.table::fread, dec=dec)} else(df<-NULL)
  
  
  return(df)
}
