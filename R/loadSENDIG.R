#' loadSENDIG
#' @param domain SEND domain to be processed
#' @return ddd Empty data frame for domain with column names extracted from SENDIG
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr filter
#' @importFrom tibble as_tibble
#' @examples
#' domain<-'BW'
#' d<-loadSENDIG(domain=domain)
#' @export
#'


loadSENDIG <- function(domain=domain){
  d<-createDict()
  dd<-as_tibble(d$SENDIG_DART_11_VICT3R_21)
  dd<-dd %>% filter(DomainPrefix==domain)
  dd<-dd %>% dplyr::select(c('VariableName')) # Select the column with the Variable Names
  dd$val='' ## add value column for temporary use only
  ddd<-pivot_wider(dd, names_from = 'VariableName', values_from = 'val')
  return(ddd)

}

