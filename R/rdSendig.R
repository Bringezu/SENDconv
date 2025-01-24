#' rdSendig
#' This module reads the SENDIG 
#' @param domain A character vector that contains the acronym for the domain
#'
#' @return SEND Converted Domain as dataframe
#' @export
#'
#' @examples
#'  domainName<-'BW'
#'  domainData<-rdSendig(domainName)
#'
rdSendig<-function(domainName) {
  d<-dictionary %>% dplyr::filter(`Domain Prefix`==domainName)
  d<-d %>% dplyr::select(c('Variable Name')) # Select the column with the Variable Names
  d$val='' ## add value column for temporary use only
  dd<-tidyr::pivot_wider(d, names_from = 'Variable Name', values_from = 'val') %>% 
    dplyr::rows_delete(dplyr::tibble(STUDYID=''))
  return(dd)
}