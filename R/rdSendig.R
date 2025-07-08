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
  d<-dict$SENDIG_DART_11_VICT3R_21 %>% dplyr::filter(`DomainPrefix`==domainName)
  d<-d %>% dplyr::select(c('VariableName')) # Select the column with the Variable Names
  d$val='' ## add value column for temporary use only
  dd<-tidyr::pivot_wider(d, names_from = 'VariableName', values_from = 'val') %>% 
    dplyr::rows_delete(dplyr::tibble(STUDYID=''))
  return(dd)
}