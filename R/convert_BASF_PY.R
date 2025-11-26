#' convert_BASF_PY
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The dataframe containing the export of Provantis
#'
#' @return SEND Converted Domain as dataframe, use BASF Provantis Export files
#' @export
#'
#' @examples
#'  domainName<-'PY'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertBW(domainName, domainData)
#'  

convert_BASF_PY<-function(domainName, domainData) {
  # load SEND names from Standard
  SEND_names<-rdSendig('PY')
  out_data<-tibble::as_tibble(domainData)
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[5]<-names(SEND_names[3]) # USUBJID
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  # browser()
  out_data<-out_data %>% tibble::add_column(DOMAIN='PY',.before="USUBJID") # add Domain column
  
  ########################## PREGNANCY STATUS ###################  
  pregnancy<-out_data %>% dplyr::select(STUDYID, USUBJID, `Preg. type`,`C-S date`,`C-S gest. day`)
  names(pregnancy)<-c('STUDYID', 'USUBJID', 'PYORRES', 'PYDTC', 'PYDY')
  pregnancy$DOMAIN<-'PY'
  pregnancy$PYSEQ<-'NULL'
  pregnancy$PYSEQ<-'NULL'
  pregnancy$PYTESTCD<-'PREGSTAT'
  pregnancy$PYTEST<-'PREGNANCY STATUS'
  pregnancy$PYORRESU<-'NULL'
  pregnancy$PYSTRESC<-toupper(pregnancy$PYORRES)
  pregnancy$PYSTRESN<-'NULL'
  pregnancy$PYSTRESU<-'NULL'
  pregnancy$PYRESCAT<-toupper(pregnancy$PYORRES)
  pregnancy$PYRESLOC<-'NULL'
  pregnancy$PYSTAT<-'NULL'
  pregnancy$PYREASND<-'NULL'
  pregnancy$PYMETHOD<-'NULL'
  pregnancy$PYEXCLFL<-'NULL'
  pregnancy$PYREASEX<-'NULL'
  pregnancy$RPHASE<-'GESTATION'
  pregnancy$RPPLDY<-'NULL'
  pregnancy$PYRPDY<-'NULL'
  pregnancy$PYDY <- as.character(pregnancy$PYDY)
  l<-Sys.getlocale()
  Sys.setlocale("LC_ALL", "English")
  date_format <- "%d-%b-%Y %H:%M"
  # # Convert Dates
  pregnancy$PYDTC<- format(as.POSIXct(pregnancy$PYDTC,format="%d-%b-%Y %H:%M"))
  # 
  Sys.setlocale("LC_ALL", "de_DE.UTF-8")
  
  SEND_names<-SEND_names %>% tibble::add_row(pregnancy)
  
  ########################## IMPLANTS LEFT ###################
  
  implants<-out_data %>% dplyr::select(STUDYID, USUBJID, `Implants (left)`,`C-S date`,`C-S gest. day`)
  names(implants)<-c('STUDYID', 'USUBJID', 'PYORRES', 'PYDTC', 'PYDY')
  implants$DOMAIN<-'PY'
  implants$PYSEQ<-'NULL'
  implants$PYSEQ<-'NULL'
  implants$PYTESTCD<-'IMPLANTS'
  implants$PYTEST<-toupper('Number of Implantations')
  implants$PYORRESU<-'NULL'
  implants$PYSTRESC<-toupper(implants$PYORRES)
  implants$PYSTRESN<-'NULL'
  implants$PYSTRESU<-'NULL'
  implants$PYRESCAT<-toupper(implants$PYORRES)
  implants$PYRESLOC<-'LEFT'
  implants$PYSTAT<-'NULL'
  implants$PYREASND<-'NULL'
  implants$PYMETHOD<-'NULL'
  implants$PYEXCLFL<-'NULL'
  implants$PYREASEX<-'NULL'
  implants$RPHASE<-'GESTATION'
  implants$RPPLDY<-'NULL'
  implants$PYRPDY<-'NULL'
  implants$PYDY <- as.character(implants$PYDY)
  l<-Sys.getlocale()
  Sys.setlocale("LC_ALL", "English")
  date_format <- "%d-%b-%Y %H:%M"
  # # Convert Dates
  implants$PYDTC<- format(as.POSIXct(implants$PYDTC,format="%d-%b-%Y %H:%M"))
  # 
  Sys.setlocale("LC_ALL", "de_DE.UTF-8")
  
  SEND_names<-SEND_names %>% tibble::add_row(implants)
  
  ########################## IMPLANTS RIGHT ###################
  
  implants<-out_data %>% dplyr::select(STUDYID, USUBJID, `Implants (right)`,`C-S date`,`C-S gest. day`)
  names(implants)<-c('STUDYID', 'USUBJID', 'PYORRES', 'PYDTC', 'PYDY')
  implants$DOMAIN<-'PY'
  implants$PYSEQ<-'NULL'
  implants$PYSEQ<-'NULL'
  implants$PYTESTCD<-'IMPLANTS'
  implants$PYTEST<-toupper('Number of Implantations')
  implants$PYORRESU<-'NULL'
  implants$PYSTRESC<-toupper(implants$PYORRES)
  implants$PYSTRESN<-'NULL'
  implants$PYSTRESU<-'NULL'
  implants$PYRESCAT<-toupper(implants$PYORRES)
  implants$PYRESLOC<-'RIGHT'
  implants$PYSTAT<-'NULL'
  implants$PYREASND<-'NULL'
  implants$PYMETHOD<-'NULL'
  implants$PYEXCLFL<-'NULL'
  implants$PYREASEX<-'NULL'
  implants$RPHASE<-'GESTATION'
  implants$RPPLDY<-'NULL'
  implants$PYRPDY<-'NULL'
  implants$PYDY <- as.character(implants$PYDY)
  
  Sys.setlocale("LC_ALL", "English")
  date_format <- "%d-%b-%Y %H:%M"
  # # Convert Dates
  implants$PYDTC<- format(as.POSIXct(implants$PYDTC,format="%d-%b-%Y %H:%M"))
  # 
  Sys.setlocale("LC_ALL", "de_DE.UTF-8")
  
  SEND_names<-SEND_names %>% tibble::add_row(implants)
  
  # toupper all relevant cols
  out_data<-out_data %>% dplyr::mutate_all(.funs=toupper)
  ##########################
  return(SEND_names)
  
  
}

