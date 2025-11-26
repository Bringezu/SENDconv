#' convert_BASF_CL
#'
#' @param doCLinName A character vector that contains the acronym for the doCLin
#' @param doCLinData The data frame containing the export of Provantis
#'
#' @return SEND Converted DoCLin as dataframe
#' @export
#'
#' @examples
#'  doCLinName<-'CL'
#'  doCLinData<-loadDoCLin(domainName)
#'  SEND<-convertCL(domainName, domainData)
#'
convert_BASF_CL<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names<-rdSendig('CL')
  out_data<-tibble::as_tibble(domainData)
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[5]<-names(SEND_names[3]) # USUBJID
  names(out_data)[9]<-names(SEND_names[31]) # CLDY
  names(out_data)[12]<-names(SEND_names[29]) # CLDTC
  names(out_data)[17]<-names(SEND_names[14]) # CLORRES
  names(out_data)[19]<-names(SEND_names[21]) # CLORRES
  names(out_data)[15]<-names(SEND_names[10]) # CLTEST
  names(out_data)[21]<-names(SEND_names[19]) # CLLOC
  
  
  
  out_data<-out_data %>% tibble::add_column(DOCLIN='CL',.before="USUBJID") # add Domain column
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  out_data<-out_data %>% dplyr::mutate(CLORRES  = ifelse(`Is NAD` != 'No', 'UNREMARKABLE' , CLORRES))
  
  
  # out_data<-out_data %>% dplyr::mutate(CLORRES = ifelse(CLSTAT == 'Not Examined', 'NOT EXAMINED' , CLORRES))
  out_data<-out_data %>% dplyr::mutate(CLTEST = ifelse(grepl("Clinical Observations", CLTEST), 'Routine', CLTEST))
  out_data<-out_data %>% dplyr::mutate(CLTEST = ifelse(grepl("Removal", CLTEST), 'Removal', CLTEST))
  out_data<-out_data %>% dplyr::mutate(CLTEST = ifelse(grepl("Necropsy", CLTEST), 'Necropsy', CLTEST))
  # out_data<-out_data %>% dplyr::mutate(CLDIR = replace(CLDIR, CLDIR == "left", ""))
  out_data<-out_data %>% dplyr::mutate(CLDY=as.numeric(CLDY))
  
  out_data<- out_data %>% dplyr::mutate(CLDY=ifelse(`Treatment phase`=="Pre-test", -1*CLDY, CLDY))
  # 
  # out_data<-out_data %>% dplyr::mutate(CLLAT = ifelse(CLDIR=='right', CLDIR, CLLAT))
  # out_data<-out_data %>% dplyr::mutate(CLDIR = replace(CLDIR, CLDIR == "right", ""))
  # 
  
  # modify CLSTRESC
  # out_data<- out_data %>% dplyr::mutate(CLSTRESC=as.character(CLORRES))
  
  
  # toupper all relevant cols
  out_data<-out_data %>% dplyr::mutate_all(.funs=toupper)
  
  # remove unused columns
  out_data<-out_data %>% dplyr::select(any_of(names(SEND_names)))
  
  # set correct date foCLt
  Sys.setlocale("LC_ALL", "English")
  out_data$CLDTC<- format(as.POSIXct(out_data$CLDTC,format="%d-%b-%Y %H:%M"))
  Sys.setlocale("LC_ALL", "de_DE.UTF-8")
  
  return(out_data)
}