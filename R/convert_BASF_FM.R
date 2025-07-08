#' convert_BASF_FM
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The dataframe containing the export of Provantis
#'
#' @return SEND Converted Domain as dataframe, use BASF Provantis Export files
#' @export
#'
#' @examples
#'  domainName<-'FM'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertBW(domainName, domainData)
#'  

convert_BASF_FM<-function(domainName, domainData) {
  # load SEND names from Standard
  SEND_names<-rdSendig('FM')
  out_data<-tibble::as_tibble(domainData)
  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[5]<-names(SEND_names[3]) # USUBJID
  names(out_data)[8]<-names(SEND_names[4]) # FETUSID
  names(out_data)[9]<-'SEXFETAL'
  names(out_data)[13]<-names(SEND_names[8]) # FMTEST
  names(out_data)[15]<-names(SEND_names[9]) # FMORRES
  names(out_data)[16]<-names(SEND_names[10]) # FMORRES
  names(out_data)[23]<-names(SEND_names[23]) # FMDTC
  
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  # browser()
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='FM',.before="USUBJID") # add Domain column
  
  out_data<-out_data %>% tibble::add_column(FMTESTCD="",.before="FMTEST") # add FMTESTCD column
  out_data<-out_data %>% tibble::add_column(FMSTRESN=0.0,.after="FMORRESU") # add FMSTRESN column
  out_data<-out_data %>% tibble::add_column(FMSTRESU="",.after="FMSTRESN") # add FMSTRESU column
  out_data<-out_data %>% tibble::add_column(FMSTRESC="",.after="FMSTRESU") # add FMSTRESC column
  
  # Correct FMTEST
  out_data<-out_data %>% dplyr::mutate(FMTEST = replace(FMTEST, FMTEST == 'Anogenital Distance (fetus)', 'Anogenital Distance'))
  
  
  # Set BWTESTCD according to BWTEST information
  out_data<-out_data %>% dplyr::mutate(FMTESTCD = ifelse(FMTEST == 'Fetal Weigth', 'FWT' , FMTESTCD))
  out_data<-out_data %>% dplyr::mutate(FMTESTCD = ifelse(FMTEST == 'Anogenital Distance', 'AGD' , FMTESTCD))
  out_data<-out_data %>% dplyr::mutate(FMTESTCD = ifelse(FMTEST == 'Placental Weight', 'PWT' , FMTESTCD))
  
  
  # copy values from FMORRES to BWORRESN
  out_data$FMSTRESN<-as.numeric(out_data$FMORRES)
  out_data$FMSTRESC<-as.character(out_data$FMORRES)
  
  # copy values from FMORRESU to BWSTRESU
  out_data$FMSTRESU<-out_data$FMORRESU
  
  # copy values from FMNOMDY to BWDY
  # out_data$FMNOMDY<-out_data$FMDY
  
  # set correct date fomat
  Sys.setlocale("LC_ALL", "English")
  out_data$FMDTC<- format(as.POSIXct(out_data$FMDTC,format="%d-%b-%Y %H:%M"))
  Sys.setlocale("LC_ALL", "de_DE.UTF-8")
  
  
  out_data2<-out_data %>% dplyr::distinct(USUBJID, FETUSID, .keep_all = T)
  out_data2$FMORRES<-out_data2$SEXFETAL
  out_data2$FMORRESU<-'NULL'
  out_data2$FMSTRESN<-NA
  out_data2$FMSTRESC<-out_data2$FMORRES
  out_data2$FMSTRESU<-'NULL'
  out_data2$FMTEST<-'Fetal Sex'
  out_data2$FMTESTCD<-'SEXFETAL'
  
  out_data<-out_data %>% tibble::add_row(out_data2)
  # remove unused columns
  out_data<-out_data %>% dplyr::select(any_of(names(SEND_names)))
  
  
  ##########################
  return(out_data)
  
  }

