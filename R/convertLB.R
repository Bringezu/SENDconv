#' convertLB
#'
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis for the LB parameters
#'
#' @return SEND Converted Domain as data frame for LB
#' @export
#'
#' @examples
#'  domainName<-'LB'
#'  SEND<-convertLB(domainData)
#'
convertLB<-function(domainData) {
  
  # load SEND names from Standard
  SEND_names<-rdSendig('LB')
  out_data<-tibble::as_tibble(domainData[[1]])

  
  names(out_data)[1]<-names(SEND_names[1]) # STUDYID
  names(out_data)[2]<-names(SEND_names[3]) # USUBJID
  names(out_data)[3]<-names(SEND_names[11]) # LBCAT
  names(out_data)[4]<-names(SEND_names[10]) # LBTEST
  names(out_data)[5]<-names(SEND_names[13]) # LBORRES
  names(out_data)[6]<-names(SEND_names[14]) # LBORRESU
  names(out_data)[7]<-names(SEND_names[17]) # LBSTRESC
  names(out_data)[8]<-names(SEND_names[45]) # LBDTC
  names(out_data)[9]<-names(SEND_names[47]) # LBDY
  names(out_data)[10]<-names(SEND_names[51]) # LBTPT
  names(out_data)[11]<-names(SEND_names[35]) # LBMETHOD
  
  
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
  out_data<-out_data %>% tibble::add_column(DOMAIN='LB',.before="USUBJID") # add Domain column
  out_data<-out_data %>% tibble::add_column(LBTESTCD="",.before="LBTEST") # add LBTESTCD column
  out_data<-out_data %>% tibble::add_column(LBNOMDY="",.after="LBDY") # add LBNOMDY column
  out_data<-out_data %>% tibble::add_column(LBSTRESN="",.after="LBSTRESC") # add LBSTRESN column
  out_data<-out_data %>% tibble::add_column(LBSTRESU="",.after="LBSTRESN") # add LBSTRESU column
  
  
  out_data$LBCAT<-gsub('Clinical Chemistry.*', 'CLINICAL CHEMISTRY', out_data$LBCAT)
  out_data$LBCAT<-gsub('Coagulation.*', 'HEMATOLOGY', out_data$LBCAT)
  out_data$LBCAT<-gsub('Advia.*', 'HEMATOLOGY', out_data$LBCAT)
  out_data$LBCAT<-gsub('Diuresis.*', 'URINALYSIS', out_data$LBCAT)
  out_data$LBCAT<-gsub('IPT.*', 'IPT', out_data$LBCAT)
  out_data$LBCAT<-gsub('Menarini.*', 'URINALYSIS', out_data$LBCAT)
  out_data$LBCAT<-gsub('Urine.*', 'URINALYSIS', out_data$LBCAT)
  
  # out_data$LBCAT<- toupper(out_data$LBCAT)
  
  `%ni%` <- Negate(`%in%`)
  
  
  
  out_data<-out_data %>%dplyr:: mutate(LBCAT = ifelse(LBCAT %ni% c('CLINICAL CHEMISTRY','HEMATOLOGY','URINALYSIS','IPT'), 'SPECIAL PURPOSE', LBCAT))
  
  # copy values from LBORRES to LBSTRESN
  out_data$LBSTRESN<-as.double(out_data$LBORRES)
  
  # copy values from LBORRESU to LBSTRESU
  out_data$LBSTRESU<-out_data$LBORRESU
  
  # copy values from LBNOMDY to LBDY
  out_data$LBNOMDY<-out_data$LBDY
  
  # Set correct date format
  out_data$LBDTC<- format(as.POSIXct(out_data$LBDTC,format='%Y/%m/%d %H:%M:%S'))
  
  
  # Replace Advia 120 with controlled term HEMATOLOGY
  out_data<-out_data %>% dplyr::mutate(LBCAT = ifelse(LBCAT == 'Advia 2120', 'HEMATOLOGY', LBCAT))
  
  
  # align names according to standard 
  out_data<-out_data%>%
    dplyr::select(dplyr::any_of(names(SEND_names)))
  return(out_data)
}