#' convert_BASF_FW
#'
#' @param domainName A character vector that contains the acronym for the domain
#' @param domainData The data frame containing the export of Provantis
#'
#' @return SEND Converted Domain as dataframe
#' @export
#'
#' @examples
#'  domainName<-'FW'
#'  domainData<-loadDomain(domainName)
#'  SEND<-convertFW(domainName, domainData)
#'
convert_BASF_FW<-function(domainName, domainData) {
  stopifnot(is.character(domainName), length(domainName) ==1)
  SEND_names<-rdSendig('FW')
  out_data<-domainData
  
  names(out_data)[1]<-names(SEND_names)[1] # STUDYID
  names(out_data)[5]<-names(SEND_names)[3] # USUBJID
  out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  names(out_data)[12]<-names(SEND_names)[23] # FWDY
  names(out_data)[17]<-names(SEND_names)[8] # FWTEST
  names(out_data)[19]<-names(SEND_names)[9] # FWORRES
  names(out_data)[20]<-names(SEND_names)[10] # FWORRESU
  names(out_data)[13]<-names(SEND_names)[21] # FWORRESU

  
  
  ## start pre processing
  water<-out_data %>% filter(`Food/water`=='Water')
  food<-out_data %>% filter(`Food/water`=='Food')
  
  water<-water %>% distinct(STUDYID, USUBJID, FWDY, FWDTC, FWORRES, .keep_all = T) |> 
    mutate(FWORRES = lag(FWORRES, order_by = c(USUBJID, FWDY))-lead(FWORRES), 
           DOMAIN='FW',
           RPHASE=toupper(`Repro. phase`),
           days=lead(FWDY)-FWDY,
           FWENDY=lead(FWDY),
           FWENDTC=lead(FWDTC),
           FWTEST='WATER CONSUMPTION',
           FWTESTCD='WC',
           FWDY=lag(FWDY)+1,
           type='Food') |>
    filter(FWORRES > 0) |> 
    mutate(FWORRES=as.numeric(FWORRES), 
           days=as.numeric(days)) |>
    filter(days!=0) |>
    mutate(FWSTRESC = signif(FWORRES/days, digits=4), FWORRESU='g', FWSTRESU='g/animal/day' )
  
  food<-food %>% distinct(STUDYID, USUBJID, FWDY, FWDTC, FWORRES, .keep_all = T) |> 
    mutate(FWORRES = lag(FWORRES, order_by = c(USUBJID, FWDY))-lead(FWORRES), 
           DOMAIN='FW',
           RPHASE=toupper(`Repro. phase`),
           days=lead(FWDY)-FWDY,
           FWENDY=lead(FWDY),
           FWENDTC=lead(FWDTC),
           FWTEST='FOOD CONSUMPTION',
           FWTESTCD='FC',
           FWDY=lag(FWDY)+1,
           type='Food') |>
    filter(FWORRES > 0) |> 
    mutate(FWORRES=as.numeric(FWORRES), 
           days=as.numeric(days)) |>
    filter(days!=0) |>
    mutate(FWSTRESC = signif(FWORRES/days, digits=4), FWORRESU='g', FWSTRESU='g/animal/day' )
  
  tmp<-rbind(food, water)
  
 

  # remove unused columns
  out_data<-tmp %>% dplyr::select(any_of(names(SEND_names)))
  
  
  return(out_data)
  
}