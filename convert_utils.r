library(Dict)
library(fs)
library(gridExtra)
library(tibble)
library(tibbletime)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(stringr)
library(lubridate)


# INPUT Directory
#indir<-"c:/temp/MQUEST_SEND/Provantis/DA/LIVE"
#indir<-"c:/temp/MQUEST_SEND/Provantis/DA/ARCH"
#indir<-"c:/temp/MQUEST_SEND/Provantis/IV/LIVE"
indir<-"c:/temp/MQUEST_SEND/Provantis/IV/ARCH"

## OUTPUT Directory
outdir<-paste0(indir,"/output/")


## SEND Dictionary
dictionary<-fread(file="C:\\temp\\portableApps\\Documents\\___study\\sendig.txt", quote="")




##  This function can be called to load the data exported from Business Objects
##  INPUT: The Short Name for the Domain (e.g. 'BW')
##  Procedure: All files in the input directory called SEND_(DOMAIN).csv will be read and the content will be returned.
##  OUTPUT: A list of data frames per file found.
##
load_domain <- function(domain){
  stopifnot(is.character(domain), length(domain) ==1)
  pattern<-paste0("SEND_",domain,"-.*.csv")
  files <- list.files(indir, pattern=pattern, full.names=TRUE)
  df <- lapply(files, fread)
  return(df)
}

## Name:    Domain Convert
## PURPOSE: Convert the data frame into a SEND Structure 
## INPUT:   df: Data Frame containing the Business Object DATA
##          domain: the domain name used, e.g. BW
##          Index: Optional index for selection of the file in cases when more than one input file is provided
## Output:  SEND Data Frame
## Author:  Frank Bringezu, CPS
##
convert_domain <- function(df, domain, index=1, full=T){
  stopifnot(is.character(domain), length(domain) ==1)
  stopifnot(is.list(df), length(df) >= 1)
  
  in_names<-names(df[[index]])
  SEND_names <-unlist(dictionary %>% filter(`Domain Prefix`==domain) %>% select(`Variable Name`))
  
  print(paste0("Converting:", domain))
  out_data<-as_tibble(df[[index]])
  
  
  switch(domain, 
         'BW'={

           names(out_data)[1]<-SEND_names[[1]] # STUDYID
           names(out_data)[2]<-SEND_names[[3]] # USUBJID
           names(out_data)[4]<-SEND_names[[6]] # BWTEST
           names(out_data)[5]<-SEND_names[[7]] # BWORRES
           names(out_data)[6]<-SEND_names[[8]] # BWORRESU
           names(out_data)[7]<-SEND_names[[9]] # BWSTRESC
           names(out_data)[8]<-SEND_names[[20]] # BWDTC
           names(out_data)[9]<-SEND_names[[21]] # BWDY

           out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
           out_data<-out_data %>% add_column(DOMAIN='BW',.before="USUBJID") # add Domain column
           out_data<-out_data %>% add_column(BWTESTCD="",.before="BWTEST") # add BWTESTCD column
           out_data<-out_data %>% add_column(BWNOMDY="",.after="BWDY") # add BWNOMDY column
           out_data<-out_data %>% add_column(BWSTRESN="",.after="BWSTRESC") # add BWSTRESN column
           out_data<-out_data %>% add_column(BWSTRESU="",.after="BWSTRESN") # add BWSTRESU column
           # Correct BWTEST
           out_data<-out_data %>% mutate(BWTEST = replace(BWTEST, BWTEST == 'Bodyweights', 'Body Weight'))
           out_data<-out_data %>% mutate(BWTEST = replace(BWTEST, BWTEST == 'Terminal Bodyweight', 'Terminal Body Weight'))

           # Set BWTESTCD according to BWTEST information
           out_data<-out_data %>% mutate(BWTESTCD = ifelse(BWTEST == 'Body Weight', 'BW' , 'TERMBW'))

           # copy values from BWORRES to BWORRESN
           out_data$BWSTRESN<-as.double(out_data$BWORRES)

           # copy values from BWORRESU to BWSTRESU
           out_data$BWSTRESU<-out_data$BWORRESU

           # copy values from BWNOMDY to BWDY
           out_data$BWNOMDY<-out_data$BWDY

           # set correct date fomat
           out_data$BWDTC<- format(as.POSIXct(out_data$BWDTC,format='%Y/%m/%d %H:%M:%S'))

           ## remove columns
           # return conversion result
           out_data<-out_data %>% select(-c('Parameter','Time Slot'))

           return(out_data)

           },
         

         'CL'={
           # Load dictionary for Severity from text file (can be expanded!)
           sev<-fread(file="C:\\temp\\portableApps\\Documents\\___study\\clsev.txt", quote="")
           
           names(out_data)[1]<-SEND_names[[1]] # STUDYID
           names(out_data)[2]<-SEND_names[[3]] # USUBJID
           names(out_data)[3]<-SEND_names[[10]] # CLTEST
           names(out_data)[5]<-SEND_names[[14]] # CLORRES
           names(out_data)[6]<-SEND_names[[19]] # CLLOC
          #  names(out_data)[7]<-'CLDISTR' # CLDISTR
           names(out_data)[8]<-SEND_names[[11]] # CLCAT
           # names(out_data)[9]<-SEND_names[[11]] # CLCAT
           names(out_data)[7]<-SEND_names[[21]] # CLSEV
           names(out_data)[10]<-SEND_names[[26]] # CLDTC
           names(out_data)[11]<-SEND_names[[28]] # CLDY
           names(out_data)[12]<-SEND_names[[32]] # CLTPT
           
           out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
           
           out_data<-out_data %>% add_column(DOMAIN='CL',.before="USUBJID") # add Domain column
           out_data<-out_data %>% add_column(CLTESTCD="",.before="CLTEST") # add CLTESTCD column
           out_data<-out_data %>% add_column(CLNOMDY="",.after="CLDY") # add CLNOMDY column

           cindex<-which(out_data$CLCAT=='Color')
           for (i in seq_along(cindex)) {out_data$CLORRES[cindex[i]]<-paste(c(out_data$CLORRES[cindex[i]], out_data$Modifier[cindex[i]]), collapse = ", ")}
           
           out_data$CLSEV<-gsub('\\.$', '', out_data$CLSEV)
           
           `%ni%` <- Negate(`%in%`)
           
           cindex<-which(out_data$CLSEV %ni% c('slight','moderate','severe'))
           for (i in seq_along(cindex)) {
             out_data$CLORRES[cindex[i]]<-paste0(out_data$CLORRES[cindex[i]],',',out_data$CLSEV[cindex[i]])
             out_data$CLSEV[cindex[i]]<-''
           }
           
           out_data$CLORRES<-gsub(',$', '', out_data$CLORRES)
           
           cindex<-which(out_data$CLCAT=='Severity')
           for (i in seq_along(cindex)) {out_data$CLSEV[cindex[i]]<-out_data$Modifier[cindex[i]]}
           
          # out_data$CLLOC<-''
          cindex<-which(out_data$CLCAT=='Laterality')
          for (i in seq_along(cindex)) {out_data$CLLOC[cindex[i]]<-out_data$Modifier[cindex[i]]}
           out_data$CLLOC[out_data$CLLOC == 'lighter'] <- ''
  
           out_data$CLSEV<-gsub('slight\\.', 'SLIGHT', out_data$CLSEV)
           out_data$CLSEV<-gsub('medium\\.', 'MEDIUM', out_data$CLSEV)
           out_data$CLSEV<-gsub('moderate\\.', 'MODERATE', out_data$CLSEV)
           
           
           # out_data$CLORRES[which(out_data$CLCAT=='Color')] <- paste(c( out_data$CLORRES[which(out_data$CLCAT=='Color')],  out_data$Modifier[which(out_data$CLCAT=='Color')]), collapse=", ")
           out_data<-out_data %>% mutate(CLLOC = toupper(CLLOC))
           
           # Set result as NORMAL when nothing is reported
           out_data<-out_data %>% mutate(CLORRES = replace(CLORRES, CLORRES == '', 'NORMAL'))
           
           
           out_data$CLORRES <- toupper(out_data$CLORRES)
           out_data$CLTEST <- toupper(out_data$CLTEST)
           out_data$CLTPT <- toupper(out_data$CLTPT)
           out_data$CLSEV<-toupper(out_data$CLSEV)
           
           # copy values from CLORRES to CLORRESC
           out_data$CLORRESC<-out_data$CLORRES
           
           # copy values from LBNOMDY to BWDY
           out_data$CLNOMDY<-out_data$CLDY
           
           # Remove time and format as Date
           # out_data$CLDTC<-as.Date(gsub("/","-",out_data$CLDTC))
           # out_data$CLDTC<-strptime(out_data$CLDTC,format='%Y/%m/%d %H:%M:%S')
           out_data$CLDTC<- format(as.POSIXct(out_data$CLDTC,format='%Y/%m/%d %H:%M:%S'))
           
           
           # out_data$CLCAT<-''
           
           ## remove columns
           # return conversion result
           out_data<-out_data %>% select(-c('Modifier','NAD'))
           
           return(out_data)
           
         },
         'CV'={
           
           names(out_data)[1]<-SEND_names[[1]] # STUDYID
           names(out_data)[2]<-SEND_names[[3]] # USUBJID
           # names(out_data)[3]<-SEND_names[[6]] # CVTEST
           names(out_data)[4]<-SEND_names[[8]] # CVTEST
           names(out_data)[5]<-SEND_names[[10]] # CVORRES
           names(out_data)[6]<-SEND_names[[11]] # CVORRESU
           names(out_data)[7]<-SEND_names[[12]] # CVSTRESC
           names(out_data)[8]<-SEND_names[[24]] # CVDTC
           names(out_data)[9]<-SEND_names[[26]] # CVDY
           
           out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
           
           out_data<-out_data %>% add_column(DOMAIN='CV',.before="USUBJID") # add Domain column
           out_data<-out_data %>% add_column(CVNOMDY="",.after="CVDY") # add CVNOMDY column
           out_data<-out_data %>% add_column(CVSTRESN="",.after="CVSTRESC") # add CVSTRESN column
           out_data<-out_data %>% add_column(CVSTRESU="",.after="CVSTRESN") # add CVSTRESU column
           
           
           # copy values from CVORRES to CVSTRESN
           out_data$CVSTRESN<-as.double(out_data$CVORRES)
           
           # copy values from CVORRESU to CVSTRESU
           out_data$CVSTRESU<-out_data$CVORRESU
           
           # copy values from CVNOMDY to CVDY
           out_data$CVNOMDY<-out_data$CVDY
           
           # Remove time and format as Date
           out_data$CVDTC<- format(as.POSIXct(out_data$CVDTC,format='%Y/%m/%d %H:%M:%S'))
           
           # remove unused columns
           out_data<-out_data %>% select(-c('Activity'))
           
           return(out_data)
           
         },
      'DS'={
           names(out_data)[1]<-SEND_names[[1]] # STUDYID
           names(out_data)[2]<-SEND_names[[3]] # USUBJID
           names(out_data)[4]<-SEND_names[[10]] # DSSTDY
           names(out_data)[5]<-SEND_names[[9]] # DSSTDTC
           names(out_data)[6]<-SEND_names[[5]] # DSTERM
           names(out_data)[7]<-SEND_names[[6]] # DSDECOD
           
           out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
           
           out_data<-out_data %>% add_column(DOMAIN='DS',.before="USUBJID") # add Domain column
           out_data<-out_data %>% add_column(DSNOMDY="",.after="DSSTDY") # add DSNOMDY column
           out_data<-out_data %>% add_column(DSUSCHFL="",.after="DSNOMDY") # add DSUSCHFL column
           
           
           # copy values from DSSTDY to DSNOMDY
           out_data$DSNOMDY<-out_data$DSSTDY
           
           out_data$DSUSCHFL<-""
           out_data<-out_data %>% mutate(DSUSCHFL = replace(DSUSCHFL, !grepl('RECOVERY|Recovery|Scheduled|SCHEDULED', DSTERM), 'Y'))
           out_data<-out_data %>% mutate(DSUSCHFL = replace(DSUSCHFL,DSSTDTC=="", ''))
           
           return(out_data)
           
         }, 
       'DM'= {
           names(out_data)[1]<-SEND_names[[1]] # STUDYID
           names(out_data)[2]<-SEND_names[[3]] # USUBJID
           names(out_data)[3]<-SEND_names[[14]] # SEX
           names(out_data)[4]<-SEND_names[[10]] # BRTHDTC
           # not used names(out_data)[5]
           # not used names(out_data)[6]
           names(out_data)[7] <-SEND_names[[7]] # RFXSTDTC
           names(out_data)[8] <-SEND_names[[8]]# RFXENDTC
           # not used names(out_data)[9]
           # not used names(out_data)[10]
           # not used names(out_data)[11]
           # not used names(out_data)[12]
           names(out_data)[13]<-SEND_names[[18]] # ARMCD
           names(out_data)[14]<-SEND_names[[19]] # ARM
           names(out_data)[15]<-SEND_names[[15]] # SPECIES
           names(out_data)[16]<-SEND_names[[16]] # STRAIN
           
           out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
           
           out_data<-out_data %>% add_column(DOMAIN='DM',.before="USUBJID") # add Domain column
           
           # Convert Dates
           out_data$BRTHDTC<- format(as.POSIXct(out_data$BRTHDTC,format='%Y/%m/%d %H:%M:%S'))
           out_data$RFXSTDTC<- format(as.POSIXct(out_data$RFXSTDTC,format='%Y/%m/%d %H:%M:%S'))
           out_data$RFXENDTC<- format(as.POSIXct(out_data$RFXENDTC,format='%Y/%m/%d %H:%M:%S'))
           
           # remove unused columns
          # out_data<-out_data %>% select(-c('Date of Death','Time of Death','Animal Current Cage Number', 'Cage Entry Date','Cage Exit Date','Group Number'))
           out_data<-out_data %>% select(-c('Time of Death','Animal Current Cage Number','Cage Exit Date','Group Number'))
           
           return(out_data)
           
         },
       'LB' = {
           names(out_data)[1]<-SEND_names[[1]] # STUDYID
           names(out_data)[2]<-SEND_names[[3]] # USUBJID
           names(out_data)[3]<-SEND_names[[11]] # LBCAT
           names(out_data)[4]<-SEND_names[[10]] # LBTEST
           names(out_data)[5]<-SEND_names[[13]] # LBORRES
           names(out_data)[6]<-SEND_names[[14]] # LBORRESU
           names(out_data)[7]<-SEND_names[[17]] # LBSTRESC
           names(out_data)[8]<-SEND_names[[45]] # LBDTC
           names(out_data)[9]<-SEND_names[[47]] # LBDY
           names(out_data)[10]<-SEND_names[[51]] # LBTPT
           names(out_data)[11]<-SEND_names[[35]] # LBMETHOD
           
           
           out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
           
           out_data<-out_data %>% add_column(DOMAIN='LB',.before="USUBJID") # add Domain column
           out_data<-out_data %>% add_column(LBTESTCD="",.before="LBTEST") # add LBTESTCD column
           out_data<-out_data %>% add_column(LBNOMDY="",.after="LBDY") # add LBNOMDY column
           out_data<-out_data %>% add_column(LBSTRESN="",.after="LBSTRESC") # add LBSTRESN column
           out_data<-out_data %>% add_column(LBSTRESU="",.after="LBSTRESN") # add LBSTRESU column
           
           
           out_data$LBCAT<-gsub('Clinical Chemistry.*', 'CLINICAL CHEMISTRY', out_data$LBCAT)
           out_data$LBCAT<-gsub('Coagulation.*', 'HEMATOLOGY', out_data$LBCAT)
           out_data$LBCAT<-gsub('Advia.*', 'HEMATOLOGY', out_data$LBCAT)
           out_data$LBCAT<-gsub('Diuresis.*', 'URINALYSIS', out_data$LBCAT)
           out_data$LBCAT<-gsub('IPT.*', 'IPT', out_data$LBCAT)
           out_data$LBCAT<-gsub('Menarini.*', 'URINALYSIS', out_data$LBCAT)
           out_data$LBCAT<-gsub('Urine.*', 'URINALYSIS', out_data$LBCAT)
           
           # out_data$LBCAT<- toupper(out_data$LBCAT)
           
           `%ni%` <- Negate(`%in%`)

           cindex<-which(out_data$LBCAT %ni% c('CLINICAL CHEMISTRY','HEMATOLOGY','URINALYSIS','IPT'))
           for (i in seq_along(cindex)) {
             out_data$LBCAT[cindex[i]]<-'SPECIAL PURPOSE'
          }

           # copy values from LBORRES to LBSTRESN
           out_data$LBSTRESN<-as.double(out_data$LBORRES)
           
           # copy values from LBORRESU to BWSTRESU
           out_data$LBSTRESU<-out_data$LBORRESU
           
           # copy values from LBNOMDY to BWDY
           out_data$LBNOMDY<-out_data$LBDY
           
           # Set correct date format
           out_data$LBDTC<- format(as.POSIXct(out_data$LBDTC,format='%Y/%m/%d %H:%M:%S'))
           
           
           # Replace Advia 120 with controlled term HEMATOLOGY
           out_data<-out_data %>% mutate(LBCAT = replace(LBCAT, LBCAT == 'Advia 2120', 'HEMATOLOGY'))
           
           return(out_data)
           
         },
      'EG'={
           
           names(out_data)[1]<-SEND_names[[1]] # STUDYID
           names(out_data)[2]<-SEND_names[[3]] # USUBJID
           names(out_data)[3]<-SEND_names[[10]] # EGCAT
           names(out_data)[4]<-SEND_names[[9]] # EGTEST
           names(out_data)[5]<-SEND_names[[12]] # EGORRES
           names(out_data)[6]<-SEND_names[[13]] # EGORRES
           names(out_data)[7]<-SEND_names[[14]] # EGSTRESC
           names(out_data)[8]<-SEND_names[[31]] # EGDTC
           names(out_data)[9]<-SEND_names[[33]] # EGDY

           out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
           
           out_data<-out_data %>% add_column(DOMAIN='EG',.before="USUBJID") # add Domain column
           out_data<-out_data %>% add_column(EGTESTCD="",.before="EGTEST") # add EGTESTCD column
           out_data<-out_data %>% add_column(EGNOMDY="",.after="EGDY") # add EGNOMDY column
           
           # copy values from EGDY to EGNOMDY
           out_data$EGNOMDY<-out_data$EGDY
           
           # Set Date Fomat
           out_data$EGDTC<- format(as.POSIXct(out_data$EGDTC,format='%Y/%m/%d %H:%M:%S'))

           return(out_data)
           
         },
       'EX'={
           
           names(out_data)[1]<-SEND_names[[1]] # STUDYID
           names(out_data)[2]<-SEND_names[[3]] # USUBJID
           names(out_data)[3]<-SEND_names[[9]] # EXDOSTXT
           names(out_data)[4]<-SEND_names[[21]] # EXSTDTC
           names(out_data)[5]<-SEND_names[[23]] # EXSTDY
           # names(out_data)[6]<-SEND_names[[13]] # TIME-sLOT UNCHANED 
           names(out_data)[7]<-SEND_names[[10]] # EXDOSU
           names(out_data)[9]<-SEND_names[[8]] # EXDOSE
           names(out_data)[13]<-SEND_names[[7]] # EXTRT
           names(out_data)[14]<-SEND_names[[18]] # EXVAMT
           names(out_data)[15]<-SEND_names[[13]] # EXROUTE
           names(out_data)[16]<-SEND_names[[19]] # EXVAMTU
           names(out_data)[18]<-SEND_names[[17]] # EXTRTV
           
           
           
           out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
           
           out_data<-out_data %>% add_column(DOMAIN='EX',.before="USUBJID") # add Domain column

           
            
                              
           # remove unused columns
           out_data<-out_data %>% select(-c('Time Slot', 
                                            'Calculated Dose',
                                            'Dose Body Weight', 
                                            'Body Weight Unit', 
                                            'Group Number', 
                                            'Dose Colour'))
       
           
           out_data$EXTRTV<-toupper(iconv(out_data$EXTRTV, "LATIN2", "UTF-8"))
           out_data$EXDOSTXT<-toupper(iconv(out_data$EXDOSTXT, "LATIN2", "UTF-8"))
           out_data$EXROUTE<-toupper(iconv(out_data$EXROUTE, "LATIN2", "UTF-8"))
           out_data$EXDOSU<-toupper(iconv(out_data$EXDOSU, "LATIN2", "UTF-8"))
           out_data$EXVAMTU<-toupper(iconv(out_data$EXVAMTU, "LATIN2", "UTF-8"))
           
           
           # Set Date Fomat
           out_data$EXSTDTC<- format(as.POSIXct(out_data$EXSTDTC,format='%Y/%m/%d %H:%M:%S'))
           
           return(out_data)
           
         },
         'FC'={
           # change domain to FW 
           domain="FW"
           SEND_names <-unlist(dictionary %>% filter(`Domain Prefix`==domain) %>% select(`Variable Name`))
           
           names(out_data)[1]<-SEND_names[[1]] # STUDYID
           names(out_data)[2]<-SEND_names[[4]] # POOLID
           names(out_data)[3]<-SEND_names[[8]] # FWTEST
           names(out_data)[4]<-SEND_names[[7]] # FWTESTCD
           names(out_data)[5]<-SEND_names[[18]] # FWDTC
           names(out_data)[6]<-SEND_names[[20]] # FWDY
           names(out_data)[7]<-SEND_names[[9]] # FWORRES
           names(out_data)[8]<-SEND_names[[10]] # FWORRESU
           #names(out_data)[9]<-SEND_names[[33]] # EGDY
           
           # out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
           
           out_data<-out_data %>% add_column(DOMAIN='FW',.before="POOLID") # add Domain column
           
           out_data<-out_data %>% mutate(FWORRES=ifelse(is.na(FWORRES), `Food Residue`,FWORRES ))
           out_data<-out_data %>% mutate(FWORRESU=ifelse(FWORRESU=="", `Units (Residue)`, FWORRESU))
           
           out_data<-out_data %>% mutate(FWSTRESN = as.numeric(out_data$FWORRES), FWSTRESU = FWORRESU)
           out_data<-out_data %>% mutate(FWSTRESC = as.character(out_data$FWORRES))
           
           
           # remove unused columns
           out_data<-out_data %>% select(-c('Units (Waste)','Food Waste', 'Food Residue','Units (Residue)'))

           
           # Set Date Fomat
           out_data$FWDTC<- format(as.POSIXct(out_data$FWDTC,format='%Y/%m/%d %H:%M:%S'))
           
           
           return(out_data)
           
         },
   'MA' = {
           names(out_data)[1]<-SEND_names[[1]] # STUDYID
           names(out_data)[2]<-SEND_names[[3]] # USUBJID
           names(out_data)[3]<-SEND_names[[17]] # MASPEC
           names(out_data)[4]<-SEND_names[[14]] # MASTAT
           names(out_data)[6]<-SEND_names[[12]] # MAORRES
           # names(out_data)76]<-SEND_names[[]] # NVL--> MAORRES
           names(out_data)[8]<-SEND_names[[13]] # MASTRESC
           # names(out_data)[9]<-SEND_names[[18]] # QUALIFIER --> MAANTREG
           names(out_data)[10]<-SEND_names[[18]] # MAANTREG
           names(out_data)[11]<-SEND_names[[22]] # Sub-Locator --> MADIR 
           names(out_data)[12]<-SEND_names[[21]] # MALAT
           # names(out_data)[13]<-SEND_names[[18]] # Distribution --> MAANTREG
           names(out_data)[14]<-SEND_names[[25]] # MASEV
           names(out_data)[18]<-SEND_names[[27]] # MADTC
           names(out_data)[19]<-SEND_names[[28]] # MADY
           
           
           
           out_data<-out_data %>% add_column(DOMAIN='MA',.before="USUBJID") # add Domain column
           
           
           out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
           
           # out_data$mod<-str_match(out_data$MIORRES,"(;|,.*){1,7}")
           # out_data$mod[,1]<-gsub(';','',out_data$mod[,1])
           # tmp<-out_data$mod[,1] %>% str_split(",")
           # out_data$MISEV<-unlist(lapply(1:length(tmp), function(x) trimws(unlist(tmp[[x]][5]))))
           # out_data$MIDISTR<-unlist(lapply(1:length(tmp), function(x) trimws(unlist(tmp[[x]][4]))))
           # out_data$MILAT<-unlist(lapply(1:length(tmp), function(x) trimws(unlist(tmp[[x]][3]))))
           # out_data$MISTRESC<-unlist(lapply(1:length(tmp), function(x) trimws(unlist(tmp[[x]][1]))))
           
           out_data<-out_data %>% mutate(MAORRES = ifelse(NVL == 'No Visible Lesions', 'NO VISIBLE LESIONS' , MAORRES))
           out_data<-out_data %>% mutate(MAORRES = ifelse(MASTAT == 'Not Examined', 'NOT EXAMINED' , MAORRES))
           out_data<-out_data %>% mutate(MAANTREG = ifelse(Distribution != '' & MAANTREG !='', paste0(MAANTREG,', ',Distribution), MAANTREG))
           out_data<-out_data %>% mutate(MAANTREG = ifelse(Distribution != '' & MAANTREG =='', paste0(Distribution), MAANTREG))
           out_data<-out_data %>% mutate(MAANTREG = ifelse(Qualifier != '', paste0(MAANTREG,', ',Qualifier), MAANTREG))
           
           out_data<-out_data %>% mutate(MALAT = ifelse(MADIR=='left', MADIR, MALAT))
           out_data<-out_data %>% mutate(MADIR = replace(MADIR, MADIR == "left", ""))
           
           ##alternative solution
           #out_data$MADIR<-gsub('left', '', out_data$MADIR)
           
           out_data<-out_data %>% mutate(MALAT = ifelse(MADIR=='right', MADIR, MALAT))
           out_data<-out_data %>% mutate(MADIR = replace(MADIR, MADIR == "right", ""))
           
           
           # out_data<-out_data %>% filter(MADIR == 'LEFT') %>% mutate(MALAT=MADIR) %>% mutate (MADIR='')
           # out_data<-out_data %>% filter(MADIR == 'RIGHT') %>% mutate(MALAT=MADIR) %>% mutate (MADIR='')
           
           # remove unused columns
           out_data<-out_data %>% select(-c('NVL','Tissue NE Reason','Cause of Death','Qualifier','Distribution','Associated Observations','Associated Masses'))
           
           
           # copy values from CVNOMDY to CVDY
           out_data$MANOMDY<-out_data$MADY
           
           # toupper all relevant cols
           out_data$MAORRES<-toupper(out_data$MAORRES)
           out_data$MASPEC<-toupper(out_data$MASPEC)
           out_data$MASTRESC<-toupper(out_data$MASTRESC)
           out_data$MALAT<-toupper(out_data$MALAT)
           out_data$MASEV<-toupper(out_data$MASEV)
           out_data$MASTAT<-toupper(out_data$MASTAT)
           out_data$MAANTREG<-toupper(out_data$MAANTREG)
           out_data$MADIR<-toupper(out_data$MADIR)
           
           # Set Date Fomat
           out_data$MADTC<- format(as.POSIXct(out_data$MADTC,format='%Y/%m/%d %H:%M:%S'))
           
           return(out_data)
         },
  'MI' = {
           names(out_data)[1]<-SEND_names[[1]] # STUDYID
           names(out_data)[2]<-SEND_names[[3]] # USUBJID
           names(out_data)[3]<-SEND_names[[12]] # MIORRES
           names(out_data)[4]<-SEND_names[[20]] # MISPEC
           names(out_data)[5]<-SEND_names[[17]] # MISTAT
           names(out_data)[8]<-SEND_names[[13]] # Morphology --> MISTRESC
           names(out_data)[9]<-SEND_names[[21]] # Locator --> MIANTREG
           names(out_data)[10]<-SEND_names[[25]] # sub-locator --> MIDIR
           names(out_data)[13]<-SEND_names[[24]] # Symmetry -> MILAT
           names(out_data)[14]<-SEND_names[[16]] # MIDISTR
           # names(out_data)[15]<-SEND_names[[16]] # Qualifier --> ??
           names(out_data)[16]<-SEND_names[[28]] # MISEV
           names(out_data)[17]<-SEND_names[[30]] # MIDTC
           names(out_data)[18]<-SEND_names[[31]] # MIDY
           
           out_data<-out_data %>% add_column(DOMAIN='MI',.before="USUBJID") # add Domain column
           

           out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
  
           
           out_data<-out_data %>% mutate(MIORRES = ifelse(NVL == 'No Visible Lesions', 'NO VISIBLE LESIONS' , MIORRES))
           out_data<-out_data %>% mutate(MIORRES = ifelse(MISTAT == 'Not Examined', 'NOT EXAMINED' , MIORRES))
           out_data<-out_data %>% mutate(MIANTREG = ifelse(MIDIR != '' & MIANTREG !='', paste0(MIANTREG,', ',MIDIR), MIANTREG))
           out_data<-out_data %>% mutate(MIANTREG = ifelse(Qualifier != '' & MIANTREG !='', paste0(MIANTREG,', ',Qualifier), MIANTREG))
           out_data<-out_data %>% mutate(MIANTREG = ifelse(Qualifier != '' & MIANTREG =='', paste0(Qualifier), MIANTREG))
           out_data<-out_data %>% mutate(MILAT = ifelse(MIDIR == 'left', paste0(MIDIR), MILAT))
           out_data<-out_data %>% mutate(MIDIR = ifelse(MIDIR == 'left', '', MIDIR))
           out_data<-out_data %>% mutate(MILAT = ifelse(MIDIR == 'right', paste0(MIDIR), MILAT))
           out_data<-out_data %>% mutate(MIDIR = ifelse(MIDIR == 'right', '', MIDIR))
           
           # remove unused columns
           out_data<-out_data %>% select(-c('NVL', 'Tisssue NE Reason', 'Qualifier','Cause of Death','MPF'))
           
           # copy values from MINOMDY to MIDY
           out_data$MINOMDY<-out_data$MIDY
           
           # toupper all relevant cols
           out_data$MIORRES<-toupper(out_data$MIORRES)
           out_data$MIDISTR<-toupper(out_data$MIDISTR)
           out_data$MISPEC<-toupper(out_data$MISPEC)
           out_data$MISTRESC<-toupper(out_data$MISTRESC)
           out_data$MIANTREG<-toupper(out_data$MIANTREG)
           out_data$MILAT<-toupper(out_data$MILAT)
           out_data$MISEV<-toupper(out_data$MISEV)
           out_data$MISTAT<-toupper(out_data$MISTAT)
           out_data$MIDIR<-toupper(out_data$MIDIR)
           out_data$MIDTC<- format(as.POSIXct(out_data$MIDTC,format='%Y/%m/%d %H:%M:%S'))
           
           return(out_data)
         },
   'OM' = {
     names(out_data)[1]<-SEND_names[[1]] # STUDYID
     names(out_data)[2]<-SEND_names[[3]] # USUBJID
     names(out_data)[3]<-SEND_names[[26]] # Avtivity --> OMNOMLBL 
     names(out_data)[4]<-SEND_names[[14]] # Parameter --> OMSPEC
     names(out_data)[5]<-SEND_names[[7]] # Result Value --> OMORRES
     names(out_data)[6]<-SEND_names[[8]] # Result Unit --> OMORRESU
     names(out_data)[7]<-SEND_names[[9]] # Textual Value --> OMORRESC
     names(out_data)[8]<-SEND_names[[23]] # Result Time --> OMDTC
     names(out_data)[9]<-SEND_names[[24]] # Result Day --> OMDY
     
     
     out_data<-out_data %>% add_column(DOMAIN='OM',.before="USUBJID") # add Domain column
     
     
     out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
     


     # remove unused columns
     out_data<-out_data %>% select(-c('Time Slot'))
     
     # toupper all relevant cols
     out_data$OMSPEC<-toupper(out_data$OMSPEC)
     out_data$OMNOMLBL<-toupper(out_data$OMNOMLBL)
     
     # format date / time
     out_data$OMDTC<- format(as.POSIXct(out_data$OMDTC,format='%Y/%m/%d %H:%M:%S'))
     
     return(out_data)
   },
  
  
  'RE' = {
    names(out_data)[1]<-SEND_names[[1]] # STUDYID
    names(out_data)[2]<-SEND_names[[3]] # USUBJID
    names(out_data)[3]<-SEND_names[[7]] # Avtivity --> RETESTCD
    names(out_data)[4]<-SEND_names[[8]] # Parameter --> RETEST
    names(out_data)[5]<-SEND_names[[10]] # Result Value --> REORRES
    names(out_data)[6]<-SEND_names[[11]] # Result Unit --> REORRESU
    names(out_data)[7]<-SEND_names[[12]] # Textual Value --> RESTRESC
    names(out_data)[8]<-SEND_names[[24]] # Result Time --> REDTC
    names(out_data)[9]<-SEND_names[[26]] # Result Day --> REDY
    
    out_data<-out_data %>% add_column(DOMAIN='RE',.before="USUBJID") # add Domain column
    out_data<-out_data %>% add_column(RESTRESN='',.before="REDTC") # add RESTRESN column
    out_data<-out_data %>% add_column(RESTRESU='',.before="REDTC") # add RESTRESN column
    
    out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
    
    # remove unused columns
    out_data<-out_data %>% select(-c('Time Slot'))
    
    # capitalize
    out_data$RETESTCD<-toupper(out_data$RETESTCD)
    out_data$RETEST<-toupper(out_data$RETEST)
    
    out_data$RESTRESN<-as.numeric(out_data$REORRES)
    out_data$RESTRESU<-out_data$REORRESU
    
    
    # format date / time
    out_data$REDTC<- format(as.POSIXct(out_data$REDTC,format='%Y/%m/%d %H:%M:%S'))
    
    
    return(out_data)
  },
  
 
  'TA' = {
    names(out_data)[1]<-SEND_names[[1]] # STUDYID
    names(out_data)[2]<-SEND_names[[3]] # Group Number --> ARMCD 
    names(out_data)[3]<-SEND_names[[10]] # Group Name  --> EPOCH 
    names(out_data)[4]<-SEND_names[[4]] # Group Type --> ARM

    
    out_data<-out_data %>% add_column(DOMAIN='TA',.before="ARMCD") # add Domain column

        
    # remove unused columns
    if (full) {
      
      names(out_data)<-toupper(names(out_data))
      return(out_data)
    } else {
      out_data<-out_data %>% select(-c('Compound',
                                     'Test Material Amount',
                                     'Route of Administration',
                                     'Administration Route Units',
                                     'Dose Colour',
                                     'Vehicle',
                                     'Species',
                                     'Strain'))

      return(out_data)
    } 
    },
    
    
    'VS' = {
      names(out_data)[1]<-SEND_names[[1]] # STUDYID
      names(out_data)[2]<-SEND_names[[3]] # USUBJID
      names(out_data)[3]<-SEND_names[[7]] # Avtivity --> VSTESTCD
      names(out_data)[4]<-SEND_names[[8]] # Parameter --> VSTEST
      names(out_data)[5]<-SEND_names[[12]] # Result Value --> VSORRES
      names(out_data)[6]<-SEND_names[[13]] # Result Unit --> VSORRESU
      names(out_data)[7]<-SEND_names[[14]] # Textual Value --> VSSTRESC
      names(out_data)[8]<-SEND_names[[27]] # Result Time --> VSDTC
      names(out_data)[9]<-SEND_names[[29]] # Result Day --> VSDY
      
      out_data<-out_data %>% add_column(DOMAIN='VS',.before="USUBJID") # add Domain column
      out_data<-out_data %>% add_column(VSSTRESN='',.before="VSDTC") # add RESTRESN column
      out_data<-out_data %>% add_column(VSSTRESU='',.before="VSDTC") # add RESTRESN column
      
      out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
      
      # remove unused columns
      out_data<-out_data %>% select(-c('Time Slot'))
      
      # capitalize
      out_data$VSTESTCD<-toupper(out_data$VSTESTCD)
      out_data$VSTEST<-toupper(out_data$VSTEST)
      
      out_data$VSSTRESN<-as.numeric(out_data$VSORRES)
      out_data$VSSTRESU<-out_data$VSORRESU
      
      
      # format date / time
      out_data$VSDTC<- format(as.POSIXct(out_data$VSDTC,format='%Y/%m/%d %H:%M:%S'))
      
      
      return(out_data)
    },
  'TS' = {
    names(out_data)[1]<-SEND_names[[1]] # STUDYID
    names(out_data)[2]<-SEND_names[[3]] # USUBJID
    names(out_data)[3]<-SEND_names[[7]] # Avtivity --> VSTESTCD
    names(out_data)[4]<-SEND_names[[8]] # Parameter --> VSTEST
    names(out_data)[5]<-SEND_names[[12]] # Result Value --> VSORRES
    names(out_data)[6]<-SEND_names[[13]] # Result Unit --> VSORRESU
    names(out_data)[7]<-SEND_names[[14]] # Textual Value --> VSSTRESC
    names(out_data)[8]<-SEND_names[[27]] # Result Time --> VSDTC
    names(out_data)[9]<-SEND_names[[29]] # Result Day --> REDY
    
    out_data<-out_data %>% add_column(DOMAIN='VS',.before="USUBJID") # add Domain column
    out_data<-out_data %>% add_column(VSSTRESN='',.before="VSDTC") # add RESTRESN column
    out_data<-out_data %>% add_column(VSSTRESU='',.before="VSDTC") # add RESTRESN column
    
    out_data$USUBJID<-paste0(out_data$STUDYID,"-",out_data$USUBJID) # modify USUBJID
    
    # remove unused columns
    out_data<-out_data %>% select(-c('Time Slot'))
    
    # capitalize
    out_data$RETESTCD<-toupper(out_data$VSTESTCD)
    out_data$RETEST<-toupper(out_data$VSTEST)
    
    out_data$VSSTRESN<-as.numeric(out_data$VSORRES)
    out_data$VSSTRESU<-out_data$VSORRESU
    
    
    # format date / time
    out_data$VSDTC<- format(as.POSIXct(out_data$VSDTC,format='%Y/%m/%d %H:%M:%S'))
    
    
    return(out_data)
  },
    
           print(paste0("Domain: ", domain," not configured!"))
  )       
         
  
}


process_fc<-function() {
  
  
  ## Load FC domain 

  fc<-load_domain('FC')
  fcc<-convert_domain(fc, 'FC', index=1)
  
  
}



## Domain Export
## PURPOSE: Export the data frame in file
## INPUT: Domain: Data Frame containing the SEND DATA
##        Name: The Name for the Domain, e.g. BW
## Output: None
## Author: Frank Bringezu, CPS
##

export_domain<-function(domain, name, fileformat){
  stopifnot(is.data.frame(domain), length(domain) >=1)
  stopifnot(is.character(fileformat), fileformat %in% c('csv','pdf'))
  stopifnot(is.character(name), length(name) ==1)
  
  switch (fileformat,
   'pdf' = {
       pdf(paste0('SEND_',domain,'.pdf'))
       grid.table(domain)
       dev.off()
      },
    'csv' = {
      # Encoding(domain)<-'UTF-8'
      #write.table(domain, file=paste0(outdir,'SEND_',name,'.txt'), quote = F, fileEncoding = 'UTF-8', row.names = F, na='', dec='.', sep='\t')
      fwrite(domain, file=paste0(outdir,'SEND_',name,'.txt'), quote = F, showProgress=T, row.names = F, na='', dec='.', sep='\t')
      # write_delim(domain, path = paste0(outdir,'SEND_',name,'.txt'), delim = "\t")
    }
   )
  
}  




