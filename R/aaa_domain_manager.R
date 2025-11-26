#'Create a new environment and define a tibble with Acronyms and functions
the<-new.env(parent = emptyenv())
the$conf<-tibble::tibble(name=c('BW', 'CL','CV', 'DM','DS', 'EX','FC','LB', 'MA', 'MI', 'OM', 'RE','TA','TS',  'VS'), 
                            run=c('convertBW', 'convertCL','convertCV', 'convertDM','convertDS', 'convertEX','convertFW','convertLB', 'convertMA', 'convertMI', 'convertOM', 'convertRE','convertTA','convertTS', 'convertVS'))

the$conf<-tibble::tibble(name=c('BW', 'CL','DM','EX','FM','FX', 'MA', 'MI', 'OM', 'PY'), 
                         run=c('convert_BASF_BW', 'convert_BASF_CL', 'convert_BASF_DM', 'convert_BASF_EX','convert_BASF_FM','convert_BASF_FX', 'convert_BASF_MA', 'convert_BASF_MI', 'convert_BASF_OM', 'convert_BASF_PY'))



#'Report configuration
#'@export
config<-function(){
  the$conf
}

#'Change configuration at run time
#'@export
set_conf<-function(conf=tibble()){
  old<-the$conf
  the$conf<-conf
  invisible(old)
}

#'Remove empty cells and put NA inside
#'@export
make.true.NA <- function(x) if(is.character(x)||is.factor(x)){
  is.na(x) <- x %in% c("NA", "<NA>",""); x} else {
    x}