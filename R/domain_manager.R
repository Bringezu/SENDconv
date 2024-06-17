#'Create a new environment and define a tibble with Acronyms and functions
the<-new.env(parent = emptyenv())
the$conf<-tibble::tibble(name=c('BW', 'CL','CV', 'DM','DS', 'EX','FW','LB', 'MA', 'MI', 'OM', 'RE','TA', 'VS'), 
                            run=c('convertBW', 'convertCL','convertCV', 'convertDM','convertDS', 'convertEX','convertFW','convertLB', 'convertMA', 'convertMI', 'convertOM', 'convertRE','convertTA', 'convertVS'))

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