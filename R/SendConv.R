#' SendConv
#' The main file to manange the conversion. 
#' To control which domains are converted, the system uses a local environment.
#' @param indir directory for input files
#' @param outdir directory for outout files 
#'
#'
#' @examples
#' c<-config() # load configuration
#' # select domains (Here only BW and DM)
#' c<-c %>% dplyr::filter(name %in% c("BW", "DM"))
#' indir<-'c:/temp/mquest_send/Provantis/DA/ARCH'
#' outdir<-'c:/temp/mquest_send/Provantis/DA/ARCH/output'
#' SendConv(indir, outdir)
SendConv<-function(indir, outdir){
  stopifnot(file.exists(indir))
  stopifnot(file.exists(outdir))
  
  domains_to_be_analysed<-config()
  
  for (i in seq_along(domains_to_be_analysed$name)) {
    print(paste0('Converting: ', domains_to_be_analysed$name[i]))
    d<-loadDomain(domains_to_be_analysed$name[i], indir)
    f<-match.fun(domains_to_be_analysed$run[i])
    cd<-f(domains_to_be_analysed$name[i], d)
    exportDomain(cd,'csv', outdir)
  }
  
}
  
  
