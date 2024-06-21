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
  
  c<-config()
  
  for (i in seq_along(c)) {
    d<-loadDomain(c$name[i], indir)
    f<-match.fun(c$run[i])
    cd<-f(c$name[i], d)
    exportDomain(cd,'csv', outdir)
  }
  
}
  
  
