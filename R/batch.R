
# define directories
# indir <- data source directory
indir<-'/opt/provantis/DA/LIVE/'

# outdir target directory for storing the SEND files
outdir<-paste0(indir, 'out/')

# load configuration from environment
c<-SendConv::config()

for (i in seq_along(c$run)) {
  d<-loadDomain(c$name[i])
  dd<-eval(d$run[i], d)
  exportDomain(dd, 'txt', outdir)
}