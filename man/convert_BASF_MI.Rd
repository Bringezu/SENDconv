% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_BASF_MI.R
\name{convert_BASF_MI}
\alias{convert_BASF_MI}
\title{convert_BASF_MI}
\usage{
convert_BASF_MI(domainName, domainData)
}
\arguments{
\item{domainName}{A character vector that contains the acronym for the domain}

\item{domainData}{The data frame containing the export of Provantis}
}
\value{
SEND Converted Domain as data frame
}
\description{
convert_BASF_MI
}
\examples{
 domainName<-'MI'
 domainData<-loadDomain(domainName)
 SEND<-convertMA(domainName, domainData)

}
