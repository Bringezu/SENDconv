% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_BASF_EX.R
\name{convert_BASF_EX}
\alias{convert_BASF_EX}
\title{convert_BASF_EX}
\usage{
convert_BASF_EX(domainName, domainData)
}
\arguments{
\item{domainName}{A character vector that contains the acronym for the domain}

\item{domainData}{The data frame containing the export of Provantis}
}
\value{
SEND Converted Domain as data frame
}
\description{
convert_BASF_EX
}
\examples{
 domainName<-'EX'
 domainData<-loadDomain(domainName)
 SEND<-convertMA(domainName, domainData)

}
