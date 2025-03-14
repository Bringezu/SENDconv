% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertVS.R
\name{convertVS}
\alias{convertVS}
\title{convertVS
This module converts the Provantis Export for Vital Signs to a SEND like structure leading to a VS Domain.}
\usage{
convertVS(domainData)
}
\arguments{
\item{domainData}{The data frame containing the export of Provantis}

\item{domainName}{A character vector that contains the acronym for the domain}
}
\value{
SEND Converted Domain as data frame
}
\description{
convertVS
This module converts the Provantis Export for Vital Signs to a SEND like structure leading to a VS Domain.
}
\examples{
 SEND<-convertVS(domainData)

}
