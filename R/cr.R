#' CDISC Controlled terminology 
#'
#' The controlled terminology in this datset supports Define-XML v2.1. 
#' Additional terminology will be developed on an ongoing basis to support this standard. 
#' Future additions to the terminology set will be handled via the terminology change request and maintenance process.
#' 
#' SDTM and Define-XML are international standards for clinical research data tabulation and submission, 
#' and are approved by the U.S. Food and Drug Administration as standard electronic submission formats.  
#' CDISC and NCI work together to develop and maintain Define-XML controlled terminology, which is also 
#' integrated and distributed as part of NCI Thesaurus.
#' Further information is available at:
#'  http://www.cancer.gov/cancertopics/terminologyresources/CDISC
#'  Version: Q3 2024	2024-09-27
#'
#' @format ## `csv`
#' A data frame with 13301 rows and 8 columns:
#' \describe{
#'   \item{code}{A C-code with a leading character C followed by a sequence of digits used for uniquely identifying each concept in NCI Thesaurus (NCIt), including all CDISC concepts.}
#'   \item{Codelist Code}{An NCIt C-code assigned to the parent codelist name.}
#'   \item{Codelist Extensible}{Logical T/F Defines if controlled terms may be added to the codelist.}
#'   \item{Codelist Name}{descriptive name of the codelist which is also referred to as the codelist label in the implementation guide}
#'   \item{CDISC Submission Value}{This is the specific value expected for submissions. Each value corresponding to a Codelist Name}
#'   \item[CDISC Synonym(s)}]{Applicable synonyms for the CDISC Submission ValueE. These terms should not be submitted, but are included for collection and mapping purposes}
#'   \item{CDISC Definition}{Value of an ALT_DEFINITION property of a CDISC source for a particular concept. In many cases an existing NCI definition has been used.}
#'   \item{NCI Preferred Term}{ value of a PREFERRED_NAME property for a concept in NCIt. **NOTE - This column designates the human readable, fully specified preferred term corresponding to the NCI C-code, and is especially helpful for searching NCIt to get the entire concept with links to all instances of the term.}
#'   
#'   ...
#' }
#' @source <https://evs.nci.nih.gov/ftp1/CDISC/Define-XML/>
"ct"