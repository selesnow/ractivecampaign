% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ac_get_deal_stages.R
\name{ac_get_deal_stages}
\alias{ac_get_deal_stages}
\title{Get Stage of Deals}
\usage{
ac_get_deal_stages(title = NULL, d_groupid = NULL)
}
\arguments{
\item{title}{Filter by deal stages' titles. Any stages whose titles partial-match the filter value are returned}

\item{d_groupid}{Filter by pipeline's id}
}
\value{
tibble with dictionary of deal stages
}
\description{
Stages are a way to group deals within a pipeline. A pipeline can have any number of stages within it and each stage can have any number of deals within it.
}
\examples{
\dontrun{
Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")

deal_stages <- ac_get_deal_stages()
}
}
