% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ac_get_accounts.R
\name{ac_get_accounts}
\alias{ac_get_accounts}
\title{Retrieve all existing account}
\usage{
ac_get_accounts(search = NULL, count_deals = FALSE)
}
\arguments{
\item{search}{Search by name}

\item{count_deals}{Whether to compute the contactCount and dealCount counts for the number of contacts/deals associated with each account. Set it to true to include the right counts. If set to false or omitted from the call, then contactCount and dealCount will not be counted and be simply displayed as 0.}
}
\value{
tibble with account list
}
\description{
Retrieve all existing account
}
\examples{
\dontrun{
Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")

my_accounts <- ac_get_accounts(search = 'name')
}

}
\seealso{
\href{https://developers.activecampaign.com/reference#accounts}{Accounts}
}
