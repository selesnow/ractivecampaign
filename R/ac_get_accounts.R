
#' Retrieve all existing account
#'
#' @param search Search by name
#' @param count_deals Whether to compute the contactCount and dealCount counts for the number of contacts/deals associated with each account. Set it to true to include the right counts. If set to false or omitted from the call, then contactCount and dealCount will not be counted and be simply displayed as 0.
#' @seealso \href{https://developers.activecampaign.com/reference#accounts}{Accounts}
#' @return tibble with account list
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
#' Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")
#'
#' my_accounts <- ac_get_accounts(search = 'name')
#' }
#'
ac_get_accounts <- function(
  search = NULL,
  count_deals = FALSE
  ){

  ac_check_auth()

  # vars
  is_first_iteration <- TRUE
  limit  <- 100
  offset <- 0
  total  <- NA
  res    <- list()

  while ( (is.na(total) | offset <= total) | is_first_iteration  ) {

    # send request
    ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/accounts"),
               query = list(limit  = limit,
                            offset = offset,
                            search = search,
                            count_deals = count_deals),
               add_headers("Api-Token" = Sys.getenv('ACTIVECAMPAGN_API_TOKEN')))

    data <- content(ans)

    if ( status_code(ans) > 299 ) {
      stop(data$message)
    }

    out_data <- tibble(data = data$accounts) %>%
                unnest_wider(data)

    is_first_iteration <- FALSE
    offset <- offset + limit
    total  <- as.integer(data$meta$total)
    res <- append(res, list(out_data))

    Sys.sleep(0.25)

  }

  res <- bind_rows(res)

  return(res)

}
