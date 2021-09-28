
#' Retrieve List of Account Custom Field Values
#'
#' @param customer_account_id Filter results by a specific account, see \code{\link{ac_get_accounts}}
#'
#' @return tibble with account custom fields values
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
#' Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")
#'
#' contacts <- ac_get_custom_account_fields_values()
#' }
ac_get_custom_account_fields_values <- function(
  customer_account_id = NULL
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
    ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/accountCustomFieldData"),
               query = list(limit  = limit,
                            offset = offset,
                            customer_account_id = customer_account_id),
               add_headers("Api-Token" = Sys.getenv('ACTIVECAMPAGN_API_TOKEN')))

    data <- content(ans)

    if ( status_code(ans) > 299 ) {
      stop(data$message)
    }

    out_data <- tibble(data = data$accountCustomFieldData) %>%
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
