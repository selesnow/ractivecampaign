
#' Retrieve List of Deal Custom Field Values
#'
#' @param deal_id Filter results by a specific deal
#'
#' @return tibble with deal custom fields values
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
#' Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")
#'
#' deal_field_values <- ac_get_custom_deal_fields_values()
#' }
ac_get_custom_deal_fields_values <- function(
  deal_id = NULL
) {

  ac_check_auth()

  if ( length(deal_id) > 0 ) {
    deal_id <- paste0(deal_id, collapse = ',')
  }

  # vars
  is_first_iteration <- TRUE
  limit  <- 100
  offset <- 0
  total  <- NA
  res    <- list()

  while ( (is.na(total) | offset <= total) | is_first_iteration  ) {

    # send request
    retry(
      {
      ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/dealCustomFieldData"),
                 query = list(limit  = limit,
                              offset = offset,
                              "filters[dealId]" = deal_id),
                 add_headers("Api-Token" = Sys.getenv('ACTIVECAMPAGN_API_TOKEN')))
      },
      until = ~ status_code(.) == 200,
      interval  = getOption('ractivecampaig.interval'),
      max_tries = getOption('ractivecampaig.max_tries')
    )

    data <- content(ans)

    if ( status_code(ans) > 299 ) {
      stop(data$message)
    }

    out_data <- tibble(data = data$dealCustomFieldData) %>%
                unnest_wider(data, transform = list(fieldValue = function(x) paste(x, collapse = ', ')))

    is_first_iteration <- FALSE
    offset <- offset + limit
    total  <- as.numeric(data$meta$total)
    res <- append(res, list(out_data))

    Sys.sleep(0.25)

  }

  res <- bind_rows(res)

  return(res)
}
