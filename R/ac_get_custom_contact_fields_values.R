
#' Retrieve List of Contacts Custom Field Values
#'
#' @param field_id ID of the field the value belongs to.
#' @param val Value of the custom field for a specific contact
#'
#' @return tibble with contacts field values
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
#' Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")
#'
#' contacts <- ac_get_custom_contact_fields_values()
#' }
ac_get_custom_contact_fields_values <- function(
  field_id = NULL,
  val = NULL
) {

  ac_check_auth()

  # vars
  is_first_iteration <- TRUE
  limit  <- 100
  offset <- 0
  total  <- NA
  res    <- list()

  while ( (is.na(total) | offset <= total) | is_first_iteration  ) {

    # send request
    ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/fieldValues"),
               query = list(limit  = limit,
                            offset = offset,
                            "filters[fieldid]" = field_id,
                            "filters[val]" = val),
               add_headers("Api-Token" = Sys.getenv('ACTIVECAMPAGN_API_TOKEN')))

    data <- content(ans)

    if ( status_code(ans) > 299 ) {
      stop(data$message)
    }

    out_data <- tibble(data = data$fieldValues) %>%
                unnest_wider(data)

    is_first_iteration <- FALSE
    offset <- offset + limit
    total  <- data$meta$total
    res <- append(res, list(out_data))

    Sys.sleep(0.25)

  }

  res <- bind_rows(res)

  return(res)
}
