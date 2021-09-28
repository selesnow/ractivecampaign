#' Retrieve all existing pipelines
#'
#' @param title Filter by pipeline's title. The filter matches any pipeline titles that contain the provided title (i.e. "Contact" matches all of "In Contact", "To Contact", and "Contact Pipeline").
#' @param have_stages Filter by whether pipelines have deal stages. Can be either 1 or 0. If 1, only pipelines with at least one stage will be returned.
#'
#' @return tibble with pipline dictionary
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
#' Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")
#'
#' deal_piplines <- ac_get_deal_piplines()
#' }
ac_get_deal_piplines <- function(
  title = NULL,
  have_stages = NULL
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
    ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/dealGroups"),
               query = list(limit  = limit,
                            offset = offset,
                            "filters[title]" = title,
                            "filters[have_stages]" = have_stages),
               add_headers("Api-Token" = Sys.getenv('ACTIVECAMPAGN_API_TOKEN')))

    data <- content(ans)

    if ( status_code(ans) > 299 ) {
      stop(data$message)
    }

    out_data <- tibble(data = data$dealGroups) %>%
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
