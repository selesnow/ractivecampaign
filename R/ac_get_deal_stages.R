
#' Get Stage of Deals
#'
#' @param title Filter by deal stages' titles. Any stages whose titles partial-match the filter value are returned
#' @param d_groupid Filter by pipeline's id
#'
#' @return tibble with dictionary of deal stages
#' @export
#' @description Stages are a way to group deals within a pipeline. A pipeline can have any number of stages within it and each stage can have any number of deals within it.
#'
#' @examples
#' \dontrun{
#' Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
#' Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")
#'
#' deal_stages <- ac_get_deal_stages()
#' }
ac_get_deal_stages <- function(
  title = NULL,
  d_groupid = NULL
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
    ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/dealStages"),
               query = list(limit  = limit,
                            offset = offset,
                            "filters[title]" = title,
                            "filters[d_groupid]" = d_groupid),
               add_headers("Api-Token" = Sys.getenv('ACTIVECAMPAGN_API_TOKEN')))

    data <- content(ans)

    if ( status_code(ans) > 299 ) {
      stop(data$message)
    }

    out_data <- tibble(data = data$dealStages) %>%
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
