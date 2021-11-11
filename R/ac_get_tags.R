#' Get list of all tags
#'
#' @param search Filter by name of tag(s)
#'
#' @return tibble with tags metadata
#' @export
#'
#' @examples
#' \dontrun{
#' tags <- ac_get_tags()
#' }
ac_get_tags <- function(
  search = NULL
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
    retry(
      {
    ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/tags"),
               query = list(limit  = limit,
                            offset = offset,
                            search = search),
               add_headers("Api-Token" = Sys.getenv('ACTIVECAMPAGN_API_TOKEN')))
      },
    until = ~ status_code(.) == 200,
    interval  = getOption('ractivecampaig.max_tries'),
    max_tries = getOption('ractivecampaig.interval')
    )


    data <- content(ans)

    if ( status_code(ans) > 299 ) {
      stop(data$message)
    }

    out_data <- tibble(data = data$tags) %>%
                unnest_wider(data)

    is_first_iteration <- FALSE
    offset <- offset + limit
    total  <- as.numeric(data$meta$total)
    res <- append(res, list(out_data))

    Sys.sleep(0.25)

  }

  res <- bind_rows(res)

  return(res)

}
