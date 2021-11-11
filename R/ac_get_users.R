#' Return list of all existing users
#'
#' @return tibble with users metadata
#' @export
#' @seealso See \href{https://developers.activecampaign.com/reference?#list-all-users}{ActiveCampaign API documentation}
#'
#' @examples
#' \dontrun{
#' users <- ac_get_users()
#' }
ac_get_users <- function() {

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
        ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/users"),
                   query = list(limit  = limit,
                                offset = offset),
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

    out_data <- tibble(data = data$users) %>%
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
