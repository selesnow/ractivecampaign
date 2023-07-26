#' Return list of all existing user groups
#'
#' @return tibble with users group metadata
#' @export
#' @seealso See \href{https://developers.activecampaign.com/reference/list-all-groups}{ActiveCampaign API documentation}
#'
#' @examples
#' \dontrun{
#' users <- ac_get_users()
#' }
ac_get_user_groups <- function() {

  ac_check_auth()

  # vars
  is_first_iteration <- TRUE
  limit  <- 100
  offset <- 0
  total  <- NA
  res    <- list()

  while ( (is.na(total) | offset <= total) | is_first_iteration ) {

    # send request
    retry(
      {
        ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/groups/"),
                   query = list(limit  = limit,
                                offset = offset),
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

    out_data <- tibble(data = data$groups) %>%
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
