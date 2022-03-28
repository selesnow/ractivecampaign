#' Get deal notes
#'
#' @param deal_id integer, deal id
#'
#' @return tibble with deal notes data
#' @export
ac_get_deal_notes <- function(
    deal_id
) {

  ac_check_auth()

  # send requests
  list_data <- pblapply(deal_id, function(did) {

    retry(
      {
        ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/deals/{did}/notes"),
                   query = list(limit  = 100),
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

    out_data <- tibble(data = data$notes) %>%
                unnest_wider('data')

    Sys.sleep(0.25)

    return(out_data)

  }
  )

  out_data <- bind_rows(list_data)

  return(out_data)

}
