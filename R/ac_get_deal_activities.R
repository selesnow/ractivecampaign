
#' Retrieve all activies if deals
#'
#' @param deal_id IDs of deal
#'
#' @return tibble with deal activies
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
#' Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")
#'
#' c_deal_activies<- ac_get_deal_activities(c(11, 54, 67))
#' }
ac_get_deal_activities <- function(
  deal_id
) {

  ac_check_auth()

  # send requests
  list_data <- pblapply(deal_id, function(did) {

    retry(
      {
      ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/deals/{did}/dealActivities"),
                 query = list(limit  = 100),
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

    out_data <- tibble(data = data$dealActivities) %>%
                unnest_wider('data')

    Sys.sleep(0.25)

    return(out_data)

  }
  )

  out_data <- bind_rows(list_data)

  return(out_data)

}
