
#' Retrieve Campaign Aggregate Revenues
#'
#' @param campaign_ids IDs of campaign
#'
#' @return tibble with campaings revenue data
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
#' Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")
#'
#' c_agr_rev <- ac_get_campaigns_aggregate_revenues(c(11, 54, 67))
#' }
ac_get_campaigns_aggregate_revenues <- function(
  campaign_ids
) {

  ac_check_auth()

  # send requests
  list_data <- pblapply(campaign_ids, function(cid) {

      retry(
            {
              ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/campaigns/{cid}/aggregateRevenues"),
                         query = list(limit  = 100),
                         add_headers("Api-Token" = Sys.getenv('ACTIVECAMPAGN_API_TOKEN')))
            },
            until =  ~ status_code(.) == 200,
            interval  = getOption('ractivecampaig.max_tries'),
            max_tries = getOption('ractivecampaig.interval')
          )

      data <- content(ans)

      if ( status_code(ans) > 299 ) {
          stop(data$message)
      }

      out_data <- tibble(data = data$aggregateRevenues) %>%
                  unnest_wider(data)

      Sys.sleep(0.25)

      return(out_data)

  }
  )

  out_data <- bind_rows(list_data)

  return(out_data)

}
