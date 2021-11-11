#' Get contacts tags
#'
#' @param contact_id contact id
#'
#' @return tibble with contacts tags
#' @export
ac_get_contact_tags <- function(
  contact_id
){

  ac_check_auth()

  # send requests
  list_data <- pblapply(contact_id, function(cid) {


    retry(
      {ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/contacts/{cid}/contactTags"),
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

    out_data <- tibble(data = data$contactTags) %>%
                unnest_wider('data')

    Sys.sleep(0.25)

    return(out_data)

  }
  )

  out_data <- bind_rows(list_data)

  return(out_data)

}
