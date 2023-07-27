
#' Retrieve user groups by user
#'
#' @param user_id IDs of users
#'
#' @return tibble with user groups
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
#' Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")
#'
#' user_groups <- ac_get_group_by_user(c(11, 54, 67))
#' }
ac_get_group_by_user <- function(
    user_id
) {

  ac_check_auth()

  # send requests
  list_data <- pblapply(user_id, function(uid) {

    retry(
      {
        ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/users/{uid}/userGroup"),
                   query = list(limit  = 100),
                   add_headers("Api-Token" = Sys.getenv('ACTIVECAMPAGN_API_TOKEN')))
      },
      until = ~ status_code(.) == 200 || grepl(pattern = "No Result found", x = content(.)$message, ignore.case = T),
      interval  = getOption('ractivecampaig.interval'),
      max_tries = getOption('ractivecampaig.max_tries')
    )

    data <- content(ans)

    if ( !is.null(data$message) ) {
      warning(data$message)
      return(NULL)
    }

    if ( status_code(ans) > 299 ) {
      stop(data$message)
    }

    out_data <- tibble(data = data) %>%
      unnest_wider('data')

    Sys.sleep(0.25)

    return(out_data)

  }
  )

  out_data <- bind_rows(list_data)

  return(out_data)

}
