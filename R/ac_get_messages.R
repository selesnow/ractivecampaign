
#' Retrieve all sended messages
#'
#' @return tibble with messages info
#' @export
#' @seealso \href{https://developers.activecampaign.com/reference#messages}{Messages}
#' @examples
#' \dontrun{
#' Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
#' Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")
#'
#' msgs <- ac_get_messages()
#' }
ac_get_messages <- function(
){

  ac_check_auth()

  # vars
  is_first_iteration <- TRUE
  limit  <- 100
  offset <- 0
  total  <- NA
  res    <- list()

  while ( (is.na(total) | offset <= total) | is_first_iteration  ) {

    # send request
    ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/messages"),
               query = list(limit  = limit,
                            offset = offset),
               add_headers("Api-Token" = Sys.getenv('ACTIVECAMPAGN_API_TOKEN')))

    data <- content(ans)

    if ( status_code(ans) > 299 ) {
      stop(data$message)
    }

    out_data <- tibble(data = data$messages) %>%
                unnest_wider(data)

    is_first_iteration <- FALSE
    offset <- offset + limit
    total  <- as.integer(data$meta$total)
    res <- append(res, list(out_data))

    Sys.sleep(0.25)

  }

  res <- bind_rows(res)

  return(res)

}
