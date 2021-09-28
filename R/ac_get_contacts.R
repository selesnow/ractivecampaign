
#' Retrieve Contacts
#'
#' @param ids Filter contacts by ID. Can be repeated for multiple IDs.
#' @param email Email address of the contact you want to get
#' @param email_like Filter contacts that contain the given value in the email address
#' @param exclude Exclude from the response the contact with the given ID
#' @param formid Filter contacts associated with the given form
#' @param id_greater Only include contacts with an ID greater than the given ID
#' @param id_less Only include contacts with an ID less than the given ID
#' @param listid Filter contacts associated with the given list
#' @param search Filter contacts that match the given value in the contact names, organization, phone or email
#' @param segmentid Return only contacts that match a list segment (this param initially returns segment information, when it is run a second time it will return contacts that match the segment)
#' @param seriesid Filter contacts associated with the given automation
#' @param status Filter contact by status: -1 - Any, 0 - Unconfirmed, 1 -	Active, 2 - Unsubscribed, 3 - Bounced
#' @param tagid Filter contacts associated with the given tag
#' @param created_before Filter contacts that were created prior to this date
#' @param created_after Filter contacts that were created after this date
#' @param updated_before Filter contacts that were updated before this date
#' @param updated_after Filter contacts that were updated after this date
#' @param waitid Filter by contacts in the wait queue of an automation block
#' @param in_group_lists Set this to TRUE in order to return only contacts that the current user has permissions to see.
#'
#' @return tibble with contacts metadata
#' @seealso \href{https://developers.activecampaign.com/reference#contact}{Contacts}
#' @export
#'
#' @description Contacts are the center of activity in 'ActiveCampaign' and represent the people that the owner of an 'ActiveCampaign' account is marketing to or selling to.
#'
#' @examples
#' \dontrun{
#' Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
#' Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")
#'
#' contacts <- ac_get_contacts()
#' }
ac_get_contacts <- function(
  ids = NULL,
  email = NULL,
  email_like = NULL,
  exclude = NULL,
  formid = NULL,
  id_greater = NULL,
  id_less = NULL,
  listid = NULL,
  search = NULL,
  segmentid = NULL,
  seriesid = NULL,
  status = NULL,
  tagid = NULL,
  created_before = NULL,
  created_after = NULL,
  updated_before = NULL,
  updated_after = NULL,
  waitid = NULL,
  in_group_lists = NULL
) {

  ac_check_auth()

  # vars
  is_first_iteration <- TRUE
  limit  <- 100
  offset <- 0
  total  <- NA
  res    <- list()

  # query
  qry <- list(limit  = limit,
              offset = offset,
              email = email,
              email_like = email_like,
              exclude = exclude,
              formid = formid,
              id_greater = id_greater,
              id_less = id_less,
              listid = listid,
              search = search,
              segmentid = segmentid,
              seriesid = seriesid,
              status = status,
              tagid = tagid,
              "filters[created_before]" = created_before,
              "filters[created_after]" = created_after,
              "filters[updated_before]" = updated_before,
              "filters[updated_after]" = updated_after,
              "filters[waitid]" = waitid,
              in_group_lists = in_group_lists)

  # q param
  if ( !is.null(ids) ) {
    # ids
    ids <- lapply(ids, function(x) list("ids[]" = x))
    qry <- append(qry, unlist(ids))
  }

  while ( (is.na(total) | offset <= total) | is_first_iteration  ) {

    # send request
    ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/contacts"),
               query = qry,
               add_headers("Api-Token" = Sys.getenv('ACTIVECAMPAGN_API_TOKEN')))

    data <- content(ans)

    if ( status_code(ans) > 299 ) {
      stop(data$message)
    }

    out_data <- tibble(data = data$contacts) %>%
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
