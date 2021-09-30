
#' Retrieve all existing deals
#'
#' @param search Search text to use with search_field parameter.
#' @param search_field Field to search for. Available values: all (All three fields: title, contact, and org), title (Deal's title), contact (Deal's primary contact's first name and last name. If search parameter has more than one word, first word is used for matching first name of contacts and rest of the words are used for matching last name of contacts.), org (Deal's primary contact's organization name).
#' @param title Filter by deal's title
#' @param stage Filter by deal's stage
#' @param group Filter by deal's pipeline
#' @param status Filter by deal's status. 0 - Open, 1 - Won, 2 - Lose.
#' @param owner Filter by deal's owner
#' @param nextdate_range Filter by deal's tasks due dates. Available values: upcoming (Deals with tasks that are due within 24 hours.), scheduled (Deals with tasks that are due in more than 24 hours), overdue (Deals with tasks that are past due dates.), no-task (Deals without any task.).
#' @param tag Filter by tag names associated with deal's primary contact. Available values: greater than 0 (Deals with primary contacts that have the tag with given id), -1 (Deals with primary contacts that have any tag), -2 (Deals with primary contacts that have no tag)
#' @param tasktype Filter by deals that have tasks with given type
#' @param created_before Returns deals that are created less than given date
#' @param created_after Returns deals that are created greater than or equal to given date
#' @param updated_before Returns deals that are updated less than given date
#' @param updated_after Returns deals that are updated greater than or equal to given date
#' @param organization Filter by deal's primary contact's organization's id
#' @param minimum_value In USD with dollar portion. Returns deals whose values are greater than or equal to given
#' @param maximum_value In USD with dollar portion. Returns deals whose values are less than or equal to given
#' @param score_greater_than In a format of <score_id>:<score_value>. Returns deals whose score value is greater than given value
#' @param score_less_than In a format of <score_id>:<score_value>. Returns deals whose score value is less than given value
#' @param score In a format of <score_id>:<score_value>. Returns deals whose score value is equal to given value
#' @seealso \href{https://developers.activecampaign.com/reference#deal}{Deals}
#' @return tibble with deals data
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = "YOUR_TOKEN")
#' Sys.setenv('ACTIVECAMPAGN_API_URL' = "https://<your-account>.api-us1.com")
#'
#' deals <- ac_get_deals()
#' }
ac_get_deals <- function(
  search             = NULL,
  search_field       = NULL,
  title              = NULL,
  stage              = NULL,
  group              = NULL,
  status             = NULL,
  owner              = NULL,
  nextdate_range     = NULL,
  tag                = NULL,
  tasktype           = NULL,
  created_before     = NULL,
  created_after      = NULL,
  updated_before     = NULL,
  updated_after      = NULL,
  organization       = NULL,
  minimum_value      = NULL,
  maximum_value      = NULL,
  score_greater_than = NULL,
  score_less_than    = NULL,
  score              = NULL
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
    ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/deals"),
               query = list(limit  = limit,
                            offset = offset,
                            "filters[search]"             = search,
                            "filters[search_field]"       = search_field,
                            "filters[title]"              = title,
                            "filters[stage]"              = stage,
                            "filters[group]"              = group,
                            "filters[status]"             = status,
                            "filters[owner]"              = owner,
                            "filters[nextdate_range]"     = nextdate_range,
                            "filters[tag]"                = tag,
                            "filters[tasktype]"           = tasktype,
                            "filters[created_before]"     = created_before,
                            "filters[created_after]"      = created_after,
                            "filters[updated_before]"     = updated_before,
                            "filters[updated_after]"      = updated_after,
                            "filters[organization]"       = organization,
                            "filters[minimum_value]"      = minimum_value,
                            "filters[maximum_value]"      = maximum_value,
                            "filters[score_greater_than]" = score_greater_than,
                            "filters[score_less_than]"    = score_less_than,
                            "filters[score]"              = score_less_than),
               add_headers("Api-Token" = Sys.getenv('ACTIVECAMPAGN_API_TOKEN')))

    data <- content(ans)

    if ( status_code(ans) > 299 ) {
      stop(data$message)
    }

    out_data <- tibble(data = data$deals) %>%
                unnest_wider(data)

    is_first_iteration <- FALSE
    offset <- offset + limit
    total  <- data$meta$total
    res <- append(res, list(out_data))

    Sys.sleep(0.25)

  }

  res <- bind_rows(res)%>%
         mutate(value = as.numeric('value') / 100)

  return(res)
}
