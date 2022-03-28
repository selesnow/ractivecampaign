#' Retrieve a list of existing tasks
#'
#' @param title character, The title to be assigned to the task.
#' @param reltype character, The name of the relating object (see relationships table).
#' @param relid integer, The id of the relational object for this task.
#' @param status integer, Task status means complete or incomplete. 1 is complete and 0 is incomplete.
#' @param note character, The content describing the task.
#' @param duedate date, Due date of the task.
#' @param d_tasktypeid integer, The type of the task based on the available Task Types in the account.
#' @param userid integer, User ID this task belongs to.
#' @param due_after character, Filter deal tasks that are due after a specific date.
#' @param due_before character, Filter deal tasks that are due before a specific date.
#' @param duedate_range character, Filter deal tasks that are due between specific date range.
#' @param assignee_userid integer, The id of the user a task is assigned to.
#' @param outcome_id integer The id of a task outcome that the task belongs to.
#'
#' @return tibble with tasks data
#' @export
ac_get_tasks <- function(
  title           = NULL,
  reltype         = NULL,
  relid           = NULL,
  status          = NULL,
  note            = NULL,
  duedate         = NULL,
  d_tasktypeid    = NULL,
  userid          = NULL,
  due_after       = NULL,
  due_before      = NULL,
  duedate_range   = NULL,
  assignee_userid = NULL,
  outcome_id      = NULL
) {

  # cli options
  oldpar <- options(
    'cli.progress_show_after',
    'cli.progress_clear',
    'cli.spinner'
  )

  on.exit(options(oldpar))

  ac_progress_bar_set()

  ac_check_auth()

  # vars
  is_first_iteration <- TRUE
  limit  <- 100
  offset <- 0
  total  <- NA
  res    <- list()

  # pb
  cli_progress_bar("Loading deals")

  while ( (is.na(total) | offset <= total) | is_first_iteration  ) {
    # send request
    retry(
      {
        ans <- GET(str_glue("{Sys.getenv('ACTIVECAMPAGN_API_URL')}/api/3/dealTasks"),
          query = list(
            limit  = limit,
            offset = offset,
            "filters[title]"           = title,
            "filters[reltype]"         = reltype,
            "filters[relid]"           = relid,
            "filters[status]"          = status,
            "filters[note]"            = note,
            "filters[duedate]"         = duedate,
            "filters[d_tasktypeid]"    = d_tasktypeid,
            "filters[userid]"          = userid,
            "filters[due_after]"       = due_after,
            "filters[due_before]"      = due_before,
            "filters[duedate_range]"   = duedate_range,
            "filters[assignee_userid]" = assignee_userid,
            "filters[outcome_id]"      = outcome_id),
          add_headers("Api-Token"      = Sys.getenv('ACTIVECAMPAGN_API_TOKEN')))
      },
      until = ~ status_code(.) == 200,
      interval  = getOption('ractivecampaig.interval'),
      max_tries = getOption('ractivecampaig.max_tries')
    )

    data <- content(ans)

    if ( status_code(ans) > 299 ) {
      stop(data$message)
    }

    out_data <- tibble(data = data$dealTasks) %>%
                unnest_wider(data)

    is_first_iteration <- FALSE
    offset <- offset + limit
    total  <- as.numeric(data$meta$total)
    res <- append(res, list(out_data))

    Sys.sleep(0.25)
    # pb
    cli_progress_update()
  }

  # pb
  cli_progress_update(force = TRUE)

  res <- bind_rows(res)

  return(res)

}
