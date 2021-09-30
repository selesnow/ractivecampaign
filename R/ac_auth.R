#' Authorization. Set API URL and Key
#'
#' @param url API URL
#' @param key API Key
#'
#' @description Your API key can be found in your account on the Settings page under the "Developer" tab. Each user in your ActiveCampaign account has their own unique API key.
#'
#' @seealso \href{https://developers.activecampaign.com/reference#authentication}{Authentication}
#' @return No return value, called for side effects
#' @export
ac_auth <- function(
  url,
  key
) {

  # set environ variables
  Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = key)
  Sys.setenv('ACTIVECAMPAGN_API_URL' = url)

  cli_alert_success("API token and url set successfully!")
}

ac_manual_auth <- function() {

  while ( identical( Sys.getenv('ACTIVECAMPAGN_API_TOKEN'), '' ) ) {

    key <- readline("API Key: ")

    if ( key == '' ) {
      cli_alert_warning("Please set your API Key, for detail see https://developers.activecampaign.com/reference#authentication")
    } else {
      Sys.setenv('ACTIVECAMPAGN_API_TOKEN' = key)
    }
  }

  while ( identical( Sys.getenv('ACTIVECAMPAGN_API_URL'), '' ) ) {

    url <- readline("API Url: ")

    if ( url == '' ) {
      cli_alert_warning("Please set your API URL, for detail see https://developers.activecampaign.com/reference#authentication")
    } else {
      Sys.setenv('ACTIVECAMPAGN_API_URL' = url)
    }
  }

  cli_alert_success("API token and url set successfully!")

}

ac_check_auth <- function() {

  if ( interactive() ) ac_manual_auth()

  if ( identical(Sys.getenv("ACTIVECAMPAGN_API_TOKEN"), "") | identical(Sys.getenv("ACTIVECAMPAGN_API_URL"), "") ) {

    stop("Unauthorize! Please set API URL and Token in system variable ACTIVECAMPAGN_API_TOKEN and ACTIVECAMPAGN_API_URL, or use ac_auth() for authorization.")

  }
}

