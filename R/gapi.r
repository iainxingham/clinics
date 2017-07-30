# Clinics R package
# Code using the Google API

#' gAPI_authenticate()
#'
#' Handles signing in to Google. If necessary will open a browser window to sign in.
#'
#' Requires a file called \code{credentials.dat} containing a client ID and a client secret. These need
#' to be obtained from Google - Google API console. Put the client ID in a variable called \code{gAPIclientID}
#' and the secret in \code{gAPIsecret}. Create the \code{credentials.dat} file with \code{save(gAPIclientID, gAPIsecret, file="credentials.dat")}
#'
#' @example
#' gAPI_authenticate()
#'
gAPI_authenticate <- function() {
  load("credentials.dat")
  options("googleAuthR.client_id" = gAPIclientID)
  options("googleAuthR.client_secret" = gAPIsecret)
  options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/calendar"))
  gar_auth()
}

# Internal functions
# Get calendar events within a time period
gAPI_get_events <- function(start_time=now(), end_time=now()+days(3)) {
  f <- gar_api_generator("https://www.googleapis.com/calendar/v3/calendars/primary/events",
                         "GET",
                         pars_args = list(timeMin=toRFC3339(start_time), timeMax=toRFC3339(end_time)),
                         data_parse_function = function(x) x)
  f()$items
}

# Create a calendar event
gAPI_create_event <-function(event) {
  f <- gar_api_generator("https://www.googleapis.com/calendar/v3/calendars/primary/events",
                         "POST", pars_args = list(),
                         data_parse_function = function(x) x)
  f(the_body = event)
}
