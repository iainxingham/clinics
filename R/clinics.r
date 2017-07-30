# Clinics R package

#' Clinics
#'
#' A package to insert events with a slightly complicated recurrence
#' pattern into a google calendar. Clearly this is very specific to my own timetable.
#'
#' Ulterior motive: learn R package creation.
#'
#' The google API requires a client ID and secret to be saved in
#' \code{credentials.dat}. See \code{gAPI_authenticate()} for more details.
#'
#' Typical work flow (create clinics in the calendar for the next 28 days):-
#'
#' \code{gAPI_authenticate()}
#' \code{appts <- get_existing_appointments(today(), today()+days(28)}
#' \code{create_clinic_events(today(), today()+days(28), excluded=appts$exclude, bank_hols=scrape_bh_dates())}
#' \code{post_Sunday_clinics(appts)}
#'
#' @importFrom lubridate dmy dst now wday today
#' @importFrom rvest read_html html_nodes html_table
#' @importFrom googleAuthR gar_auth gar_api_generator
NULL
