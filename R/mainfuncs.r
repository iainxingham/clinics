# Clinics R package
# Main functions


#' create_clinic_events()
#'
#' Create a list of events to load straight to google calendar
#'
#' @param start First day of time period to create events in
#' @param end Last day of time period
#' @param excluded A date vector containing days to be excluded ie no events to be created on these days
#' @param bank_hols A date vector containing the dates of any bank holidays in the time period. No events
#'   will be created on bank holidays
#'
#' @example
#' create_clinic_events(dmy("1/1/17"), dmy("7/1/17"), NULL, scrape_bh_dates())
#'
create_clinic_events <- function(start, end, excluded=NULL, bank_hols=NULL) {
  dates_seq <-seq(from=start, to=end, by='days')

  f <- gar_api_generator("https://www.googleapis.com/calendar/v3/calendars/primary/events",
                         "POST", pars_args = list(),
                         data_parse_function = function(x) x)

  for(day_temp in dates_seq) {
    day_in_question <- as.Date(day_temp, origin=origin)

    if(!((day_in_question %in% bank_hols) | (day_in_question %in% excluded))) {

      day_of_week <- wday(day_in_question)

      if(day_of_week == 2) {
        # Monday
        f(the_body = make_event("Clinic", "BGH", day_in_question, hm("09:00"), hm("13:00")))
        f(the_body = make_event("Sleep clinic", "BGH", day_in_question, hm("13:30"), hm("17:00")))
      }
      else if(day_of_week == 3) {
        # Tuesday
        f(the_body = make_event("Clinic", "RBH", day_in_question, hm("09:00"), hm("13:00")))
      }
      else if(day_of_week == 5) {
        # Thursday
        f(the_body = make_event("Clinic", "AVH", day_in_question, hm("13:30"), hm("17:00")))
      }
    }
  }
}

#' clinic_list()
#'
#' Take a start and end day and find work days
#'
#' @param start First day of time period to create events in
#' @param end Last day of time period
#' @param excluded A date vector containing days to be excluded ie no events to be created on these days
#' @param bank_hols A date vector containing the dates of any bank holidays in the time period. No events
#'   will be created on bank holidays
#'
#' @return A data frame containing details of clinics to create
clinic_list <- function(start, end, excluded=NULL, bank_hols=NULL) {
  dates_seq <-seq(from=start, to=end, by='days')

  clinics <- NULL

  for(day_temp in dates_seq) {
    day_in_question <- as.Date(day_temp, origin=origin)

    if(!((day_in_question %in% bank_hols) | (day_in_question %in% excluded))) {

      day_of_week <- wday(day_in_question)

      if(day_of_week == 2) {
        # Monday
        clinics <- rbind(clinics, add_clinic(day_in_question, "AM", "BGH"))
        clinics <- rbind(clinics, add_clinic(day_in_question, "PM", "BGH", "Sleep clinic"))
      }
      else if(day_of_week == 3) {
        # Tuesday
        clinics <- rbind(clinics, add_clinic(day_in_question, "AM", "RBH"))
      }
      else if(day_of_week == 5) {
        # Thursday
        clinics <- rbind(clinics, add_clinic(day_in_question, "PM", "AVH"))
      }
    }
  }
  return(clinics)
}

#' get_existing_appointments()
#' Get a list of appointments already in calendar to avoid when creating clinics
#' @param start Start of period to search for pre existing appointments in
#' @param end End of time period
#'
#' @return A list containing two items, both vectors of dates. \code{$exclude} contains days with
#'   events that would otherwise clash with clinics. \code{$sundays} contains Sunday on calls
#'
#' @example
#' get_existing_appointments(dmy("1/1/17), dmy("1/2/17"))
get_existing_appointments <- function(start=today(), end=today()+days(7)) {
  excluded <- NULL
  ICUSundays <- NULL

  evlist <- gAPI_get_events(start, end)
  for(evi in 1:nrow(evlist)) {
    ev <- evlist[evi,]
    # All day or multiday event
    if(is.na(ev$start$dateTime)) {
      if(is.null(excluded))
        excluded <- seq(from=ymd(ev$start$date), to=(ymd(ev$end$date)-days()), by='days')
      else excluded <- c(excluded, seq(from=ymd(ev$start$date), to=(ymd(ev$end$date)-days()), by='days'))

      if(wday(ev$start$date) == 1)
        if(ev$summary == "ICU") {
          if(is.null(ICUSundays)) ICUSundays <- ymd(ev$start$date)+days()
          else ICUSundays <- c(ICUSundays, ymd(ev$start$date)+days())
        }
    }

    # Short event
    else {
      day_of_week <- wday(ev$start$dateTime)

      if(day_of_week %in% c(1,4,6,7)) next

      event_interval <- as.interval(ymd_hms(ev$start$dateTime), ymd_hms(ev$end$dateTime))
      day_in_question <- as.Date(ev$start$dateTime)

      if(day_of_week == 2)
        clinic_interval <- as.interval(day_in_question+hm("09:00"),
                                       day_in_question+hm("17:00"))
      else if(day_of_week == 3)
        clinic_interval <- as.interval(day_in_question+hm("09:00"),
                                       day_in_question+hm("13:00"))
      else if(day_of_week == 5)
        clinic_interval <- as.interval(day_in_question+hm("13:00"),
                                       day_in_question+hm("17:00"))

      if(int_overlaps(event_interval, clinic_interval)) {
        if(is.null(excluded)) excluded <- day_in_question
        else excluded <- c(excluded, day_in_question)
      }
    }
  }

  if((!is.null(excluded)) && (!is.null(ICUSundays))) excluded <- c(excluded, ICUSundays)
  return(list(exclude=unique(excluded), sundays=ICUSundays))
}

#' post_Sunday_clinics()
#'
#' This function creates clinics on Monday afternoon only (not morning) after a Sunday on call
#'
#' @param appts A list of pre existing appointments created with get_existing_appointments()
#'
#' @example
#' # Using default parameters for get_existing_appointments()
#' post_sunday_clinics(get_existing_appointments())
post_Sunday_clinics <- function(appts) {
  if(!is.null(appts$sundays))
    for(monday in appts$sunday) {
      gAPI_create_event(make_event("Sleep clinic", "BGH",
                                   as.Date(monday, origin=origin),
                                   hm("13:30"), hm("17:00")))
    }
}
