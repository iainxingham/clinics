# Clinics R package
# Miscellaneous functions etc

#' toRFC3339()
#'
#' Convert lubridate time to RFC 3339 for use by google API
#' See https://tools.ietf.org/html/rfc3339
#'
#' Assumes a UK location and corrects for British Summer Time if necessary
#'
#' @param x A date-time object
#'
#' @return A string containing the date-time in an RFC 3339 compatible format
#'
#' @examples
#' toRFC3339(now())
#' toRFC3339(dmy("1/1/17"))
#'
toRFC3339 <- function(x) {
  if(dst(x)) timeoffset <- "+01:00"
  else timeoffset <- "Z"
  sprintf("%d-%.2d-%.2dT%.2d:%.2d:%05.2f%s", year(x), month(x), day(x),
          hour(x), minute(x), second(x), timeoffset)
}

# Headers for writing csv file
headers <- c("Subject", "Start Date", "Start Time", "End Date", "End Time",
             "All Day Event", "Description", "Location", "Private")

# Helper function to create a clinic for the csv export functions
add_clinic <- function(clinic_day, clinic_session, location, clinic_type="Clinic") {
  # Create data frame structure
  Subject <- clinic_type
  StartDay <- clinic_day
  StartTime <- ifelse(clinic_session == "AM", "9:00",  "13:30")
  EndDay <- clinic_day
  EndTime <- ifelse(clinic_session == "AM", "13:00", "17:00")
  AllDay <- FALSE
  Desc <- ""
  Location <- location
  Private <- FALSE
  newclinic <- data.frame(Subject, StartDay, StartTime, EndDay, EndTime, AllDay, Desc,
                          Location, Private, stringsAsFactors = FALSE)
  return(newclinic)
}

# Make an event object for use with gAPI_create_event
make_event <- function(name, location, date, start, end) {
  list(summary = name, location = location,
       start = list(dateTime = toRFC3339(date + start), timeZone = "Europe/London"),
       end = list(dateTime = toRFC3339(date + end), timeZone = "Europe/London"))
}
