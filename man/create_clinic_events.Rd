% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mainfuncs.r
\name{create_clinic_events}
\alias{create_clinic_events}
\title{create_clinic_events()}
\usage{
create_clinic_events(start, end, excluded = NULL, bank_hols = NULL)
}
\arguments{
\item{start}{First day of time period to create events in}

\item{end}{Last day of time period}

\item{excluded}{A date vector containing days to be excluded ie no events to be created on these days}

\item{bank_hols}{A date vector containing the dates of any bank holidays in the time period. No events
will be created on bank holidays}
}
\description{
Create a list of events to load straight to google calendar
}
\examples{
create_clinic_events(dmy("1/1/17"), dmy("7/1/17"), NULL, scrape_bh_dates())

}
