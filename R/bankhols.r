# Bank Holidays

#' Scrape bank holiday dates from UK government website
#'
#' @return A Date vector containing bank holidays for England
#'   for the current and next year
#' @examples
#' bh_list <- scrape_bh_dates()
scrape_bh_dates <- function() {
  bh_webpage <- read_html("https://www.gov.uk/bank-holidays")
  bh_page_table <- html_nodes(bh_webpage, 'table')
  bh1 <- html_table(bh_page_table)
  # Could get Scotland or Northern Ireland by altering which
  # bit of the table is read ie bh[[x]]
  append(dmy(sapply(bh1[[1]][1], paste, colnames(bh1[[1]][1]))),
         dmy(sapply(bh1[[2]][1], paste, colnames(bh1[[2]][1]))))
}
