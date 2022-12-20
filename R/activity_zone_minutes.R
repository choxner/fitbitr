#' @include fitbitr.R
#' @include common.R
# Constants
url_active_zone_minutes <- paste0(url_api, "activities/active-zone-minutes/")

#' @title Get Activity Zone Minutes Time Series
#'
#' @description
#'   \code{get_azm_time_series()} returns time series data in the specified range for a given resource.
#'
#' @inheritParams inheritparams_token
#' @param base_date The range start date. A Date class object or a string in the format yyyy-MM-dd or today.
#' @param end_date The end date of the range. A Date class object or a string in the format yyyy-MM-dd.
#' @param date The end date of the period specified. A Date class object or a string in the format yyyy-MM-dd.
#' @param period The range for which data will be returned. Options are "1d", "7d", "30d", "1w", "1m", "3m", "6m", "1y", or "max".
#' @inheritParams inheritparams_simplify
#'
#' @details
#'  
#'
#'  See \url{https://dev.fitbit.com/build/reference/web-api/active-zone-minutes-timeseries} for more details.
#'
#' @export
get_azm_time_series <- function(token, date="", period="", base_date="", end_date="", simplify=TRUE)
{
  url <- if(date != "" && period != ""){
    paste0(url_active_zone_minutes, sprintf("date/%s/%s.json", format_date(date), period))
  } else if(base_date != "" & end_date != ""){
    paste0(url_active_zone_minutes, sprintf("date/%s/%s.json", format_date(base_date), format_date(end_date)))
  } else{
    stop("Error: Need to enter combination of date/period or base_date/end_date")
  }
  tidy_output(get(url, token), simplify)
}



#' @title Get Activity Intraday Time Series
#'
#' @description
#'   \code{get_activity_intraday_time_series()} returns intraday time series data in the specified range for a given resource.
#'   Access to the Intraday Time Series for personal use (accessing your own data) is available through the "Personal" App Type.
#'
#' @inheritParams inheritparams_token
#' @inheritParams inheritparams_date
#' @param detail_level Number of data points to include. Either 1min or 15min. Optional.
#' @param start_time The start of the period, in the format HH:mm. Optional.
#' @param end_time The end of the period, in the format HH:mm. Optional.
#' @inheritParams inheritparams_simplify
#'
#' @details
#'  Add details here
#'
#'  See \url{https://dev.fitbit.com/build/reference/web-api/intraday/get-azm-intraday-by-interval/} for more details.
#'
#' @export
get_azm_intraday_time_series <- function(token, date, detail_level="15min", start_time=NULL, end_time=NULL, simplify=TRUE)
{
  date <- format_date(date)
  url <- if(!is.null(start_time) && !is.null(end_time)){
    date2 <- if(start_time < end_time){
      "1d"
    } else{
      date2 <- as.Date(date) + 1
    }
    paste0(url_activity, sprintf("date/%s/%s/%s/time/%s/%s.json", date, date2, detail_level, start_time, end_time))
  } else{
    paste0(url_activity, sprintf("date/%s/1d/%s.json", date, detail_level))
  }
  tidy_output(get(url, token), simplify)
}
