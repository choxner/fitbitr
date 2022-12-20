#' @include fitbitr.R
#' @include common.R
# Constants
url_active_zone_minutes <- paste0(url_api, "activities/active-zone-minutes/")

#' @title Get Activity Zone Minutes Time Series
#'
#' @description
#'   \code{get_activity_time_series()} returns time series data in the specified range for a given resource.
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
#'  See \url{https://dev.fitbit.com/reference/web-api/activity/#get-activity-time-series} for more details.
#'
#' @export
get_azm_time_series <- function(token, date="", period="", base_date="", end_date="", simplify=TRUE)
{
  url <- if(date != "" && period != ""){
    paste0(url_active_zone_minutes, sprintf("%s/date/%s/%s.json", resource_path, format_date(date), period))
  } else if(base_date != "" & end_date != ""){
    paste0(url_active_zone_minutes, sprintf("%s/date/%s/%s.json", resource_path, format_date(base_date), format_date(end_date)))
  } else{
    stop("Error: Need to enter combination of date/period or base_date/end_date")
  }
  tidy_output(get(url, token), simplify)
}