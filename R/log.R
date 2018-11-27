#' Calculates response rates and logs to a csv file.
#'
#' @param x A data.frame
#' @param outputFile The path to the output file.
#' @param responseFlag A character string of the column in data flagging completions.
#' @param reportID A character vector containing the column names in x containing the reporting IDs (e.g., ORGID, LEVEL1_ID, etc.)
#' @param reportTitle A character vector containing the column names in x containing the report titles (e.g., Organization, LEVEL1, etc.).
#'
#' @importFrom magrittr %>%
log_responserate_data <- function(x, outputFile="Response_Log.csv", responseFlag="QCOMP", reportID, reportTitle) {

  calculate_rr <- function(reportID,reportTitle) {
    x %>%
    dplyr::rename_(responseFlag=responseFlag,
                   reportID=reportID,
                   reportTitle=reportTitle) %>%
    dplyr::group_by(reportID,reportTitle) %>%
    dplyr::summarize(Responses=sum(responseFlag,na.rm=TRUE),
                     Population=n()) %>%
    dplyr::mutate(RR=Responses/Population)
  }

  if(length(reportID) == 1) {

    rr_data <- calculate_rr(reportID,reportTitle)

  } else {

    rr_data <-  purrr::map2_dfr(reportID, reportTitle, calculate_rr) %>%
      dplyr::filter(!(is.na(reportID) | reportID=="")) %>%
      dplyr::arrange(reportID)

  }

  write.csv(rr_data, outputFile, row.names = FALSE)

}
