#' Calculates response rates and logs to a csv file.
#'
#' @param x A data.frame
#' @param outputFile The path to the output file.
#' @param responseFlag A character string of the column in data flagging completions.
#' @param reportID A character vector containing the column names in x containing the reporting IDs (e.g., ORGID, LEVEL1_ID, etc.)
#' @param reportTitle A character vector containing the column names in x containing the report titles (e.g., Organization, LEVEL1, etc.).
#'
#' @importFrom magrittr %>%
log_responserate_data <- function(x, outputFile="Response_Log.csv", responseFlag="QCOMP", reportID, reportTitle, userList=NULL, demogRollUp=NULL) {
  # note to self: still need to add in demographic

  # ungroup, as using add=TRUE for grouping to enable org demographic reporting (needs 3 grouping variables)
  x <- x %>% ungroup()

  calculate_rr <- function(x2,reportID,reportTitle) {
    x2 %>%
    dplyr::rename_(responseFlag=responseFlag,
                   reportID=reportID,
                   reportTitle=reportTitle) %>%
    dplyr::group_by(reportID,reportTitle,add=TRUE) %>%
    dplyr::summarize(Responses=sum(responseFlag,na.rm=TRUE),
                     Population=n()) %>%
    dplyr::mutate(RR=Responses/Population)
  }

  filter_reportGroup <- function(reportID, reportTitle, telkeys) {
    x %>%
      dplyr::filter(telkey %in% as.character(telkeys)) %>%
      dplyr::mutate(ID=reportID,
                    Title=reportTitle)
  }

  if(length(reportID) < 1) {
    #error message

  } else if(length(reportID) == 1) {

    rr_data <- calculate_rr(x,reportID,reportTitle)

  } else if(!is.null(userList)) {

    rr_data <- purrr::map2_dfr(reportID, reportTitle,
                               ~ calculate_rr(filter_reportGroup(.x, .y, userList[.x][!is.na(userList[.x])]),"ID","Title"))

  } else if(!is.null(demogRollUp)) {

    rr_data <- x %>%
      dplyr::rename_(ORGID=reportID) %>%
      dplyr::select(c("ORGID",responseFlag,unique(demogRollup$Demographic_in_data))) %>%
      group_by(ORGID) %>%
      tidyr::gather("Demographic_in_data","Group_in_data",unique(demogRollup$Demographic_in_data)) %>%
      dplyr::left_join(demogRollup,by=c("ORGID"=reportID,"Demographic_in_data","Group_in_data")) %>%
      dplyr::filter(!is.na(Group)) %>%
      calculate_rr("Demographic","Group") %>%
      dplyr::rename(Demographic=reportID,Group=reportTitle) %>%
      dplyr::left_join(demogRollup,by=c("ORGID"=reportID,"Demographic","Group")) %>%
      dplyr::arrange(ORGID,Sort) %>%
      dplyr::select(ORGID,Demographic,Group,Responses,Population,RR)

  } else {

    rr_data <-  purrr::map2_dfr(reportID, reportTitle, calculate_rr, x2=x) %>%
      dplyr::filter(!(is.na(reportID) | reportID=="")) %>%
      dplyr::arrange(reportID)

  }

  write.csv(rr_data, outputFile, row.names = FALSE)

}
