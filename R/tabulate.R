#' Calculates response rates and logs to a csv file.
#'
#' @param x A data.frame
#' @param outputFile The path to the output file.
#' @param responseFlag A character string of the column in data flagging completions.
#' @param reportID A character vector containing the column names in x containing the reporting IDs (e.g., ORGID, LEVEL1_ID, etc.)
#' @param reportTitle A character vector containing the column names in x containing the report titles (e.g., Organization, LEVEL1, etc.).
#'
#' @importFrom magrittr %>%
tabulate_responserate_data <- function(x, reportID, reportTitle, responseFlag="QCOMP", outputFile="Response_Log.csv", userList=NULL, demogRollUp=NULL) {
  # note to self: reportTitle not used for Demographic reporting. Set to NULL? Add check of equal length?

  # ungroup, as using add=TRUE for grouping to enable org demographic reporting (needs 3 grouping variables)
  x <- x %>% dplyr::ungroup()

  calculate_rr <- function(x2,reportID,reportTitle) {
    x2 %>%
    dplyr::rename_(responseFlag=responseFlag,
                   reportID=reportID,
                   reportTitle=reportTitle) %>%
    dplyr::group_by(reportID,reportTitle,add=TRUE) %>%
    dplyr::summarize(Responses=sum(responseFlag,na.rm=TRUE),
                     Population=n()) %>%
    dplyr::mutate(RR=Responses*100/Population)
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
  rr_data

}


tabulate_questionscore_data <- function(x, reportID, reportTitle, questions=WESquestions18, outputFile="Scores_Log.csv", userList=NULL, demogRollUp=NULL) {


  qscores <- paste0(questions$Q_COLUMN,"S")
  qcats <- paste0(questions$Q_COLUMN,"_3cat")

  calculate_scores <- function(x2,reportID,reportTitle) {

    s1 <- x2 %>%
      dplyr::rename_(reportID=reportID,
                     reportTitle=reportTitle) %>%
      dplyr::select(reportID,reportTitle,qscores) %>%
      dplyr::group_by(reportID,reportTitle,add=TRUE) %>%
      dplyr::summarize_all(dplyr::funs(mean), na.rm=TRUE) %>%
      tidyr::gather("Question","Score",-c("reportID","reportTitle")) %>%
      dplyr::mutate(Question=gsub("S","",Question),Rounded=rounded(Score)) %>%
      dplyr::arrange(reportID)

    s2 <- x2 %>%
      dplyr::rename_(reportID=reportID,
                     reportTitle=reportTitle) %>%
      dplyr::select(reportID,reportTitle,qcats) %>%
      dplyr::group_by(reportID,reportTitle,add=TRUE) %>%
      tidyr::gather("Question","Category",-c("reportID","reportTitle")) %>%
      dplyr::filter(!is.na(Category)) %>%
      dplyr::group_by(Question,add=TRUE) %>%
      dplyr::summarize(Disagree=sum(Category==1)*100/n(), Neutral=sum(Category==2)*100/n(), Agree=sum(Category==3)*100/n()) %>%
      dplyr::mutate(Question=gsub("_3cat","",Question)) %>%
      dplyr::arrange(reportID)

    dplyr::left_join(s1,s2,by=c("reportID","reportTitle","Question")) %>%
      dplyr::left_join(questions[!(colnames(questions) %in% c("Q_ID"))],by=c("Question"="Q_COLUMN"))

  }

  if(length(reportID) == 1) {

    scores_data <- calculate_scores(x,reportID,reportTitle)

  } else {

    scores_data <-  purrr::map2_dfr(reportID, reportTitle, calculate_scores, x2=x) %>%
      dplyr::filter(!(is.na(reportID) | reportID=="")) %>%
      dplyr::arrange(reportID)
  }

  write.csv(scores_data,outputFile,row.names = FALSE)
  scores_data
}


tabulate_driverscore_data <- function(x, reportID, reportTitle, questions=WESquestions18, outputFile="Drivers_Log.csv", userList=NULL, demogRollUp=NULL) {

  drivers <- questions %>%
    dplyr::select(MODEL_LINKAGE) %>%
    dplyr::filter(MODEL_LINKAGE!="") %>%
    dplyr::pull() %>%
    unique()

  calculate_drivers <- function(x2,reportID,reportTitle) {
    x2 %>%
      dplyr::rename_(reportID=reportID,
                     reportTitle=reportTitle) %>%
      dplyr::select(reportID,reportTitle,drivers,Engagement) %>%
      dplyr::group_by(reportID,reportTitle,add=TRUE) %>%
      dplyr::summarize_all(dplyr::funs(mean), na.rm=TRUE) %>%
      tidyr::gather("Drivers","Scores",-c("reportID","reportTitle")) %>%
      dplyr::mutate(Rounded=rounded(Scores)) %>%
      dplyr::arrange(reportID) %>%
      dplyr::left_join(rbind(unique(questions[c("MODEL_LINKAGE","Driver_ID")]),c("Engagement","dr_00")),by=c("Drivers"="MODEL_LINKAGE"))
  }


  if(length(reportID) == 1) {

    driver_data <- calculate_drivers(x,reportID,reportTitle)

  } else {

    driver_data <-  purrr::map2_dfr(reportID, reportTitle, calculate_drivers, x2=x) %>%
      dplyr::filter(!(is.na(reportID) | reportID=="")) %>%
      dplyr::arrange(reportID)
  }


  write.csv(driver_data,outputFile,row.names = FALSE)
  driver_data

}
