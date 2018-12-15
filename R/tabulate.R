#' Tabulates response rates and logs to a csv file.
#'
#' @param x A data.frame
#' @param reportID A character vector containing the column names in x containing the reporting IDs (e.g., ORGID, LEVEL1_ID, etc.)
#' @param reportTitle A character vector containing the column names in x containing the report titles (e.g., Organization, LEVEL1, etc.).
#' @param responseFlag A character string of the column in data flagging completions. Default is "QCOMP".
#' @param outputFile The path to the output file. Default is "Response_Log.csv"
#' @return A data.frame with tabulated response rates
#' @examples
#' tabulate_responserate_data(data,reportID="ID",reportTitle = "Title",outputFile="BCPS_Response_Log.csv")
#'
#' @importFrom magrittr %>%
tabulate_responserate_data <- function(x, reportID, reportTitle, responseFlag="QCOMP", outputFile="Response_Log.csv", userList=NULL) {

  # calculate RRs for selected group
  calculate_rr <- function(x2,reportID,reportTitle) {
    x2 %>%
    dplyr::rename_(responseFlag=responseFlag,
                   reportID=reportID,
                   reportTitle=reportTitle) %>%
    dplyr::group_by(reportID,reportTitle) %>%
    dplyr::summarize(Responses=sum(responseFlag,na.rm=TRUE),
                     Population=n()) %>%
    dplyr::mutate(RR=Responses*100/Population)
  }

  # filter group - needed for userList defined groups
  filter_reportGroup <- function(reportID, reportTitle, telkeys) {
    x %>%
      dplyr::filter(telkey %in% as.character(telkeys)) %>%
      dplyr::mutate(ID=reportID,
                    Title=reportTitle)
  }

  if(length(reportID) < 1) {
    #error message

  } else if(length(reportID) > 1) {

    if(!is.null(userList)) {

      rr_data <- purrr::map2_dfr(reportID, reportTitle,
                                 ~ calculate_rr(filter_reportGroup(.x, .y, userList[.x][!is.na(userList[.x])]),"ID","Title"))

    } else { # rollup

      rr_data <-  purrr::map2_dfr(reportID, reportTitle, calculate_rr, x2=x) %>%
        dplyr::filter(!(is.na(reportID) | reportID=="")) %>%
        dplyr::arrange(reportID)

    }


  } else { # length(reportID) == 1

     rr_data <- calculate_rr(x,reportID,reportTitle)

  }

  write.csv(rr_data, outputFile, row.names = FALSE)
  rr_data

}

#' Tabulates response rates and logs to a csv file - for demographic breakdowns.
#'
#' @param x A data.frame
#' @param reportID A character vector containing the column names in x containing the reporting IDs (e.g., ORGID, LEVEL1_ID, etc.)
#' @param demogRollup A data.frame containing the demographic groupings to be reported. See WESdemogRollup18, or WESOrgdemogRollup18 for reference. Must include the columns reportID, Demographic, Group, Demographic_in_data, Group_in_data and Sort.
#' @param responseFlag A character string of the column in data flagging completions. Default is "QCOMP".
#' @param outputFile The path to the output file. Default is "Response_Log.csv"
#' @return A data.frame with tabulated response rates
#' @examples
#' tabulate_responserate_demogdata(x=data, reportID="ORGID18", demogRollup = demog_rollup_org)
#'
#' @importFrom magrittr %>%
tabulate_responserate_demogdata <- function(x, reportID, demogRollup, responseFlag="QCOMP", outputFile="Response_Log.csv"){

  rr_data <- x %>%
    dplyr::rename_(responseFlag=responseFlag,
                   reportID=reportID) %>%
    dplyr::select(responseFlag, reportID, unique(demogRollup$Demographic_in_data)) %>%
    tidyr::gather("Demographic_in_data", "Group_in_data", unique(demogRollup$Demographic_in_data)) %>%
    dplyr::left_join(demogRollup, by=c("reportID", "Demographic_in_data", "Group_in_data")) %>%
    dplyr::filter(!is.na(Group)) %>%
    dplyr::group_by(reportID, Demographic, Group, Sort) %>%
    dplyr::summarize(Responses=sum(responseFlag,na.rm=TRUE),
                     Population=n()) %>%
    dplyr::mutate(RR=Responses*100/Population) %>%
    dplyr::arrange(reportID,Sort) %>%
    dplyr::select(-Sort)

  write.csv(rr_data, outputFile, row.names = FALSE)
  rr_data
}



#' Tabulates question scores and logs to a csv file.
#'
#' @param x A data.frame
#' @param reportID A character vector containing the column names in x containing the reporting IDs (e.g., ORGID, LEVEL1_ID, etc.)
#' @param reportTitle A character vector containing the column names in x containing the report titles (e.g., Organization, LEVEL1, etc.).
#' @param questions A data.frame containing the questions used in the survey. Default is the questions file from WES 2018. See WESquestions18 for reference.
#' @param outputFile The path to the output file. Default is "Scores_Log.csv"
#' @return A data.frame with tabulated response rates
#' @examples
#' tabulate_questionscore_data(data,reportID="ID",reportTitle = "Title",outputFile="BCPS_Scores_Log.csv")
#'
#' @importFrom magrittr %>%
tabulate_questionscore_data <- function(x, reportID, reportTitle, questions=WESquestions18, outputFile="Scores_Log.csv", userList=NULL) {


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


#' Tabulates question scores and logs to a csv file - for demographic breakdowns.
#'
#' @param x A data.frame
#' @param reportID A character vector containing the column names in x containing the reporting IDs (e.g., ORGID, LEVEL1_ID, etc.)
#' @param demogRollup A data.frame containing the demographic groupings to be reported. See WESdemogRollup18, or WESOrgdemogRollup18 for reference. Must include the columns reportID, Demographic, Group, Demographic_in_data, Group_in_data and Sort.
#' @param questions A data.frame containing the questions used in the survey. Default is the questions file from WES 2018. See WESquestions18 for reference.
#' @param outputFile The path to the output file. Default is "Scores_Log.csv"
#' @return A data.frame with tabulated response rates
#' @examples
#' tabulate_questionscore_demogdata(data,reportID="ORGID18",demogRollup = demog_rollup_org)
#'
#' @importFrom magrittr %>%
tabulate_questionscore_demogdata <- function(x, reportID, demogRollup, questions=WESquestions18, outputFile="Scores_Log.csv") {


  qscores <- paste0(questions$Q_COLUMN,"S")
  qcats <- paste0(questions$Q_COLUMN,"_3cat")

  x2 <- x %>%
    dplyr::rename_(reportID=reportID) %>%
    dplyr::select(reportID, qscores, qcats, unique(demogRollup$Demographic_in_data)) %>%
    tidyr::gather("Demographic_in_data", "Group_in_data", unique(demogRollup$Demographic_in_data)) %>%
    dplyr::left_join(demogRollup, by=c("reportID", "Demographic_in_data", "Group_in_data")) %>%
    dplyr::filter(!is.na(Group)) %>%
    dplyr::select(-Demographic_in_data,-Group_in_data)

  s1 <- x2 %>%
    dplyr::select(reportID,Demographic,Group,Sort,qscores) %>%
    dplyr::group_by(reportID,Demographic,Group,Sort,add=TRUE) %>%
    dplyr::summarize_all(dplyr::funs(mean), na.rm=TRUE) %>%
    tidyr::gather("Question","Score",-c("reportID","Demographic","Group","Sort")) %>%
    dplyr::mutate(Question=gsub("S","",Question),Rounded=rounded(Score)) %>%
    dplyr::arrange(reportID,Sort)

  s2 <- x2 %>%
    dplyr::select(reportID,Demographic,Group,Sort,qcats) %>%
    dplyr::group_by(reportID,Demographic,Group,Sort) %>%
    tidyr::gather("Question","Category",-c("reportID","Demographic","Group","Sort")) %>%
    dplyr::filter(!is.na(Category)) %>%
    dplyr::group_by(Question,add=TRUE) %>%
    dplyr::summarize(Disagree=sum(Category==1)*100/n(), Neutral=sum(Category==2)*100/n(), Agree=sum(Category==3)*100/n()) %>%
    dplyr::mutate(Question=gsub("_3cat","",Question)) %>%
    dplyr::arrange(reportID,Sort)

  scores_data <- dplyr::left_join(s1,s2,by=c("reportID","Demographic","Group","Sort","Question")) %>%
    dplyr::left_join(questions[!(colnames(questions) %in% c("Q_ID"))],by=c("Question"="Q_COLUMN"))


  write.csv(scores_data,outputFile,row.names = FALSE)
  scores_data

}

#' Tabulates driver/engagement scores and logs to a csv file - for demographic breakdowns.
#'
#' @param x A data.frame
#' @param reportID A character vector containing the column names in x containing the reporting IDs (e.g., ORGID, LEVEL1_ID, etc.)
#' @param demogRollup A data.frame containing the demographic groupings to be reported. See WESdemogRollup18, or WESOrgdemogRollup18 for reference. Must include the columns reportID, Demographic, Group, Demographic_in_data, Group_in_data and Sort.
#' @param questions A data.frame containing the questions used in the survey. Default is the questions file from WES 2018. See WESquestions18 for reference.
#' @param outputFile The path to the output file. Default is "Driver_Log.csv"
#' @return A data.frame with tabulated response rates
#' @examples
#' tabulate_driverscore_demogdata(data, reportID="ORGID18", demogRollup = demog_rollup_org)
#'
#' @importFrom magrittr %>%
tabulate_driverscore_data <- function(x, reportID, reportTitle, questions=WESquestions18, outputFile="Drivers_Log.csv", userList=NULL) {

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

  } else { # rollup

    driver_data <-  purrr::map2_dfr(reportID, reportTitle, calculate_drivers, x2=x) %>%
      dplyr::filter(!(is.na(reportID) | reportID=="")) %>%
      dplyr::arrange(reportID)
  }


  write.csv(driver_data,outputFile,row.names = FALSE)
  driver_data

}

#' Tabulates driver/engagement scores and logs to a csv file.
#'
#' @param x A data.frame
#' @param reportID A character vector containing the column names in x containing the reporting IDs (e.g., ORGID, LEVEL1_ID, etc.)
#' @param reportTitle A character vector containing the column names in x containing the report titles (e.g., Organization, LEVEL1, etc.).
#' @param questions A data.frame containing the questions used in the survey. Default is the questions file from WES 2018. See WESquestions18 for reference.
#' @param outputFile The path to the output file. Default is "Driver_Log.csv"
#' @return A data.frame with tabulated response rates
#' @examples
#' tabulate_driverscore_data(data,reportID="ID",reportTitle = "Title",outputFile="BCPS_Driver_Log.csv")
#'
#' @importFrom magrittr %>%
tabulate_driverscore_demogdata <- function(x, reportID, demogRollup, questions=WESquestions18, outputFile="Drivers_Log.csv") {

  drivers <- questions %>%
    dplyr::select(MODEL_LINKAGE) %>%
    dplyr::filter(MODEL_LINKAGE!="") %>%
    dplyr::pull() %>%
    unique()

  driver_data <- x %>%
    dplyr::rename_(reportID=reportID) %>%
    dplyr::select(reportID, drivers, Engagement, unique(demogRollup$Demographic_in_data)) %>%
    tidyr::gather("Demographic_in_data", "Group_in_data", unique(demogRollup$Demographic_in_data)) %>%
    dplyr::left_join(demogRollup, by=c("reportID", "Demographic_in_data", "Group_in_data")) %>%
    dplyr::filter(!is.na(Group)) %>%
    dplyr::select(-Demographic_in_data,-Group_in_data) %>%
    dplyr::group_by(reportID, Demographic, Group, Sort) %>%
    dplyr::summarize_all(dplyr::funs(mean), na.rm=TRUE) %>%
    tidyr::gather("Drivers","Scores",-c("reportID","Demographic","Group","Sort")) %>%
    dplyr::mutate(Rounded=rounded(Scores)) %>%
    dplyr::left_join(rbind(unique(questions[c("MODEL_LINKAGE","Driver_ID")]),c("Engagement","dr_00")),by=c("Drivers"="MODEL_LINKAGE")) %>%
    dplyr::arrange(reportID,Sort,Driver_ID)

  write.csv(driver_data,outputFile,row.names = FALSE)
  driver_data

}
