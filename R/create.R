
# Notes about function:
# x must contain response rates for all groups in desired excel file (e.g., includes org results for wu tables)
# x must have column reportID in format ORGID_x_x with the number of _ referring to the workunit level
# _99 refers to not reported work units and they will be filtered out
# other columns in x: Population, Responses, RR
# require type to be "workunit" or "demographic"
create_responserate_sheet <- function(x,type="workunit") {

  x <- dplyr::ungroup(x)

  if(type=="workunit") {

    # Get ORG by selecting substring of ID prior to first "_"
    rr_table <- x %>%
      dplyr::mutate(ORG = ifelse(stringr::str_count(reportID,"_")==0,
                                 reportID,
                                 substr(reportID,1,(regexpr("_",reportID)-1)))) %>%
      dplyr::filter(stringr::str_count(reportID,"99")==0)  # Filter NES work units

    # Get number of levels
    num_levels <- max(stringr::str_count(rr_table$reportID,"_"))

    # Spead level names across columns
    # Use NA instead of "" so that the labels aren't cut off in Excel --> Not sure this works anymore
    for(i in 0:num_levels){
      rr_table[paste("Level",i,sep="")] <- ifelse(stringr::str_count(rr_table$reportID,"_")==i,rr_table$reportTitle,NA)
    }

    rr_table <- rr_table %>%
      dplyr::mutate(`Response Rate`=rounded(RR)/100) %>%  # Divide by 100 to use Excel percent formatting
      dplyr::rename(Employees=Population,Respondents=Responses) %>%  # Rename columns to desired output names
      dplyr::select(ORG,paste("Level",0:num_levels,sep=""),Employees,Respondents,`Response Rate`)  # Order columns to desired output order (appending org for splitting)

    split_R <- split(rr_table,rr_table$ORG)  # split file by ORG

    purrr::walk(unique(rr_table$ORG), ~ write_responserate_sheet(as.data.frame(split_R[[.x]][-1]),outputFile=paste0(.x,".xlsx")))

  } else if(type=="demographic") {

    demog_response <- function(ORG) {

      if(nrow(split_R[[ORG]])>1){  # Only create tables for ORGs with reportable demographics (i.e., more than just ORG results)

        #empty <- matrix(rep(NA,ncol(split_R[[ORG]])), nrow=1)
        #empty <- data.frame(empty)
        #colnames(empty) <- colnames(split_R[[ORG]])
        empty <- rep(NA,ncol(split_R[[ORG]]))
        names(empty) <- colnames(split_R[[ORG]])
        for(i in nrow(split_R[[ORG]]):2) {
          if(split_R[[ORG]][i,"Demographic"]!=split_R[[ORG]][i-1,"Demographic"]){
            split_R[[ORG]] <- dplyr::bind_rows(split_R[[ORG]][1:i-1,],empty,split_R[[ORG]][-(1:i-1),])
          } else {
            split_R[[ORG]][i,"Demographic"] <- NA
          }
        }

        split_R[[ORG]] <- split_R[[ORG]] %>%
          dplyr::mutate(`Response Rate`=rounded(RR)/100) %>%  # Divide by 100 to use Excel percent formatting
          dplyr::rename(Employees=Population,Respondents=Responses) %>%  # Rename columns to desired output names
          dplyr::select(Demographic,Group,Employees,Respondents,`Response Rate`)

        write_responserate_sheet(as.data.frame(split_R[[ORG]]),outputFile=paste0("Demog Table - ",ORG,".xlsx"))

      }
    }

      split_R <- split(x,x$reportID) # split file by reportID
      purrr::walk(unique(x$reportID), demog_response)

  } else {
    simpleError("type needs to be either 'workunit' or 'demographic'")
  }

}

create_driverscore_sheet <- function(x,type="workunit") {

  x <- dplyr::ungroup(x)

  # Number of drivers
  num_drivers <- length(unique(x$Drivers))

  driver_table <- x %>%
    dplyr::rename(`Driver Name`=Drivers,
                  `Average Score`=Rounded) %>%
    dplyr::mutate(`Legend for Model`= dplyr::case_when(`Average Score`<55 ~ "Understand your challenges",
                                                       `Average Score` %in% 55:64 ~ "Focus on improvements",
                                                       `Average Score` %in% 65:74 ~ "Leverage your strengths",
                                                       `Average Score` %in% 75:84 ~ "Celebrate your successes",
                                                       TRUE ~ "Model your achievements"),
                  `Compare to Organization`=NA)

  if(type=="workunit") {

    # Number of levels
    num_levels <- max(stringr::str_count(x$reportID,"_"))

    wu_drivers <- function(ORG) {

      # Spead level names across columns
      for(i in 0:num_levels){
        split_D[[ORG]][paste("Level",i,sep="")] <- ifelse(stringr::str_count(split_D[[ORG]]$reportID,"_")==i,split_D[[ORG]]$reportTitle,"")
      }

      split_D[[ORG]] <- split_D[[ORG]] %>%
        dplyr::select(paste0("Level",0:num_levels),c("Driver Name","Average Score","Compare to Organization","Legend for Model"))

      # Fill in work unit names for filters
      for(i in (num_levels-1):0){ # Reverse through the levels
        for(j in 2:nrow(split_D[[ORG]])){  # Starting with the second row
          # If the cell to the right is not empty and the current cell is, then set the current cell equal to the name in the cell above
          if(!split_D[[ORG]][[paste("Level",i+1,sep="")]][j]=="" & split_D[[ORG]][[paste("Level",i,sep="")]][j]=="") {
            split_D[[ORG]][[paste("Level",i,sep="")]][j]=split_D[[ORG]][[paste("Level",i,sep="")]][j-1]
          }
        }
      }

      # Calculate Compare to organization column
      # For all rows minus those with organization results (rows 1 to num_drivers), subtract org score from WU score
      split_D[[ORG]]$`Compare to Organization`[-(1:num_drivers)] <- split_D[[ORG]]$`Average Score`[-(1:num_drivers)] -
        rep(split_D[[ORG]]$`Average Score`[(1:num_drivers)],((nrow(split_D[[ORG]])/num_drivers)-1))

      write_driverscore_sheet(as.data.frame(split_D[[ORG]]),outputFile=paste0(ORG,".xlsx"))
    }

    wu_driver_table <- driver_table %>%
      dplyr::mutate(ORG = ifelse(stringr::str_count(reportID,"_")==0,
                                 reportID,
                                 substr(reportID,1,(regexpr("_",reportID)-1)))) %>%
      dplyr::filter(stringr::str_count(reportID,"99")==0)  # Filter NES work units

    split_D <- split(wu_driver_table,wu_driver_table$ORG) ## split file by ORG
    purrr::walk(unique(wu_driver_table$ORG), wu_drivers)

  } else if(type=="demographic") {

    demog_drivers <- function(ORG) {  # Run by ORG

      if(nrow(split_D[[ORG]])>num_drivers) { # Only create tables for ORGs with reportable demographics (i.e., more than just ORG results)

        # Calculate Compare to organization column
        # For all rows minus those with organization results (rows 1 to num_drivers), subtract org score from demog score
        split_D[[ORG]]$`Compare to Organization`[-(1:num_drivers)] <- (split_D[[ORG]]$`Average Score`[-(1:num_drivers)] -
                                                                         rep(split_D[[ORG]]$`Average Score`[(1:num_drivers)],
                                                                             ((nrow(split_D[[ORG]])/num_drivers)-1)))

        write_driverscore_sheet(as.data.frame(split_D[[ORG]][-1]),outputFile=paste0("Demog Table - ",ORG,".xlsx"))

      }
    }

    demog_driver_table <- driver_table %>%
      dplyr::select(reportID,Demographic,Group,`Driver Name`,`Average Score`,`Compare to Organization`,`Legend for Model`)

    split_D <- split(demog_driver_table, demog_driver_table$reportID) # split file by reportID
    purrr::walk(unique(demog_driver_table$reportID), demog_drivers)


  } else {
    simpleError("type needs to be either 'workunit' or 'demographic'")
  }

}

create_questionscore_sheet <- function(x,type="workunit") {

  x <- dplyr::ungroup(x)
  num_qs <- length(unique(x$Q_TEXT))

  scores_table <- x %>%
    dplyr::select(-Question) %>%
    dplyr::mutate(`Compare Average Score to Organization`=NA,
                  Disagree = rounded(Disagree)/100,
                  Neutral = rounded(Neutral)/100,
                  Agree = rounded(Agree)/100) %>%
    dplyr::rename(`Linkage to Model`=MODEL_LINKAGE,
                  `Survey Section`=Q_SECTION,
                  Question=Q_TEXT,
                  `% Disagree`=Disagree,
                  `% Neutral`=Neutral,
                  `% Agree`=Agree,
                  `Average Score`=Rounded)

  if(type=="workunit"){

    # Number of levels
    num_levels <- max(stringr::str_count(scores_table$reportID,"_"))

    wu_scores <- function(ORG) {

      # Spead level names across columns
      # Use NA instead of "" so that the labels aren't cut off in Excel
      for(i in 0:num_levels){
        split_S[[ORG]][paste("Level",i,sep="")] <- ifelse(stringr::str_count(split_S[[ORG]]$reportID,"_")==i,split_S[[ORG]]$reportTitle,"")
      }

      split_S[[ORG]] <- split_S[[ORG]] %>%
        dplyr::select(paste("Level",0:num_levels,sep=""),
                      c("Linkage to Model","Survey Section","Question","% Disagree","% Neutral","% Agree",
                        "Average Score","Compare Average Score to Organization"))

      # Fill in work unit names for filters
      for(i in (num_levels-1):0){ # Reverse through the levels
        for(j in 2:nrow(split_S[[ORG]])){  # Starting with the second row
          # If the cell to the right is not empty and the current cell is, then set the current cell equal to the name in the cell above
          if(!split_S[[ORG]][[paste("Level",i+1,sep="")]][j]=="" & split_S[[ORG]][[paste("Level",i,sep="")]][j]=="") {
            split_S[[ORG]][[paste("Level",i,sep="")]][j]=split_S[[ORG]][[paste("Level",i,sep="")]][j-1]
          }
        }
      }

      split_S[[ORG]]$`Compare Average Score to Organization`[-(1:num_qs)] <- split_S[[ORG]]$`Average Score`[-(1:num_qs)] -
        rep(split_S[[ORG]]$`Average Score`[(1:num_qs)],((nrow(split_S[[ORG]])/num_qs)-1))

      write_questionscore_sheet(as.data.frame(split_S[[ORG]]),outputFile=paste0(ORG,".xlsx"))
    }


    wu_scores_table <- scores_table %>%
      dplyr::mutate(ORG = ifelse(stringr::str_count(reportID,"_")==0,
                                 reportID,
                                 substr(reportID,1,(regexpr("_",reportID)-1)))) %>%
      dplyr::filter(stringr::str_count(reportID,"99")==0)  # Filter NES work units

    split_S <- split(wu_scores_table, wu_scores_table$ORG) ## split file by ORG
    purrr::walk(unique(wu_scores_table$ORG), wu_scores)

  } else if(type=="demographic") {

    demog_scores <- function(ORG) {  # Run by ORG

      if(nrow(split_S[[ORG]])>num_qs) {  # Only create tables for ORGs with reportable demographics (i.e., more than just ORG results)

        split_S[[ORG]]$`Compare Average Score to Organization`[-(1:num_qs)] <- split_S[[ORG]]$`Average Score`[-(1:num_qs)] -
          rep(split_S[[ORG]]$`Average Score`[(1:num_qs)],((nrow(split_S[[ORG]])/num_qs)-1))

        write_questionscore_sheet(as.data.frame(split_S[[ORG]][-1]),outputFile=paste0("Demog Table - ",ORG,".xlsx"))
      }
    }

    demog_scores_table <- scores_table %>%
      dplyr::select(reportID,Demographic,Group,`Linkage to Model`,`Survey Section`,Question,`% Disagree`,`% Neutral`,`% Agree`,`Average Score`,
             `Compare Average Score to Organization`)

    split_S <- split(demog_scores_table,demog_scores_table$reportID)  # split file by ORG
    purrr::walk(unique(demog_scores_table$reportID), demog_scores)


  } else {
    simpleError("type needs to be either 'workunit' or 'demographic'")
  }


}






