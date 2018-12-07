tabulate_responserate_data <- function(x,rollup=TRUE,ovData=NULL) {

  if(rollup==TRUE) {

    #Note to self: Add ovData (i.e. if org data not included in wu log data)

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

    purrr::walk(unique(rr_table$ORG), ~ create_responserate_sheet(split_R[[.x]][-1],outputFile=paste0(.x,".xlsx")))

  } else {

    ## Other RR tab options:
    ## Demographic tables
    ## - Corporate or by organization
    ## - whether or not overall data included in log data

  }

}
