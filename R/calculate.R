
#' Convert 1 - 5 scale to 100pt scale.
#'
#'@param x A data.frame with survey responses on a scale from 1 to 5.
#'@param questions A data.frame containing the questions used in the survey. Default is the questions file from WES 2018. See WESquestions18 for reference.
#'@return The initial data.frame with additional columns for each 100pt scale recode. Additional column names have the format <Q_COLUMN>S.
calculate_scores <- function(x, questions=WESquestions18) {

  # Drop scores if already exist.
  x <- x[!(colnames(x) %in% paste0(questions$Q_COLUMN,"S"))]

  # Calculate scores
  for (i in questions$Q_COLUMN[1:length(questions$Q_COLUMN)]) {
    x[[paste(i,"S",sep="")]][x[[i]]==1] <- 0
    x[[paste(i,"S",sep="")]][x[[i]]==2] <- 25
    x[[paste(i,"S",sep="")]][x[[i]]==3] <- 50
    x[[paste(i,"S",sep="")]][x[[i]]==4] <- 75
    x[[paste(i,"S",sep="")]][x[[i]]==5] <- 100
  }

  x
}

#' Convert 1 - 5 scale into 3 categories.
#'
#'@param x A data.frame with survey responses on a scale from 1 to 5.
#'@param questions A data.frame containing the questions used in the survey. Default is the questions file from WES 2018. See WESquestions18 for reference.
#'@return The initial data.frame with additional columns for each category recode. Additional column names have the format <Q_COLUMN>_3cat.
calculate_categories <- function(x, questions=WESquestions18) {

  # Drop categories if already exist.
  x <- x[!(colnames(x) %in% paste0(questions$Q_COLUMN,"_3cat"))]

  # Calculate categories
  for (i in questions$Q_COLUMN[1:length(questions$Q_COLUMN)]) {
    x[[paste(i,"_3cat",sep="")]][x[[i]]==1] <- 1
    x[[paste(i,"_3cat",sep="")]][x[[i]]==2] <- 1
    x[[paste(i,"_3cat",sep="")]][x[[i]]==3] <- 2
    x[[paste(i,"_3cat",sep="")]][x[[i]]==4] <- 3
    x[[paste(i,"_3cat",sep="")]][x[[i]]==5] <- 3
  }

  x
}

#' Calculate drivers as defined in questions table.
#'
#'@param x A data.frame with survey responses on a scale from 1 to 5.
#'@param questions A data.frame containing the questions used in the survey. Default is the questions file from WES 2018. See WESquestions18 for reference.
#'@return The initial data.frame with additional columns for each driver as listed in the questions table.
calculate_drivers <- function(x, questions=WESquestions18) {

  # Drop drivers if already exist.
  x <- x[!(colnames(x) %in% questions$MODEL_LINKAGE)]

  # Calculate driver scores
  for(i in unique(questions$MODEL_LINKAGE[!(questions$MODEL_LINKAGE==c(""))])) {
    x[[i]] <- rowMeans(x[paste(questions$Q_COLUMN[questions$MODEL_LINKAGE==i],"S",sep="")])
  }

  x
}

#' Calculate engagement as defined by user.
#'
#'@param x A data.frame with survey responses on a scale from 1 to 5.
#'@param eng_char A character vector with the names of the columns of x used to calculate engagement. Default is c("BC Public Service Commitment", "Organization Satisfaction", "Job Satisfaction")
#'@return The initial data.frame with additional column Engagement.
calculate_engagement <- function(x, eng_char=c("BC Public Service Commitment", "Organization Satisfaction", "Job Satisfaction")) {

  # Drop Engagement if already exists.
  x <- x[!(colnames(x) %in% c("Engagement"))]

  # Calculate Engagement scores
  x$Engagement <- rowMeans(x[eng_char])

  x

}
