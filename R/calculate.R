
#' Calculate Survey Scores and Categories
#'
#'@description \code{calculate_scores} adds new \emph{100pt scale} variables based on the \emph{5pt scale} variables provided.
#'
#'@description \code{calculate_categories}  adds new \emph{category} (i.e., \emph{3pt scale}) variables based on the \emph{5pt scale} variables provided.
#'
#'@description \code{calculate_drivers} adds new \emph{driver} variables calculated as the average of the \emph{100pt scale} variables for that driver, defined by \code{questions$MODEL_LINKAGE}.
#'
#'@description \code{calculate_engagement} adds a new \emph{engagement} variable calculated as the average of the \emph{driver} variables in \code{eng_char}.
#'
#'@details  \code{calculate_scores} takes a data.frame containing \emph{5pt scale} variables and appends new \emph{100pt scale} variables based on the following conversion:
#' \itemize{
#'   \item 1 --> 0
#'   \item 2 --> 25
#'   \item 3 --> 50
#'   \item 4 --> 75
#'   \item 5 --> 100
#' }
#'
#' \code{calculate_categories} takes a data.frame containing \emph{5pt scale} variables and appends new \emph{category} variables based on the following conversion:
#' \itemize{
#'   \item 1 --> 1
#'   \item 2 --> 1
#'   \item 3 --> 2
#'   \item 4 --> 3
#'   \item 5 --> 3
#' }
#'
#' The questions data.frame input provides the framework for the survey. It must contain the variables
#' \code{Q_ID,  Q_COLUMN, MODEL_LINKAGE, Q_TEXT, Q_TEXT_FORMAT, Q_SECTION,} and \code{Driver_ID}. \code{Q_COLUMN} contains the names of the \emph{5pt scale} variables of x.
#' \code{MODEL_LINKAGE} provides the bridge between the survey questions and the drivers in the WES model. See the default questions data.frame \code{WESquestions18} for details.
#'
#'@note As the \emph{driver} variables are needed to calculate \code{Engagement}, running \code{calculate_engagement} will call \code{calculate_drivers}. Similarly,
#'as the \emph{100pt scale} variables are needed to calculate the \emph{drivers}, \code{calculate_drivers} will call \code{calculate_scores}.
#'Hence, a call to \code{calculate_engagement}, will result in the question and driver scores being calculated and appended to the returning data.frame.
#'
#'@param x A data.frame.
#'
#'@param questions A data.frame containing the questions used in the survey. Default is the questions file from WES 2018. See \code{WESquestions18} for reference.
#'
#'@return A data.frame with the same variables as x plus additional columns depending on the function used.
#'\emph{100pt scale} variables have the format <Q_COLUMN>S and \emph{category} variables have the format <Q_COLUMN>_3cat.
calculate_scores <- function(x, questions=WESquestions18) {

  # Check that all variables of questions$Q_COLOUMN exist in x
  if(length(x[colnames(x) %in% questions$Q_COLUMN]) != length(questions$Q_COLUMN)){

    simpleError("x does not contain all the survey questions listed in questions$Q_COLUMN. Check for spelling errors")

  } else {

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
}


#'@rdname calculate_scores
calculate_categories <- function(x, questions=WESquestions18) {

  # Check that all variables of questions$Q_COLOUMN exist in x
  if(length(x[colnames(x) %in% questions$Q_COLUMN]) != length(questions$Q_COLUMN)){

    simpleError("x does not contain all the survey questions listed in questions$Q_COLUMN. Check for spelling errors")

  } else {
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
}


#' @rdname calculate_scores
calculate_drivers <- function(x, questions=WESquestions18) {

  # Calculate question scores
  x <- calculate_scores(x,questions)

  # Drop drivers if already exist.
  x <- x[!(colnames(x) %in% questions$MODEL_LINKAGE)]

  # Calculate driver scores
  for(i in unique(questions$MODEL_LINKAGE[!(questions$MODEL_LINKAGE==c(""))])) {
    x[[i]] <- rowMeans(x[paste(questions$Q_COLUMN[questions$MODEL_LINKAGE==i],"S",sep="")])
  }

  x

}


#'@param eng_char A character vector with the names of the \emph{driver} variables used to calculate \code{Engagement}.
#'
#'@rdname calculate_scores
calculate_engagement <- function(x, questions=WESquestions18, eng_char=c("BC Public Service Commitment", "Organization Satisfaction", "Job Satisfaction")) {

  # check that eng_char are listed in questions$MODEL_LINKAGE
  if(length(eng_char[eng_char %in% WESquestions18$MODEL_LINKAGE]) != length(eng_char)) {

    simpleError("eng_char drivers are not all contained in questions$MODEL_LINKAGE. Check for spelling errors.")

  }else {

    # Drop Engagement if already exists.
    x <- x[!(colnames(x) %in% c("Engagement"))]

    # Calculate driver scores
    x <- calculate_drivers(x,questions)

    # Calculate Engagement scores
    x$Engagement <- rowMeans(x[eng_char])

    x

  }
}
