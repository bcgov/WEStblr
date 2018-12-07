

recode_scores <- function(x, questions=WESquestions18) {

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

recode_categories <- function(x, questions=WESquestions18) {

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

recode_drivers <- function(x, questions=WESquestions18) {

  # Drop drivers if already exist.
  x <- x[!(colnames(x) %in% questions$MODEL_LINKAGE)]

  # Calculate driver scores
  for(i in unique(questions$MODEL_LINKAGE[!(questions$MODEL_LINKAGE==c(""))])) {
    x[[i]] <- rowMeans(x[paste(questions$Q_COLUMN[questions$MODEL_LINKAGE==i],"S",sep="")])
  }

  x
}


recode_engagement <- function(x, eng_char=c("BC Public Service Commitment", "Organization Satisfaction", "Job Satisfaction")) {

  # Drop Engagement if already exists.
  x <- x[!(colnames(x) %in% c("Engagement"))]

  # Calculate Engagement scores
  x$Engagement <- rowMeans(x[eng_char])

  x

}
