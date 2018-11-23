# header row: text headers (color = 9 is white, "#234075" is navy)
csH1 <- function(outwb) {
  csH1 <- xlsx::CellStyle(outwb) +
    xlsx::Font(outwb, color = 9, isBold = TRUE) +
    xlsx::Fill(backgroundColor="#234075", foregroundColor = "#234075", pattern="SOLID_FOREGROUND") +
    xlsx::Alignment(v="VERTICAL_CENTER")
}

# header row: stat headers (aligned center)
csH2 <- function(outwb) {
  csH2<- xlsx::CellStyle(outwb) +
    xlsx::Font(outwb, color = 9, isBold = TRUE) +
    xlsx::Fill(backgroundColor="#234075", foregroundColor = "#234075", pattern="SOLID_FOREGROUND") +
    xlsx::Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER", wrapText = TRUE)
}

# text cells
csRegular <- function(outwb) {
  csRegular <- xlsx::CellStyle(outwb) +
    xlsx::Alignment(v="VERTICAL_CENTER", wrapText = TRUE)
}

# stat cells (e.g., counts, scores, differences)
csStat <- function(outwb) {
  csStat <- xlsx::CellStyle(outwb) +
    xlsx::Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER")
}

# percent cells (e.g., response rates)
csPerc <- function(outwb) {
  csPerc <- xlsx::CellStyle(outwb) +
    xlsx::Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER") +
    xlsx::DataFormat("0%")
}


# coloured cellstyles - based on WES legend ----

#Understand your challenges
csU <- function(outwb){
  csU <- xlsx::CellStyle(outwb) +
    xlsx::Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER") +
    xlsx::Fill(backgroundColor="#ffaa71",foregroundColor = "#ffaa71",pattern="SOLID_FOREGROUND")
}


#Focus on improvements
csF <- function(outwb){
  csF <- xlsx::CellStyle(outwb) +
    xlsx::Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER") +
    xlsx::Fill(backgroundColor="#ffe36d",foregroundColor = "#ffe36d",pattern="SOLID_FOREGROUND")
}


#Leverage your strengths
csL <- function(outwb){
  csL<- xlsx::CellStyle(outwb) +
    xlsx::Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER") +
    xlsx::Fill(backgroundColor="#98e096",foregroundColor = "#98e096",pattern="SOLID_FOREGROUND")
}


#Celebrate your successes
csC <- function(outwb){
  csC <- xlsx::CellStyle(outwb) +
    xlsx::Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER") +
    xlsx::Fill(backgroundColor="#abcbef",foregroundColor = "#abcbef",pattern="SOLID_FOREGROUND")
}


#Model your achievements
csM <- function(outwb){
  csM <- xlsx::CellStyle(outwb) +
    xlsx::Alignment(h="ALIGN_CENTER", v="VERTICAL_CENTER") +
    xlsx::Fill(backgroundColor="#c5a7bf",foregroundColor = "#c5a7bf",pattern="SOLID_FOREGROUND")
}



