#' Create a formatted excel response rate sheet from tabulated data.
#'
#' @param x A data.frame
#' @param outputFile The path to the output file.
#' @param inputFile The path to the input file if one exists. For example to append sheet to a readme.xlsx file. If NULL, a new workbook will be created.
#' @param sheetName A character string with the sheet name. The default is "Response Rates".
create_responserate_sheet <- function(x, outputFile, inputFile=NULL, sheetName="Response Rates") {

  # create or load workbook
  if(is.null(inputFile)){
    outwb <- xlsx::createWorkbook(type="xlsx")
  } else {
    outwb <- xlsx::loadWorkbook(inputFile)

    ## remove existing sheet, sheetName, if applicable
    existing <- xlsx::getSheets(outwb)
    if(sheetName %in% names(existing)) {
      removeSheet(outwb,sheetName=sheetName)
    }
  }

  # create sheet
  sheet <- xlsx::createSheet(outwb, sheetName=sheetName)

  # assign styles
  ## expects last three columns to be stat and perc (e.g.,Employees, Respondents, Response Rate)
  ## create a blank list vector to store column styles, naming elements by column index
  responseStyles <- vector("list", ncol(x))
  names(responseStyles) <- 1:ncol(x)
  for(i in 1:ncol(x)){
    if(ncol(x) - i >= 3) {
      ## Demographic/Group names
      responseStyles[[i]] <- csRegular(outwb)
    }else if(ncol(x) - i >= 1){
      ## Employees/Respondents
      responseStyles[[i]] <- csStat(outwb)
    }else{
      ## RR
      responseStyles[[i]] <- csPerc(outwb)
    }
  }

  # add data to sheet with column styles as assigned in responseStyles, and header row style csH1
  xlsx::addDataFrame(x, sheet, col.names = TRUE, row.names = FALSE, startRow=1, startColumn=1,
               colStyle=responseStyles, colnamesStyle = csH1(outwb), showNA = FALSE, characterNA = "")

  # set column widths: slimmer for last 3 columns (Employees, Respondents, RR)
  xlsx::setColumnWidth(sheet, colIndex=1:(ncol(x)-3),colWidth = 25.5)
  xlsx::setColumnWidth(sheet, colIndex=(ncol(x)-2):ncol(x),colWidth = 16.5)

  # triple the row height
  xlsx::setRowHeight(xlsx::getRows(sheet),multiplier=3)

  # reassign cellstyles for stat headings (last 3 columns)
  rowh <- xlsx::getRows(sheet,rowIndex = 1)
  cellh <- xlsx::getCells(rowh,colIndex = (ncol(x)-2):ncol(x))
  for(i in cellh) { xlsx::setCellStyle(i,cellStyle = csH2(outwb)) }

  # save workbook
  xlsx::saveWorkbook(outwb, outputFile)

}

create_driverscore_sheet <- function(x, outputFile, inputFile=outputFile, sheetName="Engagement Model"){

  # load or create workbook
  ## likely loading existing file, so try load first
  outwb <- tryCatch(xlsx::loadWorkbook(inputFile), error=function(err) {xlsx::createWorkbook(type="xlsx")})

  # remove existing sheet, sheetName, if applicable
  existing <- xlsx::getSheets(outwb)
  if(!is.null(existing)){
    if(sheetName %in% names(existing)) {
      removeSheet(outwb,sheetName=sheetName)
    }
  }

  # create sheet
  sheet <- xlsx::createSheet(outwb, sheetName=sheetName)

  # Add in filters from A1 to last column and row of output
  xlsx::addAutoFilter(sheet, cellRange = paste("A1:",LETTERS[ncol(x)],nrow(x)+1,sep=""))

  # Freeze top row
  xlsx::createFreezePane(sheet, rowSplit=2, colSplit = 1)

  # assign styles
  ## expects last three columns to be stat columns (e.g.,Average score, Compare to organization, Legend for model)
  ## create a blank list vector to store column styles, naming elements by column index
  driverStyles <- vector("list", ncol(x))
  names(driverStyles) <- 1:ncol(x)
  for(i in 1:ncol(x)){
    if(ncol(x)-i >= 3) {
      driverStyles[[i]] <- csRegular(outwb)
    }else{
      driverStyles[[i]] <- csStat(outwb)
    }
  }

  # add data to sheet with column styles as assigned in driverStyles, and header row style csH1
  xlsx::addDataFrame(x, sheet, col.names = TRUE, row.names = FALSE, startRow=1, startColumn=1,
               colStyle=driverStyles, colnamesStyle = csH1(outwb), showNA = FALSE, characterNA = "")

  # set column widths, last 3 columns vary
  xlsx::setColumnWidth(sheet, colIndex=1:ncol(x), colWidth = 25.5)
  xlsx::setColumnWidth(sheet, colIndex=(ncol(x)-2), colWidth = 10.5)
  xlsx::setColumnWidth(sheet, colIndex=(ncol(x)-1), colWidth = 16.5)
  #setColumnWidth(sheet, colIndex=ncol(x),colWidth = 25.5)

  # triple the row height
  xlsx::setRowHeight(xlsx::getRows(sheet),multiplier=3)

  # reassign cellstyles for stat headings (last 3 columns)
  rowh <- xlsx::getRows(sheet, rowIndex = 1)
  cellh <- xlsx::getCells(rowh, colIndex = (ncol(x)-2):ncol(x))
  for(i in cellh) { xlsx::setCellStyle(i, cellStyle = csH2(outwb)) }

  # reassign cellstyles for legend for model column
  ## find row indeces for each ranking
  rowsU <- which(x$`Legend for Model` == "Understand your challenges")
  rowsF <- which(x$`Legend for Model` == "Focus on improvements")
  rowsL <- which(x$`Legend for Model` == "Leverage your strengths")
  rowsC <- which(x$`Legend for Model` == "Celebrate your successes")
  rowsM <- which(x$`Legend for Model` == "Model your achievements")

  ## set cellstyles
  for(r in c("U","F","L","C","M")) {  # for each letter listed
    rows <- xlsx::getRows(sheet, rowIndex = get(paste0("rows", r)) + 1)
    cells <- xlsx::getCells(rows, colIndex = ncol(x))
    for(i in cells) { xlsx::setCellStyle(i, cellStyle = get(paste0("cs", r))(outwb)) }
  }

  # save workbook
  xlsx::saveWorkbook(outwb, outputFile)

}

create_questionscores_sheet <- function(x, outputFile, inputFile=outputFile, sheetName="All Survey Topics"){

  # load or create workbook
  ## likely loading existing file, so try load first
  outwb <- tryCatch(xlsx::loadWorkbook(inputFile), error=function(err) {xlsx::createWorkbook(type="xlsx")})

  # remove existing sheet, sheetName, if applicable
  existing <- xlsx::getSheets(outwb)
  if(!is.null(existing)){
    if(sheetName %in% names(existing)) {
      removeSheet(outwb,sheetName=sheetName)
    }
  }

  # create sheet
  sheet <- xlsx::createSheet(outwb, sheetName=sheetName)

  # Add in filters from A1 to last column and row of output
  xlsx::addAutoFilter(sheet, cellRange = paste("A1:",LETTERS[ncol(x)],nrow(x)+1,sep=""))

  # Freeze top row
  xlsx::createFreezePane(sheet, rowSplit=2, colSplit = 1)

  # assign styles
  ## expects last 5 columns to be stat/perc columns
  ## create a blank list vector to store column styles, naming elements by column index
  scoreStyles <- vector("list", ncol(x))
  names(scoreStyles) <- 1:ncol(x)
  for(i in 1:ncol(x)){
    if(ncol(x)-i >=5) {
      scoreStyles[[i]] <- csRegular(outwb)
    }else if(ncol(x)-i >=2){
      scoreStyles[[i]] <- csPerc(outwb)
    }else{
      scoreStyles[[i]] <- csStat(outwb)
    }
  }

  # add data to sheet with column styles as assigned in scoreStyles, and header row style csH1
  xlsx::addDataFrame(x, sheet, col.names = TRUE, row.names = FALSE, startRow=1, startColumn=1,
               colStyle=scoreStyles, colnamesStyle = csH1(outwb), showNA = FALSE, characterNA = "")

  # set column widths, last 6 columns vary
  xlsx::setColumnWidth(sheet, colIndex=1:ncol(x),colWidth = 25.5)
  xlsx::setColumnWidth(sheet, colIndex=(ncol(x)-5),colWidth = 35.5)
  xlsx::setColumnWidth(sheet, colIndex=(ncol(x)-4):(ncol(x)-1),colWidth = 10.5)

  # triple the row height
  xlsx::setRowHeight(xlsx::getRows(sheet),multiplier=3)

  # reassign cellstyles for stat headings (last 5 columns)
  rowh <- xlsx::getRows(sheet,rowIndex = 1)
  cellh <- xlsx::getCells(rowh,colIndex = (ncol(x)-4):ncol(x))
  for(i in cellh) { xlsx::setCellStyle(i,cellStyle = csH2(outwb)) }

  # save workbook
  xlsx::saveWorkbook(outwb, outputFile)
}
