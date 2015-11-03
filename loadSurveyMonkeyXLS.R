library(xlsx); library(dplyr); library(tidyr); library(ggplot2); library(magrittr)
#TODO check for presence of RespondentID, create if missing (needed for plot stats)
loadSurveyMonkeyXLS <- function(fname, idcols = 1:9) {
  header <- names(read.xlsx2(fname, sheetIndex = 1, endRow = 1, check.names = F)) 
  #blank headers indicate additional responses under the same header as the previous - fill those in
  for(i in seq_len(length(header)-1)) if(header[i+1] == " ") header[i+1] <- header[i]
  #load in with unaltered column names to capture second level headers
  dat <- read.xlsx2(fname, sheetIndex = 1, startRow = 2, check.names = F)

  #data frame to hold various properties learned about each question
  #starting with first and second level headers
  qProps <- data.frame(header = factor(header), 
                       header2 = names(dat), stringsAsFactors = F)
  
  names(dat)[idcols] <- as.character(qProps[idcols, "header"])
  dat <- data.frame(dat) #fix invalid/duplicate names for dplyr/tidyr methods to work
  
  #row names create a key for lookup of properties by column name/question
  row.names(qProps) <- names(dat)
  qProps$varNames <- names(dat) #variable to preserve names through dplyr ops
  
  makeNA <- function(x) {
    if("" %in% levels(x)) levels(x)[levels(x) == ""] <- NA
    x
  }
  dat %<>% mutate_each(funs(makeNA))
  print("NAs mutated")

  qProps$empty <- sapply(dat, function(x)all(is.na(x)))
  #Questions where every answer is unique and not a number are likely to be free text
  print("empties")
  qProps$uniqueAnswers <- !qProps$empty &
    sapply(dat, function(x)all(table(x) == 1))
  print("unique")
  suppressWarnings(qProps$numbers <- !qProps$empty &
      sapply(dat, function(x)all(!is.na(as.numeric(as.character(na.omit(x)))))))
  print("numbers")
  #Questions where there is only one type of answer and it matches the 
  #subheading are multiple response questions (checkboxes). For multiple
  #response matrix questions, the response is part of the subheading (which also
  #contains the question)
  qProps$multiBlockItemTypes <- sapply(names(dat), function(x){
    items <- levels(dat[[x]])
    if(length(items) == 1 && items == qProps[x, "header2"]) return(1)
    if(length(items) == 1 && grepl(items, qProps[x, "header2"])) return(2)
    if(length(na.omit(dat[[x]])) == 1) return(-1)
    0
  })
  qProps %<>% mutate(multiBlockItems = multiBlockItemTypes > 0,
                     multiMatrixItems = multiBlockItemTypes == 2,
                     lonely  = multiBlockItemTypes == -1)


  print("multis")
  # Free text answers selected as those where every anser is unique, not a
  # number, and not part of a multiple response block (which would match if only
  # one non-missing answer was present)
  qProps %<>% mutate(trueOthers = header2 == "Open-Ended Response" | 
                       grepl("please specify", header2, ignore.case = T),
                     likelyOthers = (uniqueAnswers & !(multiBlockItems | numbers)),
                     others = trueOthers | likelyOthers)
  
  # ID single item responses to ignore extra header level when naming question
  qProps %>% filter(!trueOthers) %>% magrittr::extract2("header") %>% table %>%
    magrittr::extract(. == 1) %>% names ->
    singles
  qProps %<>% mutate(singletons = header %in% singles)
  
  colNameGroups <- split(qProps$varNames, qProps$header)
  row.names(qProps) <- qProps$varNames

  blockType <- sapply(colNameGroups, function(colNames) {
    if(length(colNames) < 2) return("")
    if(any(qProps[colNames, "multiBlockItems"])) return("multiBlock")
    if(any(qProps[colNames, "multiMatrixItems"])) return("multiMatrix")
    if(all(qProps[colNames, "numbers"])) return("numericBlock")
    if(sum(qProps[colNames, "lonely"]) > 1) return("lonelyBlock")
    "block"
  })
  qProps %<>% mutate(blockType = blockType[header])
  qProps %<>% mutate(#multiBlock = blockType[header] == "multiBlock",
                     #multiMatrix = blockType[header] == "multiMatrix",
                     #numericBlock = blockType[header] == "numericBlock",
                     #block = blockType[header] == "block",
                     blockExtra = ((blockType == "multiBlock" & !multiBlockItems) | 
                       (blockType == "multiMatrix" &  !multiMatrixItems)) & !empty)
  # item labels for single response (radio button) matrices
  qProps %<>% mutate(subgroup = ifelse(blockType == "multiBlock" | singletons, NA, header2))

  qProps %<>% mutate(type  = ifelse(singletons, "Single Question", "Response Block")) %>% 
    #mutate(type = ifelse(blockType == "lonelyBlock", , type)) %>%
    #mutate(type = ifelse(empty, "Empty", type)) %>%
    mutate(type = ifelse(blockType == "multiBlock", "Multiple Response Question", type)) %>%
    mutate(type = ifelse(blockType == "multiMatrix", "Multiple Response Block", type)) %>%
    mutate(type = ifelse(others | blockExtra, "Free Text", type)) %>%
    mutate(type = ifelse(numbers, "Numeric Entry", type)) %>%
    mutate(type = ifelse(blockType == "numericBlock", "Numeric Block", type)) 
    #mutate(type = ifelse(block & lonely, "Response Block", type))
  
    
  print("props determined")
  #names(dat)[idcols] <- qProps[idcols, "header"]
  gathercols <- names(dat)[-idcols]
  dat <- gather_(dat, "question", "response", gathercols, na.rm=TRUE)
  print("data gathered")

  row.names(qProps) <- qProps$varNames
  dat$question <- as.character(dat$question)
  dat$subgroup <- qProps[dat$question, "subgroup"]
  dat$type <- as.factor(qProps[dat$question, "type"])
  #tweak MR matrix questions because the true response of value is in the 2nd level header
  #TODO make option to swap first and second regmatches for group/value
  multimatrices <- qProps$type == "Multiple Response Block" #qProps$multiMatrix & !qProps$others
  if(any(multimatrices)) {
    key <- regexec("(.+) - (.+)", qProps[multimatrices, "header2"]) %>% 
      regmatches(x = qProps[multimatrices, "header2"]) %>% 
      do.call(what = rbind)
    if(!is.null(key)) {
      row.names(key) <- qProps[multimatrices , "varNames"]
      multimatrixrows <- which(dat$question %in% qProps[multimatrices, "varNames"])
      dat$response[multimatrixrows] <- key[dat$question[multimatrixrows], 2]
      dat$subgroup[multimatrixrows] <- key[dat$question[multimatrixrows], 3]
    }
  }
  dat$subgroup <- as.factor(dat$subgroup)

  #dat$question[!multimatrixrows] <- qProps[dat$question[!multimatrixrows], "header"]
  dat$question <- qProps[dat$question, "header"]
  
  #dat$question <- as.factor(dat$question)
  
  dat
}


removeHTML <- function(x) {
  gsub("<.*?>", " ", x)
}

exportFreeText <- function(data) {
  data %<>% filter(type == "Free Text", !is.na(response), !(response %in% c("NA", "N/A", "n/a")))
  lapply(split(data, data$question), function(x) {
    d = data.frame(x$response)
    names(d)[1] <- as.character(x$question[1])
    write.csv(d, paste0(substring(gsub("[^[:alnum:]]","",x$question[1]), 1, 150),
                     ".csv", c = ""))
  }) %>% invisible
}