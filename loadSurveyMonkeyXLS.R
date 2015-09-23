library(xlsx); library(dplyr); library(tidyr); library(ggplot2); library(magrittr)

loadSurveyMonkeyXLS <- function(fname, idcols = 1:9) {
  header <- names(read.xlsx2(fname, sheetIndex = 1, endRow = 1, check.names = F)) 
  #blank headers indicate additional responses under the same header as the previous - fill those in
  for(i in seq_len(length(header)-1)) if(header[i+1] == " ") header[i+1] <- header[i]
  dat <- read.xlsx2(fname, sheetIndex = 1, startRow = 2, check.names = F)
    #header <- gsub("<.*?>", "", header)
  header2 <- names(dat)
  dat <- data.frame(dat) #fix invalid/duplicate names
  #create key for lookup of headers by column name
  names(header) <- names(header2) <- names(dat)
  
  #separate off free text responses
  others <- grep("specify|open.ended|suggestions", names(dat), ignore.case = T)
  freeText <- dat[ , union(idcols,others)]
  names(freeText) <- header[names(freeText)]
  dat <- dat[, -others]
  header <- header[-others]
  header2 <- header2[-others]
  

  #ID multiple response item blocks for later grouping
  multiblocktypes <- sapply(names(dat), function(x){
    items <- levels(dat[[x]])
    items <- setdiff(items, "")
    if(length(items) == 1 && items == header2[x]) return(1)
    if(length(items) == 1 && grepl(items, header2[x])) return(2)
    0
  })
  multiblocks <- multiblocktypes > 0
  multimatrices <- multiblocktypes == 2
  
  makeNA <- function(x) {
    if(class(x) == "factor") levels(x)[levels(x) == ""] <- NA
    x
  }
  dat %<>% mutate_each(funs(makeNA))
  
  #ID single item responses to ignore extra header level
  singletons <- header %in% names(table(header))[table(header) == 1]

  names(dat)[idcols] <- header[idcols]
  #create key for updating the question levels later
  header <- ifelse(multiblocks | singletons, header, paste(header, header2, sep = ": "))
  
  gathercols <- setdiff(names(dat), names(dat)[idcols]) 
  dat %<>% gather_("question", "response", gathercols, na.rm=TRUE)
  #rename questions using their headers
  #header <- gsub("\\.+", " ", header)
  #header <- sub(" :", ":", header)
  #header <- sub(" $", "", header)
  dat$question <- as.character(dat$question)
  #tweak MR matrix questions because the response of value is part of the 2nd level header
  key <- regexec("(.+) - (.+)", header2[multimatrices]) %>% 
    regmatches(x = header2[multimatrices]) %>% do.call(what = rbind)
  multimatrixrows <- which(dat$question %in% names(header2)[multimatrices])
  dat$response[multimatrixrows] <- key[dat$question[multimatrixrows], 2]
  dat$question[multimatrixrows] <- key[dat$question[multimatrixrows], 3]  
    
  dat$question <- ifelse(is.na(header[dat$question]), dat$question, header[dat$question])
  dat$question <- as.factor(dat$question)
  
  list(data=dat, freeText=freeText)
}


