library(xlsx); library(dplyr); library(tidyr); library(ggplot2); library(magrittr)

loadSurveyMonkeyXLS <- function(fname, idcols) {
  header <- names(read.xlsx2(fname, sheetIndex = 1, endRow = 1)) 
  #blank headers indicate additional responses under the same header as the previous - fill those in
  for(i in seq_along(header)) if(grepl("^X\\.", header[i+1])) header[i+1] <- header[i]
  dat <- read.xlsx2(fname, sheetIndex = 1, startRow = 2)
  names(header) <- names(dat)
  
  makeNA <- function(x) {
    if(class(x) == "factor") levels(x)[levels(x) == ""] <- NA
    x
  }
  dat %<>% mutate_each(funs(makeNA))
  
  #separate off free text responses
  others <- c(grep("specify", names(dat), ignore.case = T),
              grep("open.ended", names(dat), ignore.case = T))
  freeText <- dat[ , union(idcols,others)]
  names(freeText) <- header[names(freeText)]
  dat <- dat[, -others]
  header <- header[-others]
  
#   #fix one multi-response block that is weird
#   weird <- names(dat)[header == "Please.choose.the.patient.population.that.best.describes.your.area.of.practice....Choose.all.that.apply."]
#   for(i in weird) {
#     
#     dat[ , i] <- ifelse(is.na(dat[ , i]), NA, gsub("\\.+", " ",i))
#   }
  
  #ID multiple response item blocks for later grouping
  multiblocks <- sapply(names(dat), function(x){
    items <- gsub("[^[:alnum:]]", "", unique(na.exclude(dat[ , x])))
    if(length(items) == 1 && grepl(items, gsub("[^[:alpha:]]", "", x))) return(TRUE)
    FALSE
  })
  #ID single item responses to ignore extra header level
  singletons <- header %in% names(table(header))[table(header) == 1]
  
  gathercols <- setdiff(names(dat), names(dat)[idcols]) 
  names(dat)[idcols] <- header[idcols]
  
  dat %<>% gather_("question", "response", gathercols, na.rm=TRUE)
  #rename questions using their headers
  header <- ifelse(multiblocks | singletons, header, paste(header, names(header), sep = ": "))
  header <- gsub("\\.+", " ", header)
  header <- sub(" :", ":", header)
  header <- sub(" $", "", header)
  dat$question <- as.factor(header[as.character(dat$question)])
  
  list(data=dat, freeText=freeText)
}


