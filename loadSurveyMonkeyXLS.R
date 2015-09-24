library(xlsx); library(dplyr); library(tidyr); library(ggplot2); library(magrittr)

loadSurveyMonkeyXLS <- function(fname, idcols = 1:9) {
  header <- names(read.xlsx2(fname, sheetIndex = 1, endRow = 1, check.names = F)) 
  #blank headers indicate additional responses under the same header as the previous - fill those in
  for(i in seq_len(length(header)-1)) if(header[i+1] == " ") header[i+1] <- header[i]
  #load in with unaltered column names to capture second level headers
  dat <- read.xlsx2(fname, sheetIndex = 1, startRow = 2, check.names = F)
  print("files loaded")
  #data frame to hold various properties learned about each question
  #starting with first and second level headers
  qProps <- data.frame(header = header, header2 = names(dat), stringsAsFactors = F)
  
  names(dat)[idcols] <- qProps[idcols, "header"]
  dat <- data.frame(dat) #fix invalid/duplicate names for dplyr/tidyr methods to work
  
  #row names create a key for lookup of properties by column name/question
  row.names(qProps) <- names(dat)
  
  makeNA <- function(x) {
    if(class(x) == "factor") levels(x)[levels(x) == ""] <- NA
    x
  }
  dat %<>% mutate_each(funs(makeNA))
  print("NAs mutated")

  #Questions where every answer is unique and not a number are likely to be free text
  qProps$uniqueAnswers <- sapply(dat, function(x)all(table(x) == 1))
  print("unique")
  #suppressWarnings(qProps$numbers <- sapply(dat, function(x)any(!is.na(as.numeric(x)))))
  qProps$numbers <- F
  print("numbers")
  #Questions where there is only one type of answer and it matches the 
  #subheading are multiple response questions (checkboxes). For multiple
  #response matrix questions, the response is part of the subheading (which also
  #contains the question)
  qProps$multiblocktypes <- sapply(names(dat), function(x){
    items <- levels(dat[[x]])
    #items <- setdiff(items, "")
    if(length(items) == 1 && items == qProps[x, "header2"]) return(1)
    if(length(items) == 1 && grepl(items, qProps[x, "header2"])) return(2)
    0
  })
  qProps %<>% mutate(multiblocks = multiblocktypes > 0)
  qProps %<>% mutate(multimatrices = multiblocktypes == 2)
  print("multis")
  # Free text answers selected as those where every anser is unique, not a
  # number, and not part of a multiple response block (which would match if only
  # one non-missing answer was present)
  qProps %<>% mutate(others = uniqueAnswers & !(multiblocks | numbers))

  # ID single item responses to ignore extra header level when naming question
  qProps %<>% mutate(singletons = header %in% names(table(header))[table(header) == 1])
  # item labels for single response (radio button) matrices
  qProps %<>% mutate(subgroup = ifelse(multiblocks | singletons, NA, header2))
  
  qProps %<>% mutate(type  = ifelse(numbers, "Numeric Entry", "Response Block")) %>% 
    mutate(type = ifelse(singletons, "Single Question", type)) %>%
    mutate(type = ifelse(multiblocks, "Multiple Response Question", type)) %>%
    mutate(type = ifelse(multimatrices, "Multiple Response Block", type)) %>%
    mutate(type = ifelse(others, "Free Text", type))
  print("props determined")
  #names(dat)[idcols] <- qProps[idcols, "header"]
  gathercols <- setdiff(names(dat), c(names(dat)[idcols], row.names(qProps[qProps$others, ]))) 
  dat <- gather_(dat, "question", "response", gathercols, na.rm=TRUE)
  print("data gathered")
  dat$question <- as.character(dat$question)

  dat$subgroup <- as.factor(qProps[dat$question, "subgroup"])
  dat$type <- as.factor(qProps[dat$question, "type"])
  #tweak MR matrix questions because the true response of value is in the 2nd level header
  #TODO make this optional
  key <- regexec("(.+) - (.+)", qProps[qProps$multimatrices, "header2"]) %>% 
    regmatches(x = qProps[qProps$multimatrices, "header2"]) %>% do.call(what = rbind)
  multimatrixrows <- which(dat$question %in% row.names(qProps)[qProps$multimatrices])
  dat$response[multimatrixrows] <- key[dat$question[multimatrixrows], 2]
  dat$subgroup[multimatrixrows] <- key[dat$question[multimatrixrows], 3]

  #dat$question[!multimatrixrows] <- qProps[dat$question[!multimatrixrows], "header"]
  dat$question <- qProps[dat$question, "header"]
  
  dat$question <- as.factor(dat$question)
  
  dat
}


