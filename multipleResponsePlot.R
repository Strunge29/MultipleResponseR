require(ggplot2); require(dplyr); require(Hmisc)
multipleResponsePlot <- function(responses, categories) {
  
  #d <- d[!is.na(d[[demographic]]) & d[[demographic]] != "other", ]
  #ct <- chisq.test(d$answer, d[[demographic]])
  ct <- chisq.test(responses, categories)
  obs <- as.data.frame(ct$observed)
  expect <- as.data.frame(as.table(ct$expected))
  chi_table_names <- c("answer", "demographic", "freq")
  names(obs) <- chi_table_names
  names(expect) <- chi_table_names
  
  ggplot(obs, aes(x = answer, y = freq, fill = demographic)) +
    geom_bar(stat="identity", position="stack") + 
    geom_bar(data = expect, 
             mapping = aes(fill = NULL, color = demographic),
             stat = "identity", position="stack", alpha = 0) +
    scale_color_hue(l=45, c = 125, guide=guide_legend(title="Expected Distribution", order=2)) + 
    theme(axis.text.x = element_text(angle=90)) + 
    labs(y = "Number of responses", x = "")  + 
    guides(fill = guide_legend(title = "Survey Responses", order = 1))
  
}

explorePlots <- function(..., scales) {
  #TODO: check structure (maybe S3 class?)
  dat <- combineSurveyData(...)
  dat <- dat %>% filter(type != "Free Text") 
  lapply(split(dat, list(dat$questionId, dat$type), drop = T), 
                   function(answers) {
    scale <- which(sapply(scales, function(x)all(answers$response %in% x)))
    if(length(scale) == 1) answers$response <- 
        ordered(answers$response, levels = scales[[scale]]) else 
          if(length(scale) > 1)
            warning("Multiple scales matched to responses for '", 
                    dat$question[1], 
                    "'. Plotting as discrete/nominal responses. If ordinal is desired, ensure all items in exactly one scale match to responses for this question.")
#     n <- suppressWarnings(as.numeric(as.character(answers$response)))
#     if(!anyNA(n)) { 
#       answers$response <- n
#       answers$type <- "Numeric Entry"
#     }
    if(answers$type[1] == "Numeric Entry") answers$response <- as.numeric(ansers$response)
    plotQuestion(answers, 
                 splitBy = ifelse(length(unique(dat$Survey)) > 1, "Survey", NA))
  }) %>% invisible
}
#TODO: extract the file creation to seperate method (maybe S3 print method)
plotQuestion <- function(answers, splitBy = NA, pop.estimates = T) {
  t <- as.character(unique(answers$type))
  if (length(t) > 1) stop("Unable to plot question block with multiple response types")
#   if(length(outputFolder)) {
#     if(!dir.exists(outputFolder)) dir.create(outputFolder)
#     fname <- paste0(outputFolder, "/", substring(gsub("[^[:alnum:]]","",title), 1, 150), ".png", collapse = "")
#     i <- 1
#     while(file.exists(fname)) {
#       fname <- paste0(outputFolder, "/", substring(gsub("[^[:alnum:]]","",title), 1, 150), i, ".png", collapse = "")
#       i <- i + 1
#     }
#     png(fname, height = 600, width = 800)
#   }

  #xLabsLength <- sum(nchar(unique(as.character(answers$response))))
  switch(t,
         `Response Block` =
         {responseBlockPlot(answers, splitBy, pop.estimates)},
         `Multiple Response Block` = 
         {multipleResponseBlockPlot(answers, splitBy, pop.estimates)},
         `Single Question` = 
         {singleQuestionPlot(answers, splitBy, pop.estimates)},
         `Numeric Entry` = 
         {numericEntryPlot(answers, splitBy, pop.estimates)},
         `Multiple Response Question` = 
         {multipleResponseQuestionPlot(answers, splitBy, pop.estimates)})
}

combineSurveyData <- function(...) {
  dataList <- list(...)
  if(length(dataList) > 1){
    dataList <- lapply(dataList, ensureSampleSizeAvailable)
    #Create keys of question names (with duplicates resolved) to question ids
    surveyQs <- lapply(dataList, function(x)unique(x[c("question", "questionId")])) %>%
      lapply(mutate_each, funs = "as.character") %>%
      lapply(function(qs) {
        while(anyDuplicated(qs$question)) {
          qs$question[duplicated(qs$question)] <- 
            paste0(qs$question[duplicated(qs$question)], "1")
        }
        qs
      })
    #create new set of unique question ids that combines matching
    #question names across surveys
    newQids  <- lapply(surveyQs, extract2, "question") %>% unlist %>% unique %>% 
      data.frame(question = ., questionId = paste0("Q",seq_along(.)))
    #map from input data question names to duplicate resolved question names
    #to new global unique ids
    dataList <- mapply(function(s, q){
      s$questionId %<>% match(q$questionId) %>% extract(q$question, .) %>% 
        match(newQids$question) %>% extract(newQids$questionId, .)
      s}, dataList, surveyQs, SIMPLIFY = F)
    
    #TODO: rather than suppress all warnings, preconvert factors
    suppressWarnings(answers <- bind_rows(dataList, .id = "Survey"))
    answers <- mutate(answers, question = factor(question), 
                      questionId = factor(questionId),
                      type = factor(type),
                      Survey = factor(Survey))
  }  else answers <- dataList[[1]]
  answers
}

convertResponsesToProportions <- function(answers,factor = NA) {
  vectorizeBinomInt <- function(counts, sizes, which) {
    mapply(function(c,s,w) binom.test(c, s)$conf.int[w],
           c = counts, s = sizes, MoreArgs = list(w = which))
  }
  answers <- ensureSampleSizeAvailable(answers)
  grps <- list("sampSize", "subgroup", "response")
  if(!is.na(factor)) grps <- c(as.character(factor), grps)
  answers %>% group_by_(.dots = grps) %>%
    summarise(count = n()) %>% 
     mutate(prop = count/sampSize,
            upr = vectorizeBinomInt(count, sampSize, 2),
            lwr = vectorizeBinomInt(count, sampSize, 1))
}

ensureSampleSizeAvailable <- function(answers) {
  if(is.null(answers$sampSize)) answers <- 
      mutate(answers, sampSize = length(unique(RespondentID)))
  answers
}

responseBlockPlot <- function(answers, splitBy = NA,
                              pop.estimates = T, dotRatioFactor = 15) {
  ratio <- ifelse(is.na(splitBy), 
                  max(table(answers$response, answers$subgroup)),
                  max(table(answers$response, answers$subgroup, answers[[splitBy]]))) *
    length(levels(factor(answers$subgroup)))
  ratio <- pmin(1, 1 - (ratio - dotRatioFactor)/(ratio + dotRatioFactor*2))
  #geom_dotplot won't do dodge by height, so offsets are added manually instead
  if(is.na(splitBy)) answers$offset <- 0 else
    answers$offset <- (as.numeric(factor(answers[[splitBy]])) - 
                         mean(seq_along(unique(answers[[splitBy]]))))/10 
  #TODO will this ggplot call work with nominal data? no
  plt <- ggplot(answers, aes(x = subgroup, y = as.numeric(response) + offset)) +
    geom_dotplot(binaxis = "y", stackdir = "center", binwidth = .1, 
                 dotsize = .9, stackratio = ratio, color = NA) +
    scale_y_discrete(limits = levels(answers$response)) + 
    labs(x = "Item", y = "Response")
  if(pop.estimates) {
    if(is.ordered(answers$response)) plt <- plt +
        stat_summary(aes(linetype = "Mean and\n95% Confidence Interval",
                         y = as.numeric(response)), 
                     fun.data = mean_cl_normal, size = 1,
                     position = position_dodge(width = .5),
                     shape = 21, color = "grey40")  +
        scale_linetype_manual(name = "Population Estimates", values = 1)
    #TODO: add population estimates for nominal data
  }
  if(!is.na(splitBy)) plt <- plt + aes_string(fill = splitBy)
  tweakPlotDisplay(answers, plt, xAxisTextField = "subgroup")
}

multipleResponseBlockPlot <- function(answers, pop.estimates = T) {
  plt <- ggplot(convertResponsesToProportions(answers), 
                aes(x = response, fill = subgroup, y = prop,
                                       ymax = upr, ymin = lwr)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Response", y = "Proportion")
  if(pop.estimates) plt <- plt + 
      geom_errorbar(aes(color= "95% Confidence Interval\nof the Proportion"),
                    position = "dodge") + 
      scale_color_manual(name = "Population Estimates", values = "grey50")
  tweakPlotDisplay(answers, plt, xAxisTextField = "response")
}

multipleResponseQuestionPlot <- function(answers, splitBy = NA, pop.estimates = T) {
#   dataList <- list(...)
#   multiData <- length(dataList) > 1
#   if(multiData){
#     dataList <- lapply(dataList, ensureSampleSizeAvailable)
#     #TODO: rather than suppress all warnings, preconvert factors
#     suppressWarnings(answers <- bind_rows(dataList, .id = "Survey"))
#   }  else answers <- dataList[[1]]
  plt <- ggplot(
    convertResponsesToProportions(answers, splitBy), 
    aes(x = response, y = prop,
        ymax = upr, ymin = lwr)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Response", y = "Proportion")
  if(pop.estimates) plt <- plt + 
      geom_errorbar(aes(color= "95% Confidence Interval\nof the Proportion"),
                    position = position_dodge(width = .885), width = .5) + 
      scale_color_manual(name = "Population Estimates", values = "grey50")
  if(!is.na(splitBy)) plt <- plt + aes_string(fill = splitBy)
  
  tweakPlotDisplay(answers, plt, xAxisTextField = "response")
}

numericEntryPlot <- function(answers, pop.estimates = T,
                             summaryFun = ifelse(nrow(answers) > 4, smean.cl.normal, smean.cl.boot)) {
  if(length(unique(answers$subgroup)) > 1) return(numericBlockPlot(answers, pop.estimates))
  plt <- ggplot(answers, aes(x = response)) + geom_bar(position = "dodge") +
    labs(x = "Response", y = "Count")
  if(pop.estimates) {
    est <- do.call(summaryFun, list(answers$response))
    plt <- plt +
      geom_errorbarh(
        aes_q(y = -.05, x = unname(est[1]), 
            xmin = unname(est[2]),
            xmax = unname(est[3]),
            color = "Mean and\n95% Confidence Interval"),
        height = 0, size = 3) +
      geom_point(aes(y = -.05, x = mean(response), 
                     color = "Mean and\n95% Confidence Interval"), size = 8) +
      scale_color_manual(name = "Population Estimates", values = "grey50")
  }
  tweakPlotDisplay(answers, plt, xAxisTextField = NA)
}

numericBlockPlot <- function(answers, pop.estimates = T, dotRatioFactor = 30, 
                             nBins = 30, summaryFun = mean_cl_boot) {
  ratio <- max(tabulate(cut(answers$response, nBins))) *
    length(levels(factor(answers$subgroup)))
  ratio <- pmin(1, 1 - (ratio - dotRatioFactor)/(ratio + dotRatioFactor*2))
  plt <- ggplot(answers, aes(x = subgroup, y = response)) +
    geom_dotplot(binaxis = "y", stackdir = "center", 
                 dotsize = 1, stackratio = ratio) +
    labs(x = "Item", y = "Response")
  if(pop.estimates) {
    plt <- plt +
        stat_summary(aes(color = "Mean and\n95% Confidence Interval"), 
                     fun.data = summaryFun, size = .75)  +
        scale_color_manual(name = "Population Estimates", values = "grey50")
  }
  tweakPlotDisplay(answers, plt, xAxisTextField = "subgroup")  
}

singleQuestionPlot <- function(answers, pop.estimates = T) {
  if(is.ordered(answers$response)) {
    plt <- mutate(answers, response = as.numeric(response)) %>%  
      numericEntryPlot(pop.estimates)
    plt <- plt + scale_x_discrete(limits = levels(answers$response))
    } else {
      plt <- ggplot(convertResponsesToProportions(answers), 
                    aes(x = response, y = prop,
                        ymax = upr, ymin = lwr)) +
        geom_bar(stat = "identity") + 
        labs(x = "Response", y = "Proportion")
      if(pop.estimates) plt <- plt + 
          geom_errorbar(aes(color= "95% Confidence Interval\nof the Proportion"),
                        width = .5) +
          scale_color_manual(name = "Population Estimates", values = "grey50")
      }               

  tweakPlotDisplay(answers, plt, xAxisTextField = "response")
}

tweakPlotDisplay <- function(answers, plt, xAxisTextField) {
  title <- as.character(answers$question[1])
  s <- unique(answers$subgroup)
  if(length(s) == 1 && !is.na(s[1])) 
    title <- paste0(title, s[1], collapse = " ")
  title <- breakStrings(title, 75)
  plt <- plt + ggtitle(title) + theme_classic() + theme(legend.position = "bottom")
  if(nchar(title) > 250) plt <- plt + theme(plot.title = element_text(size = 8)) else
    plt <- plt + theme(plot.title = element_text(size = 10))
  xTextLen <- sum(nchar(unique(as.character(answers[[xAxisTextField]]))))
  if(xTextLen > 150) {
    lvls <- answers[[xAxisTextField]]
    if(is.ordered(lvls)) lvls <- as.character(levels(lvls)) else
      lvls <- as.character(unique(lvls))
    plt <- plt + coord_flip() + 
      scale_x_discrete(limits = lvls, labels = breakStrings(lvls, 35))
  } else if(xTextLen > 75) {
    plt <- plt + theme(axis.text.x = element_text(angle = 6, vjust = .75, hjust = .5))
  }
  plt
}

breakStrings <- function(x, cutoff = 118) {
  spaces <- gregexpr(" ", x)
  pos <- mapply(function(y, mid){y[which.min(abs(y - mid))]},
                y = spaces, mid = nchar(x)/2)
  x2 <- x
  substr(x2, pos, pos) <- "\n"
  ifelse(nchar(x) > cutoff, x2, x)
}
