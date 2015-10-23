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

explorePlots <- function(dat, scales, outputFolder = character(0)) {
  #todo, check structure (maybe S3 class?)
  dat <- dat %>% mutate(sampSize = length(unique(RespondentID))) %>%
    filter(type != "Free Text") 
    

  invisible(lapply(split(dat, list(dat$question, dat$type), drop = T), 
                   function(answers) {
    scale <- which(sapply(scales, function(x)all(answers$response %in% x)))
    if(length(scale) == 1) answers$response <- 
        ordered(answers$response, levels = scales[[scale]]) else if(length(scale) > 1)
          warning("Multiple scales matched to responses for '", 
                  dat$question[1], 
                  "'. Plotting as discrete/nominal responses. If ordinal is desired, ensure all items in exactly one scale match to responses for this question.")
    n <- suppressWarnings(as.numeric(as.character(answers$response)))
    if(!anyNA(n)) answers$response <- n
    plotQuestion(answers, as.character(answers$question[1]), outputFolder)
  }))
  
}
#TODO: extract the file creation to seperate method (maybe S3 print method)
plotQuestion <- function(answers, title, outputFolder = character(0), 
                         pop.estimates = T) {
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

  xLabsLength <- sum(nchar(unique(as.character(answers$response))))
  ylab <- "Count"
  xlab <- "Response" 
  `Population Estimates` <- "Mean and\n95% Confidence Interval"
  answers %>% group_by(subgroup, response, sampSize) %>%
    summarise(count = n()) %>% 
    mutate(prop = count/sampSize) -> 
    answerProportions 
  if(pop.estimates) answerProportions <- 
    mutate(answerProportions, 
           upr = binom.test(count, sampSize)$conf.int[2],
           lwr = binom.test(count, sampSize)$conf.int[1])
    switch(
    t,
    `Response Block` = {
      ylab <- "Response"
      xlab <- "Item" 
      ratio <- max(table(answers$response, answers$subgroup)) *
               length(levels(factor(answers$subgroup)))
      ratio <- pmin(1, 1 - (ratio - 15)/(ratio + 30))
      plt <- ggplot(answers, aes(x = subgroup, y = as.numeric(response))) +
        geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 1, 
                     dotsize = .1, stackratio = ratio) +
        scale_y_discrete(limits = levels(answers$response))
      if(pop.estimates && is.ordered(answers$response)) plt <- plt +
        stat_summary(aes(color = `Population Estimates`), 
                     fun.data = mean_cl_normal, 
                     size = 1.5)
      xLabsLength <- sum(nchar(unique(as.character(answers$subgroup))))},
    `Multiple Response Block` = {
#       answers %>% group_by(subgroup, response, sampSize) %>%
#         summarise(count = n()) %>% 
#         mutate(prop = count/sampSize, upr = binom.test(count, sampSize)$conf.int[2],
#                lwr = binom.test(count, sampSize)$conf.int[1]) -> 
#         d
      plt <- ggplot(answerProportions, aes(x = response, fill = subgroup, y = prop,
                           ymax = upr, ymin = lwr)) +
        geom_bar(stat = "identity", position = "dodge")
      if(pop.estimates) plt <- plt + 
        geom_errorbar(aes(color= `Population Estimates`),
                      position = "dodge", size = 1.5)},
    {if(is.ordered(answers$response) || is.numeric(answers$response)) plt <-
        ggplot(answers, aes(x = response)) + geom_bar()
      if(pop.estimates) plt <- plt +
          geom_errorbarh(
            aes(y = -.05, x = mean(as.numeric(response)), 
                xmin = t.test(as.numeric(response))$conf.int[1],
                xmax = t.test(as.numeric(response))$conf.int[2],
                color = "Mean and 95% CI"),
            height = 0, size = 1.5) +
          geom_point(aes(y = -.05, x = mean(as.numeric(response)), 
                         color = "Mean and 95% CI"), size = 4) else plt <-
        ggplot(answerProportions, aes())
          
    if(is.ordered(answers$response)) plt <- plt + 
        scale_x_discrete(limits = levels(answers$response))

      })
  s <- unique(answers$subgroup)
  if(length(s) == 1 && !is.na(s[1])) 
    title <- paste0(title, s[1], collapse = " ")
  if(nchar(title) > 118) {
    spaces <- gregexpr(" ", title)[[1]]
    pos <- spaces[which.min(abs(spaces - nchar(title)/2))]
    substr(title, pos, pos) <- "\n"
  }
  plt <- plt + 
    labs(title= title, x = xlab, y = ylab) + 
    theme_classic()
  if(nchar(title) > 250) plt <- plt + theme(plot.title = element_text(size = 8)) else
    plt <- plt + theme(plot.title = element_text(size = 10))
  if(xLabsLength > 50) 
    plt <- plt + theme(axis.text.x = element_text(angle = 6, vjust = .75, hjust = .5))
  #print(plt)
  #if(length(outputFolder)) dev.off()
  plt
}

breakStrings <- function(x) {
  spaces <- gregexpr(" ", x)
  pos <- mapply(function(y, mid){y[which.min(abs(y - mid))]},
                y = spaces, mid = nchar(x)/2)
  substr(x, pos, pos) <- "\n"
  x
}
