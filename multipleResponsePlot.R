require(ggplot2)
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
  comb <- dat
  
  sapply(levels(comb$question), function(q) {
    
    answers <- filter(comb, question == q)
    if(nrow(answers) == 0) return(NULL)
    #convert ordered scale factors
    scale <- which(sapply(scales, function(x)all(answers$response %in% x)))
    if(length(scale) == 1) answers$response <- ordered(answers$response, levels = scales[[scale]])
    #convert numeric responses
    n <- suppressWarnings(as.numeric(as.character(answers$response)))
    if(!anyNA(n)) answers$response <- n
    
    if(length(outputFolder)) {
      if(!dir.exists(outputFolder)) dir.create(outputFolder)
      fname <- paste0(outputFolder, "/", substring(gsub("[^[:alnum:]]","",q), 1, 150), ".png", collapse = "")
      i <- 1
      while(file.exists(fname)) {
        fname <- paste0(outputFolder, "/", substring(gsub("[^[:alnum:]]","",q), 1, 150), i, ".png", collapse = "")
        i <- i + 1
      }
      png(fname, height = 600, width = 800)
    }
    if(nchar(q) > 118) {
      spaces <- gregexpr(" ", q)[[1]]
      pos <- spaces[which.min(abs(spaces - nchar(q)/2))]
      substr(q, pos, pos) <- "\n"
    }
    plt <- ggplot(data = answers, aes(x = response)) + 
      geom_bar(position="dodge") +
      #scale_fill_discrete(labels = c(pre = "Before", post = "After")) +
      labs(title= q, x = "Response", y = "Count") + 
      #guides(fill = guide_legend(title = "Time")) + 
      theme_bw() 
    if(nchar(q) > 250) plt <- plt + theme(plot.title = element_text(size = 8)) else
      plt <- plt + theme(plot.title = element_text(size = 10))
    if(length(scale) == 1) plt <- plt + scale_x_discrete(limits = scales[[scale]])
    if(sum(nchar(unique(as.character(answers$response)))) > 50) 
      plt <- plt + theme(axis.text.x = element_text(angle = 90, vjust = .5))
    print(plt)
    #browser()
    if(length(outputFolder)) dev.off()
    #print(plt)
    #browser()
  })
  NULL
}