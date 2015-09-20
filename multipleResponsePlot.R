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