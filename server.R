server <- function(input, output){
  output$map <- renderPlotly({
    commentcounts <- group_by(commentcounts,movie_title) %>% summarize(count = number_counts)
    ggplotly(commentcounts %>% 
               arrange(desc(count)) %>%
               slice(1:10) %>%
               ggplot(., aes(x= reorder(movie_title, -count), y=count)) +
               geom_bar(stat='identity', fill="#1CCCC6") + 
               ylab("Comment Count") + xlab ("Movie title") + 
               ggtitle("Comment count of the Oscar nominated movies") + 
               theme_minimal() + 
               scale_x_discrete(labels = function(labels) {
                 sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
               }))
  })
  
  # show histogram using googleVis
  output$hist <- renderPlotly({
    ggplotly(ggplot(commentlength, aes(x=commentlength)) + 
               geom_histogram(aes(fill = ..count..))+ 
               ylab("Frequency") + xlab ("Number of words") + 
               ggtitle("Histogram of the lengths of each reviews") + 
               theme_minimal())
  })
  
  #top 10 words in comments
  output$words <- renderPlotly({
    ggplotly(tidy_total  %>%
               filter(.,movie==input$selected) %>%
               count(word, sort = TRUE) %>% 
               arrange(desc(n)) %>%
               slice(c(2:7,9:12)) %>% #remove movie, film 
               ggplot(., aes(x=reorder(word,-n), y=n))  +  geom_bar(stat='identity', fill="#1CCCC6") + 
               ylab("Frequency") + xlab ("Top 10 terms") + 
               ggtitle(paste("Top 10 words in",input$selected)) + 
               theme_minimal())
  })
  
  output$sentiment <- renderPlotly({
    ggplotly(tidy_total  %>% 
               filter(.,movie==input$selected) %>%
               inner_join(nrc) %>%
               count(sentiment, sort = TRUE) %>% 
               ggplot(., aes(x= sentiment, y=n))+ geom_bar(stat="identity",aes(fill = sentiment))+
               ylab("Sentiments") + xlab ("Sentiment Scores") + 
               ggtitle(paste("Sentiment Scores of " ,input$selected)) + 
               theme_minimal())
  })
  
  
  output$intime <- renderPlotly({
    tidy_total_bing <- tidy_total %>% 
      filter(.,movie==input$selected) %>%
      inner_join(bing) %>% group_by(date,sentiment) %>% summarize(number_comment = n())%>% 
      spread(., key=sentiment, value = number_comment)
    tidy_total_bing[is.na(tidy_total_bing)]<-0
    
    ggplotly(tidy_total_bing%>% 
               arrange(desc(date)) %>% 
               ggplot(.) + geom_line(aes(x=date, y=negative), color = "red")+
               geom_line(aes(x=date, y=positive), color = "blue") +
               ylab("Comment count") + xlab ("Date") + 
               ggtitle(paste(input$selected,"Comment count change over the date")) + 
               theme_minimal())
  })
  
  output$cloudtitle = renderText({
    input$selected
  })
  output$wordcloud <- renderPlot({
    tidy_total  %>% 
      filter(.,movie==input$selected) %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 8) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 250)
  })
  
  output$bigramtitle = renderText({
    input$selected
  })
  
  output$bigram <- renderPlot({
    tidy_total %>%
      filter(.,movie==input$selected) %>%
      count_bigrams() %>% 
      arrange(desc(n)) %>% 
      top_n(50) %>% 
      filter(!str_detect(word1, "\\d"),
             !str_detect(word2, "\\d")) %>%
      visualize_bigrams()
  })
  
  # show statistics using infoBox
  output$comments1 <- renderInfoBox({
    comment_count <- commentcounts[movie_title==input$selected,2]
    infoBox("Comments", comment_count, icon = icon("han"))
  })
  
  output$likes1 <- renderInfoBox({
    tidy_total_like = tidy_total %>% filter(.,movie==input$selected) 
    like_sum = sum(tidy_total_like[,"likes"])
    infoBox("Likes", like_sum, icon = icon("hand-o-down"))
  })
  
  output$replies1 <- renderInfoBox({
    tidy_total_replies = tidy_total %>% filter(.,movie==input$selected , has_replies =="TRUE")
    tidy_true = nrow(tidy_total_replies)
    tidy_percentile = round(tidy_true/(nrow(tidy_total %>% filter(.,movie==input$selected))),3)
    infoBox("Reply Ratio",
            paste(tidy_percentile, "%"), 
            icon = icon("calculator"), fill = TRUE)
  })
  
  output$comments2 <- renderInfoBox({
    comment_count <- commentcounts[movie_title==input$selected,2]
    infoBox("Comments", comment_count, icon = icon("han"))
  })
  
  output$likes2 <- renderInfoBox({
    tidy_total_like = tidy_total %>% filter(.,movie==input$selected) 
    like_sum = sum(tidy_total_like[,"likes"])
    infoBox("Likes", like_sum, icon = icon("hand-o-down"))
  })
  
  output$replies2 <- renderInfoBox({
    tidy_total_replies = tidy_total %>% filter(.,movie==input$selected , has_replies =="TRUE")
    tidy_true = nrow(tidy_total_replies)
    tidy_percentile = round(tidy_true/(nrow(tidy_total %>% filter(.,movie==input$selected))),3)
    infoBox("Reply Ratio",
            paste(tidy_percentile, "%"), 
            icon = icon("calculator"), fill = TRUE)
  })
  
  output$comments3 <- renderInfoBox({
    comment_count <- commentcounts[movie_title==input$selected,2]
    infoBox("Comments", comment_count, icon = icon("han"))
  })
  
  output$likes3 <- renderInfoBox({
    tidy_total_like = tidy_total %>% filter(.,movie==input$selected) 
    like_sum = sum(tidy_total_like[,"likes"])
    infoBox("Likes", like_sum, icon = icon("hand-o-down"))
  })
  
  output$replies3 <- renderInfoBox({
    tidy_total_replies = tidy_total %>% filter(.,movie==input$selected , has_replies =="TRUE")
    tidy_true = nrow(tidy_total_replies)
    tidy_percentile = round(tidy_true/(nrow(tidy_total %>% filter(.,movie==input$selected))),3)
    infoBox("Reply Ratio",
            paste(tidy_percentile, "%"), 
            icon = icon("calculator"), fill = TRUE)
  })
  
  output$comments4 <- renderInfoBox({
    comment_count <- commentcounts[movie_title==input$selected,2]
    infoBox("Comments", comment_count, icon = icon("han"))
  })
  
  output$likes4 <- renderInfoBox({
    tidy_total_like = tidy_total %>% filter(.,movie==input$selected) 
    like_sum = sum(tidy_total_like[,"likes"])
    infoBox("Likes", like_sum, icon = icon("hand-o-down"))
  })
  
  output$replies4 <- renderInfoBox({
    tidy_total_replies = tidy_total %>% filter(.,movie==input$selected , has_replies =="TRUE")
    tidy_true = nrow(tidy_total_replies)
    tidy_percentile = round(tidy_true/(nrow(tidy_total %>% filter(.,movie==input$selected))),3)
    infoBox("Reply Ratio",
            paste(tidy_percentile, "%"), 
            icon = icon("calculator"), fill = TRUE)
  })
  
  output$comments5 <- renderInfoBox({
    comment_count <- commentcounts[movie_title==input$selected,2]
    infoBox("Comments", comment_count, icon = icon("han"))
  })
  
  output$likes5 <- renderInfoBox({
    tidy_total_like = tidy_total %>% filter(.,movie==input$selected) 
    like_sum = sum(tidy_total_like[,"likes"])
    infoBox("Likes", like_sum, icon = icon("hand-o-down"))
  })
  
  output$replies5 <- renderInfoBox({
    tidy_total_replies = tidy_total %>% filter(.,movie==input$selected , has_replies =="TRUE")
    tidy_true = nrow(tidy_total_replies)
    tidy_percentile = round(tidy_true/(nrow(tidy_total %>% filter(.,movie==input$selected))),3)
    infoBox("Reply Ratio",
            paste(tidy_percentile, "%"), 
            icon = icon("calculator"), fill = TRUE)
  })
  
} 