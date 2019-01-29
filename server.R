library(DT)
library(shiny)
library(googleVis)
library(tm)
library(wordcloud)

shinyServer(function(input, output){
  ##############
  temp = list.files(pattern="*.csv", path="./data")
  for (i in 1:length(temp)){ 
    assign(temp[i], read.csv(paste0("./data/",temp[i])))
  }
  
  commentcounts = data.frame( 
    "movie_title" = c("Call Me By Your Name", "Darkest Hour","Dunkirk","Get Out","Lady Bird",
                      "Phantom Thread","The Post","The Shape of Water","Three Billboards Outside Ebbing Missouri"),
    "number_songs"= c(nrow(`Call Me by Your Name.csv`), nrow(`Darkest Hour.csv`),nrow(Dunkirk.csv),nrow(`Get Out.csv`),nrow(`Lady Bird.csv`)
                      ,nrow(`Phantom Thread.csv`), nrow(`The Post.csv`), nrow(`The Shape of Water.csv`),nrow(`Three Billboards Outside Ebbing Missouri.csv`))
  )
  ###############
  output$map <- renderPlot({
    commentcounts <- group_by(commentcounts,movie_title) %>% summarize(length = number_songs)
    commentcounts %>% 
      arrange(desc(length)) %>%
      slice(1:10) %>%
      ggplot(., aes(x= reorder(movie_title, -length), y=length)) +
      geom_bar(stat='identity', fill="#1CCCC6") + 
      ylab("Comment Count") + xlab ("Movie title") + 
      ggtitle("Comment count of the Oscar nominated movies") + 
      theme_minimal() + 
      scale_x_discrete(labels = function(labels) {
        sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
      })
  })
  ############
  commentlength = c(str_count(`Call Me by Your Name.csv`$comment_text,"\\S+"), str_count(`Darkest Hour.csv`$comment_text,"\\S+"),
                    str_count(Dunkirk.csv$comment_text,"\\S+"),str_count(`Get Out.csv`$commentText,"\\S+"),
                    str_count(`Lady Bird.csv`$comment_text,"\\S+"),str_count(`Phantom Thread.csv`$comment_text,"\\S+"), 
                    str_count(`The Post.csv`$comment_text,"\\S+"), str_count(`The Shape of Water.csv`$comment_text,"\\S+"),
                    str_count(`Three Billboards Outside Ebbing Missouri.csv`$comment_text,"\\S+"))
  commentlength = commentlength[commentlength<80]
  commentlength = data.frame(commentlength)
  remove_reg <- "&amp;|&lt;|&gt;"
  nrc <- get_sentiments("nrc")
  bing = get_sentiments("bing")
  #############
  # show histogram using googleVis
  output$hist <- renderPlot({
    ggplot(commentlength, aes(x=commentlength)) + 
      geom_histogram(aes(fill = ..count..))+ 
      ylab("Frequency") + xlab ("Number of words") + 
      ggtitle("Histogram of the lengths of each reviews") + 
      theme_minimal()
  })
  
  ##########
  #Call Me By Your Name
  tidy_Call_Me <- `Call Me by Your Name.csv` %>% 
    filter(!str_detect(comment_text, "^RT")) %>%
    mutate(text = str_remove_all(comment_text, remove_reg)) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]")) 
  #Darkest Hours
  tidy_Darkest <- `Darkest Hour.csv` %>% 
    filter(!str_detect(comment_text, "^RT")) %>%
    mutate(text = str_remove_all(comment_text, remove_reg)) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]")) 
  
  #Dunkirk
  tidy_Dunkirk <- Dunkirk.csv %>% 
    filter(!str_detect(comment_text, "^RT")) %>%
    mutate(text = str_remove_all(comment_text, remove_reg)) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]")) 
  
  #Get Out
  tidy_Getout <- `Get Out.csv` %>% 
    filter(!str_detect(comment_text, "^RT")) %>%
    mutate(text = str_remove_all(comment_text, remove_reg)) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]"))
  
  #Lady Bird
  tidy_Lady <- `Lady Bird.csv` %>% 
    filter(!str_detect(comment_text, "^RT")) %>%
    mutate(text = str_remove_all(comment_text, remove_reg)) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]")) 
  
  #Phantom Thread
  tidy_Phantom <- `Phantom Thread.csv` %>% 
    filter(!str_detect(comment_text, "^RT")) %>%
    mutate(text = str_remove_all(comment_text, remove_reg)) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]")) 
  
  #The Post
  tidy_Post <- `The Post.csv` %>% 
    filter(!str_detect(comment_text, "^RT")) %>%
    mutate(text = str_remove_all(comment_text, remove_reg)) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]")) 
  
  #The Shape of Water
  tidy_Shape <- `The Shape of Water.csv` %>% 
    filter(!str_detect(comment_text, "^RT")) %>%
    mutate(text = str_remove_all(comment_text, remove_reg)) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]")) 
  
  #Three Billboards Outisde Ebbing Missouri
  tidy_ThreeB <- `Three Billboards Outside Ebbing Missouri.csv` %>% 
    filter(!str_detect(comment_text, "^RT")) %>%
    mutate(text = str_remove_all(comment_text, remove_reg)) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]")) 
  ###########
  
  ###########
  #Call Me By Your Name
  tidy_Call_Me_sentiment<-tidy_Call_Me %>%
    inner_join(nrc) %>%
    count(sentiment, sort = TRUE) 
  
  #Darkest hour
  tidy_Darkest_sentiment<-tidy_Darkest %>%
    inner_join(nrc) %>%
    count(sentiment, sort = TRUE) 
  
  #Dunkirk
  tidy_Dunkirk_sentiment<-tidy_Dunkirk %>%
    inner_join(nrc) %>%
    count(sentiment, sort = TRUE) 
  
  #Get Out
  tidy_Getout_sentiment<-tidy_Getout %>%
    inner_join(nrc) %>%
    count(sentiment, sort = TRUE) 
  
  #Lady Bird
  tidy_Lady_sentiment<-tidy_Lady%>%
    inner_join(nrc) %>%
    count(sentiment, sort = TRUE) 
  
  #Phantom Thread
  tidy_Phantom_sentiment<-tidy_Phantom %>%
    inner_join(nrc) %>%
    count(sentiment, sort = TRUE) 
  
  #The Post
  tidy_Post_sentiment<-tidy_Post %>%
    inner_join(nrc) %>%
    count(sentiment, sort = TRUE)
  
  #Shape of Water
  tidy_Shape_sentiment<-tidy_Shape %>%
    inner_join(nrc) %>%
    count(sentiment, sort = TRUE) 
  
  #Three Billboards Outside Ebbing Missouri
  tidy_ThreeB_sentiment<-tidy_ThreeB %>%
    inner_join(nrc) %>%
    count(sentiment, sort = TRUE) 
  ###########
  
  ########
  time_Call_Me <- tidy_Call_Me %>% 
    mutate(., date = anydate(timestamp/1000)) %>% 
    inner_join(bing) %>% group_by(date,sentiment) %>% summarize(number_comment = n())%>% 
    spread(., key=sentiment, value = number_comment)
  time_Call_Me[is.na(time_Call_Me)]<-0
  
  time_Darkest <- tidy_Darkest %>% 
    mutate(., date = anydate(timestamp/1000)) %>% 
    inner_join(bing) %>% group_by(date,sentiment) %>% summarize(number_comment = n())%>% 
    spread(., key=sentiment, value = number_comment)
  time_Darkest[is.na(time_Darkest)]<-0
  
  time_Dunkirk <- tidy_Dunkirk %>% 
    mutate(., date = anydate(timestamp/1000)) %>% 
    inner_join(bing) %>% group_by(date,sentiment) %>% summarize(number_comment = n())%>% 
    spread(., key=sentiment, value = number_comment)
  time_Dunkirk[is.na(time_Dunkirk)]<-0
  
  time_Getout <- tidy_Getout %>% 
    mutate(., date = anydate(timestamp/1000)) %>% 
    inner_join(bing) %>% group_by(date,sentiment) %>% summarize(number_comment = n())%>% 
    spread(., key=sentiment, value = number_comment)
  time_Getout[is.na(time_Getout)]<-0
  
  time_Lady <- tidy_Lady %>% 
    mutate(., date = anydate(timestamp/1000)) %>% 
    inner_join(bing) %>% group_by(date,sentiment) %>% summarize(number_comment = n())%>% 
    spread(., key=sentiment, value = number_comment)
  time_Lady[is.na(time_Lady)]<-0
  
  time_Phantom <- tidy_Phantom %>% 
    mutate(., date = anydate(timestamp/1000)) %>% 
    inner_join(bing) %>% group_by(date,sentiment) %>% summarize(number_comment = n())%>% 
    spread(., key=sentiment, value = number_comment)
  time_Phantom[is.na(time_Phantom)]<-0
  
  time_Post <- tidy_Post %>% 
    mutate(., date = anydate(timestamp/1000)) %>% 
    inner_join(bing) %>% group_by(date,sentiment) %>% summarize(number_comment = n())%>% 
    spread(., key=sentiment, value = number_comment)
  time_Post[is.na(time_Post)]<-0
  
  time_Shape <- tidy_Shape %>% 
    mutate(., date = anydate(timestamp/1000)) %>% 
    inner_join(bing) %>% group_by(date,sentiment) %>% summarize(number_comment = n())%>% 
    spread(., key=sentiment, value = number_comment)
  time_Shape[is.na(time_Shape)]<-0
  
  time_ThreeB <- tidy_ThreeB %>% 
    mutate(., date = anydate(timestamp/1000)) %>% 
    inner_join(bing) %>% group_by(date,sentiment) %>% summarize(number_comment = n())%>% 
    spread(., key=sentiment, value = number_comment)
  time_ThreeB[is.na(time_ThreeB)]<-0
  
  
  #########
  
  # show data using DataTable
  output$words1 <- renderPlot({tidy_Call_Me  %>% 
    count(word, sort = TRUE) %>% 
    arrange(desc(n)) %>%
    slice(c(2:7,9:12)) %>%
    ggplot(., aes(x=reorder(word,-n), y=n))  +  geom_bar(stat='identity', fill="#1CCCC6") + 
    ylab("Frequency") + xlab ("Top 10 terms") + 
    ggtitle("Top 10 words in Call Me By Your Name reviews") + 
    theme_minimal()
  })
  
  output$words2 <- renderPlot({
    tidy_Darkest  %>% 
      count(word, sort = TRUE) %>% 
      arrange(desc(n)) %>%
      slice(c(1, 3:10,12)) %>%
      ggplot(., aes(x=reorder(word,-n), y=n))  +  geom_bar(stat='identity', fill="#1CCCC6") + 
      ylab("Frequency") + xlab ("Top 10 terms") + 
      ggtitle("Top 10 words in Darkest Hour reviews") + 
      theme_minimal()
  })
  
  output$words3 <- renderPlot({
    tidy_Dunkirk  %>% 
      count(word, sort = TRUE)  %>% 
      arrange(desc(n)) %>%
      slice(c(2:5, 7:11,13)) %>%
      ggplot(., aes(x=reorder(word,-n), y=n))  +  geom_bar(stat='identity', fill="#1CCCC6") + 
      ylab("Frequency") + xlab ("Top 10 terms") + 
      ggtitle("Top 10 words in Dunkirk reviews") + 
      theme_minimal()
  })
  
  output$words4 <- renderPlot({
    tidy_Getout  %>% 
      count(word, sort = TRUE)  %>% 
      arrange(desc(n)) %>%
      slice(2:11) %>%
      ggplot(., aes(x=reorder(word,-n), y=n))  +  geom_bar(stat='identity', fill="#1CCCC6") + 
      ylab("Frequency") + xlab ("Top 10 terms") + 
      ggtitle("Top 10 words in Get Out reviews") + 
      theme_minimal()
  })
  
  output$words5 <- renderPlot({
    tidy_Lady  %>% 
      count(word, sort = TRUE) %>% 
      arrange(desc(n)) %>%
      slice(c(2:4, 6:12)) %>%
      ggplot(., aes(x=reorder(word,-n), y=n))  +  geom_bar(stat='identity', fill="#1CCCC6") + 
      ylab("Frequency") + xlab ("Top 10 terms") + 
      ggtitle("Top 10 words in Lady Bird reviews") + 
      theme_minimal()
  })
  
  output$words6 <- renderPlot({
    tidy_Phantom  %>% 
      count(word, sort = TRUE) %>% 
      arrange(desc(n)) %>%
      slice(c(2:4,6:12)) %>%
      ggplot(., aes(x=reorder(word,-n), y=n))  +  geom_bar(stat='identity', fill="#1CCCC6") + 
      ylab("Frequency") + xlab ("Top 10 terms") + 
      ggtitle("Top 10 words in Phantom Thread reviews") + 
      theme_minimal()
  })
  
  output$words7 <- renderPlot({
    tidy_Post  %>% 
      count(word, sort = TRUE) %>% 
      arrange(desc(n)) %>%
      slice(c(2:9,11:12)) %>%
      ggplot(., aes(x=reorder(word,-n), y=n))  +  geom_bar(stat='identity', fill="#1CCCC6") + 
      ylab("Frequency") + xlab ("Top 10 terms") + 
      ggtitle("Top 10 words in The Post reviews") + 
      theme_minimal()
  })
  
  output$words8 <- renderPlot({
    tidy_Shape  %>% 
      count(word, sort = TRUE) %>% 
      arrange(desc(n)) %>%
      slice(c(2:10,12)) %>%
      ggplot(., aes(x=reorder(word,-n), y=n))  +  geom_bar(stat='identity', fill="#1CCCC6") + 
      ylab("Frequency") + xlab ("Top 10 terms") + 
      ggtitle("Top 10 words in Shape of Water reviews") + 
      theme_minimal()
  })
  
  output$words9 <- renderPlot({
    tidy_ThreeB  %>% 
      count(word, sort = TRUE) %>% 
      arrange(desc(n)) %>%
      slice(c(3:10,12:13)) %>%
      ggplot(., aes(x=reorder(word,-n), y=n))  +  geom_bar(stat='identity', fill="#1CCCC6") + 
      ylab("Frequency") + xlab ("Top 10 terms") + 
      ggtitle("Top 10 words in Three Billboards Outside Ebbing Missouri reviews") + 
      theme_minimal()
  })
  
  output$sentiment1 <- renderPlot({
    tidy_Call_Me_sentiment%>% 
      ggplot(., aes(x= sentiment, y=n))+ geom_bar(stat="identity",aes(fill = sentiment))+
      ylab("Sentiments") + xlab ("Sentiment Scores") + 
      ggtitle("Sentiment Scores of Call Me By Your Name") + 
      theme_minimal()
  })
  output$sentiment2 <- renderPlot({
    tidy_Darkest_sentiment%>% 
      ggplot(., aes(x= sentiment, y=n))+ geom_bar(stat="identity",aes(fill = sentiment))+
      ylab("Sentiments") + xlab ("Sentiment Scores") + 
      ggtitle("Sentiment Scores of Darket Hour") + 
      theme_minimal()
  })
  output$sentiment3 <- renderPlot({
    tidy_Dunkirk_sentiment%>% 
      ggplot(., aes(x= sentiment, y=n))+ geom_bar(stat="identity",aes(fill = sentiment))+
      ylab("Sentiments") + xlab ("Sentiment Scores") + 
      ggtitle("Sentiment Scores of Dirkirk") + 
      theme_minimal()
  })
  output$sentiment4 <- renderPlot({
    tidy_Getout_sentiment%>% 
      ggplot(., aes(x= sentiment, y=n))+ geom_bar(stat="identity",aes(fill = sentiment))+
      ylab("Sentiments") + xlab ("Sentiment Scores") + 
      ggtitle("Sentiment Scores of Get Out") + 
      theme_minimal()
  })
  output$sentiment5 <- renderPlot({
    tidy_Lady_sentiment%>% 
      ggplot(., aes(x= sentiment, y=n))+ geom_bar(stat="identity",aes(fill = sentiment))+
      ylab("Sentiments") + xlab ("Sentiment Scores") + 
      ggtitle("Sentiment Scores of Lady Bird") + 
      theme_minimal()
  })
  output$sentiment6 <- renderPlot({
    tidy_Phantom_sentiment%>% 
      ggplot(., aes(x= sentiment, y=n))+ geom_bar(stat="identity",aes(fill = sentiment))+
      ylab("Sentiments") + xlab ("Sentiment Scores") + 
      ggtitle("Sentiment Scores of Phantom Thread") + 
      theme_minimal()
  })
  output$sentiment7 <- renderPlot({
    tidy_Post_sentiment%>% 
      ggplot(., aes(x= sentiment, y=n))+ geom_bar(stat="identity",aes(fill = sentiment))+
      ylab("Sentiments") + xlab ("Sentiment Scores") + 
      ggtitle("Sentiment Scores of The Post") + 
      theme_minimal()
  })
  output$sentiment8 <- renderPlot({
    tidy_Shape_sentiment%>% 
      ggplot(., aes(x= sentiment, y=n))+ geom_bar(stat="identity",aes(fill = sentiment))+
      ylab("Sentiments") + xlab ("Sentiment Scores") + 
      ggtitle("Sentiment Scores of Shape of Water") + 
      theme_minimal()
  })
  output$sentiment9 <- renderPlot({
    tidy_ThreeB_sentiment%>% 
      ggplot(., aes(x= sentiment, y=n))+ geom_bar(stat="identity",aes(fill = sentiment))+
      ylab("Sentiments") + xlab ("Sentiment Scores") + 
      ggtitle("Sentiment Scores of Three Billboards Outside Ebbing Missouri") + 
      theme_minimal()
  })
  
  output$intime1 <- renderPlot({
    time_Call_Me%>% 
      arrange(desc(date)) %>% 
      ggplot(.) + geom_line(aes(x=date, y=negative), color = "red")+
      geom_line(aes(x=date, y=positive), color = "blue") +
      ylab("Comment count") + xlab ("Date") + 
      ggtitle("Call Me By Your Name Comment count change over the date") + 
      theme_minimal()
  })
  output$intime2 <- renderPlot({
    time_Darkest%>% 
      arrange(desc(date)) %>% 
      ggplot(.) + geom_line(aes(x=date, y=negative), color = "red")+
      geom_line(aes(x=date, y=positive), color = "blue") +
      ylab("Comment count") + xlab ("Date") + 
      ggtitle("The Darkest Hour Comment count change over the date") +  
      theme_minimal()
  })
  output$intime3 <- renderPlot({
    time_Dunkirk%>% 
      arrange(desc(date)) %>% 
      ggplot(.) + geom_line(aes(x=date, y=negative), color = "red")+
      geom_line(aes(x=date, y=positive), color = "blue") +
      ylab("Comment count") + xlab ("Date") + 
      ggtitle("Dunkirk Comment count change over the date") +  
      theme_minimal()
  })
  output$intime4 <- renderPlot({
    time_Getout%>% 
      arrange(desc(date)) %>% 
      ggplot(.) + geom_line(aes(x=date, y=negative), color = "red")+
      geom_line(aes(x=date, y=positive), color = "blue") +
      ylab("Comment count") + xlab ("Date") + 
      ggtitle("Get Out Comment count change over the date") +  
      theme_minimal()
  })
  output$intime5 <- renderPlot({
    time_Lady%>% 
      arrange(desc(date)) %>% 
      ggplot(.) + geom_line(aes(x=date, y=negative), color = "red")+
      geom_line(aes(x=date, y=positive), color = "blue") +
      ylab("Comment count") + xlab ("Date") + 
      ggtitle("Lady Bird Comment count change over the date") +  
      theme_minimal()
  })
  output$intime6 <- renderPlot({
    time_Phantom%>% 
      arrange(desc(date)) %>% 
      ggplot(.) + geom_line(aes(x=date, y=negative), color = "red")+
      geom_line(aes(x=date, y=positive), color = "blue") +
      ylab("Comment count") + xlab ("Date") + 
      ggtitle("Phatom Thread Comment count change over the date") +  
      theme_minimal()
  })
  output$intime7 <- renderPlot({
    time_Post%>% 
      arrange(desc(date)) %>% 
      ggplot(.) + geom_line(aes(x=date, y=negative), color = "red")+
      geom_line(aes(x=date, y=positive), color = "blue") +
      ylab("Comment count") + xlab ("Date") + 
      ggtitle("The Post Comment count change over the date") +  
      theme_minimal()
  })
  output$intime8 <- renderPlot({
    time_Shape%>% 
      arrange(desc(date)) %>% 
      ggplot(.) + geom_line(aes(x=date, y=negative), color = "red")+
      geom_line(aes(x=date, y=positive), color = "blue") +
      ylab("Comment count") + xlab ("Date") + 
      ggtitle("Shape of Water Comment count change over the date") +  
      theme_minimal()
  })
  output$intime9 <- renderPlot({
    time_ThreeB%>% 
      arrange(desc(date)) %>% 
      ggplot(.) + geom_line(aes(x=date, y=negative), color = "red")+
      geom_line(aes(x=date, y=positive), color = "blue") +
      ylab("Comment count") + xlab ("Date") + 
      ggtitle("Thee Billboards Outside Ebbing Missouri Comment count change over the date") +  
      theme_minimal()
  })
  
  output$wordcloud1 <- renderPlot({
    tidy_Call_Me %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 250)
  })
  output$wordcloud2 <- renderPlot({
    tidy_Darkest %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 250)
  })
  output$wordcloud3 <- renderPlot({
    tidy_Dunkirk %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 250)
  })
  output$wordcloud4 <- renderPlot({
    tidy_Getout %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 250)
  })
  output$wordcloud5 <- renderPlot({
    tidy_Lady %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 250)
  })
  output$wordcloud6 <- renderPlot({
    tidy_Phantom %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 250)
  })
  output$wordcloud7 <- renderPlot({
    tidy_Post %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 250)
  })
  output$wordcloud8 <- renderPlot({
    tidy_Shape %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 250)
  })
  output$wordcloud9 <- renderPlot({
    tidy_ThreeB %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 250)
  })
  
  #########
  #Call Me By Your Name
  count_bigrams <- function(dataset) {
    dataset %>%
      unnest_tokens(bigram, comment_text, token = "ngrams", n = 2) %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% stop_words$word,
             !word2 %in% stop_words$word) %>%
      count(word1, word2, sort = TRUE)
  }
  visualize_bigrams <- function(bigrams) {
    set.seed(2016)
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    bigrams %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      ggtitle("Network graph of bigrams") +
      theme_void()
  }
  
  Call_Me_bigrams <- `Call Me by Your Name.csv` %>%
    count_bigrams()
  
  Darkest_bigrams <- `Darkest Hour.csv` %>%
    count_bigrams()
  
  Dunkirk_bigrams <- Dunkirk.csv %>%
    count_bigrams()
  
  Getout_bigrams <- `Get Out.csv`%>%
    count_bigrams()
  
  Lady_bigrams <- `Lady Bird.csv` %>%
    count_bigrams()
  
  Phantom_bigrams <- `Phantom Thread.csv` %>%
    count_bigrams()
  
  Post_bigrams <- `The Post.csv` %>%
    count_bigrams()
  
  Shape_bigrams <- `The Shape of Water.csv` %>%
    count_bigrams()
  
  ThreeB_bigrams <- `Three Billboards Outside Ebbing Missouri.csv` %>%
    count_bigrams()
  #########
  
  output$bigram1 <- renderPlot({
    Call_Me_bigrams %>%
      filter(n > 3,
             !str_detect(word1, "\\d"),
             !str_detect(word2, "\\d")) %>%
      visualize_bigrams()
  })
  
  output$bigram2 <- renderPlot({
    Darkest_bigrams %>%
      filter(n > 3,
             !str_detect(word1, "\\d"),
             !str_detect(word2, "\\d")) %>%
      visualize_bigrams()
  })
  output$bigram3 <- renderPlot({
    Dunkirk_bigrams %>%
      filter(n > 3,
             !str_detect(word1, "\\d"),
             !str_detect(word2, "\\d")) %>%
      visualize_bigrams()
  })
  output$bigram4 <- renderPlot({
    Getout_bigrams %>%
      filter(n > 3,
             !str_detect(word1, "\\d"),
             !str_detect(word2, "\\d")) %>%
      visualize_bigrams()
  })
  output$bigram5 <- renderPlot({
    Lady_bigrams %>%
      filter(n > 3,
             !str_detect(word1, "\\d"),
             !str_detect(word2, "\\d")) %>%
      visualize_bigrams()
  })
  output$bigram6 <- renderPlot({
    Phantom_bigrams %>%
      filter(n > 3,
             !str_detect(word1, "\\d"),
             !str_detect(word2, "\\d")) %>%
      visualize_bigrams()
  })
  output$bigram7 <- renderPlot({
    Post_bigrams %>%
      filter(n > 3,
             !str_detect(word1, "\\d"),
             !str_detect(word2, "\\d")) %>%
      visualize_bigrams()
  })
  output$bigram8 <- renderPlot({
    Shape_bigrams %>%
      filter(n > 3,
             !str_detect(word1, "\\d"),
             !str_detect(word2, "\\d")) %>%
      visualize_bigrams()
  })
  output$bigram9 <- renderPlot({
    ThreeB_bigrams %>%
      filter(n > 3,
             !str_detect(word1, "\\d"),
             !str_detect(word2, "\\d")) %>%
      visualize_bigrams()
  })
  # show statistics using infoBox
  output$maxBox <- renderInfoBox({
    max_value <- max(state_stat[,input$selected])
    max_state <- 
      state_stat$state.name[state_stat[,input$selected] == max_value]
    infoBox(max_state, max_value, icon = icon("hand-o-up"))
  })
  output$minBox <- renderInfoBox({
    min_value <- min(state_stat[,input$selected])
    min_state <- 
      state_stat$state.name[state_stat[,input$selected] == min_value]
    infoBox(min_state, min_value, icon = icon("hand-o-down"))
  })
  output$avgBox <- renderInfoBox(
    infoBox(paste("AVG.", input$selected),
            mean(state_stat[,input$selected]), 
            icon = icon("calculator"), fill = TRUE))
})