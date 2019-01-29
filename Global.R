library(DT)
library(shiny)
library(shinydashboard)
library(googleVis)
library(tm)
library(wordcloud)

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

