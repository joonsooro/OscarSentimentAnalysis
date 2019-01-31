library(DT)
library(shiny)
library(shinydashboard)
library(tm)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(anytime)
library(tidyr)
library(plotly)
library(reshape2)
library(igraph)
library(ggraph)
library(rsconnect)
library(fontawesome)

remove_reg <- "&amp;|&lt;|&gt;"
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

#get the names of the .csv files as a list
temp = list.files(pattern="*.csv", path="./data")
#get each dataframe with the names
for (i in 1:length(temp)){ 
  assign(paste0("dataframe",i), read.csv(paste0("./data/",temp[i]))%>% 
           mutate(., movie = gsub(as.character(temp[i]), pattern=".csv$", replacement="")))
}

#create a dataframe to show the comment counts of the movies
movie_title = c()
number_count = c()
for(i in 1:length(temp)){
  movie_title = c(movie_title, gsub(as.character(temp[i]), pattern=".csv$", replacement=""))
  number_count = c(number_count, nrow(get(paste0("dataframe",(i)))))
}
commentcounts = data.frame("movie_title"=movie_title, "number_counts"=number_count)

#create a dataframe to show the string counts of the movies
commentlength =c()
for(i in length(temp)){
  commentlength = c(commentlength, str_count(get(paste0("dataframe",(i)))$comment_text,"\\S+"))
}
commentlength = commentlength[commentlength<80]
commentlength = data.frame(commentlength)

#create a comprehensive dataframe to "selectize" each movies as input
totaldataframe = data.frame()
for (i in 1:length(temp)){ 
  totaldataframe =  rbind(totaldataframe, get(paste0("dataframe",(i))))
}

tidy_total = totaldataframe%>% 
  filter(!str_detect(comment_text, "^RT")) %>%
  mutate(text = str_remove_all(comment_text, remove_reg)) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]")) %>% 
  mutate(., date = anydate(timestamp/1000))
tidy_total = within(tidy_total, rm(timestamp))

tidy_total_bing <- tidy_total %>% 
  inner_join(bing) %>% group_by(date,sentiment) %>% summarize(number_comment = n())%>% 
  spread(., key=sentiment, value = number_comment)
tidy_total_bing[is.na(tidy_total_bing)]<-0

choice = c()
for(i in 1:length(temp)){
  choice = c(choice, gsub(as.character(temp[i]), pattern=".csv$", replacement=""))
}
choice
colnames(tidy_total)
levels(as.factor(tidy_total$movie))

#bigram functions
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


