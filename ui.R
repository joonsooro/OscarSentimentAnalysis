#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram

shinyUI(dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    
    sidebarUserPanel("NYC DSA",
                     image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
    sidebarMenu(
      menuItem("EDA", tabName = "map", icon = icon("map")),
      menuItem("Top 10 words", tabName = "data", icon = icon("database")),
      menuItem("Sentiments", tabName = "Sentiments", icon = icon("sentiment")),
      menuItem("Sentiments-Time", tabName = "Sentiments-Time", icon= icon("Sentiments-Time")),
      menuItem("WordClouds", tabName = "wordcloud", icon = icon("wordcloud")),
      menuItem("Bigrams", tabName = "bigram", icon = icon("bigram"))
    )
    
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "map",
              fluidRow(box(plotOutput("map"), height = 450, width = 50),
                       box(plotOutput("hist"), height = 450, width = 50))),
      tabItem(tabName = "data",
              fluidRow(box(plotOutput("words1"), height = 400,width =50)),
              fluidRow(box(plotOutput("words2"), height = 400, width =50)),    
              fluidRow(box(plotOutput("words3"), height = 400, width =50)),
              fluidRow(box(plotOutput("words4"), height = 400, width =50)),
              fluidRow(box(plotOutput("words5"), height = 400, width =50)),
              fluidRow(box(plotOutput("words6"), height = 400, width =50)),
              fluidRow(box(plotOutput("words7"), height = 400, width =50)),
              fluidRow(box(plotOutput("words8"), height = 400, width =50)),   
              fluidRow(box(plotOutput("words9"), height = 400, width =50))
                       ),  
      tabItem(tabName = "Sentiments",
               fluidRow(box(plotOutput("sentiment1"), height = 400,width =50)),
               fluidRow(box(plotOutput("sentiment2"), height = 400, width =50)),    
               fluidRow(box(plotOutput("sentiment3"), height = 400, width =50)),
               fluidRow(box(plotOutput("sentiment4"), height = 400, width =50)),
               fluidRow(box(plotOutput("sentiment5"), height = 400, width =50)),
               fluidRow(box(plotOutput("sentiment6"), height = 400, width =50)),
               fluidRow(box(plotOutput("sentiment7"), height = 400, width =50)),
               fluidRow(box(plotOutput("sentiment8"), height = 400, width =50)),   
               fluidRow(box(plotOutput("sentiment9"), height = 400, width =50))
      ),
      tabItem(tabName = "Sentiments-Time",
              fluidRow(box(plotOutput("intime1"), height = 400,width =50)),
              fluidRow(box(plotOutput("intime2"), height = 400, width =50)),    
              fluidRow(box(plotOutput("intime3"), height = 400, width =50)),
              fluidRow(box(plotOutput("intime4"), height = 400, width =50)),
              fluidRow(box(plotOutput("intime5"), height = 400, width =50)),
              fluidRow(box(plotOutput("intime6"), height = 400, width =50)),
              fluidRow(box(plotOutput("intime7"), height = 400, width =50)),
              fluidRow(box(plotOutput("intime8"), height = 400, width =50)),   
              fluidRow(box(plotOutput("intime9"), height = 400, width =50))
      ),
      
      tabItem(tabName = "wordcloud",
              fluidRow(box(plotOutput("wordcloud1"), title = "Call Me By Your Name",height = 400,width =50)),
              fluidRow(box(plotOutput("wordcloud2"), title = "Darkest Hour", height = 400, width =50)),    
              fluidRow(box(plotOutput("wordcloud3"),  title = "Dunkirk", height = 400, width =50)),
              fluidRow(box(plotOutput("wordcloud4"), title = "Get Out", height = 400, width =50)),
              fluidRow(box(plotOutput("wordcloud5"), title = "Lady Bird", height = 400, width =50)),
              fluidRow(box(plotOutput("wordcloud6"), title = "Phantom Thread",height = 400, width =50)),
              fluidRow(box(plotOutput("wordcloud7"), title = "The Post",height = 400, width =50)),
              fluidRow(box(plotOutput("wordcloud8"), title = "The Shape of Water",height = 400, width =50)),   
              fluidRow(box(plotOutput("wordcloud9"), title = "Three Billboards Outside Ebbing Missouri",height = 400, width =50))
      ),
      tabItem(tabName = "bigram",
              fluidRow(box(plotOutput("bigram1"), title = "Call Me By Your Name",height = 500,width =50)),
              fluidRow(box(plotOutput("bigram2"), title = "Darkest Hour", height = 500, width =50)),    
              fluidRow(box(plotOutput("bigram3"),  title = "Dunkirk", height = 500, width =50)),
              fluidRow(box(plotOutput("bigram4"), title = "Get Out", height = 500, width =50)),
              fluidRow(box(plotOutput("bigram5"), title = "Lady Bird", height = 500, width =50)),
              fluidRow(box(plotOutput("bigram6"), title = "Phantom Thread",height = 500, width =50)),
              fluidRow(box(plotOutput("bigram7"), title = "The Post",height = 500, width =50)),
              fluidRow(box(plotOutput("bigram8"), title = "The Shape of Water",height = 500, width =50)),   
              fluidRow(box(plotOutput("bigram9"), title = "Three Billboards Outside Ebbing Missouri",height = 500, width =50))
      )
                       
    )
  )
))
  