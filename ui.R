
ui <- dashboardPage(
  dashboardHeader(title = "2018 Oscar Nominee"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("EDA", tabName = "map", icon = icon("map")),
      menuItem("Top 10 words", tabName = "data", icon = icon("database")),
      menuItem("Sentiments", tabName = "Sentiments", icon = icon("")),
      menuItem("Sentiments-Time", tabName = "Sentiments-Time", icon= icon("Sentiments-Time")),
      menuItem("WordClouds", tabName = "wordcloud", icon = icon("wordcloud")),
      menuItem("Bigrams", tabName = "bigram", icon = icon("bigram"))
    ),
    selectizeInput("selected",
                   "Select Movie",
                   choice)
    
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "map",
              fluidRow(box(plotlyOutput("map"), height = 450, width = 12),
                       box(plotlyOutput("hist"), height = 450, width = 12))),
      tabItem(tabName = "data",
              fluidRow(infoBoxOutput("comments1"),
                       infoBoxOutput("likes1"),
                       infoBoxOutput("replies1")),
              fluidRow(box(plotlyOutput("words"), height = 450,width =12))
      ),  
      tabItem(tabName = "Sentiments",
              fluidRow(infoBoxOutput("comments2"),
                       infoBoxOutput("likes2"),
                       infoBoxOutput("replies2")),
              fluidRow(box(plotlyOutput("sentiment"), height = 450,width =12))
      ),
      tabItem(tabName = "Sentiments-Time",
              fluidRow(infoBoxOutput("comments3"),
                       infoBoxOutput("likes3"),
                       infoBoxOutput("replies3")),
              fluidRow(box(plotlyOutput("intime"), height = 450,width =12))
      ),
      tabItem(tabName = "wordcloud",
              fluidRow(infoBoxOutput("comments4"),
                       infoBoxOutput("likes4"),
                       infoBoxOutput("replies4")),
              fluidRow(box(plotOutput("wordcloud"), title = textOutput('cloudtitle'), height = 450,width =12))
      ),
      tabItem(tabName = "bigram",
              fluidRow(infoBoxOutput("comments5"),
                       infoBoxOutput("likes5"),
                       infoBoxOutput("replies5")),
              fluidRow(box(plotOutput("bigram"), title = textOutput('bigramtitle'),height = 500,width =16))
      )
      
    )
  )
)
  