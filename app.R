library(shiny)
library(shinydashboard)
library(rtweet)
library(tidyverse)
library(wordcloud)
library(tidytext)

# UI header
header <- dashboardHeader(title = "Project WOKECLOUD")

# UI sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    # twitter handle input
    textInput("handle", "Twitter Handle HERE?", placeholder = "e.g. charlesmurray"),
    
    # button to pull twitter data
    actionButton("grab", "Grab tweets", class = "btn-success"),
    
    menuItem("Wokecloud", tabName = "page_wordcloud", icon = icon("cloud")),
    menuItem("Github Repo", icon = icon("github"), href = "https://github.com/cirxi/wokecloud")
    )
  )

# UI body
body <- dashboardBody(
  
  tabItem(tabName = "page_wordcloud",
          
          # Application title
          titlePanel("WOKECLOUD GENERATOR"),
        
          # wordcloud
          plotOutput("wordcloud", width = "700px", height = "700px"),
          
          # generate button
          actionButton("generate", "Generate", class = "btn-success btn-block")),
  )

ui <- dashboardPage(header, sidebar, body)

# Server
server <- function(input, output, session) {
  
  # tweets will only pulled when grab button is clicked
  tweets <- eventReactive(input$grab, {
    get_timeline(input$handle, n = 500)
    })
  
  # custom stopwords for scrubbing
  custom_stopwords <- as.data.frame(c("im", "ive", "dont", "youre", "lol", "lmao")) %>%
    setNames("word")
  
  # scrubbing tweets for wordcloud when generate button is pressed
  tweets_wordcloud <- eventReactive(input$generate, {
    as.data.frame(tweets() %>%
      filter(is_retweet == FALSE) %>% # removing retweets
      select(text) %>%
      gsub(pattern = "https\\S*", replacement = "") %>% # gsub cleaning code from https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
      gsub(pattern = "[^\x01-\x7F]", replacement = "") %>% 
      gsub(pattern = "@\\S*", replacement = "") %>% 
      gsub(pattern = "amp", replacement = "") %>% 
      gsub(pattern = "[\r\n]", replacement = "") %>% 
      gsub(pattern = "[[:punct:]]", replacement = "")) %>% 
    setNames("text") %>% 
      unnest_tokens(word, text) %>% # breaking down tweets into tokens
      anti_join(get_stopwords(source = "smart")) %>% 
      anti_join(custom_stopwords) %>% 
      count(word, sort = TRUE)
  })
  
  # wordcloud script
  output$wordcloud <- renderPlot(wordcloud(words = tweets_wordcloud()$word, freq = tweets_wordcloud()$n,
                                           min.freq = 10, max.words = 100, colors = brewer.pal(8, "Dark2"),
                                           scale = c(4, 0.5)))
  
}

# Run the application
shinyApp(ui = ui, server = server)
