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
          actionButton("execute", "Generate", class = "btn-success btn-block")),
  )

ui <- dashboardPage(header, sidebar, body)

# Server
server <- function(input, output, session) {
  
  # tweets will only pulled and scrubbed when execute button is clicked
  # gsub cleaning code from https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
  tweets <- eventReactive(input$execute, {
    as.data.frame(get_timeline(input$handle, n = 1000) %>%
                    filter(is_retweet == FALSE) %>% 
                    select(text) %>%
                    gsub(pattern = "https\\S*", replacement = "") %>%
                    gsub(pattern = "[^\x01-\x7F]", replacement = "") %>% 
                    gsub(pattern = "@\\S*", replacement = "") %>% 
                    gsub(pattern = "amp", replacement = "") %>% 
                    gsub(pattern = "[\r\n]", replacement = "") %>% 
                    gsub(pattern = "[[:punct:]]", replacement = "")) %>% 
      setNames("text")
  })
  
  # splitting chunk of tweets into tokens
  tweets_words <- reactive({
    unnest_tokens(as.data.frame(tweets()), 
                  word, text)})
  
  # removing stopwords (with lexicon and with a custom list)
  custom_stopwords <- as.data.frame(c("im", "ive", "dont", "youre", "lol", "lmao")) %>%
    setNames("word")
  tweets_tokens <- reactive({anti_join(tweets_words(), get_stopwords(source = "smart")) %>% 
      anti_join(custom_stopwords)
      })
  
  # summarising words and calculating counts
  tweets_wordcloud <- reactive({count(tweets_tokens(), word, sort = TRUE)})
  
  # wordcloud script
  output$wordcloud <- renderPlot(wordcloud(words = tweets_wordcloud()$word, freq = tweets_wordcloud()$n,
                                           min.freq = 10, max.words = 100, colors = brewer.pal(8, "Dark2"),
                                           scale = c(4, 0.5)))
  
}

# Run the application
shinyApp(ui = ui, server = server)
