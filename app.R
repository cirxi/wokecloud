library(shiny)
library(rtweet)
library(tidyverse)
library(wordcloud)
library(tidytext)

# UI
ui <- fluidPage(
  
  # Application title
  titlePanel("WOKECLOUD GENERATOR"),
  
  # twitter handle input
  textInput("handle", "Twitter Handle HERE?", placeholder = "e.g. charlesmurray"),
  
  # wordcloud
  plotOutput("wordcloud", width = "1000px", height = "700px"),
  
  # generate button
  actionButton("execute", "Generate", class = "btn-block")
  
)

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
  
  # removing stopwords  
  tweets_tokens <- reactive({anti_join(tweets_words(), get_stopwords(source = "smart"))})
  
  custom_stopwords <- as.data.frame(c("im", "ive")) %>% setNames("word")
  tweets_tokens_cleaned <- reactive({anti_join(tweets_tokens(), custom_stopwords)})
  
  
  # summarising words and calculating counts
  tweets_wordcloud <- reactive({count(tweets_tokens_cleaned(), word, sort = TRUE)})
  
  # wordcloud script
  output$wordcloud <- renderPlot(wordcloud(words = tweets_wordcloud()$word, freq = tweets_wordcloud()$n,
                                           min.freq = 10, max.words = 100, colors = brewer.pal(8, "Dark2"),
                                           scale = c(4, 0.5)))
  
}

# Run the application
shinyApp(ui = ui, server = server)
