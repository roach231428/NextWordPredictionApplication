#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(magrittr)
library(tm)
library(text2vec)

load("en_US_0.01_withPkn_cleaned.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Next word prediction application"),
   
   # Sidebar with a slider input for number of words
   sidebarLayout(
      sidebarPanel(
         textInput("text", 
                   "Type setence here:", 
                   width = '400px'),
         sliderInput("words",
                     "Number of predicted words:",
                     min = 1,
                     max = 20,
                     value = 5)
      ),

      mainPanel(
         textOutput("nextWord")
      )
   )
)

server <- function(input, output) {
  removeWebLang = function(text){
    text %>% gsub("#[^ ]{1,}", " ", .) %>%            # Remove hashtags
      gsub("[^ ]{1,}@[^ ]{1,}", " ", .) %>%    # Remove email address
      gsub("[^ ]{1,}://[^ ]{1,}", " ", .) %>%  # Remove wibsite address
      return
  }
  
  # Make corpus which has been cleaned
  makeCorpus = function(fileLines, proportion = 0.5){
    corpus = fileLines %>% sample(., proportion*length(.)) %>% 
      removeWebLang %>% tolower %>%
      removePunctuation(preserve_intra_word_contractions = TRUE) %>% 
      removeNumbers %>% stripWhitespace %>% return
  }
  
   output$nextWord <- renderText({
     text = input$text %>% makeCorpus(1) %>% strsplit(" ") %>% .[[1]] %>% tail(3)
     n = length(text)
     result = NULL
     
     if(n >= 3){
       quad = en_US_all_quadgram[term123 == paste(text[n-2], text[n-1], text[n], sep="_"), 
                                 c('prediction', 'pkn', 'ngram'), with = FALSE] %>% head
       tri = en_US_all_trigram[term12 == paste(text[n-1], text[n], sep="_"), 
                               c('prediction', 'pkn', 'ngram'), with = FALSE] %>% head
       bi = en_US_all_bigram[term1 == text[n], c('prediction', 'pkn', 'ngram'), with = FALSE] %>% head
       uni = en_US_all_unigram %>% head
       
       result = rbind(result, quad,tri,bi,uni) 
     }
     if(n >= 2){
       quad = en_US_all_quadgram[term23 == paste(text[n-1], text[n], sep="_"), 
                                 c('prediction', 'pkn', 'ngram'), with = FALSE] %>% head
       tri = en_US_all_trigram[term2 == paste(text[n], sep="_"), 
                               c('prediction', 'pkn', 'ngram'), with = FALSE] %>% head
       bi = en_US_all_bigram[term1 == text[n], c('prediction', 'pkn', 'ngram'), with = FALSE] %>% head
       uni = en_US_all_unigram %>% head
       
       result = rbind(result, quad,tri,bi,uni) 
     }
     if(length(text) >= 1){
       quad = en_US_all_quadgram[term3 == text[n], c('prediction', 'pkn', 'ngram'), with = FALSE] %>% head
       tri = en_US_all_trigram[term2 == text[n], c('prediction', 'pkn', 'ngram'), with = FALSE] %>% head
       bi = en_US_all_bigram[term1 == text[n], c('prediction', 'pkn', 'ngram'), with = FALSE] %>% head
       uni = en_US_all_unigram %>% head
       
       result = rbind(result, quad,tri,bi,uni) 
     } else{ 
       result = en_US_all_unigram %>% head(10)
     }
     
     if(nrow(result) < input$words){
       nwords = nrow(result)
     } else { nwords = input$words }
     
     result %>% setorder(-pkn) %>% .$prediction %>% unique %>% head(nwords) %>% paste(collapse = ", ") %>% return
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

