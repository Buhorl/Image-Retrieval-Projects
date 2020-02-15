library(dplyr)
library(wordcloud)
library(wordcloud2)
library(cld2) # For detecting languages
library(tm) # For text mining
library(RColorBrewer)

#We set a seed for the WC
set.seed(2020)

#We load the file
corpus <- read.csv("./corpus.csv")

#Task 1: Creating a new column.
selected_nan <- ifelse(corpus$seer_stage == 8 | corpus$seer_stage == 9, T, F)
selected_late <- ifelse(corpus$seer_stage == 4 | corpus$seer_stage == 5 | corpus$seer_stage == 7, T, F)
selected_early <- ifelse(corpus$seer_stage == 1 | corpus$seer_stage == 2 | corpus$seer_stage == 3 | corpus$seer_stage == 6, T, F)

corpus <- corpus %>% 
  mutate(seer_stage_2 = ifelse(
    selected_late, "later_stage",
    ifelse(selected_nan, "nan", "early_stage") 
  )) #I guess filtering out the 8 and 9 means creating another category

#Task 2: 
  #We build a function
get_frequent_words <- function(dataset) {
  ## Read corpus
  docs <- Corpus(VectorSource(unlist(dataset$MEDICATION_SUMMARY)))
  
  ## Preprocess corpus
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  ## Build term document matrix
  docs_term_matrix <- as.matrix(TermDocumentMatrix(docs))
  sorted_docs_term_matrix <- sort(rowSums(docs_term_matrix), decreasing=TRUE)
  frequent_words <- data.frame(word = names(sorted_docs_term_matrix), freq=sorted_docs_term_matrix)
  
  frequent_words
}
fw_early <- get_frequent_words(filter(corpus, seer_stage_2 == "early_stage"))
fw_late <- get_frequent_words(filter(corpus, seer_stage_2 == "later_stage"))
fw_all <- get_frequent_words(corpus)

#We create the wc using wordcloud
wordcloud(fw_early$word, fw_early$freq, min.freq = 5, max.words = 100,
          random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale=c(2,.5))
wordcloud(fw_late$word, fw_late$freq, min.freq = 5, max.words = 100,
          random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale=c(2,.5))
wordcloud(fw_all$word, fw_all$freq, min.freq = 5, max.words = 100,
          random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale=c(2,.5))

#We create the wc using wordcloud2
wordcloud2(fw_early, 
           minRotation = 0, maxRotation = 0, color ='random-dark', size = 0.7, fontFamily ='Arial', ellipticity = 1, shape ='circle')
wordcloud2(fw_late,
           minRotation = 0, maxRotation = 0, color ='random-dark', size = 0.7, fontFamily ='Arial', ellipticity = 1, shape ='circle')
wordcloud2(fw_all,
           minRotation = 0, maxRotation = 0, color ='random-dark', size = 0.7, fontFamily ='Arial', ellipticity = 1, shape ='circle')

#Task 3:

