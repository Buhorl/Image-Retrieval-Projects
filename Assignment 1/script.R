library(dplyr)
library(wordcloud)
library(wordcloud2)
library(cld2) # For detecting languages
library(tm) # For text mining
library(RColorBrewer)
library(udpipe)
library(lattice)


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
          random.order = FALSE, colors = brewer.pal(8, "Dark2"))
wordcloud(fw_late$word, fw_late$freq, min.freq = 5, max.words = 100,
          random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale=c(2,.5))
wordcloud(fw_all$word, fw_all$freq, min.freq = 5, max.words = 100,
          random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale=c(2,.5))

#We create the wc using wordcloud2
wordcloud2(fw_early, 
           minRotation = 0, maxRotation = 0, color ='random-dark', size = 1, fontFamily ='Arial', ellipticity = 0.5, shape ='circle')
wordcloud2(fw_late,
           minRotation = 0, maxRotation = 0, color ='random-dark', size = 1, fontFamily ='Arial', ellipticity = 0.5, shape ='circle')
wordcloud2(fw_all,
           minRotation = 0, maxRotation = 0, color ='random-dark', size = 1, fontFamily ='Arial', ellipticity = 0.5, shape ='circle')

#Now we do the AKE
corpus <- corpus %>% mutate(MEDICATION_SUMMARY = tolower(MEDICATION_SUMMARY))
corpus_early <- corpus %>% filter(seer_stage_2 == 'early_stage')
corpus_late <- corpus %>% filter(seer_stage_2 == 'later_stage')

model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.4-190531.udpipe')

early <- udpipe_annotate(udmodel_english, corpus_early$MEDICATION_SUMMARY)
early_df <- data.frame(early)

stats_early <- keywords_rake(x = early_df, term = "lemma", group = "doc_id", 
                             relevant = early_df$upos %in% c("NOUN"))
stats_early$key <- factor(stats_early$keyword, levels = rev(stats_early$keyword))
barchart(key ~ rake, data = head(subset(stats_early, freq > 1), 20), col = "red", 
         main = "Early stage keywords identified by RAKE", 
         xlab = "Rake")

late <- udpipe_annotate(udmodel_english, corpus_late$MEDICATION_SUMMARY)
late_df <- data.frame(late)
stats_late <- keywords_rake(x = late_df, term = "lemma", group = "doc_id", 
                            relevant = late_df$upos %in% c("NOUN"))
stats_late$key <- factor(stats_late$keyword, levels = rev(stats_late$keyword))
barchart(key ~ rake, data = head(subset(stats_late, freq > 1), 20), col = "blue", 
         main = "Late stage keywords identified by RAKE", 
         xlab = "Rake")

all <- udpipe_annotate(udmodel_english, corpus$MEDICATION_SUMMARY)
all_df <- data.frame(all)
stats_all <- keywords_rake(x = all_df, term = "lemma", group = "doc_id", 
                           relevant = all_df$upos %in% c("NOUN"))
stats_all$key <- factor(stats_all$keyword, levels = rev(stats_all$keyword))
barchart(key ~ rake, data = head(subset(stats_all, freq > 1), 20), col = "grey", 
         main = "All cases keywords identified by RAKE", 
         xlab = "Rake")

#Loading the RAKE results for the WC
#rearly <- read.csv("./rake_early.csv")
#rlate <- read.csv("./rake_late.csv")
#rall <- read.csv("./rake_all.csv")

#We adapt the data
df.rearly <- stata_early #df.rearly <- data.frame(kword = rearly$keyword, freq = rearly$rake)
df.rlate <- stata_late #df.rlate <- data.frame(kword = rlate$keyword, freq = rlate$rake)
df.rall <- stata_all #df.rall <- data.frame(kword = rall$keyword, freq = rall$rake)

#We create the wc using wordcloud
wordcloud(df.rearly$kword, fw_late$freq, min.freq = 5, max.words = 100,
          random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale=c(2,.5))
wordcloud(df.rlate$kword, fw_early$freq, min.freq = 5, max.words = 100,
          random.order = FALSE, colors = brewer.pal(8, "Dark2"))
wordcloud(df.rall$kword, fw_all$freq, min.freq = 5, max.words = 100,
          random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale=c(2,.5))

#We create the wc using wordcloud2
wordcloud2(df.rearly,
           minRotation = 0, maxRotation = 0, color ='random-dark', size = 0.8, fontFamily ='Arial', ellipticity = 0.5, shape ='circle')
wordcloud2(df.rlate, 
           minRotation = 0, maxRotation = 0, color ='random-dark', size = 0.8, fontFamily ='Arial', ellipticity = 0.5, shape ='circle')
wordcloud2(df.rall,
           minRotation = 0, maxRotation = 0, color ='random-dark', size = 0.8, fontFamily ='Arial', ellipticity = 0.5, shape ='circle')
