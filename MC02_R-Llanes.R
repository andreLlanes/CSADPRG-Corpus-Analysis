library(stringr) 
library(dplyr)    
library(tidytext)
library(ggplot2)
library(lubridate)
library(wordcloud)
library(tidyr)

data_loading <- function (file_path, column_name){
  
  data <- read.csv(file_path, encoding = "UTF-8")
  concatenated_columns <- paste(data[[column_name]], collapse = " ")
  return(concatenated_columns)
}

info <- data_loading("fake_tweets.csv", "text")

# REQUIREMENT 1: Word Count
word_count <- function(param){
  words <- strsplit(param, " ")[[1]]
  
  return (length(words))
}

# REQUIREMENT 2: Vocabulary Size
vocab_size <- function(param){
  words <- unlist(strsplit(param, " "))
  words <- tolower(words[words != ""])
  
  return (length (unique(words)))
}

# REQUIREMENT 3: Word Frequency
word_freq_df <- function(param){
  words <- unlist(strsplit(param, " "))
  words <- tolower(words[words != ""])
  count <- table(words)           
  count <- sort(count, decreasing = TRUE)
  
  word_df <- as.data.frame(count, stringsAsFactors = FALSE)
  colnames(word_df) <- c("Word", "Count")
  return (word_df)
}


# WordCLoud function 1.1
word_freq <- function(param){
  words <- unlist(strsplit(param, " "))
  words <- tolower(words[words != ""])
  count <- table(words)           
  count <- sort(count, decreasing = TRUE)
  return (count)
}

# WordCloud function 1.2
frequency_wordcloud <- function(param){
  cloud <- word_freq(param)
  twenty <- head(cloud, 20)
  x11()
  wordcloud(names(twenty), freq = twenty, max.words = 20, random.order = FALSE, colors = "purple")
}

# REQUIREMENT 4: Character Frequency
ch_freq <- function(param){
  characterz <- unlist(strsplit(param, ""))
  characterz <- tolower(characterz[characterz != " "])
  charFreq <- table(characterz)
  charFreq <- sort(charFreq, decreasing = TRUE)
  
  char_df <- as.data.frame(charFreq, stringsAsFactors = FALSE)
  colnames(char_df) <- c("Character", "Count")
  return (char_df)
}

# REQUIREMENT 5: Top 20 Words
top_20 <- function(param){
  top_20_freq <- word_freq(param)
  top_20_df <- as.data.frame(top_20_freq, stringsAsFactors = FALSE)
  colnames(top_20_df) <- c("Word", "Count")
  return (head(top_20_df, 20))
}

# REQUIREMENT 6: Stop Word Frequency
count_words_in_text <- function(file_path, words) {
  data5 <- read.csv(file_path)
  
  data5$text <- tolower(data5$text)
  words <- tolower(words)
  
  word_counts <- sapply(words, function(word) {
    sum(str_count(data5$text, paste0("\\b", word, "\\b")))
  })
  
  word_counts_df <- as.data.frame(cbind(words, word_counts))
  colnames(word_counts_df) <- c("Character", "Count")

  return(word_counts_df)
}

# Bar Chart
data1 <- read.csv("fake_tweets.csv", header = TRUE)
head(data1$date_created) 
data1$date_created <- parse_date_time(data1$date_created, orders = "ymd HMS")
head(data1$date_created)

data1$month <- month(data1$date_created, label = TRUE, abbr = TRUE)
head(data1$month)

monthly_counts <- data1 %>%
  group_by(month) %>%
  summarise(total_posts = n(), .groups = "drop")

windows()
ggplot(monthly_counts, aes(x = month, y = total_posts)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Monthly Posts", x = "Month", y = "no. of posts") +
  theme_minimal()


# PIE CHART
data2 <- read.csv("fake_tweets.csv", header = TRUE)


# Count symbols and calculate percentages
non_word_count <- data2 %>%
  select(text) %>%
  summarise(all_text = paste(text, collapse = "")) %>%
  mutate(symbols = str_extract_all(all_text, "[^A-Za-z0-9\\s]")) %>%
  unnest(symbols) %>%
  count(symbols) %>%
  mutate(percentage = round(n / sum(n) * 100, 1),  # Calculate and round percentage
         symbol_label = paste0(symbols, " (", percentage, "%)"))  # Create label

# Plot without displaying percentages on the chart
windows()
ggplot(non_word_count, aes(x = "", y = n, fill = symbol_label)) + 
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Symbols in Text Corpus", fill = "Symbols and Percentages") +
  scale_fill_brewer(palette = "Set3") + 
  theme_void()


# Corpus Analysis 1
cat("Word Count: ", word_count(info), "\n")

# Corpus Analysis 2
cat("Vocabulary Size: ", vocab_size(info), "\n")

# Corpus Analysis 3
word_dataF <- word_freq_df(info)
print (word_dataF)

# Corpus Analysis 4
ch_dataF <- ch_freq(info)
print (ch_dataF)

# Data Visualization 1
frequency_wordcloud(info)

# Corpus Analysis 5
top_df <- top_20(info)
print (top_df)

# Corpus Analysis 6
words_to_count <- c("of", "so", "but", "or", "them", "to", "have", "what", "and", "in")
stop_words_fq <- count_words_in_text("fake_tweets.csv", words_to_count)
print(stop_words_fq)

# REQUIREMENT 6 CORPUS ANALYSIS 6
new_info <- read.csv("fake_tweets.csv")
text <- new_info$text

stop_list <- c("of", "so", "but", "or", "them", "to", "have", "what", "and", "in")
for (i in 1:length(text)){
  
  row_text <- text[i]
  words <- unlist(strsplit(row_text, "\\s+"))
  without_stop <- words[!words %in% stop_list]
  
  print(paste(without_stop, collapse = " "))
  
}



