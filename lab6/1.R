library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

# Read the text file
text <- readLines(file.choose())

# Create a text corpus
TextDoc <- Corpus(VectorSource(text))

# Define a content transformer to replace patterns with space
toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))

# Apply the content transformer to replace certain patterns
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
TextDoc <- tm_map(TextDoc, toSpace, "ˆa“")
TextDoc <- tm_map(TextDoc, toSpace, ":")
TextDoc <- tm_map(TextDoc, toSpace, ";")
TextDoc <- tm_map(TextDoc, toSpace, ",")
TextDoc <- tm_map(TextDoc, toSpace, "ˆITM")

# Convert text to lowercase
TextDoc <- tm_map(TextDoc, content_transformer(tolower))

# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)

# Remove common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))

# Remove specific words
TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team"))

# Remove punctuation
TextDoc <- tm_map(TextDoc, removePunctuation)

# Strip whitespace
TextDoc <- tm_map(TextDoc, stripWhitespace)

# Perform stemming
TextDoc <- tm_map(TextDoc, stemDocument)

# Custom replacements
TextDoc <- tm_map(TextDoc, content_transformer(function(x) gsub(pattern = "mathemat", replacement = "math", x)))
TextDoc <- tm_map(TextDoc, content_transformer(function(x) gsub(pattern = " r ", replacement = " Rlanguage ", x)))

# Create a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)

# Convert the matrix to a data frame of word frequencies
dtm_m <- as.matrix(TextDoc_dtm)
dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)

# Display the top 5 most frequent words
head(dtm_d, 5)

# Plot a bar chart of the top 20 most frequent words
barplot(dtm_d[1:20,]$freq, las = 2, names.arg = dtm_d[1:20,]$word,
        col = "lightgreen",
        main = "Top 20 most frequent words in the forms of knowledge assessment for the Big Data training courses",
        ylab = "Word frequencies")

# Generate a word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, scale = c(5, 0.5),
          min.freq = 1,
          max.words = 100, random.order = FALSE,
          rot.per = 0.40,
          colors = brewer.pal(8, "Dark2"))

# Find associations with specific terms
findAssocs(TextDoc_dtm, terms = c("program", "algorithm", "math", "statist"), corlimit = 0.5)
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 30), corlimit = 0.5)

# Perform sentiment analysis using different methods
syuzhet_vector <- get_sentiment(text, method = "syuzhet")
bing_vector <- get_sentiment(text, method = "bing")
afinn_vector <- get_sentiment(text, method = "afinn")

# Display the sentiment scores
head(syuzhet_vector)
summary(syuzhet_vector)
head(bing_vector)
summary(bing_vector)
head(afinn_vector)
summary(afinn_vector)

# Compare sentiment vectors
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

# Get NRC sentiment
d <- get_nrc_sentiment(as.vector(dtm_d$word))
head(d, 10)

# Aggregate sentiment scores
td <- data.frame(t(d))
td_new <- data.frame(rowSums(td[1:56]))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL

# Plot NRC sentiment distribution
td_new2 <- td_new[1:8,]
ggplot(td_new2, aes(x = sentiment, y = count, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Count") +
  ggtitle("Survey sentiments") +
  theme_minimal()

