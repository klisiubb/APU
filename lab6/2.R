# Załadowanie potrzebnych pakietów
library(tidytext)
library(igraph)
library(ggraph)
library(dplyr)
library(tidyr)
# Wczytanie tekstu z pliku
fileName <- "text.txt"
text <- readChar(fileName, file.info(fileName)$size)

# Konwersja tekstu do ramki danych
text_df <- data.frame(line = 1, text = text)
# Tokenizacja tekstu
tidy_text <- text_df %>%
  unnest_tokens(word, text)

# Ładowanie i modyfikowanie stop words
data(stop_words)

# Dodawanie słów do stop words
additional_stop_words <- data.frame(
  word = c("thy", "hath", "mar’d", "columbus", "christopher"),
  lexicon = "OLD_WORDS"
)
stop_words <- rbind(stop_words, additional_stop_words)

# Usuwanie stop words z tokenów
tidy_text <- tidy_text %>%
  anti_join(stop_words, by = "word")

# Liczenie częstotliwości słów
word_counts <- tidy_text %>%
  count(word, sort = TRUE)
print(word_counts)
# Tworzenie bigramów
text_bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# Liczenie częstotliwości bigramów
bigram_counts <- text_bigrams %>%
  count(bigram, sort = TRUE)
print(bigram_counts)

# Separacja bigramów na pojedyncze słowa
bigrams_separated <- text_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtracja bigramów z stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Liczenie częstotliwości bigramów po filtracji
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
print(bigram_counts)
# Łączenie bigramów
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# Tworzenie grafów dla wybranych bigramów
# Przykładowo, dla bigramów zawierających "columbus"
bigram_graph <- bigram_counts %>%
  filter(word1 == "columbus" | word2 == "columbus") %>%
  graph_from_data_frame()

# Definicja strzałek do grafów
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

# Wizualizacja grafu dla bigramów związanych z "columbus"
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                 arrow = a, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), position = "identity") +
  theme_void()
# Pierwszy poziom
bigram_graph1 <- bigram_counts %>%
  filter(word1 %in% c("columbus", "explorer", "voyage") | word2 %in% c("columbus", "explorer", "voyage"))

# Drugi poziom
bigram_graph2 <- bigram_counts %>%
  filter(word1 %in% bigram_graph1$word1 | word1 %in% bigram_graph1$word2 |
           word2 %in% bigram_graph1$word1 | word2 %in% bigram_graph1$word2)

# Trzeci poziom
bigram_graph3 <- bigram_counts %>%
  filter(word1 %in% bigram_graph2$word1 | word1 %in% bigram_graph2$word2 |
           word2 %in% bigram_graph2$word1 | word2 %in% bigram_graph2$word2)

# Wizualizacja grafów
ggraph(bigram_graph1 %>% graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                 arrow = a, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

ggraph(bigram_graph2 %>% graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                 arrow = a, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

ggraph(bigram_graph3 %>% graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                 arrow = a, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

