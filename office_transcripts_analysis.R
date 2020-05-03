library(tidyverse)
library(stringr)
library(tidytext)
library(plotly)
library(data.table)
library(visNetwork)
library(igraph)
library(sentimentr)
library(textstem)
library(topicmodels)

data <- readRDS("scraped_transcripts.RDS")
data$title <- as.character(data$title)
data$transcript <- as.character(data$transcript)
data$season <- as.integer(str_extract(data$title, "\\d{2}")) # season number is first 2 numbers
data$episode <- as.integer(str_extract(data$title, "(?<=x)\\d{2}")) # x preceeds episode numbers

table(data$season) # 19 is error caused by website
data <- data %>% filter(season != 19)

length(unique(data$name)) # 820 different people
data$name <- trimws(str_replace_all(data$name, "[^[:alpha:]]", " ")) # drop special chars, trim ws
length(unique(data$name)) # down to 771 diff people

agrep('michael', unique(data$name), max.distance = 2, value = T, ignore.case = T)
michael_typos <- c("Michel|Michael|MIchael|Micahel|Micael|Micheal|Michae|Michal|Mihael|Micael")
data$name <- str_replace_all(data$name, michael_typos, "Michael")

agrep('dwight', unique(data$name), max.distance = 2, value = T, ignore.case = T)
data$name <- str_replace_all(data$name, 'Dight', "Dwight")

agrep('phyllis', unique(data$name), max.distance = 2, value = T, ignore.case = T)
phyllis_typos <- c("Phylis|Phyliss")
data$name <- str_replace_all(data$name, phyllis_typos, "Phyllis")

agrep('darryl', unique(data$name), max.distance = 2, value = T, ignore.case = T)
darryl_typos <- c("Darry|Daryl|Darrly")
data$name <- str_replace_all(data$name, darryl_typos, "Darryl")
data$name <- str_replace_all(data$name, 'Darryll', "Darryl")

# EXPLORING DATA

# top 12 characters
# - focusing on most important people
# - facet grid can show 3 x 4 charts at once

data %>% group_by(name) %>% 
  summarize(count = n()) %>% 
  top_n(12) %>% 
  arrange(desc(count)) %>% 
  ggplot(aes(reorder(name, count), count)) +
  geom_col(show.legend = F, color = 'black', fill = 'skyblue', width = 2/3) +
  geom_text(aes(label = count), size = 3, position = position_stack(vjust = 0.5)) +
  labs(title = 'Overall # of lines per character, showing top 12',
       x = NULL, y = NULL) +
  coord_flip() +
  theme_bw()

top12 <- data %>% group_by(name) %>% 
  summarize(count = n()) %>% 
  top_n(12) %>% 
  arrange(desc(count))

nrow(data %>% filter(name %in% top12$name)) / nrow(data) # ~80% of lines come from top 12 characters


data %>% 
  group_by(season, name) %>% 
  summarize(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  group_by(season) %>% 
  top_n(12) %>% 
  ungroup() %>% 
  mutate(season = as.factor(season),
         name = as.factor(name),
         name = reorder_within(name, Count, season)) %>% 
  ggplot(aes(name, Count, fill = season)) +
  geom_col(show.legend = F) +
  labs(title = 'Stars of seasons: Michael 1 - 7, Andy takes #8, Dwight controls #9',
       subtitle = 'Showing # of lines per character, top 12 per season',
       x = NULL, y = NULL) +
  coord_flip() +
    scale_x_reordered() +
  facet_wrap(~season, scales = 'free') +
  theme_bw()


# TOKENIZATION
# data <- data %>% select(season, name, line)
data <- data %>% rename('text' = 'line')

# before unnestinf, remove text between spec chars like [] or (), those
# aren't spoken by anyone, those just indicate the environment, or some
# external influencers (over the phone, yelling, throws paper, etc...)

data$text <- trimws(str_replace_all(data$text, '\\[(.*?)\\]', ''))

# unnest and remove stopwords

data %>% 
  group_by(name) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')))) %>%  
  count(name, word, sort = T) %>% 
  filter(name %in% top12$name) %>% 
  group_by(name) %>% 
  top_n(10, wt = n) %>%
  ungroup() %>% 
  mutate(name = as.factor(name),
         word = reorder_within(word, n, name)) %>% 
  ggplot(aes(word, n, fill = name)) +
    geom_col(show.legend = F) +
    labs(title = 'Top words hint at who people converse with and who they are in a relationship with',
         subtitle = "Showing top 10 words by top 12 characters after removing usual stopwords + words like 'yeah', 'gonna', 'uh', etc...",
         x = NULL, y = NULL) +
    coord_flip() +
    scale_x_reordered() +
    facet_wrap(~name, scales = 'free') +
    theme_bw()


# network to prove top words used (hinting at who people converse with)

data_copy <- data
data_copy$to <- lead(data_copy$name)
data_copy <- dplyr::rename(data_copy, "from" = "name")
data_copy <- data_copy %>% filter((from %in% top12$name) & 
                                      (to %in% top12$name) &
                                      (to != from) )

data_for_nw_grouped <- data_copy %>%
  group_by(from, to) %>%
  summarize(back_and_forths = n()) %>%
  ungroup()

nodes_1 <- data.frame(id = (data_copy %>% group_by(from) %>% summarize(count = n()) %>% ungroup())$from,
                    label = (data_copy %>% group_by(from) %>% summarize(count = n()) %>% ungroup())$from,
                    title = (data_copy %>% group_by(from) %>% summarize(count = n()) %>% ungroup())$count,
                    value = (data_copy %>% group_by(from) %>% summarize(count = n()) %>% ungroup())$count,
                    font.size = 30)

edges_1 <- data.frame(from = data_for_nw_grouped$from,
                    to = data_for_nw_grouped$to,
                    value = data_for_nw_grouped$back_and_forths,
                    title = data_for_nw_grouped$back_and_forths)

visNetwork(nodes_1, edges_1, 
           main = 'The Michael, Dwight, Jim & Pam quartet is where most talking happens', 
           submain = "Conversation network to back up top words used by characters; node value represents lines spoken by one to other top 11 ",
           height = '450px', width = '600px') %>% 
  visOptions(highlightNearest = list(enabled = T, degree = 0,
                                     labelOnly = T, hover = T),nodesIdSelection = T) %>% 
  visIgraphLayout(layout = "layout_with_fr", randomSeed = 1000) %>%
  visEdges(color = list(color = "lightblue", highlight = "teal", hover = "teal"),
           hoverWidth = 5) %>% 
  visInteraction(hover = T) %>% 
  visNodes(color = list(highlight = "teal"))


# image = c("office_photos/andy.jpg",
#           "office_photos/Angela.jpg",
#           "office_photos/darryl.jpg",
#           "office_photos/dwight.jpg",
#           "office_photos/erin.jpg",
#           "office_photos/jim.png",
#           "office_photos/kevin.jpg",
#           "office_photos/michael.jpg",
#           "office_photos/oscar.jpg",
#           "office_photos/pam.jpg",
#           "office_photos/phyills.jpg",
#           "office_photos/ryan.jpg")
# shapeProperties = list(useBorderWithImage = TRUE), 
# brokenImage="office_photos/andy.jpg",

data %>% 
  group_by(name) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')),
                      data.frame(word = c(tolower(unique(data$name)), 'tuna', "andy's")))) %>%  
  count(name, word, sort = T) %>% 
  filter(name %in% top12$name) %>% 
  group_by(name) %>% 
  top_n(10, wt = n) %>%
  ungroup() %>% 
  mutate(name = as.factor(name),
         word = reorder_within(word, n, name)) %>% 
  ggplot(aes(word, n, fill = name)) +
  geom_col(show.legend = F) +
  labs(title = 'With first names also taken out, top words are representative of personalities',
       subtitle = "Such as Angela's cats, Dwight's love for paper, Oscar's sexuality, Kevin's 'nice' & Ryan's business mindset",
       x = NULL, y = NULL) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~name, scales = 'free') +
  theme_bw()


# top words wordcloud

top30 <- data %>% group_by(name) %>% 
  summarize(count = n()) %>% 
  top_n(30) %>% 
  arrange(desc(count))

words <- data %>% 
  group_by(name) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')))) %>%  
  count(name, word, sort = T) %>% 
  filter(name %in% top30$name)


library(wordcloud)

wordcloud(words = (words %>% filter(name == 'Michael'))$word,
          freq = (words %>% filter(name == 'Michael'))$n, 
          min.freq = 10, 
          max.words = 20, colors = brewer.pal(10, 'OrRd'))

# tf-idf on words by characters

data %>% 
  group_by(name) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  mutate(word = lemmatize_words(word)) %>% 
  count(name, word, sort = T) %>% 
  ungroup() %>% 
  filter(name %in% top12$name) %>% 
  bind_tf_idf(term = word, document = name, n = n) %>% 
  group_by(name) %>% 
  top_n(8, wt = tf_idf) %>%
  ungroup() %>% 
  mutate(name = as.factor(name),
         word = reorder_within(word, tf_idf, name)) %>% 
  ggplot(aes(word, tf_idf, fill = name)) +
  geom_col(show.legend = F) +
  labs(title = "Nard (dog), Sprinkles, Mose, Dundie(s), Cece and WUPHF are all found by tf-idf! Amazing tool!",
       subtitle = "Running tf-idf analysis on all words spoken by top 12 characters",
       x = NULL, y = NULL) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~name, scales = 'free') +
  theme_bw()


# top bigrams by character

my_stops <- bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')))

data %>% 
  group_by(name) %>% 
  unnest_tokens(bigram, text, token = 'ngrams', n = 2) %>% 
  ungroup() %>% 
  separate(bigram, c('word_1', 'word_2'), sep = ' ') %>%
  filter((!word_1 %in% my_stops$word) & 
           (!word_2 %in% my_stops$word) & (word_1 != word_2)) %>% 
  unite(bigram, word_1, word_2, sep = ' ') %>% 
  filter(name %in% top12$name) %>% 
  count(name, bigram, sort = T) %>% 
  group_by(name) %>% 
  top_n(6, wt = n) %>% 
  ungroup() %>% 
  mutate(name = as.factor(name),
         bigram = reorder_within(bigram, n, name)) %>% 
  ggplot(aes(bigram, n, fill = name)) +
  geom_col(show.legend = F) +
  labs(title = "'Nard dog', 'Dunder Mifflin', 'Scrute Farms' and 'Party Planning (committee)' appear",
       subtitle = 'Showing top 6-10 bigrams (due to ties) by top 12 characters',
       x = NULL, y = NULL) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~name, scales = 'free') +
  theme_bw()


# vocab by person by season

# TODO



# SENTIMENTS

# sentiments # neg / pos (get_sentiments(lexicon = 'bing'))
# get_sentiments(lexicon = 'afinn') # scrores
# get_sentiments(lexicon = 'nrc') # feelings
# get_sentiments(lexicon = 'loughran') # pos / neg

# pos-neg
data %>% 
  group_by(name) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  mutate(word = lemmatize_words(word)) %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')),
                      data.frame(word = c(tolower(unique(data$name)), 'tuna', "andy's")))) %>% 
  inner_join(sentiments) %>% 
  count(name, word, sentiment, sort = T) %>% 
  ungroup() %>% 
  filter(name %in% top12$name) %>% 
  group_by(name, sentiment) %>% 
  top_n(5, wt = n) %>% 
  ungroup() %>% 
  mutate(n = ifelse(sentiment == 'positive', n, -n)) %>% 
  mutate(name = as.factor(name),
         word = reorder_within(word, n, name)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = F) +
  labs(title = 'Top positive words are used more frequently on average than top negative words',
       subtitle = "Top positive and negative words by each character",
       x = NULL, y = NULL) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~name, scales = 'free') +
  theme_bw()


# using afinn and multiplying by count
data %>% 
  group_by(name) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  mutate(word = lemmatize_words(word)) %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')),
                      data.frame(word = c(tolower(unique(data$name)), 'tuna', "andy's")))) %>% 
  count(name, word, sort = T) %>% 
  ungroup() %>% 
  filter(name %in% top12$name) %>% 
  inner_join(get_sentiments(lexicon = 'afinn')) %>% 
  mutate(sentiment_value = n * value) %>% 
  group_by(name) %>% 
  top_n(10, wt = abs(sentiment_value)) %>% 
  ungroup() %>% 
  mutate(sentiment = ifelse(value < 0, 'negative', 'positive'),
         name = as.factor(name),
         word = reorder_within(word, sentiment_value, name)) %>% 
  ggplot(aes(word, sentiment_value, fill = sentiment)) +
  geom_col(show.legend = F) +
  labs(title = "Angela, Darryl & Dwight seem to 'contribute' most negativity to their languages",
       subtitle = "Using AFINN scores and multiplying them by word counts by person",
       x = NULL, y = 'Contributed sentiment score (score * count)') +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~name, scales = 'free') +
  theme_bw()



data_with_sentiments <- data %>%
  group_by(title) %>% 
  mutate(episode_num = group_indices()) %>% 
  ungroup() %>% 
  select(season, episode, episode_num, name, text) %>% 
  mutate(sentiment = sentimentr::sentiment_by(text)$ave_sentiment)

data_with_sentiments %>% 
  filter((sentiment != 0) & (name %in% top12$name)) %>% 
  group_by(episode_num, name) %>% 
  filter((sentiment != min(sentiment)) & (sentiment != max(sentiment))) %>% # taking the min and max out of all episodes (usually errors, like whoa whoa whoa etc...)
  summarize(min_sentiment = min(sentiment),
            max_sentiment = max(sentiment),
            avg_sentiment = mean(sentiment)) %>% # least and most positive episodes taken after minmax deletions
  ungroup() %>% 
  mutate(name = as.factor(name)) %>% 
  ggplot(aes(x = episode_num), fill = 'blue', color = 'blue') +
  geom_line(aes(y = avg_sentiment), show.legend = F, alpha = 0.5) +
  geom_ribbon(aes(ymin = min_sentiment, ymax = max_sentiment), fill = 'skyblue', alpha = 1/3, show.legend = F, color = NA) +
  labs(title = "Sentiment trend analysis by person by episode does not offer insights: lots of volatility",
       subtitle = "Sentiments come from each line, aggregated on episode level; blue range represents minimum and maximum values per episode",
       x = '# of episode', y = NULL) +
  facet_wrap(~name) +
  theme_bw()


data_with_sentiments %>% 
  filter((sentiment != 0) & (name %in% top12$name)) %>% 
  group_by(season, name) %>% 
  summarize(median_sentiment = median(sentiment)) %>% # least and most positive episodes taken after minmax deletions
  ungroup() %>% 
  mutate(name = as.factor(name)) %>% 
  ggplot(aes(x = season), fill = 'blue', color = 'blue') +
  geom_line(aes(y = median_sentiment), show.legend = F, alpha = 0.5) +
  geom_point(aes(y = median_sentiment), show.legend = F, color = 'blue', fill = 'blue') +
  labs(title = "By-season trend may hint at Andy's firing, but no more insightful than by-episode trend",
       subtitle = "Sentiments come from each line, aggregated on season level",
       x = '# of season', y = NULL) +
  scale_x_continuous(breaks = seq(1, 9, by = 1)) +
  facet_wrap(~name) +
  theme_bw()


# sentiment when talking to one another

convo_sentiment <- data %>% 
  select(name, text) %>% 
  mutate(to = lead(name)) %>% 
  group_by(name) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')),
                      data.frame(word = c(tolower(unique(data$name)), 'tuna', "andy's")))) %>%
  filter((name %in% top12$name) & (to %in% top12$name) & (name != to)) %>% 
  count(name, to, word, sort = T) %>% 
  ungroup() %>% 
  inner_join(get_sentiments(lexicon = 'afinn')) %>% 
  mutate(sentiment_value = n * value) %>% 
  group_by(name, to) %>% 
  summarize(sentiment_score = sum(sentiment_value)) %>% 
  ungroup() 

convo_sentiment %>% 
  mutate(from = as.factor(name),
         to = as.factor(to),
         to = reorder_within(to, sentiment_score, from),
         sentiment = ifelse(sentiment_score < 0, 'negative', 'positive')) %>% 
  ggplot(aes(to, sentiment_score, fill = sentiment, color = sentiment)) +
  geom_col(show.legend = F, width = 2/3) +
  labs(title = "Pam & Jim are mutually very nice to one other, Dwight-to-Jim is negative, Jim-to-Dwight is positive",
       subtitle = "Using AFINN scores to calculate sentiments of conversations; Box labels represent the 'from', y axis the 'to'",
       x = 'Talking to', y = 'Contributed sentiment score (score * count)') +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~name, scales = 'free') +
  theme_bw()

# angela nicest to dwight
# andy very nice to angela
# jim nice to everyone

# network on convo sentiments

convo_count <- data %>% 
  select(name, text) %>% 
  mutate(to = lead(name)) %>% 
  group_by(name) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')),
                      data.frame(word = c(tolower(unique(data$name)), 'tuna', "andy's")))) %>%
  filter((name %in% top12$name) & (to %in% top12$name) & (name != to)) %>% 
  group_by(name, to) %>% 
  summarize(count = n()) %>% 
  ungroup()

convo <- convo_sentiment %>% inner_join(convo_count, by = c('name', 'to'))

nodes_2 <- data.frame(id = (convo %>% group_by(name) %>% summarize(count = sum(count)) %>% ungroup())$name,
                    label = (convo %>% group_by(name) %>% summarize(count = sum(count)) %>% ungroup())$name,
                    title = (convo %>% group_by(name) %>% summarize(count = sum(count)) %>% ungroup())$count,
                    value = (convo %>% group_by(name) %>% summarize(count = sum(count)) %>% ungroup())$count,
                    font.size = 30)

# all relationships
edges_2 <- data.frame(from = convo$name,
                    to = convo$to,
                    title = convo$sentiment_score,
                    color = ifelse(convo$sentiment_score < 0, 'red', 'lightgreen'),
                    physics = T,
                    arrows = c('to'),
                    smooth = T)

visNetwork(nodes_2, edges_2, 
           main = "Everyone's nice to Darryl, Jim's nice to everyone, Oscar & Dwight are quite mean", 
           submain = 'Sentiments between top characters - by sign; node # represents meaningful unqiue words spoken',
           footer = 'Select a node to focus on one person only',
           height = '400px') %>% 
  visOptions(highlightNearest = list(enabled = T, degree = 0,
                                     labelOnly = T, hover = T), 
             nodesIdSelection = T) %>% 
  visIgraphLayout(layout = "layout_with_fr", randomSeed = 1000) %>%
  visEdges(arrowStrikethrough = F,
           arrows =list(to = list(enabled = T, scaleFactor = 1))) %>% 
  visInteraction(hover = T)



# ggplot(convo, aes(sentiment_score)) + 
#   geom_histogram(binwidth = 5, color = 'blue', fill = 'skyblue') +
#   theme_bw() + 
#   labs(title = 'Distribution of total sentiment scores')

ggplot(convo, aes(reorder(name, sentiment_score), sentiment_score)) + 
  geom_boxplot(show.legend = F, alpha = 0.75, outlier.stroke = T,
               fill = 'gray', color = 'black') +
  geom_hline(yintercept = median(convo$sentiment_score), size = 1) +
  theme_bw() + 
  labs(title = 'Distribution of total sentiment scores by person',
       subtitle = 'Vertical line represent median sentiment score') +
  scale_y_continuous(breaks = seq(-130, 350, 20)) +
  coord_flip()


# strength
edges_3 <- data.frame(from = convo$name,
                    to = convo$to,
                    value = convo$sentiment_score,
                    title = convo$sentiment_score,
                    color = ifelse(convo$sentiment_score < 0, 'red', 'lightgreen'),
                    hidden = ifelse(convo$sentiment_score > -10 & 
                                      convo$sentiment_score < 30, T, F),
                    physics = T,
                    arrows = c('to'),
                    smooth = T)

visNetwork(nodes_2, edges_3, 
           main = "Jim & Pam clearly have the most positive relationship", 
           submain = 'Showing most negative and most positive relationships; node # represents meaningful unqiue words spoken',
           footer = 'Select a node to focus on one person only',
           height = '400px') %>% 
  visOptions(highlightNearest = list(enabled = T, degree = 0,
                                     labelOnly = T, hover = T), 
             nodesIdSelection = T) %>% 
  visIgraphLayout(layout = "layout_with_fr", randomSeed = 1000) %>%
  visEdges(arrowStrikethrough = T,
           arrows =list(to = list(enabled = T, scaleFactor = 1))) %>% 
  visInteraction(hover = T)


# dwight and jim sentiment sum by season
JimDwight <- data %>% 
  select(season, name, text) %>% 
  mutate(to = lead(name)) %>% 
  group_by(name) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')),
                      data.frame(word = c(tolower(unique(data$name)), 'tuna', "andy's")))) %>%
  filter((name %in% c('Jim', 'Dwight')) & (to %in% c('Jim', 'Dwight')) & (name != to)) %>%
  count(season, name, to, word) %>% 
  ungroup() %>% 
  inner_join(get_sentiments(lexicon = 'afinn')) %>% 
  mutate(sentiment_value = n * value) %>% 
  group_by(season, name, to) %>% 
  summarize(sentiment_score = sum(sentiment_value)) %>% 
  ungroup() %>% 
  mutate(conversation = paste0(name, " to ", to))


ggplot(JimDwight, aes(season, sentiment_score, color = conversation, fill = conversation)) + 
  geom_line(size = 1) + 
  geom_point(size = 1.75) +
  theme_bw() + 
  labs(subtitle = "Jim and Dwight's relationship through the 9 seasons; sentiment score by summing up AFINN scores from words",
       title = "Ending on a high note after ups and downs... Dwight nicest to Jim in last season (from AFINN sentiments)",
       x = 'season',
       y = 'total sentiment score') +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  theme(legend.key = element_blank(),
        legend.background=element_blank(),
        legend.position=c(0.8, 0.85))


cor((JimDwight %>% filter(conversation == 'Dwight to Jim'))$sentiment_score,
    (JimDwight %>% filter(conversation == 'Jim to Dwight'))$sentiment_score)


# instead of AFINN by word, sentimentR by line and aggregate on season level
# stil Jim - Dwight

JimDwight_byline <- data %>% 
  select(season, name, text) %>% 
  mutate(to = lead(name)) %>% 
  filter((name %in% c('Jim', 'Dwight')) & 
           (to %in% c('Jim', 'Dwight')) &
           (to != name)) %>% 
  mutate(sentiment = sentimentr::sentiment_by(text)$ave_sentiment) %>% 
  filter((sentiment != 0)) %>% 
  select(season, name, to, sentiment)

JimDwight_byline %>% 
  group_by(season, name, to) %>% 
  summarize(ave_sent = mean(sentiment),
            sum_sent = sum(sentiment)) %>% 
  mutate(conversation = paste0(name, " to ", to)) %>% 
  ggplot(aes(season, sum_sent, color = conversation, fill = conversation)) + 
  geom_line(size = 1) + 
  geom_point(size = 1.75) +
  theme_bw() + 
  labs(subtitle = "Jim and Dwight's relationship through the 9 seasons; sentiment score by summing up sentimentR scores on lines",
       title = "Ending on a high note after ups and downs... Dwight nicest to Jim in last season (from sentimentR)",
       x = 'season',
       y = 'total sentiment score') +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  theme(legend.key = element_blank(),
        legend.background=element_blank(),
        legend.position=c(0.85, 0.15))



# topics modeling

m <- data %>% 
  group_by(name) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  filter(name %in% top12$name) %>% 
  select(name, word) %>% 
  mutate(word = lemmatize_words(word)) %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')),
                      data.frame(word = c(tolower(unique(data$name)), 'tuna', "andy's")))) %>% 
  count(name, word, sort = T) %>% 
  ungroup()

m_dtm <- m %>% cast_dtm(name, word, n)


# 12 clusters 

m_lda <- m_dtm %>% LDA(k = 12, control = list(seed = 8080))

m_lda_gammas <- tidy(m_lda, matrix = 'gamma')

m_lda_gammas %>%  
  rename('name' = 'document') %>% 
  mutate(topic = as.factor(topic),
         name = as.factor(name)) %>% 
  ggplot(aes(topic, gamma, fill = name)) + 
  geom_point(show.legend = F, color = 'black', shape = 8) +
  facet_wrap(~name, scales = 'free') + 
  labs(title = "Only Michael & Dwight have 'multiple vocabularies'",
       subtitle = 'LDA clustering outcome: some people use very similar language (i.e.: Angela, Oscar & Ryan)',
       x = '12 topics (clusters) from LDA algo',
       y = '% of being assigned to one cluster') +
  theme_bw()


# limited # of topics

lda <- m_dtm %>% LDA(k = 3, control = list(seed = 123))

lda_betas <- tidy(lda, matrix = 'beta')

lda_betas %>% 
  group_by(topic) %>% 
  top_n(30, beta) %>% 
  ungroup() %>% 
  mutate(term = as.factor(term),
         term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = F, width = 2/3) +
  facet_wrap(~topic, scales = 'free') + 
  coord_flip() + 
  scale_x_reordered() +
  theme_bw()


lda_gammas <- tidy(lda, matrix = 'gamma')

lda_gammas %>%  
  rename('name' = 'document') %>% 
  mutate(topic = as.factor(topic),
         name = as.factor(name)) %>% 
  ggplot(aes(topic, gamma, fill = name)) + 
  geom_point(show.legend = F, color = 'black', shape = 8) +
  facet_wrap(~name, scales = 'free') + 
  labs(title = "Only Michael & Dwight have 'multiple vocabularies'",
       subtitle = 'LDA clustering outcome: some people use very similar language (i.e.: Angela, Oscar & Ryan)',
       x = '12 topics (clusters) from LDA algo',
       y = '% of being assigned to one cluster') +
  theme_bw()


# only dwight
# has multiple topics (Job, College, Lovelife, etc...)


person <- 'Holly'

x <- data %>% 
  group_by(name) %>% 
  unnest_tokens(word, text) %>% 
  ungroup() %>% 
  filter(name == person) %>% 
  select(name, word) %>% 
  mutate(word = lemmatize_words(word)) %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')),
                      data.frame(word = c(tolower(unique(data$name)), 'tuna', "andy's")))) %>% 
  count(name, word, sort = T) %>% 
  ungroup()

x_dtm <- x %>% cast_dtm(name, word, n)

x_lda <- x_dtm %>% LDA(k = 2, control = list(seed = 123))

x_lda_betas <- tidy(x_lda, matrix = 'beta')

x_lda_betas %>% 
  mutate(topic = paste0('topic_', topic)) %>% 
  spread(topic, beta) %>% 
  mutate(log_ratio = log2(topic_2 / topic_1),
         pos_neg = ifelse(log_ratio < 0, 'neg', 'pos')) %>% 
  group_by(pos_neg) %>% 
  top_n(15, abs(log_ratio)) %>% 
  ungroup() %>% 
  mutate(term = as.factor(term),
         term = reorder(term, log_ratio)) %>% 
  ggplot(aes(term, log_ratio, fill = pos_neg, color = pos_neg)) + 
    geom_col(show.legend = F, width = 2/3) +
    coord_flip() +
  labs(title = 'Largest beta-differences somewhat outline two topics',
       subtitle = paste0('Running LDA on ', person, "'s words"),
       x = 'Top terms',
       y = 'Log-ratio') +
  theme_bw()

x_lda_betas %>% 
  group_by(topic) %>% 
  top_n(30, beta) %>% 
  ungroup() %>% 
  mutate(term = as.factor(term),
         term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = F, width = 2/3) +
  facet_wrap(~topic, scales = 'free') + 
  coord_flip() + 
  scale_x_reordered() +
  labs(title = paste0('Top words for 2 clusters created for ', person),
       subtitle = paste0('Running LDA on ', person, "'s words"),
       x = 'Top terms',
       y = 'Beta') +
  theme_bw()
  
  

