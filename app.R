library(shiny)
library(shinydashboard)
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
library(wordcloud)


# CODE

data <- readRDS("www/scraped_transcripts.RDS")
data$title <- as.character(data$title)
data$transcript <- as.character(data$transcript)
data$season <- as.integer(str_extract(data$title, "\\d{2}")) # season number is first 2 numbers
data$episode <- as.integer(str_extract(data$title, "(?<=x)\\d{2}")) # x preceeds episode numbers

data <- data %>% rename('text' = 'line')

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


top12 <- data %>% group_by(name) %>% 
    summarize(count = n()) %>% 
    top_n(12) %>% 
    arrange(desc(count))


# before unnestinf, remove text between [], those
# aren't spoken by anyone, those just indicate the environment, or some
# external influencers (over the phone, yelling, throws paper, etc...)

data$text <- trimws(str_replace_all(data$text, '\\[(.*?)\\]', ''))

# for network1

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

# for wordcloud selection

top30 <- data %>% group_by(name) %>% 
    summarize(count = n()) %>% 
    top_n(30) %>% 
    arrange(desc(count))

without_names <- data %>% 
    group_by(name) %>% 
    unnest_tokens(word, text) %>% 
    ungroup() %>% 
    anti_join(bind_rows(data.frame(word = stop_words$word), 
                        data.frame(word = stopwords::stopwords(source = 'smart')),
                        data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')),
                        data.frame(word = c(tolower(unique(data$name)), 'tuna', "andy's")))) %>%  
    count(name, word, sort = T) %>% 
    filter(name %in% top30$name)

with_names <- data %>% 
    group_by(name) %>% 
    unnest_tokens(word, text) %>% 
    ungroup() %>% 
    anti_join(bind_rows(data.frame(word = stop_words$word), 
                        data.frame(word = stopwords::stopwords(source = 'smart')),
                        data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')))) %>%  
    count(name, word, sort = T) %>% 
    filter(name %in% top30$name)


# for bigrams

my_stops <- bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')))

# for sentiment trend

data_with_sentiments <- data %>%
    group_by(title) %>% 
    mutate(episode_num = group_indices()) %>% 
    ungroup() %>% 
    select(season, episode, episode_num, name, text) %>% 
    mutate(sentiment = sentimentr::sentiment_by(text)$ave_sentiment)


# for analyzing sentiment when talking to one another

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


# for network 2 (talking to each other - all relationships)

nodes_2 <- data.frame(id = (convo %>% group_by(name) %>% summarize(count = sum(count)) %>% ungroup())$name,
                      label = (convo %>% group_by(name) %>% summarize(count = sum(count)) %>% ungroup())$name,
                      title = (convo %>% group_by(name) %>% summarize(count = sum(count)) %>% ungroup())$count,
                      value = (convo %>% group_by(name) %>% summarize(count = sum(count)) %>% ungroup())$count,
                      font.size = 30)

edges_2 <- data.frame(from = convo$name,
                      to = convo$to,
                      title = convo$sentiment_score,
                      color = ifelse(convo$sentiment_score < 0, 'red', 'lightgreen'),
                      physics = T,
                      arrows = c('to'),
                      smooth = T)


# for network 3 (talking to each other - top relationships with edge weights)

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


# for Dwight vs Jim

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


JimDwight_byline <- data %>% 
    select(season, name, text) %>% 
    mutate(to = lead(name)) %>% 
    filter((name %in% c('Jim', 'Dwight')) & 
               (to %in% c('Jim', 'Dwight')) &
               (to != name)) %>% 
    mutate(sentiment = sentimentr::sentiment_by(text)$ave_sentiment) %>% 
    filter((sentiment != 0)) %>% 
    select(season, name, to, sentiment)


# for LDA

# 12 clusters

words_top_12 <- data %>% 
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

words_top_12_dtm <- words_top_12 %>% cast_dtm(name, word, n)

words_top_12_dtm_lda <- words_top_12_dtm %>% LDA(k = 12, control = list(seed = 8080))

words_top_12_dtm_lda_gammas <- tidy(words_top_12_dtm_lda, matrix = 'gamma')

# for summary

summary <- read.csv("www/summary_table.csv")
summary <- summary %>% mutate_all(as.character)
colnames(summary)[1] <- '#'




# SHINY


ui <- dashboardPage(title = 'Text Analysis on The Office',
                    skin = "green",
    
    dashboardHeader(title = 'NLP on The Office series',
                    titleWidth = '300px'),
    
    dashboardSidebar(width = '300px',
                     sidebarMenu(menuItem("Motivation and hypothesis", 
                                          tabName = 'motivation',
                                          icon = icon('lightbulb'))),
                     sidebarMenu(menuItem("Exploring the dataset", 
                                          tabName = 'explore',
                                          icon = icon('search'))),
                     sidebarMenu(menuItem("Tokens", 
                                          tabName = 'tokens',
                                          icon = icon('file-word'))),
                     sidebarMenu(menuItem("TF-IDF", 
                                          tabName = 'tfidf',
                                          icon = icon('exclamation'))),
                     sidebarMenu(menuItem("Bigrams", 
                                          tabName = 'bigrams',
                                          icon = icon('comments'))),
                     sidebarMenu(menuItem("Top positive and negative words", 
                                          tabName = 'posneg',
                                          icon = icon('theater-masks'))),
                     sidebarMenu(menuItem("Sentiment with seasonal trend", 
                                          tabName = 'trend_sent',
                                          icon = icon('history'))),
                     sidebarMenu(menuItem("Sentiments between all characters", 
                                          tabName = 'between_sent',
                                          icon = icon('project-diagram'))),
                     sidebarMenu(menuItem("Jim & Dwight's sentiment trend", 
                                          tabName = 'jd',
                                          icon = icon('handshake'))),
                     sidebarMenu(menuItem("Topic modeling with LDA", 
                                          tabName = 'lda',
                                          icon = icon('chalkboard-teacher'))),
                     sidebarMenu(menuItem("Summary on hypotheses, takeaways", 
                                          tabName = 'summary',
                                          icon = icon('bullseye')))),
    
    dashboardBody(tabItems(tabItem(tabName = "motivation",
                                   fluidRow(div(h2('Why I chose this project'), align = 'center'),
                                            br()),
                                   fluidRow(column(width = 6,
                                                   div(img(src = 'https://cdn3.whatculture.com/images/2018/11/e46c3d8de0c01966-600x338.jpg', height = 275, align = 'center'))),
                                            column(width = 6,
                                                   div(h3("It's my favorite show!"), align = "center"),
                                                   br(),
                                                   h5("I'm really looking forward to conduct my first large text analysis project on my favorite show. After watching the whole series almost 3 times I'm quite familiar with all characters, lines, stories, word usages, etc.... I'm interested to see if data can back up what I already know of the show, i.e.: there are clearly more negative (Angela) and positive (Jim) people in the show. There are some words that clearly 'belong' to a given character (such as beet farming to Dwight)."))),
                                   fluidRow(column(width = 4,
                                                   h3("What I'll look at"),
                                                   br(),
                                                   tags$ol(tags$li('Top words and bigrams used by each character'),
                                                           tags$li('Unique-to-person words'),
                                                           tags$li('Sentiment analysis by characters'),
                                                           tags$li('Sentiment analysis between characters'),
                                                           tags$li('Sentiment-trend analysis'),
                                                           tags$li('Topics'))),
                                            column(width = 4,
                                                   h3("What my hypotheses are"),
                                                   br(),
                                                   tags$ol(tags$li("Hobbies and personal activities can be extracted by tf-idf"),
                                                           tags$li("Sentiment trend follows characters' happiness and sadness (falling in love, breaking up, getting fired, etc...)"),
                                                           tags$li("Angela is the meanest (most negative) person BUT nicest to Dwight..."),
                                                           tags$li("Jim and Pam's relationship is most mutually positive"),
                                                           tags$li("Jim and Dwight's relationship got better towards the end of the show"),
                                                           tags$li("LDA can cluster topics into two groups (business and personal / fun)"))),
                                            column(width = 4,
                                                   h3("What I'll learn as a data scientist"),
                                                   br(),
                                                   tags$ul(tags$li("Tokenization (single and multi-gram)"),
                                                           tags$li("Extracting most unique-to-document words"),
                                                           tags$li("Leveraging multiple stop-word vocabularies"),
                                                           tags$li("Levaraging multiple sentiment scoring dictionaries"),
                                                           tags$li("Familiarizing myself with the LDA ML algo for topic extraction"),
                                                           tags$li("R libraries (tidytext, stringr, visNetwork, sentimentR, textstem, topicmodels)"),
                                                           tags$li("Regular Expressions"),
                                                           tags$li("First every Shiny app!"))))),
                           
                           
                           tabItem(tabName = "explore",
                                   fluidRow(div(h2("Before jumping in to NLP, let's spend some time exploring the dataset"), align = 'center'),
                                            column(width = 6,
                                                   br(),
                                                   h5('First of all, it all started with me scraping all transcripts (all seasons, all episodes) from transcripts.foreverdreaming.org'),
                                                   tags$a(href = 'https://github.com/kristofrabay/web_scraping/blob/master/the_office/office_transcripts_scraper.R',
                                                          'Find the scraper here: my GitHub'),
                                                   br(),
                                                   h5("After some (pre-)cleaning (getting rid of webpage content, separating 'line' to 'from' and 'text'), I needed to take a look at the structure and format of my table. I started my text cleaning process by extracting the season and episode numbers by RegEx (for later groupings), then I consolidated names due to typos on the website (i.e.: Micahel instead of Michael) for which I used the `agrep` function which tries to find approximate matches to a word by using a Levenshtein-like distance algo. After getting my data to a tidy, clean format, I could start looking at the first statistic: line count by character, to help me select a subgroup of all characters to work with.")),
                                            column(width = 6,
                                                   h3('Who spoke the most throughout the Series?'),
                                                   br(),
                                                   div(plotOutput('count_by_line_ggplot', height = '250px'), align = 'center'))
                                            ),
                                   fluidRow(column(width = 6,
                                                   h3("Decided to work with top 12 characters"),
                                                   br(),
                                                   h5("Going with the top 12 most 'talkative' people made sense for 3 reasons"),
                                                   tags$ol(tags$li("Chart densities are kept in control"),
                                                           tags$li("3 x 4 facet grid can be shown when comparing people"),
                                                           tags$li("~80-20 rule is followed")),
                                                   infoBox("% of lines spoken by top 12 characters", round(( nrow(data %>% filter(name %in% top12$name)) / nrow(data) ) * 100, 2), icon = icon("chart-pie"), width = 10),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   h5("Looking at the seasonal count by lines, it's clear that until Michael was on the show, he was getting most of the lines, speaking almost as much as Jim and Dwight together. After Michael left, Andy took season #8 with his promotion, while Dwight carried the series in its last season."),
                                                   h5("Now that I've looked at whose names will be coming up in the top 12, let's actually dive in to the first part of the text analysis: tokenization and looking at individual words by characters")),
                                            column(width = 6,
                                                   h3("Let's check the line count by season..."),
                                                   br(),
                                                   div(plotOutput('count_by_line_by_season_ggplot', height = '450px'), align = 'center'))
                                   )
                                   
                                   ),
                           
                           
                           tabItem(tabName = "tokens",
                                   fluidRow(div(h2("Tokenization - looking at top words (highest count) used by each character"), align = 'center'),
                                            div(h6('(Give the page 5 sec to load)'), align = 'center'),
                                            h5("As a first step to unnesting the lines to words, I got rid of lines that aren't 'spoken' but set an environment or help the viewer understand the situation. Such lines, like 'laughing histerically', 'humming', 'while eating' are all between [ and ] characters. With the help of '\\[(.*?)\\]' RegEx I can easily locate anything between [ and ]."),
                                            h5("After that I unnested the lines and removed stopwords: (1) from the tidytext package, (2) the 'smart' dictionary from the stopwords package, (3) and words I found in the text that don't really carry any meaning, such as 'yeah', 'gonna', 'uh', 'alright', 'um', 'lot' and 'hey'. Results are under: ")),
                                   fluidRow(h3('Unnesting lines and removing stop words show that mostly used words are names of other characters'),
                                            br(),
                                            div(plotOutput("top_words_by_people_plot", height = '450px', width = '650px'), align = 'center'),
                                            br()),
                                   fluidRow(h3('Names occuring in top words gave me the idea to visualize conversation between the top 12'),
                                            br(),
                                            div(visNetworkOutput('top_words_by_people_network'), align = 'center'),
                                            br()),
                                   fluidRow(h3('With names removed, top words help identify each person'),
                                            br(),
                                            div(plotOutput('top_words_by_people_no_names', height = '450px', width = '650px'), align = 'center')),
                                   fluidRow(h3('Feel free to create WordClouds from the top words used by a chosen character'),
                                            br(),
                                            h5("I've included the top 30 people, so other familiar favorites' vocabularies can be looked at!"),
                                            h5("Take a look at Jo, for example: her dogs, her city (Tallahassee), her company (Sabre) and products (printers) all come up."),
                                            br(),
                                            column(width = 4,
                                                   selectInput('wordcloud_person', 
                                                               "Choose a person you like", 
                                                               choices = top30$name, selected = 'Jo'),
                                                   hr(),
                                                   radioButtons("include_names_or_not", "Including names?",
                                                                c("Yes" = "Yes",
                                                                  "No" = "No"), 
                                                                selected = 'No'),
                                                   br(),
                                                   sliderInput("wordcloud_min",
                                                               "Minimum Frequency:",
                                                               min = 1,  max = 20, value = 3),
                                                   sliderInput("wordcloud_max",
                                                               "Maximum Number of Words:",
                                                               min = 5,  max = 300,  value = 50)),
                                            column(width = 8,
                                                   div(plotOutput('wordcloud_topwords', height = '500px'), align = 'center')))
                                   
                                   ),
                           
                           
                           tabItem(tabName = "tfidf",
                                   div(h2("Considering its easy use and interpretation, TF-IDF is an amazing tool!"), align = 'center'),
                                   div(h6('(Give the page 5 sec to load)'), align = 'center'),
                                   h5("Running the TF-IDF on my dataset truly outlines the unique-to-given-person word. By adjusting a word's document-specific (person-specific) frequency with its total frequency throughout all documents (names), most personal words that belong to their (one and only) user can be found."),
                                   br(),
                                   h5("Important note: I've conducted lemmatization for this task. Reasoning behind was, that for top words, I did want to see the 'forms' words were used, but for extracting the most personal words, I wanted to get them to their normalized forms, and then run the tf-idf algo to extract the truly personal tokens. For the lemmatization process I leveraged R's textstem library."),
                                   br(),
                                   div(plotOutput('tfidf', height = '450px', width = '650px'), align = 'center')),
                           
                           tabItem(tabName = "bigrams",
                                   div(h2("Why only look at words when we can analyze expressions?"), align = 'center'),
                                   div(h6('(Give the page 5 sec to load)'), align = 'center'),
                                   h5("Bigrams (pairs of words) give very familiar results to an Office fan: Andy's famous nard dog, Dwight's farm's name, the company all of them work for, these expressions all come up when running analysis of pairs of words."),
                                   br(),
                                   div(plotOutput('bigrams', height = '450px', width = '650px'), align = 'center')),
                           
                           
                           tabItem(tabName = "posneg",
                                   fluidRow(div(h2("Looking at simple sentiment analysis results"), align = 'center'),
                                            div(h6('(Give the page 5 sec to load)'), align = 'center'),
                                            br(),
                                            h3("I've decided to run sentiment scoring on the top characters in two separate ways: "),
                                            tags$ol(tags$li("Assigning all meaningful words to positive and negative categories like the Loughran sentiment lexicon does"),
                                                    tags$li("Assigning sentiment scores to all words by the AFINN scoring system, and multiplying word occurence with the score")),
                                            br(),
                                            column(width = 6,
                                                   div(h3("Most occurent positive and negative words"), align = 'center'),
                                                   br(),
                                                   div(plotOutput('sentiment_posneg', height = '450px', width = '580px'), align = 'center'),
                                                   br()),
                                            column(width = 6,
                                                   div(h3("Sentiment scores by multiplying AFINN with the times of occurence"), align = 'center'),
                                                   br(),
                                                   div(plotOutput('sentiment_afinn', height = '450px', width = '580px'), align = 'center'),
                                                   br()),
                                            br(),
                                            h4("Interesting to see how using a sentiment scale like AFINN and multiplying word counts with their respective scores outlines the words that contribute the most positivity or negativity to a certain character's vocabulary"))
                                    ),
                           
                           tabItem(tabName = "trend_sent",
                                   fluidRow(div(h2("Trend of average personal per episode sentiments does not offer insights to feelings of characters"), align = 'center'),
                                            div(h6('(Give the page 15-20 sec to load, sentimentR::sentiment_by needs to run to get scores per line)'), align = 'center'),
                                            br(),
                                            h5("I tried to find out if I can sort of track personal feelings and moods by running sentiment analysis per line with the help of the senitmentR package, and averaging meaningful (not equal to zero) lines on an episode level. After seeing no explainable and interpretable results from episode-aggregation I checked if averaging lines on season-levels may hint at something but the results remained somewhat disappointing."),
                                            br(),
                                            column(width = 6,
                                                   div(h3("Timeline of sentiment changes per episode"), align = 'center'),
                                                   br(),
                                                   div(plotOutput('sentiment_episode', height = '450px', width = '580px'), align = 'center'),
                                                   br()),
                                            column(width = 6,
                                                   div(h3("Timeline of sentiment changes per season"), align = 'center'),
                                                   br(),
                                                   div(plotOutput('sentiment_season', height = '450px', width = '580px'), align = 'center'),
                                                   br()),
                                            br(),
                                            h4("Looking at people one-by-one and seeing if their average sentiment per episode may hint at something failed, but I quickly turned to analyzing sentiments when people were talking to other people, and the results got substantially more interesting. Take a look on the next page!"))), 
                           
                           
                           tabItem(tabName = "between_sent",
                                   div(h2("Who's nice to whom? And who's mean to whom? Exciting stuff to prove by NLP!"), align = 'center'),
                                   div(h6('(Give the page 5 sec to load)'), align = 'center'),
                                   h4("1. Let me just give an overview of the positivity and negativity between the top 12"),
                                   h5("Jim is nice to everyone, Erin, Michael, Pam & Phyllis seem very nice as well. Angela, Darryl, Dwight and Oscar are all mean to around half of the other guys. Angela is nicest to Dwight, which is no surprice (wink wink), while meanest to Jim, which underlines thier 9 season long rivalry."),
                                   br(),
                                   div(plotOutput('sentiment_between', height = '450px', width = '750px'), align = 'center'),
                                   h4("2. Let's visualize the above plot in a network!"),
                                   br(),
                                   div(visNetworkOutput('sentiment_network_all'), align = 'center'),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   h5("I also want to highlight the 'strength' of the niceness and meanness on the edges, but keeping all edges would make the network uninterpretable, so I'll stick to a minimum and maximum threshold. I'm dropping relationships with sentiment scores between -10 and +30, which seemed logical to me based on the distribution. This range seems to contain 'normal' relatoinships, not the strong ones I want to emphasize."),
                                   br(),
                                   div(plotOutput('sent_dist_to_back_nw3', height = '350px', width = '600px'), align = 'center'),
                                   br(),
                                   h4("3. Keeping 'strongest' (either more negative or more positive) relationships in the network"),
                                   br(),
                                   div(visNetworkOutput('sentiment_network_top'), align = 'center'),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   hr()),
                           
                           tabItem(tabName = "jd",
                                   fluidRow(div(h2("Jim and Dwight DO end their relationship on a higher note"), align = 'center'),
                                            div(h6('(Give the page 10 sec to load)'), align = 'center'),
                                            br(),
                                            column(width = 6,
                                                   div(h3("Using words' AFINN scores and frequencies"), align = 'center'),
                                                   br(),
                                                   div(plotOutput('jd_afinn', height = '350px', width = '580px'), align = 'center'),
                                                   br()),
                                            column(width = 6,
                                                   div(h3("Leveraging sentimentR run by line and summing up score"), align = 'center'),
                                                   br(),
                                                   div(plotOutput('jd_lines', height = '350px', width = '580px'), align = 'center'),
                                                   br()),
                                            br(),
                                            h4("The tendecies are much alike, so the patterns that AFINN and sentimentR give seem to be valid: although Jim used to be nicer to Dwight on average, Dwight started easing up on Jim and the two ended the series being great friends who don't tease each other any more."))),
                           
                           
                           tabItem(tabName = "lda",
                                   div(h2("LDA topic analysis helps identify people with similar vocabularies, but actual topics are not extractable"), align = 'center'),
                                   div(h6('(Give the page 5 sec to load)'), align = 'center'),
                                   h4("1. Creating 12 clusters (topics) for the top 12 people to find similar speakers"),
                                   h5("Something to write here"),
                                   br(),
                                   div(plotOutput('lda_12', height = '450px', width = '750px'), align = 'center'),
                                   br(),
                                   h4("2. Arbitrarily choosing a person, and exploring their two topics"),
                                   selectInput('lda_person', 
                                               "Choose a person whose 2 topics you'd like to explore", width = '400px',
                                               choices = top30$name, selected = 'Jan'),
                                   hr(),
                                   fluidRow(column(width = 6,
                                                   h5("A: Most differing words based in log differences in topic betas"),
                                                   br(),
                                                   div(plotOutput('lda_logdiff', height = '450px', width = '580px'), align = 'center'),
                                                   br()),
                                            column(width = 6,
                                                   h5("B: Top words in each topic"),
                                                   br(),
                                                   div(plotOutput('lda_top_words', height = '450px', width = '580px'), align = 'center'),
                                                   br()))
                                  ),
                           
                           tabItem(tabName = 'summary',
                                   div(h2("What I have looked at and what I have found..."), align = 'center'),
                                   br(),
                                   h5("During this project I've conducted a thorough analysis of the whole The Office series' transcripts. I've: "),
                                   tags$ul(tags$li("Sorted out most talkative people"),
                                           tags$li("Checked their most frequently used words, expressions & most personal / unique words"),
                                           tags$li("Visualized and analyzed conversation count between them"),
                                           tags$li("Looked at their overall sentiments throughout the series"),
                                           tags$li("Analyzed and visualized their sentiments when talking to each other"),
                                           tags$li("Found similarities between their vocabulary usages and tried to assign their words to 2 topics")),
                                   br(),
                                   h5("I dove into the analysis with some pre-recorded hypotheses, of which some coule be clearly proved and disproved, and one could go either way. Let's check what I've come to with regards to my hypotheses: "),
                                   div(tableOutput('summary_table'), align = 'center'),
                                   br(),
                                   h5("As stated")
                                   )
                          
                           
                        )
            )
)
                           
                
  


server <- function(input, output) {
    
    # 1. count by line
    
    output$count_by_line_ggplot <- renderPlot({
        
        data %>% group_by(name) %>% 
            summarize(count = n()) %>% 
            top_n(12) %>% 
            arrange(desc(count)) %>% 
            ggplot(aes(reorder(name, count), count)) +
            geom_col(show.legend = F, color = 'black', fill = 'skyblue', width = 2/3) +
            geom_text(aes(label = count), size = 3, position = position_stack(vjust = 0.5)) +
            labs(title = 'Even with only 7 / 9 seasons, Michael is the King of The Office',
                 subtitle = 'Overall # of lines per character, showing top 12',
                 x = NULL, y = NULL) +
            coord_flip() +
            theme_bw()
        
    })
    
    
    # 2. count by line by season
    
    output$count_by_line_by_season_ggplot <- renderPlot({
        
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
        
    })
    
    # 3. top words by people
    
    
    output$top_words_by_people_plot <- renderPlot({
        
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
                 subtitle = "Showing top 10 words by top 12 characters after removing usual stopwords + words like 'yeah', 'um'...",
                 x = NULL, y = NULL) +
            coord_flip() +
            scale_x_reordered() +
            facet_wrap(~name, scales = 'free') +
            theme_bw()
        
    })
    
    
    # 4. network 1: conversations
    

    output$top_words_by_people_network <- renderVisNetwork({
        
        visNetwork(nodes_1, edges_1, 
                   main = list('text' = 'The Michael, Dwight, Jim & Pam quartet is where most talking happens'),
                   submain = list('text' = "Conversation network to back up top words used by characters; node value represents lines spoken by one to other top 11 ",
                                  'style' = list('font-family' = 'Georgia',
                                                 'font-size' = '10px')),
                   height = '450px', width = '600px') %>% 
            visOptions(highlightNearest = list(enabled = T, degree = 0,
                                               labelOnly = T, hover = T),nodesIdSelection = T) %>% 
            visIgraphLayout(layout = "layout_with_fr", randomSeed = 1000) %>%
            visEdges(color = list(color = "lightblue", highlight = "teal", hover = "teal"),
                     hoverWidth = 5) %>% 
            visInteraction(hover = T) %>% 
            visNodes(color = list(highlight = "teal"))
        
    })
    
    # 5. top words by people with removed names
    
    output$top_words_by_people_no_names <- renderPlot({
        
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
        
        
    })
    
    # 6. wordcloud top words
    
    
    
    observeEvent(input$include_names_or_not, {
        
        if (input$include_names_or_not == 'Yes') {
            
            output$wordcloud_topwords <- renderPlot({
                
                set.seed(20202020)
                
                wordcloud(words = (with_names %>% filter(name == input$wordcloud_person))$word,
                          freq = (with_names %>% filter(name ==input$wordcloud_person))$n, 
                          min.freq = input$wordcloud_min, 
                          max.words = input$wordcloud_max, 
                          colors = brewer.pal(9, 'Set1'))
                
            })
            
        } 
        
        if (input$include_names_or_not == 'No') {
            
            output$wordcloud_topwords <- renderPlot({
                
                set.seed(20202020)
                
                wordcloud(words = (without_names %>% filter(name == input$wordcloud_person))$word,
                          freq = (without_names %>% filter(name ==input$wordcloud_person))$n, 
                          min.freq = input$wordcloud_min, 
                          max.words = input$wordcloud_max, 
                          colors = brewer.pal(9, 'Set1'))
                
            })
        }
        
    })
    
    
    # 7. tf-idf
    
    output$tfidf <- renderPlot({
        
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
        
    })
    
    # 8. bigrams
    
    output$bigrams <- renderPlot({
        
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
        
    })
    
    # 9. sentiment count (pos / neg)
    
    output$sentiment_posneg <- renderPlot({
        
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
        
    })
    
    # 10. sentiment value (AFINN * count)
    
    output$sentiment_afinn <- renderPlot({
        
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
        
    })
    
    
    # 11. sentiment trend by episode
    
    output$sentiment_episode <- renderPlot({
        
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
                 subtitle = "Sentiments come from each line, aggregated on episode level; blue range represents min-max range",
                 x = '# of episode', y = NULL) +
            facet_wrap(~name) +
            theme_bw()
        
    })
    
    
    # 12. sentiment trend by season
    
    
    output$sentiment_season <- renderPlot({
        
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
        
    })
    
    
    # 13. sentiment between people
    
    output$sentiment_between <- renderPlot({
        
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
        
    })
    
    
    # 14. sent between people - network - all
    
    output$sentiment_network_all <- renderVisNetwork({
        
        visNetwork(nodes_2, edges_2, 
                   main = "Everyone's nice to Darryl, Jim's nice to everyone, Oscar, Dwight & Angela are quite mean", 
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
        
    })
    
    
    # 15. sent score distribution to back up threshold for network 3
    
    output$sent_dist_to_back_nw3 <- renderPlot({
        
        ggplot(convo, aes(reorder(name, sentiment_score), sentiment_score)) + 
            geom_boxplot(show.legend = F, alpha = 0.75, outlier.stroke = T,
                         fill = 'gray', color = 'black') +
            geom_hline(yintercept = median(convo$sentiment_score), size = 1) +
            theme_bw() + 
            labs(title = 'Distribution of total sentiment scores by person',
                 subtitle = 'Vertical line represent median sentiment score') +
            scale_y_continuous(breaks = seq(-130, 350, 20)) +
            coord_flip()
        
    })
    
    
    # 16. sent between people - network - top
    
    output$sentiment_network_top <- renderVisNetwork({
        
        visNetwork(nodes_2, edges_3, 
                   main = "Jim & Pam clearly have the most positive relationship", 
                   submain = 'Showing most negative and most positive relationships; node # represents meaningful unqiue words spoken',
                   footer = 'Select a node to focus on one person only',
                   height = '400px') %>% 
            visOptions(highlightNearest = list(enabled = T, degree = 0,
                                               labelOnly = T, hover = T), 
                       nodesIdSelection = T) %>% 
            visIgraphLayout(layout = "layout_with_fr", randomSeed = 1000) %>%
            visEdges(arrowStrikethrough = F,
                     arrows =list(to = list(enabled = T, scaleFactor = 1))) %>% 
            visInteraction(hover = T)
        
    })
    
    # 17. Jim-Dwight AFINN by words
    
    output$jd_afinn <- renderPlot({
        
        ggplot(JimDwight, aes(season, sentiment_score, color = conversation, fill = conversation)) + 
            geom_line(size = 1) + 
            geom_point(size = 1.75) +
            theme_bw() + 
            labs(title = "Jim-Dwight - calculating with AFINN scores by word",
                 x = 'season',
                 y = 'total sentiment score') +
            scale_x_continuous(breaks = seq(1, 9, 1)) +
            theme(legend.key = element_blank(),
                  legend.background=element_blank(),
                  legend.position=c(0.8, 0.85))
        
    })
    
    # 18. Jim-Dwight sentimentR by lines
    
    output$jd_lines <- renderPlot({
        
        JimDwight_byline %>% 
            group_by(season, name, to) %>% 
            summarize(ave_sent = mean(sentiment),
                      sum_sent = sum(sentiment)) %>% 
            mutate(conversation = paste0(name, " to ", to)) %>% 
            ggplot(aes(season, sum_sent, color = conversation, fill = conversation)) + 
            geom_line(size = 1) + 
            geom_point(size = 1.75) +
            theme_bw() + 
            labs(title = "Jim-Dwight - summing up sentimentR scores by lines",
                 x = 'season',
                 y = 'total sentiment score') +
            scale_x_continuous(breaks = seq(1, 9, 1)) +
            theme(legend.key = element_blank(),
                  legend.background=element_blank(),
                  legend.position=c(0.85, 0.15))
        
    })
    
    
    # 19. 12 clusters from LDA
    
    output$lda_12 <- renderPlot({
      
      words_top_12_dtm_lda_gammas %>%  
        rename('name' = 'document') %>% 
        mutate(topic = as.factor(topic),
               name = as.factor(name)) %>% 
        ggplot(aes(topic, gamma, fill = name)) + 
        geom_point(show.legend = F, color = 'black', shape = 8) +
        facet_wrap(~name, scales = 'free') + 
        labs(title = "Only Michael & Dwight don't have 'one clear vocabulary'",
             subtitle = 'LDA clustering outcome: some people use very similar language (i.e.: Angela, Oscar & Ryan)',
             x = '12 topics (clusters) from LDA algo',
             y = '% of being assigned to one cluster') +
        theme_bw()
      
    })
    
    # 20. reactive df by chosen name
    
    lda_person_df <- reactive({
      
      x <- data %>% 
        group_by(name) %>% 
        unnest_tokens(word, text) %>% 
        ungroup() %>% 
        filter(name == input$lda_person) %>% 
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
      
      return(x_lda_betas)
      
    })
    
    # 21. 2 clusters by people of choice - betas & log diff
    
    output$lda_logdiff <- renderPlot({
      
      lda_person_df() %>% 
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
        labs(title = paste0('Largest beta differences for ', input$lda_person),
             subtitle = paste0('Running LDA on ', input$lda_person, "'s words"),
             x = 'Top terms',
             y = 'Log-ratio') +
        theme_bw()
      
    })
    
    # 22. 2 clusters by people of choice - betas & top words / topic
    
    output$lda_top_words <- renderPlot({
      
      lda_person_df() %>% 
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
        labs(title = paste0('Top words for 2 clusters created for ', input$lda_person),
             subtitle = paste0('Running LDA on ', input$lda_person, "'s words"),
             x = 'Top terms',
             y = 'Beta') +
        theme_bw()
      
    })
    
    # 23. summary table
    
    output$summary_table <- renderTable(summary, bordered = T, width = "900px", rownames = F, colnames = T)
    
}


shinyApp(ui = ui, server = server)
