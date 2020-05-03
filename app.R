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

data <- readRDS("../scraped_transcripts.RDS")
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
                     sidebarMenu(menuItem("Sentiment on whole dataset", 
                                          tabName = 'all_sent',
                                          icon = icon('theater-masks'))),
                     sidebarMenu(menuItem("Sentiment with seasonal trend", 
                                          tabName = 'trend_sent',
                                          icon = icon('history'))),
                     sidebarMenu(menuItem("Sentiments between all characters", 
                                          tabName = 'by_char_sent',
                                          icon = icon('project-diagram'))),
                     sidebarMenu(menuItem("Jim & Dwight's sentiment trend", 
                                          tabName = 'jd',
                                          icon = icon('handshake'))),
                     sidebarMenu(menuItem("Topic modeling with LDA", 
                                          tabName = 'lda',
                                          icon = icon('chalkboard-teacher'))),
                     sidebarMenu(menuItem("Takeaways", 
                                          tabName = 'close',
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
                                                   tags$ol(tags$li("Sentiment trend follows characters' happiness and sadness (falling in love, breaking up, getting fired, etc...)"),
                                                           tags$li("Jim and Dwight's relationship got better towards the end of the show"),
                                                           tags$li("Jim and Pam's relationship is most mutually positive"),
                                                           tags$li("Hobbies and personal activities can be extracted by tf-idf"),
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
                                            h5("After that I unnested the lines and removed stopwords: (1) from the tidytext package, (2) the 'smart' dictionary from the stopwords package, (3) and words I found in the text that don't really carry any meaning, such as 'yeah', 'gonna', 'uh', 'alright', 'um', 'lot' and 'hey'. Results are under: "),
                                            column(width = 6,
                                                   h3('Unnesting lines and removing stop words show that mostly used words are names of other characters'),
                                                   br(),
                                                   div(plotOutput("top_words_by_people_plot", height = '450px', width = '550px'), align = 'center')),
                                            column(width = 6,
                                                   h3('Names occuring in top words gave me the idea to visualize conversation between the top 12'),
                                                   br(),
                                                   div(visNetworkOutput('top_words_by_people_network'), align = 'center'))
                                   ),
                                   
                                   fluidRow( h3('With names removed, top words help identify each person'),
                                             br(),
                                             div(plotOutput('top_words_by_people_no_names', height = '450px', width = '600px'), align = 'center')),
                                   
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
                                   div(plotOutput('tfidf', height = '450px'), align = 'center'))
                
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
                 subtitle = "Showing top 10 words by top 12 characters after removing usual stopwords + words like 'yeah'...",
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
                          colors = brewer.pal(10, 'Set1'))
                
            })
            
        } 
        
        if (input$include_names_or_not == 'No') {
            
            output$wordcloud_topwords <- renderPlot({
                
                set.seed(20202020)
                
                wordcloud(words = (without_names %>% filter(name == input$wordcloud_person))$word,
                          freq = (without_names %>% filter(name ==input$wordcloud_person))$n, 
                          min.freq = input$wordcloud_min, 
                          max.words = input$wordcloud_max, 
                          colors = brewer.pal(10, 'Set1'))
                
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

}


shinyApp(ui = ui, server = server)
