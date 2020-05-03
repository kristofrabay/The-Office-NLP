library(rvest)
library(tidyverse)
library(pbapply)
library(data.table)
library(stringr)
library(tidyr)

# robots.txt does not disallow .viewforum

## 1: GET LINKS TO EPISODES

url <- paste0("https://transcripts.foreverdreaming.org/viewforum.php?f=574&start=", seq(0, 175, 25))

episode_links <- rbindlist(pblapply(url, function(url) {
  
  page <- read_html(url)
  links <- page %>% html_nodes('.topictitle') %>% html_attr('href')
  
  # first char of href is a dot, get rid of it to construct whole URLs
  urls <- paste0("https://transcripts.foreverdreaming.org", str_sub(links, start = 2))
  
  return(list(urls))
  
}))

class(episode_links) # df
# first 2 and last 1 are links to not actual episodes
episode_links <- episode_links[-c(1:2, nrow(episode_links))]
saveRDS(episode_links, "links_to_episodes.RDS") # no need to re run scraper

episode_links <- readRDS("links_to_episodes.RDS")


## 2: EXTRACT TRANSCRIPTS BY EPISODE

text <- rbindlist(pblapply(episode_links$V1, function(link) {
  
  page <- read_html(link)
  
  title <- page %>% html_nodes("h2") %>% html_text()
  transcript <- page %>% html_nodes("#pagecontent p") %>% html_text()
  
  return(data.frame(title = title,
                    transcript = transcript))
  
}))

# took about 3-3.5 mins to scrape all

View(text) # 62 306 rows


## 3: CLEAN UP SOME ROWS

nrow(text[(is.na(text$transcript) | text$transcript==""), ]) # 198 empty rows
nrow(text[text$transcript %like% 'adsbygoogle']) # 198 rows with google ads
# 198 rows = 1 for each episode
text <- text[!(is.na(text$transcript) | text$transcript=="" | text$transcript %like% 'adsbygoogle'), ]


## 4: SEPARATE CHARACTER FROM LINE
# first semicolon comes right after the Name

as.data.table(table(str_match(text$transcript, "([^:]+)")))[order(-N)] %>% View()

# # this one uses the last (?) colon
# text_separated <- separate(data = text,
#                            col = transcript,
#                            into = c("name", "line"), sep = ":")

# my method: replace the first (!) colon with |, then separate by |
# hypothesis for this: there are no | signs in the text pre my adding of them
text_separated <- text %>% mutate(replace_first_colon = str_replace(transcript, "\\:", "|")) %>% 
  separate(col = replace_first_colon, into = c("name", "line"), sep = "\\|")

# story lines all start with [, lets drop them
text_separated <- text_separated %>% filter(!str_starts(name, "\\["))

# what else is NA
# 05x30 - Gag Reel is missing character names
# everything else: titles, deleted scenes with no lines and some webisodes
text_separated <- text_separated %>% filter(!is.na(line))

# will need consolidation of character names
text_separated %>% group_by(name) %>% summarize(count = n()) %>% View()

## 5: SAVE FILES TO WORK WITH

write_csv(text_separated[1:10,], "scraped_transcripts_to_show.csv")
saveRDS(text_separated, "scraped_transcripts.RDS")


## 6: IDEAS

# 1. consolidate names
# 2. word usage by top actors by season
# 3. ngram analysis - phrases by actors
# 4. sentiment analysis by actor (by season)
# 5. sentiment analysis by conversation (i.e.: Jim's sentiment when talking to Pam, Michael's sentiment when talking to Toby)
# 6. interactions between actors (network)
# 7. something with topics by actors

