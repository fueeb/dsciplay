# SCRIPT TO RETRIEVE TWEETS AND MAKE SOME FUNNY GRAPHS
# AUTHOR: LUIS CONDE
# FIRST VERSION: JANUARY 2021


# PACKAGES ----------------------------------------------------------------
library(ggplot2)                  # GRAPHICS
library(tm)                       # CORPUS (TEXT MINING)
library(rtweet)                   # RETRIEVE TWITTER DATA
library(tidyverse)                # DATA HANDLING
library(wordcloud)                # WORD CLOUD VISUALIZATIONS
library(glue)                     # PASTE CHARACTERS

# Define user to retrieve data (all you need is a Twitter account to retrieve tweets from a public user)
twitter_user <- "TMSWTCCK"
n <- 3000

# Try to retrieve tweets or likes (it has rates limitations)
user_timeline <- get_timeline(twitter_user, n)
user_likes <- get_favorites(twitter_user, n)

first_date <- as.Date(sort(user_timeline$created_at)[1])
last_date <- as.Date(sort(user_timeline$created_at)[length(user_timeline$created_at)])


# Function to graph top replies accounts
mr <- function(df_tweets){
  # Filters the oldest and newest year of data
  fyear <- format(first_date, '%Y')
  lyear <- format(last_date, '%Y')
  
  ggplot(df_tweets %>% 
           filter(reply_to_screen_name %in% c(df_tweets %>% 
                                                filter(!is.na(reply_to_screen_name)) %>% 
                                                group_by(reply_to_screen_name) %>% 
                                                tally() %>% 
                                                arrange(desc(n)) %>% 
                                                .$reply_to_screen_name %>% 
                                                .[1:10]))) +
    geom_bar(mapping = aes(x = reply_to_screen_name, fill = reply_to_screen_name)) +
    scale_fill_brewer(palette = "Spectral", labels = NULL, name = NULL, breaks = NULL) +
    coord_flip() +
    theme_minimal() +
    labs(title = glue("Accounts with most replies ({fyear}-{lyear})", 
                      x = "User", 
                      y = "Number of replies"))
}


# Function to graph most liked tweets
mlt <- function(tweets, min_likes = 4){
  first_date <- format(as.Date(sort(tweets$created_at)[1]), '%B %Y')
  last_date <- format(as.Date(sort(tweets$created_at)[length(tweets$created_at)]), '%B %Y')
  
  return(
  ggplot(tweets %>% 
           filter(screen_name %in% c(tweets %>% 
                                       group_by(screen_name) %>% 
                                       tally() %>% 
                                       filter(n > min_likes) %>% 
                                       .$screen_name))) +
    geom_bar(mapping = aes(x = screen_name, fill = screen_name)) +
    scale_fill_brewer(palette = "Spectral", labels = NULL, name = NULL, breaks = NULL) +
    coord_flip() +
    theme_minimal() +
    labs(title = glue("Accounts with most liked tweets ({first_date} - {last_date})", x = "User", y = "Number of likes")))
}


# Function to create words vector to feed wordcloud (it has a serious gap to improve)
drop_words <- c('\n', '—', '"', "vez", "cómo", "tan", "así", "voy", "sé", 
                "ver","cada", "pues", "hace", "ser", "hoy", "creo", "jajajsjs", 
                "jajajajajjaja", "jajajajajja", "jajajajajjaaj", "jajajajaj", 
                "dije", "digo")

words_resume <- function(df_tweets, min_count = 2, clean_words = drop_words){
  CORPUS <- Corpus(VectorSource(iconv(tolower(df_tweets$text[!df_tweets$is_retweet]), "UTF-8"))) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removeWords, stopwords("spanish")) %>% 
    tm_map(removeWords, stopwords("english")) %>% 
    tm_map(str_remove, "http[[:alnum:]]*") %>% 
    tm_map(removeWords, c(unique(df_tweets$screen_name), clean_words)) %>% 
    tm_map(str_replace, pattern = "puede", replacement = "puedo") %>% 
    tm_map(str_replace, pattern = "sólo", replacement = "solo") %>% 
    tm_map(str_replace, pattern = "días", replacement = "día") %>% 
    tm_map(str_replace, pattern = "xddd", replacement = "xdd") %>% 
    tm_map(str_replace, pattern = "hacer", replacement = "hace") %>% 
    tm_map(str_replace, pattern = "hacelo", replacement = "hace") %>% 
    tm_map(str_replace, pattern = "bueno", replacement = "bien") %>% 
    tm_map(str_replace, pattern = "años", replacement = "año") %>% 
    tm_map(stripWhitespace)
  
  tdm <- as.matrix(TermDocumentMatrix(CORPUS))
  
  return(sort(subset(rowSums(tdm), rowSums(tdm) > min_count)))
}

aranxa_likes <- mlt(user_likes, 25)
aranxa_likes

# If min_count is low, it could retrieve garbage. If its high, it could affect wordcloud aspect
w_fav <- words_resume(user_likes, 2)


# Plot wordcloud
set.seed(1234)
wordcloud(names(w_fav), w_fav, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Additional tests...
# Devices or services to drop
clean_devices <- c("Bitly", "Shareaholic.com", "Bitstrips", "erased136949", 
                   "Tumblr", "Deezer", "Ask.fm", "Twitter for Windows", 
                   "erased3604020", "bitly bitlink")

# Plots devices or services used
ggplot(user_timeline %>% 
         filter(!source %in% clean_devices)) +
  geom_bar(mapping = aes(x = source, fill = source)) +
  scale_fill_brewer(palette = "Spectral", labels = NULL, name = NULL, breaks = NULL) +
  coord_flip() +
  theme_minimal() +
  labs(title = glue("Devices or services used to tweet from {format(first_date, '%Y')}-{format(last_date, '%Y')}", 
                    x = "Source", 
                    y = "Number of tweets"))
