library(knitr)
opts_chunk$set(message = FALSE, warning = FALSE, cache = TRUE, cache.lazy = FALSE)
options(width = 100, dplyr.width = 100)
library(ggplot2)
theme_set(theme_light())


library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(gdata)
library(data.table)

# The following part is to plot the basic information of Trump's tweets
trump <- fread("tweets_trump.csv",select = c(1,2,6))

# Data Preparation for Tweet' data
date <- NULL
date[grep(pattern = "Dec 31",x = trump$date)] <- paste("2015",trump$date[grep(pattern = "Dec 31",x = trump$date)]) #Dec 31 of 2015
date[1:min(grep(pattern = "Dec 31",x = trump$date))-1] <- paste("2016",trump$date[1:(min(grep(pattern = "Dec 31",x = trump$date))-1)]) ## Date of 2016
date <- as.POSIXct(strptime(x = date,format = "%Y %b %e"))

date[(max(grep(pattern = "Dec 31",x = trump$date))+1):nrow(trump)] <- as.POSIXct(strptime(trump$date[(max(grep(pattern = "Dec 31",x = trump$date))+1):nrow(trump)],format = "%e %b %Y")) ## Dates year <or=2015

trump$date <- date 
trump <- trump[!is.na(trump$date),] #Removing last 24 hours tweets for simplicity


ggplot(trump[,.N,by=year(date)][order(year)],aes(x = year,y = N,fill=N))+
  geom_bar(stat="identity")+
  ggtitle("Tweets by Year")

trump$pic <- 0L
trump$pic[grep(pattern = "pic",x = trump$text)] <- 1L


ggplot(trump[,sum(pic),by=year(date)][order(year)],aes(x = year,y = V1,fill=V1))+
  geom_bar(stat="identity")+
  ggtitle("Pictures by Year")

trump$link <- 0L
trump$link[grep(pattern = "http",x = trump$text)] <- 1L

ggplot(trump[,sum(link),by=year(date)][order(year)],aes(x = year,y = V1,fill=V1))+
  geom_bar(stat="identity")+
  ggtitle("Links by Year")

#The following part to plot the basic information of Clinton's tweets

clinton <- fread("tweets_clinton.csv",select = c(1,2,6))


## Tweets Date
date <- NULL
date[grep(pattern = "Dec 31",x = clinton$date)] <- paste("2015",clinton$date[grep(pattern = "Dec 31",x = clinton$date)]) #Dec 31 of 2015
date[1:min(grep(pattern = "Dec 31",x = clinton$date))-1] <- paste("2016",clinton$date[1:(min(grep(pattern = "Dec 31",x = clinton$date))-1)]) ## Date of 2016
date <- as.POSIXct(strptime(x = date,format = "%Y %b %e"))

date[(max(grep(pattern = "Dec 31",x = clinton$date))+1):nrow(clinton)] <- as.POSIXct(strptime(clinton$date[(max(grep(pattern = "Dec 31",x = clinton$date))+1):nrow(clinton)],format = "%e %b %Y")) ## Dates year <or=2015

clinton$date <- date 
clinton <- clinton[!is.na(clinton$date),] #Removing last 24 hours tweets for simplicity

ggplot(clinton[,.N,by=year(date)][order(year)],aes(x = year,y = N,fill=N))+
  geom_bar(stat="identity")+
  ggtitle("Tweets by Year")


clinton$pic <- 0L
clinton$pic[grep(pattern = "pic",x = clinton$text)] <- 1L

ggplot(clinton[,sum(pic),by=year(date)][order(year)],aes(x = year,y = V1,fill=V1))+
  geom_bar(stat="identity")+
  ggtitle("Pictures by Year")

clinton$link <- 0L
clinton$link[grep(pattern = "http",x = clinton$text)] <- 1L

ggplot(clinton[,sum(link),by=year(date)][order(year)],aes(x = year,y = V1,fill=V1))+
  geom_bar(stat="identity")+
  ggtitle("Links by Year")


# Tweets Mining for Trump and Clinton's tweets

library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)


tweets_clinton <- read_csv("tweets_clinton.csv")
tweets_trump <- read_csv("tweets_trump.csv")


date <- NULL
date[grep(pattern = "Dec 31",x = tweets_trump$date)] <- paste("2015",tweets_trump$date[grep(pattern = "Dec 31",x = tweets_trump$date)]) #Dec 31 of 2015
date[1:min(grep(pattern = "Dec 31",x = tweets_trump$date))-1] <- paste("2016",tweets_trump$date[1:(min(grep(pattern = "Dec 31",x = tweets_trump$date))-1)]) ## Date of 2016
date <- as.POSIXct(strptime(x = date,format = "%Y %b %e"))

date[(max(grep(pattern = "Dec 31",x = tweets_trump$date))+1):nrow(tweets_trump)] <- as.POSIXct(strptime(tweets_trump$date[(max(grep(pattern = "Dec 31",x = tweets_trump$date))+1):nrow(tweets_trump)],format = "%e %b %Y")) ## Dates year <or=2015

tweets_trump$date <- date 
tweets_trump <- tweets_trump[!is.na(tweets_trump$date),] #Removing last 24 hours tweets for simplicity


date <- NULL
date[grep(pattern = "Dec 31",x = tweets_clinton$date)] <- paste("2015",tweets_clinton$date[grep(pattern = "Dec 31",x = tweets_clinton$date)]) #Dec 31 of 2015
date[1:min(grep(pattern = "Dec 31",x = tweets_clinton$date))-1] <- paste("2016",tweets_clinton$date[1:(min(grep(pattern = "Dec 31",x = tweets_clinton$date))-1)]) ## Date of 2016
date <- as.POSIXct(strptime(x = date,format = "%Y %b %e"))

date[(max(grep(pattern = "Dec 31",x = tweets_clinton$date))+1):nrow(tweets_clinton)] <- as.POSIXct(strptime(tweets_clinton$date[(max(grep(pattern = "Dec 31",x = tweets_clinton$date))+1):nrow(tweets_clinton)],format = "%e %b %Y")) ## Dates year <or=2015

tweets_clinton$date <- date 
tweets_clinton <- tweets_clinton[!is.na(tweets_clinton$date),] #Removing last 24 hours tweets for simplicity




tweets <- bind_rows(tweets_clinton %>% 
                      mutate(person = "Clinton"),
                    tweets_trump %>% 
                      mutate(person = "Trump"))%>%
  mutate(timestamp = ymd_hms(date))

# Word frequencies

library(tidytext)
library(stringr)
replace_reg1 <- "https://t.co/[A-Za-z\\d]+|"
replace_reg2 <- "http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
replace_reg <- paste0(replace_reg1, replace_reg2)
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

frequency <- tidy_tweets %>%
  group_by(person) %>%
  count(word, sort = TRUE) %>%
  left_join(tidy_tweets %>%
              group_by(person) %>%
              summarise(total = n())) %>%
  mutate(freq = n/total)


library(tidyr)
frequency <- frequency %>%
  select(person, word, freq) %>%
  spread(person, freq) %>%
  arrange(Clinton, Trump)

library(scales)
ggplot(frequency, aes(Clinton, Trump)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(person, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(Trump / Clinton)) %>%
  arrange(desc(logratio))


word_ratios %>%
  arrange(abs(logratio))


word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (Clinton/Trump)") +
  scale_fill_discrete(name = "", labels = c("Trump", "Clinton"))

