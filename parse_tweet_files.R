library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

haynes=read_excel("Data/chrisbhaynes_user_tweets.xlsx")
woj=read_excel("Data/wojespn_user_tweets.xlsx")
shams=read_excel("Data/shamscharania_user_tweets.xlsx")

#4 hour difference if not in daylight savings, 5 if in
date_since="2021-08-16"

all_reports=bind_rows(haynes,woj,shams) %>% clean_names() %>% select(tweet_id:utc,tweet_type) %>%
  filter(tweet_type != "Retweet") %>% filter(!str_detect(text,"^@")) %>% 
  select(-tweet_type) %>%
  mutate(tweet_link=paste0("https://twitter.com/",screen_name,"/status/",tweet_id)) %>%
  mutate(utc=str_sub(utc,end=-6)) %>% mutate(utc=str_replace(utc,"T"," ")) %>%
  mutate(utc=ymd_hms(utc)-hours(4)) %>% filter(utc>date_since) %>% arrange(utc) %>%
  separate(utc,sep=" ",into=c("date","time"))

write_csv(all_reports,"Data/cleaned_tweets.csv")
