library(googlesheets4)
library(janitor)
library(tidyverse)
library(ggplot2)
library(ggdark)
counts=
  read_sheet("https://docs.google.com/spreadsheets/d/1KY8s53qnl75Yoj-p6RAMYGtYtVJ_qj_Qv0fnVEiwbzY/edit#gid=1467721205",
             range="Counts!A:F") %>% clean_names()
only_teams=counts %>% filter(team != "Agent" & team != "NBA") %>% 
  pivot_longer(cols=c("haynes","shams","woj"),names_to="reporter",values_to="count") %>% 
  group_by(time_period,reporter) %>% mutate(percent_during_time_period=count/sum(count))

total_tweets=only_teams %>% 
  ggplot(aes(x=factor(time_period,levels=c("2019 S","2019 O","2020 S")),y=count,
             fill=reporter)) + 
  labs(x="Time Period",y="Number of Tweets",fill="Reporter") +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("Total Tweets Per Team Per Period from Shams Charania, Adrian Wojnarowski and Chris B Haynes") +
  facet_wrap(vars(team)) + dark_theme_gray()
ggsave("total_tweets.png",total_tweets,width=15,height=10)

total_reporters=only_teams %>% summarize(total=sum(count)) %>% 
  ggplot(aes(x=reporter,y=total,
             fill=factor(time_period,levels=c("2019 S","2019 O","2020 S")))) +
  labs(x="Reporter",y="Total Tweets",fill="Time Period") +
  geom_bar(position="dodge",stat="identity") + 
  ggtitle("Total Tweets Per Period from Shams Charania, Adrian Wojnarowski and Chris B Haynes") +
  dark_theme_gray()
ggsave("total_reporters.png",total_reporters)

percent_tweets=only_teams %>% filter(!(team %in% c("GSW","LAL"))) %>% 
  ggplot(aes(x=factor(time_period,levels=c("2019 S","2019 O","2020 S")),
             y=percent_during_time_period,fill=reporter)) + 
  labs(fill="Reporter",y="Percent of Tweets",x="Time Period") +
  geom_bar(position="dodge",stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Percentage of Tweets Per Team Per Period from Shams Charania, Adrian Wojnarowski and Chris B Haynes") +
  facet_wrap(vars(team)) + dark_theme_gray()
ggsave("percent_tweets.png",percent_tweets,width=15,height=10)

lal_gsw=only_teams %>% filter((team %in% c("GSW","LAL"))) %>% 
  ggplot(aes(x=factor(time_period,levels=c("2019 S","2019 O","2020 S")),
             y=percent_during_time_period,fill=reporter)) + 
  labs(fill="Reporter",y="Percent of Tweets",x="Time Period") +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("Percentage of Tweets Per Period for LA Lakers and Golden State Warriors from \nShams Charania, Adrian Wojnarowski and Chris B Haynes") +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(vars(team)) + dark_theme_gray()
ggsave("lal_gsw.png",lal_gsw)

agents=
  read_sheet("https://docs.google.com/spreadsheets/d/1KY8s53qnl75Yoj-p6RAMYGtYtVJ_qj_Qv0fnVEiwbzY/edit#gid=1467721205",
             range="Agents!A:E") %>% clean_names()

