library(googlesheets4)
library(janitor)
library(tidyverse)
library(ggplot2)
library(ggdark)

included_tweets=
  read_sheet("https://docs.google.com/spreadsheets/d/1KY8s53qnl75Yoj-p6RAMYGtYtVJ_qj_Qv0fnVEiwbzY/edit#gid=1467721205",
             range="ShamsWojHaynes Tweets!A:G") %>% clean_names() %>% 
  mutate(time_of_last_tweet_in_series=format(time_of_last_tweet_in_series,format="%H:%M:%S"))

team_counts=included_tweets %>% 
  #separate comma separated team lists
  mutate(teams = strsplit(teams, ", ")) %>% unnest(teams) %>% 
  #get totals for each team by reporter
  filter(!is.na(teams)) %>% group_by(teams,reporter,time_period) %>% summarize(count=n()) %>%
  #each reporter gets separate column
  pivot_wider(names_from=reporter,values_from=count,values_fill = 0) %>% ungroup() %>% 
  mutate(Total=Haynes+Shams+Woj) %>%
  arrange(time_period,teams) %>% clean_names(case="upper_camel")

sheet_write(team_counts,
            ss="https://docs.google.com/spreadsheets/d/1KY8s53qnl75Yoj-p6RAMYGtYtVJ_qj_Qv0fnVEiwbzY/edit#gid=1467721205",
            sheet="Counts")

only_teams=team_counts %>% clean_names() %>% filter(teams != "Agent" & teams != "NBA") %>% 
  pivot_longer(cols=c("haynes","shams","woj"),names_to="reporter",values_to="count") %>% 
  group_by(time_period,reporter) %>% mutate(percent_during_time_period=count/sum(count)) %>% ungroup()

total_tweets=only_teams %>%
  ggplot(aes(x=factor(time_period,levels=c("2019 S","2019 O","2020 S","2020 O","2021 S","2021 O")),
             y=count,
             fill=reporter)) + 
  labs(x="Time Period",y="Number of Tweets",fill="Reporter") +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("Total Tweets Per Team Per Period from Shams Charania, Adrian Wojnarowski and Chris B Haynes") +
  facet_wrap(vars(teams)) +
  theme(panel.spacing.x = unit(4, "mm")) + dark_theme_gray()
ggsave("total_tweets.png",total_tweets,width=15,height=10)

total_reporters=only_teams %>% group_by(time_period,reporter) %>% summarize(total=sum(count)) %>% ungroup() %>% 
  ggplot(aes(x=reporter,y=total,
             fill=factor(time_period,levels=c("2019 S","2019 O","2020 S","2020 O","2021 S","2021 O")))) +
  labs(x="Reporter",y="Total Tweets",fill="Time Period") +
  geom_bar(position="dodge",stat="identity") + 
  ggtitle("Total Tweets Per Period from Shams Charania, Adrian Wojnarowski and Chris B Haynes") +
  dark_theme_gray()
ggsave("total_reporters.png",total_reporters)

tms_w_high_percent_tweets=c("GSW","LAC","LAL")

percent_tweets_low=only_teams %>% filter(!(teams %in% tms_w_high_percent_tweets)) %>% 
  ggplot(aes(x=factor(time_period,levels=c("2019 S","2019 O","2020 S","2020 O","2021 S","2021 O")),
             y=percent_during_time_period,fill=reporter)) + 
  labs(fill="Reporter",y="Percent of Tweets",x="Time Period") +
  geom_bar(position="dodge",stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Percentage of Tweets Per Team Per Period from Shams Charania, Adrian Wojnarowski and Chris B Haynes") +
  facet_wrap(vars(teams)) + dark_theme_gray()
ggsave("percent_tweets_low.png",percent_tweets,width=15,height=10)

percent_tweets_high=only_teams %>% filter((teams %in% tms_w_high_percent_tweets)) %>% 
  ggplot(aes(x=factor(time_period,levels=c("2019 S","2019 O","2020 S","2020 O","2021 S","2021 O")),
             y=percent_during_time_period,fill=reporter)) + 
  labs(fill="Reporter",y="Percent of Tweets",x="Time Period") +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("Percentage of Tweets Per Team Per Period from Shams Charania, Adrian Wojnarowski and Chris B Haynes") +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(vars(teams)) + dark_theme_gray()
ggsave("percent_tweets_high.png",percent_tweets_high,width=15,height=10)

agent_counts=included_tweets %>% 
  #separate comma separated agent lists
  mutate(agent = strsplit(agent, ", ")) %>% unnest(agent) %>% 
  #get totals for each agent by reporter
  filter(!is.na(agent)) %>% group_by(agent,reporter) %>% summarize(count=n()) %>%
  #each reporter gets separate column
  pivot_wider(names_from=reporter,values_from=count,values_fill = 0) %>% ungroup() %>% 
  mutate(total=Haynes+Shams+Woj) %>% 
  #maximum reporter percent
  rowwise() %>% mutate(max_percent=max(`Haynes`,`Shams`,`Woj`)/total) %>% ungroup() %>%
  arrange(desc(total)) %>% clean_names(case="upper_camel")

sheet_write(agent_counts,
            ss="https://docs.google.com/spreadsheets/d/1KY8s53qnl75Yoj-p6RAMYGtYtVJ_qj_Qv0fnVEiwbzY/edit#gid=1467721205",
            sheet="Agents")

