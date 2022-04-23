# Loading Libraries and data
library(tidyverse) 
library(readxl)
library(tidytext)
options(max.print=20)
options(warn=-1)
theme_set(theme_light(base_size = 12))
chess <-read_csv("games.csv")

#Data pre-processing
chess$difference <- chess$white_rating - chess$black_rating
chess$baseTime <- as.numeric(gsub("\\+.*", "", chess$increment_code))
chess$isIncrement <- ifelse(grepl("+0", chess$increment_code, fixed = T), "No", "Yes")
chess$firstMove <- gsub(" .*", "", chess$moves)
chess$heighMove <- gsub("[^0-9]", "", chess$firstMove)
chess$wideMove <- gsub("[^a-z]", "", chess$firstMove)
chess$rated <- ifelse(is.na(chess$rated==T), "No information", chess$rated)
chess %>% group_by(white_id) %>% count(sort = T, name = "games played") %>% summary()


#Univariate Graphs

#Wins by colour in percentage
chess %>%
  group_by(winner) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(count = count/sum(count)) %>%
  ggplot(., aes(reorder(winner, -count), count))+
  geom_segment(aes(xend = winner, yend = 0), size = 0.3, colour = "gray20")+
  geom_point(size = 35, colour = "gray80", fill = "gray80", shape = 21)+
  geom_text(aes(label = paste0(round(count*100, 1), "%")), size = 6.3)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.52))+
  labs(title = "Result of games", subtitle = "in percentage", x = "Colour of winner", 
       y = "Share of wins")+
  theme_classic()

#White rating vs # of Games played
chess %>% add_count(white_id) %>% 
  distinct(.keep_all = T, white_id) %>% 
  ggplot(aes(x = white_rating,
             y = n)) + 
  geom_jitter(alpha = 0.2) +
  ylab("# Games played")

#Black rating vs # of Games played
chess %>% add_count(black_id) %>% 
  distinct(.keep_all = T, black_id) %>% 
  ggplot(aes(x = black_rating,
             y = n)) + 
  geom_jitter(alpha = 0.2) +
  ylab("# Games played")

#top 10 openings
opening<-filter(summarise(group_by(chess,opening_name), count=length(opening_name)),count>200)
ggplot(opening,aes(x=opening_name,y=count))+geom_col()+coord_flip()+theme_classic()

# 10+0 increment is most often played
increment<-filter(summarise(group_by(chess,increment_code), count=length(increment_code)),count>200)
ggplot(increment,aes(x=increment_code,y=count))+geom_col()

#Player rating frequency
rating<-rbind(chess$white_rating,chess$black_rating)

hist(rating,angle = 45, col = "grey", border = "white", main = "Player Rating", xlab = "Rating")



#Bivariate graphs

#Leaving the rankings, we go to another parameter set before the match, which is "baseTime" - the time for all moves by one player. According to this criterion, the game can be divided into 4 types: bullet (under 3 minutes), blitz (3-10 minutes), rapid (10-30 minutes) and classic game, where players have more than 30 minutes to move, which is very rare. played on the internet. We check the final results every minute from 1 to 20. It can be seen that the differences are small and often result from a small sample in some intervals (eg 19-20 minutes). There is also a slight tendency in which with the increase of time the share of wins by the player playing white increases at the expense of the wins of black. This will be confirmed by the fact that theoretically white has a slight advantage (because it starts), the implementation of which needs more time.
chess %>%
  filter(baseTime < 21) %>%
  group_by(baseTime, winner) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(., aes(baseTime, count, fill = winner))+
  geom_bar(stat = "identity", position = "fill", alpha = 0.95, width = 1)+
  scale_fill_grey(start = 0.2, end = 0.9)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(title = "Result of games", subtitle = "by base time (<= 20 min)", x = "Base time (in minutes)", fill = "Colour of winner: ",
       y = "Share of wins")+
  theme_classic()

#Box plot of rating vs type of opening
chess$type_of_eco <- case_when(
    str_detect(chess$opening_eco, "A") ~ "Flank", 
    str_detect(chess$opening_eco, "B") ~ "Semi-open", 
    str_detect(chess$opening_eco, "C") ~ "Open & French", 
    str_detect(chess$opening_eco, "D") ~ "Closed and semi-closed", 
    str_detect(chess$opening_eco, "E") ~ "Indian defences", 
)

chess %>% group_by(type_of_eco) %>% 
  add_count() %>% 
  select(type_of_eco, contains("rating"), n) %>% 
  ggplot(aes(color = n,
             x = fct_reorder(type_of_eco, 0.5*(white_rating + black_rating)),
             y = 0.5*(white_rating + black_rating))) + geom_boxplot() + 
  ylab("Rating") + xlab("Type of opening") + 
  scale_color_viridis_c() + 
  coord_flip()


# White vs Black rating w/ winner
ggplot(chess,aes(x=white_rating,y=black_rating,color=winner))+geom_point(alpha=0.5)


#Result of games by difference in rating
#Since we already have an average ranking, let's check if there is a difference in the distribution depending on the level of differentiation of the player's ranking points. They were divided into several categories at their own discretion - from ranking differences less than 5 to greater than 100. This time, however, there are no differences - the color of the pieces is drawn before the game, so the better player takes turns playing black and white. Most matches probably end with the win of the person with the better ranking, but the color of the pieces doesn't affect it.
chess %>%
  mutate(diffence_c = case_when(abs(difference) < 5 ~ "< 5",
                                between(abs(difference), 5, 10)~ "5-9",
                                between(abs(difference), 10, 20)~ "10-19",
                                between(abs(difference), 20, 30)~ "20-29",
                                between(abs(difference), 30, 40)~ "30-39",
                                between(abs(difference), 40, 50)~ "40-49",
                                between(abs(difference), 50, 100)~ "50-100",
                                abs(difference) > 100 ~ "> 100")) %>%
  group_by(diffence_c, winner) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(., aes(diffence_c, count, fill = winner))+
  geom_bar(stat = "identity", position = "fill", alpha = 0.95, width = 1)+
  scale_fill_grey(start = 0.2, end = 0.9)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  coord_flip()+
  scale_x_discrete(labels = c("< 5","5-9","10-19","20-29","30-39","40-49","50-100","> 100"))+
  labs(title = "Result of games", subtitle = "by difference in lichess rating", x = "Average rating difference", fill = "Colour of winner: ",
       y = "Share of wins")+
  theme_classic()


#types of wins either by black or white
ggplot(chess,aes(x=victory_status,fill=winner))+geom_bar(position = "dodge")+theme_classic()

#Result of games by average of rating
#Another issue seems to be crucial in explaining the result - the ranking on lichess, and therefore 3 features and the next two charts were allocated to this aspect. At the beginning, we check how the result of the match is distributed depending on the level of players. So we take the average ranking of both players and check the contribution of each result. we can see that the average ranking basically does not affect the distribution of results - greater differences result from ranges with smaller numbers. In all ranges, both in the case of high and low ranking people, the winnings for whites and blacks are between 40 and 50%, while draws don't exceed 15% of games.
chess %>%
  mutate(avg_rating = (white_rating+black_rating)/2) %>%
  mutate(avg_rating = case_when(avg_rating < 1000 ~ "< 1000",
                                between(avg_rating, 1000, 1100)~ "1000-1000",
                                between(avg_rating, 1100, 1200)~ "1100-1200",
                                between(avg_rating, 1200, 1300)~ "1200-1300",
                                between(avg_rating, 1300, 1400)~ "1300-1400",
                                between(avg_rating, 1400, 1500)~ "1400-1500",
                                between(avg_rating, 1500, 1600)~ "1500-1600",
                                between(avg_rating, 1600, 1700)~ "1600-1700",
                                between(avg_rating, 1700, 1800)~ "1700-1800",
                                between(avg_rating, 1800, 1900)~ "1800-1900",
                                between(avg_rating, 1900, 2000)~ "1900-2000",
                                between(avg_rating, 2000, 2100)~ "2000-2100",
                                between(avg_rating, 2100, 2200)~ "2100-2200",
                                between(avg_rating, 2200, 2300)~ "2200-2300",
                                between(avg_rating, 2300, 2400)~ "2300-2400",
                                avg_rating > 2400 ~ "> 2400")) %>%
  group_by(avg_rating, winner) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(., aes(avg_rating, count, fill = winner))+
  geom_bar(stat = "identity", position = "fill", alpha = 0.95, width = 1)+
  scale_fill_grey(start = 0.2, end = 0.9)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  coord_flip()+
  scale_x_discrete(labels = c("< 1000","1000-1100","1100-1200","1200-1300","1300-1400",
                              "1400-1500","1500-1600","1600-1700","1700-1800","1800-1900",
                              "1900-2000","2000-2100","2100-2200","2200-2300","2300-2400","> 2400"))+
  labs(title = "Result of games", subtitle = "by lichess rating (average from 2 players)", x = "Average rating", fill = "Colour of winner: ",
       y = "Share of wins")+
  theme_classic()

#Result of games by 1st move
#Finally, we move on to the last variables that cannot be determined before the game starts, i.e. the first move. According to the rules, the first move is made by the white pieces. The initial move can only be made with a pawn or a knight on the 3rd or 4th line. The most popular moves are to move the pawn to the D4 or E4 field. For these movements, the distribution of the final score is almost identical to that for the entire population of movements. Some moves are unusual, so their attempt is very small, so it's hard to judge if these moves really give you a big advantage to either side.
chess %>%
  group_by(firstMove, winner) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(., aes(firstMove, count, fill = winner))+
  geom_bar(stat = "identity", position = "fill", alpha = 0.95, width = 1)+
  scale_fill_grey(start = 0.2, end = 0.9)+
  coord_flip()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(title = "Result of games", subtitle = "by 1st move", x = "1st move (chess notation)", fill = "Colour of winner: ",
       y = "Share of wins")+
  theme_classic()



