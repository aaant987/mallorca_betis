library(tidyverse)

library(worldfootballR)

windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))

library(extrafont)
loadfonts(device = "win")


x1 <- get_match_summary(match_url = "https://fbref.com/en/matches/7562d480/Mallorca-Real-Betis-October-7-1990-La-Liga")
x2 <- get_match_summary(match_url = "https://fbref.com/en/matches/73628c75/Mallorca-Real-Betis-December-13-1997-La-Liga")
x3 <- get_match_summary(match_url = "https://fbref.com/en/matches/6e72a8c8/Mallorca-Real-Betis-December-5-1998-La-Liga")
x4 <- get_match_summary(match_url = "https://fbref.com/en/matches/de83c6bc/Mallorca-Real-Betis-April-30-2000-La-Liga")
x5 <- get_match_summary(match_url = "https://fbref.com/en/matches/1d2e2288/Mallorca-Real-Betis-April-28-2002-La-Liga")
x6 <- get_match_summary(match_url = "https://fbref.com/en/matches/2f687f39/Mallorca-Real-Betis-March-9-2003-La-Liga")
x7 <- get_match_summary(match_url = "https://fbref.com/en/matches/f2ea65c7/Mallorca-Real-Betis-November-30-2003-La-Liga")
x8 <- get_match_summary(match_url = "https://fbref.com/en/matches/05f1c579/Mallorca-Real-Betis-May-29-2005-La-Liga")
x9 <- get_match_summary(match_url = "https://fbref.com/en/matches/ba7d38e4/Mallorca-Real-Betis-January-8-2006-La-Liga")
x10 <- get_match_summary(match_url = "https://fbref.com/en/matches/5547c6e6/Mallorca-Real-Betis-March-17-2007-La-Liga")
x11 <- get_match_summary(match_url = "https://fbref.com/en/matches/9a99213b/Mallorca-Real-Betis-February-24-2008-La-Liga")
x12 <- get_match_summary(match_url = "https://fbref.com/en/matches/39141a28/Mallorca-Real-Betis-March-8-2009-La-Liga")
x13 <- get_match_summary(match_url = "https://fbref.com/en/matches/4d34806c/Mallorca-Real-Betis-February-4-2012-La-Liga")
x14 <- get_match_summary(match_url = "https://fbref.com/en/matches/c3b7556b/Mallorca-Real-Betis-May-20-2013-La-Liga")
x15 <- get_match_summary(match_url = "https://fbref.com/en/matches/0a6543f1/Mallorca-Real-Betis-November-30-2019-La-Liga")



mall_bet_summary <-  rbind(x1, x2, x3,
                           x4, x5, x6,
                           x7, x8, x9, 
                           x10, x11, x12,
                           x13, x14, x15)

goals <- mall_bet_summary %>% 
  filter(Event_Type == "Goal") %>% 
  group_by(Event_Type, Event_Players) %>% 
  count(Event_Type) %>% 
  arrange(desc(n))
goals








y1 <- get_match_report(match_url = "https://fbref.com/en/matches/7562d480/Mallorca-Real-Betis-October-7-1990-La-Liga")
y2 <- get_match_report(match_url = "https://fbref.com/en/matches/73628c75/Mallorca-Real-Betis-December-13-1997-La-Liga")
y3 <- get_match_report(match_url = "https://fbref.com/en/matches/6e72a8c8/Mallorca-Real-Betis-December-5-1998-La-Liga")
y4 <- get_match_report(match_url = "https://fbref.com/en/matches/de83c6bc/Mallorca-Real-Betis-April-30-2000-La-Liga")
y5 <- get_match_report(match_url = "https://fbref.com/en/matches/1d2e2288/Mallorca-Real-Betis-April-28-2002-La-Liga")
y6 <- get_match_report(match_url = "https://fbref.com/en/matches/2f687f39/Mallorca-Real-Betis-March-9-2003-La-Liga")
y7 <- get_match_report(match_url = "https://fbref.com/en/matches/f2ea65c7/Mallorca-Real-Betis-November-30-2003-La-Liga")
y8 <- get_match_report(match_url = "https://fbref.com/en/matches/05f1c579/Mallorca-Real-Betis-May-29-2005-La-Liga")
y9 <- get_match_report(match_url = "https://fbref.com/en/matches/ba7d38e4/Mallorca-Real-Betis-January-8-2006-La-Liga")
y10 <- get_match_report(match_url = "https://fbref.com/en/matches/5547c6e6/Mallorca-Real-Betis-March-17-2007-La-Liga")
y11 <- get_match_report(match_url = "https://fbref.com/en/matches/9a99213b/Mallorca-Real-Betis-February-24-2008-La-Liga")
y12 <- get_match_report(match_url = "https://fbref.com/en/matches/39141a28/Mallorca-Real-Betis-March-8-2009-La-Liga")
y13 <- get_match_report(match_url = "https://fbref.com/en/matches/4d34806c/Mallorca-Real-Betis-February-4-2012-La-Liga")
y14 <- get_match_report(match_url = "https://fbref.com/en/matches/c3b7556b/Mallorca-Real-Betis-May-20-2013-La-Liga")
y15 <- get_match_report(match_url = "https://fbref.com/en/matches/0a6543f1/Mallorca-Real-Betis-November-30-2019-La-Liga")




mall_bet_summary_report <-  bind_rows(y1, y2, y3,
                                      y4, y5, y6,
                                      y7, y8, y9, 
                                      y10, y11, y12,
                                      y13, y14, y15)






mall_bet_summary_report$Season

z1 <- get_match_results(country = "ESP", gender = "M", season_end_year = 1991, tier = "1st")
z2 <- get_match_results(country = "ESP", gender = "M", season_end_year = 1998, tier = "1st")
z3 <- get_match_results(country = "ESP", gender = "M", season_end_year = 1999, tier = "1st")
z4 <- get_match_results(country = "ESP", gender = "M", season_end_year = 2000, tier = "1st")
z5 <- get_match_results(country = "ESP", gender = "M", season_end_year = 2002, tier = "1st")
z6 <- get_match_results(country = "ESP", gender = "M", season_end_year = 2003, tier = "1st")
z7 <- get_match_results(country = "ESP", gender = "M", season_end_year = 2004, tier = "1st")
z8 <- get_match_results(country = "ESP", gender = "M", season_end_year = 2005, tier = "1st")
z9 <- get_match_results(country = "ESP", gender = "M", season_end_year = 2006, tier = "1st")
z10 <- get_match_results(country = "ESP", gender = "M", season_end_year = 2007, tier = "1st")
z11 <- get_match_results(country = "ESP", gender = "M", season_end_year = 2008, tier = "1st")
z12 <- get_match_results(country = "ESP", gender = "M", season_end_year = 2009, tier = "1st")
z13 <- get_match_results(country = "ESP", gender = "M", season_end_year = 2012, tier = "1st")
z14 <- get_match_results(country = "ESP", gender = "M", season_end_year = 2013, tier = "1st")
z15 <- get_match_results(country = "ESP", gender = "M", season_end_year = 2020, tier = "1st")


summary_match_results <- bind_rows(
  z1, z2, z3,
  z4, z5, z6,
  z7, z8, z9,
  z10, z11, z12,
  z13, z14, z15)



summary_match_results$Away <- recode(summary_match_results$Away, Betis = "Real Betis")


mall_betis_match_results <- summary_match_results %>% 
  filter(Home == "Mallorca", Away == "Real Betis") 

mall_betis_match_results %>% 
  count(Referee) %>% 
  arrange(desc(n))


mall_betis_match_results <- mall_betis_match_results %>% 
  mutate(result_betis =
           case_when(HomeGoals < AwayGoals ~ "win", 
                     HomeGoals > AwayGoals  ~ "lose",
                     HomeGoals == AwayGoals ~ "draw")) %>% 
  mutate(final_position_betis =
           case_when(Season_End_Year == 1991 ~ 20,
                     Season_End_Year == 1998 ~ 8,
                     Season_End_Year == 1999 ~ 11,
                     Season_End_Year == 2000 ~ 18,
                     Season_End_Year == 2002 ~ 6,
                     Season_End_Year == 2003 ~ 8,
                     Season_End_Year == 2004 ~ 9,
                     Season_End_Year == 2005 ~ 4,
                     Season_End_Year == 2006 ~ 14,
                     Season_End_Year == 2007 ~ 16,
                     Season_End_Year == 2008 ~ 13,
                     Season_End_Year == 2009 ~ 18,
                     Season_End_Year == 2012 ~ 13,
                     Season_End_Year == 2013 ~ 7,
                     Season_End_Year == 2020 ~ 15)) %>% 
  relocate(result_betis, .after = AwayGoals) %>% 
  relocate(final_position_betis, .after = result_betis) %>% 
  mutate(final_position_mallorca =
           case_when(Season_End_Year == 1991 ~ 15,
                     Season_End_Year == 1998 ~ 5,
                     Season_End_Year == 1999 ~ 3,
                     Season_End_Year == 2000 ~ 10,
                     Season_End_Year == 2002 ~ 16,
                     Season_End_Year == 2003 ~ 9,
                     Season_End_Year == 2004 ~ 11,
                     Season_End_Year == 2005 ~ 17,
                     Season_End_Year == 2006 ~ 13,
                     Season_End_Year == 2007 ~ 12,
                     Season_End_Year == 2008 ~ 7,
                     Season_End_Year == 2009 ~ 9,
                     Season_End_Year == 2012 ~ 8,
                     Season_End_Year == 2013 ~ 18,
                     Season_End_Year == 2020 ~ 19)) %>% 
  unite("scoreboard", HomeGoals, AwayGoals, sep = "-", remove = FALSE)






mall_betis_match_results %>% 
  count(result_betis) %>% 
  arrange(desc(n))

mall_betis_match_results %>% 
  filter(Day == "Sat") %>% 
  count(result_betis) %>% 
  arrange(desc(n))


p20 <- mall_betis_match_results %>% 
  ggplot(aes(x = interaction(scoreboard, Season_End_Year), y = final_position_betis)) +
  geom_point(aes(y = final_position_betis), color = "darkgreen", size = 8, shape = 18) +
  geom_point(aes(y = final_position_mallorca), color = "darkred", size = 8, shape = 18) +
  scale_y_reverse(limits = c(20, 1), breaks = seq(20, 1), name = "Final ranking in the championship") +
  scale_x_discrete(name = "Results by season end year") +
  theme(text=element_text(family = "Bahnschrift", face = "bold"),
        plot.background=element_rect(fill = "lightblue3"),
        panel.background = element_rect(fill = "lightblue3"),
        panel.grid = element_line(colour = "white"),
        axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black"))

p20


library(gt)


goals <- mall_bet_summary %>% 
  filter(Event_Type == "Goal") %>% 
  group_by(Event_Type, Event_Players) %>% 
  count(Event_Type) %>% 
  arrange(desc(n)) %>% 
  head(4) 
goals

goals$Event_Type <- NULL

goals <- goals %>% 
  rename(Player = Event_Players) %>% 
  rename(Goals = n)



library(hrbrthemes)
library(ggpubr)


tab_goals <- ggtexttable(head(goals), theme = ttheme("blank")) %>%
  tab_add_title(text = "", face = "plain", size = 4) %>%
  tab_add_title(text = "Players with 2 or more goals", face = "bold", 
                size = 6.5, padding = unit(0.1, "line")) %>%
  tab_add_footnote(text = "", size = 10, face = "bold.italic") +
  theme(plot.background = element_rect(fill = "lightblue3", color = "lightblue3"),
        text=element_text(family = "Bahnschrift", face = "bold"))
tab_goals




referees <- mall_betis_match_results %>% 
  count(Referee) %>% 
  arrange(desc(n)) %>% 
  rename(Matches = n) %>% 
  head(3)


tab_referees <- ggtexttable(head(referees), theme = ttheme("blank")) %>%
  tab_add_title(text = "", face = "plain", size = 4) %>%
  tab_add_title(text = "Referees with 2 or more matches", face = "bold", 
                size = 7, padding = unit(0.1, "line")) %>%
  tab_add_footnote(text = "", size = 10, face = "bold.italic") +
  theme(plot.background = element_rect(fill = "lightblue3", color = "lightblue3"),
        text=element_text(family = "Bahnschrift", face = "bold"))
tab_referees



library(hrbrthemes)
library(waffle)

waffle_result <- mall_betis_match_results %>% 
  count(result_betis) %>% 
  arrange(desc(n))


waffle_result %>% 
  ggplot(aes(fill = result_betis, values = n)) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = NULL, colour = NULL) + 
  theme(legend.position = "top",
        legend.background = element_rect(fill = "lightblue3"),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_rect(fill = "lightblue3", color = "lightblue3"),
        text=element_text(family = "Bahnschrift", face = "bold")) -> waf

p30 <- waf +
  geom_waffle(
    n_rows = 3,
    size = 5, colour = "black", flip = TRUE, 
    make_proportional = F
  ) +
  scale_fill_manual(name = "", labels = c("Draw", "Mallorca W", "Betis W"),
                    values = c("yellow", "darkred", "darkgreen")) +
  theme(legend.position = "top", 
        legend.background = element_rect(fill = "lightblue3"),
        legend.key = element_rect(fill = "black", color = "black"),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_rect(fill = "lightblue3", color = "lightblue3"),
        text=element_text(family = "Bahnschrift", face = "bold"))
p30


library(patchwork)


patchwork_ <- p20 + ((tab_goals / tab_referees+ plot_layout(ncol = 1)) / p30 )

g <- patchwork_ + plot_annotation(
  title = 'Last 15 visits of Real Betis to the Mallorca stadium',
  subtitle = '',
  caption = 'Source: fbref.com | @dataR_amateur') +
  plot_layout(widths = c(3,0.5)) +
  theme(legend.position = "top")

g <- g + plot_annotation(theme = theme(plot.background = element_rect(fill  = 'lightblue3', color = "lightblue3")))

g <- g + plot_annotation(theme = theme(text=element_text(family = "Bahnschrift", face = "bold"))) 

g <- g + plot_annotation(theme = theme(plot.title = element_text(size = 18, hjust = 0.5),
                                       plot.caption = element_text(size = 9.5, hjust = 1)))

g


