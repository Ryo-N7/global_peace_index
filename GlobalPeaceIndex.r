# Global Peace Index
library(tidyverse)
library(tidytext)
library(scales)
library(ggrepel)
library(rvest)              # for read_html(), html_table() etc...   NOTE: includes xml2
library(stringr)   


# Web scrape: -------------------------------------------------------------

url <- "https://en.wikipedia.org/wiki/Global_Peace_Index"

GPI <- url %>% read_html() %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  .[[1]] %>% 
  html_table(fill = T)


# Inspect scraped data ----------------------------------------------------

head(GPI, 5)

GPI %>% glimpse()
str(GPI)


# Tidy dataset ------------------------------------------------------------
as.numeric(GPI$`2017 rank`)   # several are tied nth place == "=10" or etc..... just manually replace as not many NAs

# namesGPI <- names(GPI)

GPI_rank <- select(GPI, Country, ends_with("rank"))

colnames(GPI_rank) <- colnames(GPI_rank) %>% tolower()
colnames(GPI_rank)

#  GPI_rank_tidy <- GPI_rank %>% gather(`2017 rank`:`2008 rank`, key = "year", value = "rank")

GPI_rank <- GPI_rank %>% gather(`2017 rank`:`2008 rank`, key = "year", value = "rank")
str(GPI_rank)

GPI_rank$year <- GPI_rank$year %>% str_replace_all("rank", "") %>% trimws()
str(GPI_rank)

# another (longer) way to do this is to just recode each name manually...
# GPI_rank <- GPI_rank %>% 
#   mutate(year = fct_recode(year,
#                                 "2008" = "2008 rank", 
#                                 "2009" = "2009 rank", 
#                                 "2010" = "2010 rank",
#                                 "2011" = "2011 rank",
#                                 "2012" = "2012 rank",
#                                 "2013" = "2013 rank",
#                                 "2014" = "2014 rank",
#                                 "2015" = "2015 rank",
#                                 "2016" = "2016 rank",
#                                 "2017" = "2017 rank"))

# useful to turn year variable into factor for when need to graph later on... + need to take out "rank" for each.
GPI_rank <- GPI_rank %>% mutate(year = as.factor(year))
levels(GPI_rank$year)     # now as factor + no "rank" afterwards.

str(GPI_rank)

# change 'rank' variable into numeric/int


as.numeric(GPI_rank$rank)
as.integer(GPI_rank$rank)
# GPI_rank <- GPI_rank %>% mutate(rank = as.integer(rank))
# GPI_rank %>% mutate(rank = str_replace(rank, "\\=", ""))                 #   take out '=' in rank values...

GPI_rank %>% head(15)
# take out manually?

GPI_rank %>% filter(year == "2008") %>% select(rank) %>% duplicated()
GPI_rank08 <- GPI_rank %>% filter(year == "2008")
GPI_rank08$rank <- as.numeric(GPI_rank08$rank)
GPI_rank08 <- GPI_rank08 %>% arrange(rank)
glimpse(GPI_rank08)




GPI_rank[10:11, 3]
GPI_rank[10:11, 3] <- c(10, 11)
GPI_rank[10:11, 3]

GPI_rank[19:20, 3]
GPI_rank[19:20, 3] <- c(19, 20)
GPI_rank[19:20, 3]

GPI_rank[41:42, 3]
GPI_rank[41:42, 3] <- c(41, 42)
GPI_rank[41:42, 3]

GPI_rank[84:85, 3]
GPI_rank[84:85, 3] <- c(84, 85)
GPI_rank[84:85, 3]

GPI_rank[97:98, 3]
GPI_rank[97:98, 3] <- c(97, 98)
GPI_rank[97:98, 3]

GPI_rank[146:147, 3]
GPI_rank[146:147, 3] <- c(146, 147)
GPI_rank[146:147, 3]

GPI_rank[155:156, 3]
GPI_rank[155:156, 3] <- c(155, 156)
GPI_rank[155:156, 3]


GPI_rank %>% glimpse()
GPI_rank$rank <- as.numeric(GPI_rank$rank)
GPI_rank %>% glimpse()

theme_peace <-  
  theme(text = element_text(family = "Gill Sans", color = "#444444")) +
  theme(plot.title = element_text(size = 24, hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5, margin = margin(r = 15))) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(margin = margin(t = 20))) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none")

GPI_rank %>% 
  filter(rank <= 10) %>% 
  mutate(jpn = ifelse(country == "Japan", T, F)) %>% 
  ggplot(aes(year, rank, group = country)) +
  geom_line(aes(color = jpn, alpha = jpn), size = 2) +
  geom_point(aes(color = jpn, alpha = jpn), size = 2.3) +
  geom_text(data = GPI_rank %>% filter(year == "2008", rank <= 10), aes(label = country, x = "2008"), color = "black", size = 4, nudge_x = -0.5) +
  geom_text(data = GPI_rank %>% filter(year == "2017", rank <= 10), aes(label = country, x = "2017"), color = "black", size = 4, nudge_x = 0.5) +
  scale_y_reverse(breaks = pretty_breaks(10)) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index\n (2008-2017)") +
  theme_peace
  
GPI_Asia %>% filter(country %in% c("Korea Republic", "Taiwan") & year == "2008")

# create sub-region of East Asia


library(gapminder)
geo <- as.tibble(gapminder)

GPI_Asia <- GPI_rank %>% filter(country %in% c("Japan", "China", "Korea Republic", "DPR Korea", 
                                "Philippines", "Taiwan")) %>% 
  mutate(region = "East Asia")

# Instead of creating new subset data, reduce clutter in environment by piping before plotting!

GPI_Asia %>%
  ggplot(aes(year, rank, group = country)) +
  geom_line() +
  geom_point() +
  geom_text_repel(data = GPI_Asia %>% filter(year == "2008"), aes(label = country, x = "2008"), color = "black", size = 4, nudge_x = -0.5) +
  geom_text(data = GPI_Asia %>% filter(year == "2017"), aes(label = country, x = "2017"), color = "black", size = 4, nudge_x = 0.5) +
  scale_y_reverse(breaks = c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160)) +
  scale_x_discrete(expand = c(0.1, 0.05)) +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (East Asia Region)\n (2008-2017)") +
  theme_peace


# split rank and score, gather them on YEAR, recombine on YEAR!

url.world_ports <- url("http://sharpsightlabs.com/wp-content/datasets/world_ports.RData")

load(url.world_ports)

glimpse(df.world_ports)
