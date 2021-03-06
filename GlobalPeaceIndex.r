# Global Peace Index
library(tidyverse)          # dplyr, tidyr, ggplot2
library(scales)             # more options for scales on plots
library(ggrepel)            # dealing with overlapping labels
library(rvest)              # for read_html(), html_table() etc...   NOTE: includes xml2
library(stringr)            # dealing with strings
library(forcats)            # change factor levels manually

# Web scrape: -------------------------------------------------------------

url <- "https://en.wikipedia.org/wiki/Global_Peace_Index"

GPI <- url %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  .[[1]] %>% 
  html_table()

wut <- url %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table(fill = T)
glimpse(wut)

.border

# Inspect scraped data ----------------------------------------------------

head(GPI, 5)

GPI %>% glimpse()

# Tidy dataset ------------------------------------------------------------
# several are tied nth place == "=10" or etc..... just manually replace as not many NAs

GPI_rank <- select(GPI, Country, ends_with("rank"))

colnames(GPI_rank) <- colnames(GPI_rank) %>% tolower()   # turn "Country" into lower case...
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

# GPI_rank <- GPI_rank %>% mutate(rank = as.integer(rank))
# GPI_rank %>% mutate(rank = str_replace(rank, "\\=", ""))         #   take out '=' in rank values...

GPI_rank %>% head(15)
# take out manually?     use duplicate()  ???
# change 'rank' variable into numeric/int

GPI_rank %>% glimpse()
GPI_rank$rank <- as.numeric(GPI_rank$rank)
GPI_rank %>% glimpse()
GPI_rank <- GPI_rank %>% arrange(year, rank)

GPI_rank %>% 
  filter(year == 2008) %>% 
  select(rank) %>%  
  duplicated() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(. == TRUE)

# use lapply() or map()
duplicated_ranks <- lapply(unique(GPI_rank$year), function(x) {
    select(x$rank) %>%  
    duplicated() %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    filter(. == TRUE)
}) 



GPI_rank[37, 3] <- 41     # Taiwan as 41 due to alphabetical order...
GPI_rank[37, 3] 
GPI_rank[38, 3] 

# Create custom theme -----------------------------------------------------

theme_peace <-  
  theme(text = element_text(family = "Gill Sans", color = "#444444", face = "bold")) +
  theme(plot.title = element_text(size = 24, hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 18)) +
  theme(axis.title = element_text(size = 14)) +
  theme(axis.title.y = element_text(angle = 0, vjust = .5, margin = margin(r = 15))) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title.x = element_text(margin = margin(t = 20))) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none")


# Plotting ----------------------------------------------------------------

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


# Subset "East Asia" region -----------------------------------------------

# create sub-region of East Asia

GPI_Asia <- GPI_rank %>% filter(country %in% c("Japan", "China", "Korea Republic", "DPR Korea", 
                                               "Philippines", "Taiwan", "Vietnam")) %>% 
  mutate(region = "East Asia")

glimpse(GPI_Asia)
GPI_Asia$rank <- as.numeric(GPI_Asia$rank)

GPI_Asia %>%
  ggplot(aes(year, as.numeric(rank), group = country)) +
  geom_line() +
  geom_point() +
  geom_text_repel(data = GPI_Asia %>% filter(year == "2008"), 
                  aes(label = country, x = "2008"), color = "black", size = 4, nudge_x = -0.9) +
  geom_text(data = GPI_Asia %>% filter(year == "2017"), 
                  aes(label = country, x = "2017"), color = "black", size = 4, nudge_x = 0.9) +
  scale_y_reverse(breaks = c(1, 5, seq(10, 160, by = 10))) +
  scale_x_discrete(expand = c(0.1, 0.05)) +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (East Asia Region)\n (2008-2017)") +
  theme_peace

# rename Korea Republic to S.Korea to fit graph label better...
GPI_Asia$country <- as.factor(GPI_Asia$country)
GPI_Asia %>% glimpse()

GPI_Asia <- GPI_Asia %>% 
   mutate(country = fct_recode(country,
                                 "S.Korea" = "Korea Republic"))

# use geom_text_repel() instead of regular geom_text() as doesnt have font-face for some stupid reason -_-"

GPI_Asia %>%
  ggplot(aes(year, as.numeric(rank), group = country)) +
  geom_line() +
  geom_point() +
  geom_label_repel(data = GPI_Asia %>% filter(year == "2008"), 
                  aes(label = country, x = "2008"), 
                  color = "black", size = 3.5, nudge_x = -0.9, 
                  fontface = "bold", segment.colour = "red") +
  geom_label_repel(data = GPI_Asia %>% filter(year == "2017"), 
                   aes(label = country, x = "2017"), 
                   color = "black", size = 3.5, nudge_x = 1.5,
                   fontface = "bold", segment.colour = "red", segment.size = 1.0) +
  scale_y_reverse(breaks = c(1, 5, seq(10, 160, by = 10))) +
  scale_x_discrete(expand = c(0.2, 0.05)) +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (East Asia Region)\n (2008-2017)") +
  theme_peace


# Piping before plotting East Asia ----------------------------------------

# Instead of creating new subset data, reduce clutter in environment by piping before plotting!
# for Error in -x : invalid argument to unary operator   >>> necessity to change variable to numeric

GPI_rank <- GPI_rank %>% 
  mutate(region = if_else(country %in% c("Japan", "China", "Korea Republic", "DPR Korea",
                                         "Philippines", "Taiwan", "Vietnam"), 
                          "East Asia", "Other"))    # all listed countries as "East Asia", others as "other"

glimpse(GPI_rank)

GPI_rank <- GPI_rank %>% 
  mutate(country = fct_recode(country,
                              "S.Korea" = "Korea Republic"))

# plot with filter()  East Asia
GPI_rank %>% 
  filter(region == "East Asia") %>% 
  ggplot(aes(year, rank, group = country)) +
  geom_line() +
  geom_point() +
  geom_text_repel(data = GPI_rank %>% filter(year == "2008", region == "East Asia"), 
                  aes(label = country, x = "2008"), 
                  color = "black", size = 4, nudge_x = -0.9) +
  geom_text_repel(data = GPI_rank %>% filter(year == "2017", region == "East Asia"), 
                  aes(label = country, x = "2017"), 
                  color = "black", size = 4, nudge_x = 0.9) +
  scale_y_reverse(breaks = c(1, 5, seq(10, 160, by = 10))) +
  scale_x_discrete(expand = c(0.2, 0.05)) +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (East Asia Region)\n (2008-2017)") +
  theme_peace

GPI_rank %>% 
  filter(rank <= 10) %>% 
  mutate(jpn = ifelse(country == "Japan", T, F))

GPI <- url %>% 
  read_html() %>% 
  html_nodes('table.wikitable:nth-child(29)') %>%
  .[[1]] %>% 
  html_table(fill = T)

######## when the nuances of geom_"blank"(data = somethingsomething) doesnt screw up my prepared code
# Then, in the next line, to filter our entire data set on the `East Asia` region that we just created. 
# All of this is possible due to the wonderful `%>%` pipes which allow for a more streamlined workflow 
# and doesn't force us to change our main dataset unless absolutely necessary as the subsetting here is 
# only called within the context of making this plot!

library(extrafont)
library(extrafontdb)
font_import() # only needs to be run once
loadfonts(device = "win")    # needed to add (device = win)!!!



GPI_rank %>% 
  filter(rank <= 10) %>% 
  mutate(jpn = ifelse(country == "Japan", T, F)) %>% 
  ggplot(aes(year, rank, group = country)) +
  geom_line(aes(color = jpn, alpha = jpn), size = 2) +
  geom_point(aes(color = jpn, alpha = jpn), size = 2.3) +
  geom_text(data = GPI_rank %>% filter(year == "2008", rank <= 10), 
            aes(label = country, x = "2008"), color = "black", size = 4, nudge_x = -1.5) +
  geom_text_repel(data = GPI_rank %>% filter(year == "2017", rank <= 10), 
                  aes(label = country, x = "2017"), color = "black", size = 4, nudge_x = 1.5) +
  scale_y_reverse(breaks = pretty_breaks(10)) +
  scale_x_discrete(expand = c(0.15, 0.1)) +
  scale_color_manual(values = c("#104E8B", "#EE2C2C")) +     # set all non-Japan to blue, Japan to red
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (Top 10)", subtitle = "(2008-2017)") +
  theme_peace




