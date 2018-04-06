write.csv(GPI_rank, file = "~/R_materials/GPI_rank.csv", row.names = FALSE)

GPI_rank <- read.csv("~/R_materials/GPI_rank.csv", header = TRUE)

library(tidyverse)
library(ggthemes)
library(dplyr)
library(scales)

glimpse(GPI_rank)

library(extrafont)                 # for fonts in titles and labels

theme_peace <-  
  theme(text = element_text(family = "Garamond", color = "#444444", face = "bold"),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(angle = 0, vjust = 0.5, margin = margin(r = 15)),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 15)),
        legend.title = element_blank(),
        legend.position = "none")

GPI_rank %>% 
  group_by(year) %>% 
  filter(rank <= 10) %>% 
  spread(country, rank) %>% 
  mutate(diff = max(rank) - min(rank))


GPI_rank %>% 
  group_by(year) %>% 
  filter(rank <= 10) %>% 
  group_by(country, year) %>% 
  mutate(diff = max(rank) - min(rank))


GPI_rank %>% 
  group_by(year) %>% 
  filter(rank <= 10) %>% 
  spread(year, rank) %>% 
  mutate(diff = `2008`-`2017`,
         diff2 = `2008`-`2016`)



GPI_rank %>% 
  filter(rank <= 10) %>% 
  ggplot(aes(year, rank, group = country)) +
  geom_line(aes(color = country), size = 2) +
  geom_point(aes(color = country), size = 2.3) +
  scale_y_reverse(breaks = pretty_breaks(10)) +
  scale_x_continuous(expand = c(0.15, 0.1), breaks = 2008:2017) +
  geom_text(data = GPI_rank %>% filter(year == "2008", rank <= 10), 
            aes(label = country), color = "black", size = 4, nudge_x = -1.5) +
  geom_text(data = GPI_rank %>% filter(year == "2017", rank <= 10), 
            aes(label = country), color = "black", size = 4, nudge_x = 1.5) +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (Top 10)", subtitle = "(2008-2017)") +
  theme_peace

# all top_10 with colors

glimpse(GPI_rank)

top_countries <- GPI_rank %>% 
  filter(rank %in% c(1:10)) %>% 
  pull(country) %>% 
  unique() %>% 
  as.character()

GPI_rank <- GPI_rank %>% 
  mutate(country = as.character(country)) %>%       # FAAAAACTTTTOOOORRSSS     AHHHHHHHHHHHHHHH
  mutate(top_10 = ifelse(country %in% top_countries, TRUE, FALSE)) %>% 
  mutate(country_col = ifelse(top_10 == TRUE, country, "zzz"))

library(RColorBrewer)
top_countries[[17]] <- c("zzz")
top_countries
cols <- colorRampPalette(brewer.pal(12, "Paired"))(length(top_countries))
brewer.pal(11, "BrBG")


GPI_rank %>% 
  ggplot(aes(year, rank, group = country)) +
  geom_line(aes(color = country_col), size = 2) +
  geom_point(aes(color = country_col), size = 2.3) +
  scale_y_reverse(breaks = pretty_breaks(10)) +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  geom_text(data = GPI_rank %>% filter(year == "2008", rank <= 10), 
            aes(label = country), color = "black", size = 4, nudge_x = -0.4, family = "Garamond") +
  geom_text(data = GPI_rank %>% filter(year == "2017", rank <= 10), 
            aes(label = country), color = "black", size = 4, nudge_x = 0.5, family = "Garamond") +
  scale_colour_manual(values = cols) +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (Top 10)", subtitle = "(2008-2017)") +
  theme_peace +
  theme(panel.background = element_blank()) +
  coord_cartesian(ylim = c(1, 10.5))



## ggflags!

library(ggflags)


flag_start <- data_frame(
  x = 2007.8, y = 1:10,
  country = c("nz", "ie", "jp", "ch", "dk", "ca", "fi", "se", "no", "at")
) 

flag_end <- data_frame(
  x = 2017.4, y = 1:10,
  country = c("is", "nz", "pt", "at", "dk", "cz", "si", "ca", "ch", "ie")
)


library(ggflags)
library(ggplot2)

GPI_rank %>% 
  ggplot(aes(year, rank, group = country)) +
  geom_line(aes(color = country_col), size = 2, alpha = 0.65) +
  geom_point(aes(color = country_col), size = 3.25, alpha = 0.85) +
  scale_y_reverse(breaks = pretty_breaks(10)) +
  scale_x_continuous(breaks = pretty_breaks(10)) +
  geom_flag(data = flag_start, aes(x = x, y = y, country = country, size = 4)) +
  geom_flag(data = flag_end, aes(x = x, y = y, country = country, size = 4)) +
  scale_colour_manual(values = cols) +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (Top 10)", subtitle = "(2008-2017)") +
  theme_peace +
  theme(panel.background = element_blank()) +
  coord_cartesian(ylim = c(1, 10.5))










# facetted
GPI_rank %>% 
  filter(rank <= 10) %>% 
  ggplot(aes(year, rank, group = country)) +
  geom_line(aes(color = country), size = 2) +
  geom_point(aes(color = country), size = 2.3) +
  scale_y_reverse(breaks = pretty_breaks(10)) +
  scale_x_discrete(expand = c(0.15, 0.1)) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (Top 10)", subtitle = "(2008-2017)") +
  theme_peace +
  theme(panel.background = element_blank()) +
  facet_wrap(~country)

# filter out countries that appear less than 3 times
GPI_rank %>% 
  filter(rank <= 10) %>% 
  group_by(country) %>% 
  mutate(appearance = (n() > 3)) %>% 
  filter(appearance == TRUE) %>% 
  ggplot(aes(year, rank, group = country)) +
  geom_line(aes(color = country), size = 2) +
  geom_point(aes(color = country), size = 2.3) +
  scale_y_reverse(breaks = pretty_breaks(10)) +
  scale_x_discrete(expand = c(0.15, 0.1)) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (Top 10)", subtitle = "(2008-2017)") +
  theme_peace +
  theme(panel.background = element_blank()) +
  facet_wrap(~country, nrow = 2)



GPI_rank %>% 
  filter(rank <= 10) %>% 
  mutate(jpn = ifelse(country == "Japan", T, F)) %>% 
  ggplot(aes(year, rank, group = country)) +
  geom_line(aes(color = jpn, alpha = jpn), size = 2) +
  geom_point(aes(color = jpn, alpha = jpn), size = 2.3) +
  geom_text(data = GPI_rank %>% filter(year == "2008", rank <= 10), 
            aes(label = country, x = "2008"), color = "black", size = 4, nudge_x = -1.5) +
  geom_text(data = GPI_rank %>% filter(year == "2017", rank <= 10), 
            aes(label = country, x = "2017"), color = "black", size = 4, nudge_x = 1.5) +
  scale_y_reverse(breaks = pretty_breaks(10)) +
  scale_x_discrete(expand = c(0.15, 0.1)) +
  scale_color_manual(values = c("#104E8B", "#EE2C2C")) + 
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (Top 10)", subtitle = "(2008-2017)")



