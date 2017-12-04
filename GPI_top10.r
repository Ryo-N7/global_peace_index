write.csv(GPI_rank, file = "~/R_materials/GPI_rank.csv", row.names = FALSE)

GPI_rank <- read.csv("~/R_materials/GPI_rank.csv", header = TRUE)


glimpse(GPI_rank)


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
  geom_line(aes(color = country, alpha = country), size = 2) +
  geom_point(aes(color = country), size = 2.3) +
  geom_text(data = GPI_rank %>% filter(year == "2008", rank <= 10), 
            aes(label = country, x = "2008"), color = "black", size = 4, nudge_x = -1.5) +
  geom_text(data = GPI_rank %>% filter(year == "2017", rank <= 10), 
            aes(label = country, x = "2017"), color = "black", size = 4, nudge_x = 1.5) +
  scale_y_reverse(breaks = pretty_breaks(10)) +
  scale_x_discrete(expand = c(0.15, 0.1)) +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (Top 10)", subtitle = "(2008-2017)") 

# all top_10 with colors
GPI_rank %>% 
  filter(rank <= 10) %>% 
  ggplot(aes(year, rank, group = country)) +
  geom_line(aes(color = country), size = 2) +
  geom_point(aes(color = country), size = 2.3) +
  geom_text(data = GPI_rank %>% filter(year == "2008", rank <= 10), 
            aes(label = country, x = "2008"), color = "black", size = 4, nudge_x = -1.5) +
  geom_text(data = GPI_rank %>% filter(year == "2017", rank <= 10), 
            aes(label = country, x = "2017"), color = "black", size = 4, nudge_x = 1.5) +
  scale_y_reverse(breaks = pretty_breaks(10)) +
  scale_x_discrete(expand = c(0.15, 0.1)) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "Year", y = "Rank") +
  ggtitle("Global Peace Index (Top 10)", subtitle = "(2008-2017)") +
  theme_peace +
  theme(panel.background = element_blank())


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







