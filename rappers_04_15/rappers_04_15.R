# loading necessary packages
library(tidyverse)
library(ggplot2)
library(Cairo)
library(ggpubr)
library(stringr)
library(extrafont)
library(ggrepel)
library(patchwork)
library(magick)

# loading rankings data
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# cleaning rankings data for typos, group names that are interpsersed by '&', 
# and splitting the data into artsits + calculating points accumulated
rankings_cleaned <- rankings %>% 
  mutate(artist = ifelse(artist == "Grandmaster Flash & The Furious Five",
                         "The Furious Five", 
                  ifelse(artist == "Pete Rock & C.L. Smooth",
                         "Pete Rock - C.L. Smooth",
                  ifelse(artist == "Afrika Bambaataa & The Soulsonic Force",
                         "The Soulsonic Force",
                  ifelse(artist == "Doug E Fresh & The Get Fresh Crew",
                         "The Get Fresh Crew", artist)))))%>% 
  separate_rows(artist, sep =  "ft.") %>%
  separate_rows(artist, sep =  "&") %>%
  separate_rows(artist, sep =  "feat") %>%
  separate_rows(artist, sep =  "and") %>%
  filter(artist != "&" & artist != "ft.") %>% 
  mutate(artist = trimws(artist)) %>% 
  mutate(artist = ifelse(artist == "Wu-Tan Clan", "Wu-Tang Clan", 
                  ifelse(artist == "The Notorious B.I.G", "The Notorious B.I.G.",
                  ifelse(artist == "Snoop Doggy Dogg", "Snoop Dogg", 
                  ifelse(artist == "OutKast", "Outkast", 
                  ifelse(artist == "Missy Elliot", "Missy Elliott", 
                  ifelse(artist == "master Flash", "Grandmaster Flash",
                  ifelse(artist == "master Melle Mel", "Grandmaster Melle Mel", artist)))))))) %>%
  group_by(artist, gender) %>% summarise(freq = n(), points = sum(points),
                                         song_numbers = length(unique(title))) %>%
  ungroup() %>% group_by(artist) %>%
  summarise(x_listed = sum(song_numbers), points_total = sum(points)) %>% ungroup() %>%
  mutate(points_to_x_listed = points_total/x_listed)


# filter for top 10 artists by total points accumulated and create a column to rank the points
top_10 <- rankings_cleaned %>% top_n(14, wt = points_total) %>% 
          mutate(rank_points = as.numeric(ordered(-points_total))) %>%
          arrange(desc(points_total))

# classify the column of rank points as factor
top_10$rank_points <- factor(top_10$rank_points)

# adjusting the artist names with white space padding so that they 
# look good when they're labeled on the lollipop plot
artist_labs <- data.frame(x = unique(top_10$points_total),
                          y = unique(top_10$rank_points),
                          names = c(
                                  str_pad("The Notorious B.I.G.",
                                            width = 45),
                                  str_pad("Tied: Outkast & Public Enemy",
                                            width = 61),
                                  str_pad("Kanye West",
                                           width = 24),
                                  str_pad("Nas", width = 10),
                                  str_pad("JAY-Z", width = 14),
                                  str_pad("Tied: 2Pac & Wu-Tang Clan",
                                          width = 55),
                                  str_pad("Dr. Dre", width = 18),
                                  str_pad("Tied: Mobb Deep & Snoop Dogg",
                                           width = 60),
                                  str_pad("Tied: Rakim & The Furious Five",
                                          width = 65),
                                  str_pad("Kendrick Lamar",
                                          width = 33))) 

# specify color scheme for the lollipop plot
colors_set <- c("#B22222", "#DC143C", "#FF0000",
                "#FF6347", "#FF7F50", "#CD5C5C",
                "#F08080", "#E9967A", "#FA8072",
                "#FFA07A")

# plot the lollipop for the top 10 artists by points
top <- ggdotchart(top_10, x = "rank_points", y = "points_total",
           palette = rev(colors_set),
           color = "rank_points",
           sorting = "descending",
           add = "segments",                             
           rotate = TRUE,
           dot.size = 8,            
           label = "points_total",
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5, 
                             family = "Andale Mono"),               
           ggtheme = theme_pubr()) + 
  xlab("Ranking by Total Points") +
  ylab("Total Points Awarded by Critics") + 
  ggtitle("Top 10 Artists Chosen by Critics") +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5, family = "Andale Mono"),
        axis.text = element_text(family = "Andale Mono"),
        axis.title = element_text(family = "Andale Mono")) +
  scale_y_continuous(limits = c(0,300)) + 
  geom_text(artist_labs, mapping = aes(x = y, y = x, label = names,
                                       family = "Andale Mono")) 
  
# the following lines reshuffle the color palette from the lollipop
# plot to fit with the color palette in the bubble plot
# at the end, both plots should have the same color for the same ranking
temp <- top_10 %>% arrange(x_listed)
x_bubbles <- c(temp$rank_points)
color_array <- NULL

bubbles_set <- sapply(1:length(x_bubbles), function(x)
               append(color_array, colors_set[x_bubbles[x]]))

bubbles_color <- c("The Furious Five" = "#FA8072",
                   "Mobb Deep" = "#E9967A",
                   "Public Enemy" = "#DC143C",
                   "Snoop Dogg" = "#E9967A",
                   "Wu-Tang Clan" = "#CD5C5C",
                   "Dr Dre" = "#F08080",
                   "2Pac" = "#CD5C5C",
                   "Rakim" = "#FA8072",
                   "The Notorious B.I.G." = "#B22222",
                   "Nas" = "#FF6347",
                   "Outkast" = "#DC143C",
                   "Kendrick Lamar" = "#FFA07A",
                   "Kanye West" = "#FF0000",
                   "JAY-Z" = "#FF7F50")

# make bubble plot. y-axis is number of songs an artist was listed on
# x-axis is number of points awarded
# bubbles sized by the ratio of points/number of songs
bubbles <- rankings_cleaned %>% filter(points_total >= 84) %>%
ggplot(aes(x=x_listed, y = points_total, 
           size = points_to_x_listed,
           color = artist)) +
geom_point(alpha = 0.5, position = "jitter") + 
scale_color_manual("Status", values = bubbles_color) + 
scale_size(range = c(.1, 24), name="artist") + theme_minimal() +
xlab("Number of Songs Including the Artist") +
ylab("Total Points Awarded by Critics") + 
ggtitle("Influence Level of Artists") + 
theme(legend.position = 'none',
      axis.text = element_text(family = "Andale Mono"),
      axis.title = element_text(family = "Andale Mono"),
      plot.title = element_text(hjust = 0.5,family = "Andale Mono")) + 
scale_y_continuous(limits = c(50,220)) +
scale_x_continuous(limits = c(0, 22)) +
geom_text_repel(data = subset(rankings_cleaned, 
                              artist == "The Furious Five" |
                              artist == "The Notorious B.I.G." |
                              artist == "JAY-Z" |
                              artist == "Rakim" |
                              artist == "Kendrick Lamar"|
                              artist == "Public Enemy" |
                              artist == "Outkast"),
         aes(x_listed,points_total,label=artist, 
             family = "Andale Mono"), 
         point.padding = 0.4, seed = 1234, size = 5,
         nudge_y = -15, nudge_x = 1.5)

# opening a png path to the image
png(filename = "rappers_04_15/rappers.png", type = "cairo",
    units = "in", width = 13.5, height = 8, pointsize = 10, res = 400)

# plotting the final figure using patchwork
top + bubbles +
plot_annotation(title = "Battle of the Best",
                subtitle = 
                "In 2019, BBC Music polled hundreds of music critics for the five best hip-hop songs from a list of over 280 songs. Each song was assigned\npoints based on their positions in critics' rankings. The lollipop chart shows the top 10 artists based on the number of points gained from\nall of their songs on the polling list.The bubble chart shows the number of points awarded by critics relative to the number of songs on the \npolling list that were performed by the top 10 artists.Bubble size shows the ratio of points awarded to number of songs listed. While The\nNotorious B.I.G. gained the most critics' points, The Furious Five has the most points awarded relative to the number of songs that they had\non the polling list.",
                
               caption = "Source: @LinhKTP",
               theme = theme(plot.title = element_text(size = 18,
                                                       family = "Andale Mono"),
                             plot.subtitle = element_text(family = "Andale Mono"),
                             plot.caption = element_text(size = 12, family = "Andale Mono")))

dev.off()
