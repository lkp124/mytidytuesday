library(tidyverse)

tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv') %>%
               group_by(winner_name) %>% summarise(avg_distance = mean(distance)) %>%
               mutate(winner_name = str_pad(winner_name, width = 51, side = "right"))

library(ggplot2)
library(extrafont)
font_import()

library(gganimate)
library(gifski)

plot <- ggplot(tdf_winners, aes(winner_name, avg_distance)) +
  geom_bar(width = 0.5, stat = "identity", color = "gray",
           alpha = 0.4) + ggtitle("Average of Distances (Kilometers) Biked by Each Winner") +
  scale_y_continuous(limits = c(0, 11000), breaks = 0:nlevels(tdf_winners$winner_name)) +
  theme_linedraw() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        legend.position = 'none',
        panel.grid = element_line(colour = "white",
                                  size = 1.5),
        panel.grid.major.y = element_line(size = 10, colour = "gray"),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "darkblue"),
        panel.background = element_rect(fill = "darkblue"))

p <- plot + coord_polar() + aes(x=reorder(winner_name, avg_distance)) +
  labs(caption="@LinhKTP") +
  theme(axis.text.x = element_text(angle = 
        360/(2*pi)*rev( pi/2 + seq( pi/length(tdf_winners$winner_name), 
                                    2*pi-pi/length(tdf_winners$winner_name), 
                                    len=length(tdf_winners$winner_name))),
        color = "white", family="Comic Sans MS", face = "bold"),
        plot.title = element_text(color="white", size=14, face="bold",
                                  family = "Comic Sans MS",
                                  hjust = 0.5,
                                  vjust = 0.2),
        plot.caption=element_text(colour="white", hjust=0.95, size=12,
                                  face = "bold")) +
  transition_reveal(avg_distance)

anim_save("tdf.gif", p)

