library(lubridate)
library(tidyverse)
library(gganimate)
library(transformr)
library(gifski)
library(hablar)
library(cowplot)

brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')

df <- brewing_materials %>% 
  filter(!str_detect(material_type, "Total")) %>% 
  mutate(month = ymd(str_glue("{year}-{str_pad(month, width = 2, pad = '0')}-01"))) %>%
  group_by(month) %>% mutate(month_all = sum_(month_current)) %>%
  ungroup() %>% group_by(type, month) %>%
  summarise(month_share = month_current/month_all) %>%
  filter(type != "Other")

df <- df %>% 
  mutate(guess = case_when(
    month == "2016-01-01" ~ "Inauguration of\nDonald Trump",
    T ~ NA_character_
  ),
  guess_y = case_when(
    month == "2016-01-01" ~ .95,
    T ~ NA_real_
  ))

df$type <- factor(df$type, levels = 
                  c("Hops (used as extracts)",
                    "Wheat and wheat products",
                    "Corn and corn products",
                    "Sugar and syrups",
                    "Rice and rice products",
                    "Barley and barley products",
                    "Hops (dry)",
                    "Malt and malt products"))


p <- ggplot(df, aes(month, month_share, color = type)) +
  geom_line(size = 1.5, position = position_dodge(width = 0.2),
            alpha = 0.6) +  scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9", 
                                                           "#009E73", "#F0E442", "#0072B2", 
                                                           "#D55E00", "#CC79A7")) +
  labs(x = NULL,
       y = "Monthly share of products",
       title = "What happened to sweet 16?",
       subtitle = "Why did the share of sugar and syrups in beer production fall from 15 % to 2% in the US?") +
  annotate("rect", xmin=as.Date("2016-01-01", "%Y-%m-%d"), 
           xmax=as.Date("2018-01-01", "%Y-%m-%d"), 
           ymin=0 , ymax=1, alpha=0.15,
           color="white", fill="gray")+
  geom_segment(data = df %>% drop_na(guess), 
               aes(month, month_share, xend = month, yend = guess_y), 
               inherit.aes = F, size = 0.5) +
  geom_label(aes(y = guess_y, label = guess), colour = "black",
             size = 5.0) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1),
                     breaks = seq(0, 1, .05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal_hgrid(font_size = 16) +
  theme(plot.title = element_text(size = 30),
        axis.title.y = element_text(hjust = 0.2),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.justification = "center") + geom_point() +
  transition_manual(type, cumulative = TRUE)

q <- animate(p, nframes = 100, fps=4, height = 518, width = 1111,
             renderer = gifski_renderer(loop = F))
anim_save("04_04.gif", q)

