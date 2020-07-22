# load packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(png)
library(grid)

# load brisbane complaints data
brisbane_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/brisbane_complaints.csv')

# filter brisbane complaint data for only public defecation info
poo <- brisbane_complaints %>% filter(category == "Defecating In Public") %>%
       mutate(date_range = ifelse(date_range == "1st-quarter-2016-17.csv",
                                  "jul-to-sep-2016.csv", date_range)) %>%
       mutate(init_month = ifelse(grepl("jan", date_range), 01,
                           ifelse(grepl("apr", date_range), 04,
                           ifelse(grepl("jul", date_range), 07,
                                  10)))) %>%
       mutate(year = ifelse(grepl("2016", date_range), 2016,
                     ifelse(grepl("2017", date_range), 2017,
                     ifelse(grepl("2018", date_range), 2018,
                     ifelse(grepl("2019", date_range), 2019,
                            2020))))) %>%
      mutate(month_year = paste0("1","-",init_month, "-", year)) 

poo$month_year <- dmy(poo$month_year)
      
################################
##### Part 1: when is poo? #####
################################

# read in images needed for the plot
poop <- readPNG("poop.png")
poop <- rasterGrob(poop, interpolate = TRUE)

when <- poo %>% group_by(month_year) %>% summarise(total_poo = n())

when_poo <- ggplot(when, aes(x = month_year, y = total_poo)) + 
            geom_line(linetype = "dashed", color = "darkgoldenrod") + 
            theme_classic() + 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(family = "Andale Mono", hjust = 0.5),
                  axis.title.x = element_text(family = "Andale Mono"),
                  axis.title.y = element_text(family = "Andale Mono"),
                  axis.text.x = element_text(family = "Andale Mono"),
                  plot.background = element_rect(fill = "lightyellow"),
                  panel.background = element_rect(fill = "lightyellow")) + 
            ggtitle("When Did Poop Peak?") + 
            ylab(expression("Complaints Number")) + 
            xlab("Time (quarterly)") +
            annotate(geom = "curve", x = as.Date("2017-12-01"), 
                                     y = 95, 
                                     xend = as.Date("2017-02-05"), 
                                     yend = 100, 
                     curvature = .1, arrow = arrow(length = unit(2, "mm"))) +
            annotate(geom = "text", x = as.Date("2017-12-01"), 
                                    y = 85, 
                                    label = "100 poop complaints\nin the first quarter\nof 2017",
                                    hjust = 0.5,
                                    family = "Andale Mono") +
            sapply(1:nrow(when), function(x)
                   annotation_custom(grob=poop, xmin=as.Date(when$month_year[x] - 27), 
                                     xmax=as.Date(when$month_year[x] + 19), 
                                     ymin=when$total_poo[x] - 5, ymax=when$total_poo[x] + 5)) 
  
#################################
###### Part 2: who has poo? ######
#################################

# read in images needed for the plot
dog <- readPNG("dog.png")
dog <- rasterGrob(dog, interpolate = TRUE)

other_animal <- readPNG("other_animal.png")
other_animal <- rasterGrob(other_animal, interpolate = TRUE)

who_poo <- poo %>% group_by(animal_type) %>% summarise(total_poo = n()) %>%  
           ggplot(aes(x = animal_type, y = log10(2200))) + 
           geom_col(fill = "lightyellow") + theme_classic() + 
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 plot.title = element_text(family = "Andale Mono", hjust = 0.5),
                 axis.title.x = element_text(family = "Andale Mono"),
                 axis.title.y = element_text(family = "Andale Mono"),
                 axis.text.x = element_text(family = "Andale Mono"),
                 plot.background = element_rect(fill = "lightyellow"),
                 panel.background = element_rect(fill = "lightyellow")) + 
           annotation_custom(grob=dog, xmin=0, xmax=2, 
                             ymin=0, ymax=log10(955)) +
           annotation_custom(grob=other_animal, xmin=1,xmax=3,
                           ymin=0, ymax=log10(22)) +
           annotate(geom = "text", x = 1, y = log10(1800),
                    label = "955 Complaints",
                    fontface = "bold",
                    family = "Andale Mono") +
           annotate(geom = "text", x=2, y=log10(30), 
                    label ="22 Complaints",
                    fontface="bold",
                    family = "Andale Mono") +
           ggtitle("Who Pooped?") + 
           ylab(expression("Log"[10]*"(Complaints Number)")) + 
           xlab("Animal Type")

######################################
###### Part 3: where is poo? #########
######################################

# create the top 5 poopiest places dataset
where <- poo %>% group_by(suburb) %>% summarise(total_poo = n()) %>%
arrange(desc(total_poo)) %>% top_n(7, total_poo) %>%
mutate(rank_points = as.numeric(ordered(-total_poo)))

# make sure the ranking in the data is set as factor
where$rank_points <- factor(where$rank_points)

# set color palette for the data
colors_set <- c("#482F2E", "#57432D", "#67572D",
                "#776B2C", "#877F2C")

# suburb labels
poo_labs <- data.frame(x = unique(where$total_poo),
                          y = unique(where$rank_points),
                          names = c(
                            str_pad("Aspley",
                                    width = 18),
                            str_pad("Tied: Carseldine & West End",
                                    width = 60),
                            str_pad("Forest Lake",
                                    width = 28),
                            str_pad("Yeronga", width = 20),
                            str_pad("Tied: Carindale & Toowong", width = 56)))

# create final dot chart
where_poo <- ggdotchart(where,x = "rank_points", y = "total_poo",
             palette = rev(colors_set), color = "rank_points",
             sorting = "descending", add = "segments",                             
             rotate = TRUE,dot.size = 8,            
             label = "total_poo",
             font.label = list(color = "white", size = 9, 
                             vjust = 0.5, 
                             family = "Andale Mono"),               
             ggtheme = theme_pubr()) + 
             xlab("Ranking") +
             ylab("Complaints Number") + 
             ggtitle("Where is Poop?") +
             theme(legend.position = 'none',
                   plot.title = element_text(hjust = 0.5, family = "Andale Mono"),
                   axis.text = element_text(family = "Andale Mono"),
                   axis.title = element_text(family = "Andale Mono"),
                   plot.background = element_rect(fill = "lightyellow"),
                   panel.background = element_rect(fill = "lightyellow")) +
             scale_y_continuous(limits = c(0,50)) +
             geom_text(poo_labs, mapping = aes(x = y, y = x, label = names,
                                               family = "Andale Mono")) 

###############################
### Putting it all together ###
###############################

# opening a png path to the image
png(filename = "animal_poop.png", type = "cairo",
    units = "in", width = 13.5, height = 8, pointsize = 10, res = 400)

fin <- when_poo / (who_poo + where_poo)

fin + plot_annotation(title = "Brisbane Poop Alert!",
                      subtitle = "From April 2016 to June 2020, 165 suburbs in Brisbane, Australia, filed complaints for animals defecating in public. The following explore\nthe number of complaints filed over time, total number of complaints received by different animals, and the suburbs with the most public\npooping complaints.",
                      caption = "Visualization: @LinhKTP\nSource:#TidyTuesday Week 30",
                      theme = theme(plot.title = element_text(size = 18,
                                                              family = "Andale Mono", color = "brown"),
                                    plot.subtitle = element_text(family = "Andale Mono"),
                                    plot.caption = element_text(size = 12, family = "Andale Mono"),
                                    plot.background = element_rect(fill = "lightyellow")))

dev.off()
