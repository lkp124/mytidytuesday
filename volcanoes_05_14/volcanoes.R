library(ggplot2)
library(tidyverse)
library(viridis)
library(png)
library(grid)
library(ggrepel)
library(Cairo)

# read in bush images
bush <- readPNG("volcanoes/bushes.png")
bush <- rasterGrob(bush, interpolate = TRUE)

# read in dark cloud images
dark_cloud <- readPNG("volcanoes/darkclouds.png")
dark_cloud <- rasterGrob(dark_cloud, interpolate = TRUE) 

# all eruption events from TidyTuesday data
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')

# the volcanoes were assigned numbers that were inconvenient for 
# plotting the lava. I'm simply giving them new numbers (somewhat randomly)
names <- events %>% distinct(volcano_name) %>% arrange(volcano_name) %>%
         mutate(new_number = row_number())

# calculate the total number of eruption events for each volcano
# also assign some "weights" in the column "colors" for each volcano
# dpending on the number of times they have rupted. This makes 
# it easy to plot pretty points later on with viridis scale. 
eruption_numbers <- events %>% merge(names, by = "volcano_name") %>%
                    group_by(volcano_name,new_number) %>% summarise(events_total = n()) %>%
                    mutate(colors = ifelse(events_total <= 127, 9,
                                    ifelse(events_total > 127 &
                                           events_total <= 254, 8,
                                    ifelse(events_total > 254 & 
                                           events_total <= 381, 7,
                                    ifelse(events_total > 381 &
                                           events_total <= 508, 6,
                                    ifelse(events_total > 508 &
                                           events_total <= 635, 5,
                                    ifelse(events_total > 635 &
                                           events_total <= 762, 4,
                                    ifelse(events_total > 762 &
                                           events_total <= 889, 3,
                                    ifelse(events_total > 889 &
                                           events_total <= 1016, 2,1))))))))) %>% ungroup() %>% 
                      mutate(volcano_name = ifelse(volcano_name == "Fournaise, Piton de la",
                                                   "Piton de la Fournaise", volcano_name))

# making sure that the numbers are read as factors by scale_color_viridis
eruption_numbers$colors = factor(eruption_numbers$colors)

# plotting x and y positions of the mountain
shape_positions_x <- c(-500, 0, 900, 1300)
shape_positions_y <-  c(2000, 0, 0, 2000)

# combine x and y position together into a data frame. 
# give them the same number to make fill shape color the same later on
mountain <- data.frame(shape_positions_x, shape_positions_y) %>%
            mutate(fill_col = 1)

# making sure that the fill_col number is recognized as a factor
mountain$fill_col <- as.factor(mountain$fill_col) 

# here i began making the gradient background
bk_palette <- colorRampPalette(c("cadetblue1", "firebrick1"))
bk_pas <- rev(bk_palette(20))
g <- rasterGrob(bk_pas, width=unit(1,"npc"), height = unit(1,"npc"), 
                interpolate = TRUE) 

# begin exporting the plot
png(filename = "volcanoes/volcano.png", type = "cairo",
    units = "in", width = 6, height = 6, pointsize = 10, res = 400)

ggplot() + annotation_custom(g, xmin = -Inf, xmax = Inf,
                             ymin = -Inf, ymax = Inf) + # this line plots the background
geom_polygon(data = mountain,
             aes(x = shape_positions_x, y = shape_positions_y,fill = fill_col),
             color = "chocolate4",
             size = 2) +
scale_fill_manual(values = alpha("chocolate4", 0.6)) + # plot the mountain
geom_point(data = eruption_numbers, aes(new_number, events_total, colour = colors),
           alpha = 0.6, stroke = 1.5) + geom_jitter() + # plot the "lava" or number of total events
geom_label_repel(data=subset(eruption_numbers, events_total >= 572),
            aes(new_number,events_total,label = volcano_name),
            fill = "black", color = 'white', size = 4,
            family = "Andale Mono", nudge_y = -100,
            min.segment.length = 0) + # labeling the points for top 4 explosive volcanoes
scale_color_viridis_d(option = "plasma") + # use plasma viridis scale for the points
theme(panel.background = element_rect(fill = alpha("cadetblue1",0.5)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(family = "Andale Mono"),
        axis.title.y = element_text(family = "Andale Mono"),
        plot.title = element_text(family = "Andale Mono", hjust = 0.5),
        plot.caption = element_text(family = "Andale Mono", size = 10),
        legend.position = 'none') + ylab("Total Number of Eruption Events\n") + 
ggtitle("Number of Eruptions Since 4040 B.C.E.\nfor All Documented Vocanoes") + 
labs(caption = "Visualization: @LinhKTP\nData: #TidyTuesday Week 20\nImages: clipartbarn.com & clipartmax.com")+
scale_y_reverse(position = "left", limits = c(2000, -300), expand = c(0,0)) + # reverse y scale
scale_x_continuous(limits = c(-500, 1300),expand = c(0,0)) + 
# add dark clouds
annotation_custom(grob = dark_cloud,
                    xmin = -1000, xmax = 300,
                    ymin = 500, ymax = -200) + 
annotation_custom(grob = dark_cloud,
                  xmin = -400, xmax = 1300,
                  ymin = 700, ymax = -50) +
annotation_custom(grob = dark_cloud,
                    xmin = 700, xmax = 2000,
                    ymin = 500, ymax = -200)  +

# add some shrubs
annotation_custom(grob = bush,
                  xmin = -400, xmax = 300,
                  ymin = -2100, ymax = -1800) +
annotation_custom(grob = bush,
                    xmin = -550, xmax = -100,
                    ymin = -2100, ymax = -1500) +
annotation_custom(grob = bush,
                    xmin = 300, xmax = 700,
                    ymin = -2000, ymax = -1700) + 
annotation_custom(grob = bush,
                    xmin = 600, xmax = 1000,
                    ymin = -2100, ymax = -1600) +
annotation_custom(grob = bush, 
                    xmin = 900, xmax = 1200,
                    ymin = -2100, ymax = - 1800) + 
annotation_custom(grob = bush, 
                    xmin = 1100, xmax = 1400,
                    ymin = -2100, ymax = - 1700) + 
annotation_custom(grob = bush,
                  xmin = -40, xmax = 500,
                  ymin = -2100, ymax = -1800) 

dev.off()
