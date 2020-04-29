library(tidyverse)
library(imputeTS)
library(lubridate)
library(sonify)
library(ggplot2)
library(forecast)
library(ggforce)
library(png)
library(grid)
library(extrafont)
library(patchwork)
library(Cairo)

# reading in the data
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

# filtering out grosses data for only lion king
# add_rows for the weeks without collected data and list as NA
# change ticket price = 0 (for 2 weeks) to NA, then
lion_king <- grosses %>% filter(show == "The Lion King") %>%
                 select(week_ending, avg_ticket_price) %>% 
                 add_row(week_ending = c(as.Date.character("2006-06-11"), 
                                         as.Date.character("2015-12-27")),
                         avg_ticket_price = c(NA, NA)) %>% arrange(week_ending) %>% 
                 mutate(avg_ticket_price = na_if(avg_ticket_price, 0)) 

# impute the NA ticket price using kalman smoothing because it shows
# both seasonality and trend
imputed <- lion_king %>% mutate(avg_ticket_price = na_kalman(avg_ticket_price)) %>%
           mutate(Week = strftime(week_ending, format = "%V"),
                  Year = strftime(week_ending, format = "%Y")) %>%
           filter(Year != 1997 & Year != 2020)

# visually checking to see if the imputed values are reasonable 
plotNA.imputations(lion_king$avg_ticket_price, imputed$avg_ticket_price)

#-------------------------------------------
# begin painting "cycle of price" picture
#-------------------------------------------

# for years that don't have data from the 52 and/or 53
# week, put those avg_ticket_price as 0. this makes
# the stacked area chart look more continuous
plot_data <- as.data.frame(xtabs(avg_ticket_price~Week + Year,
                                 imputed))

# set color scheme for the 'mountains'
gradient <- c("#BF7891", "#B87592", "#B27293", "#AC6F94",
              "#A66C95", "#9F6996", "#996697","#936398",
              "#8D6099", "#865D9A", "#805A9B", "#7A579C", 
              "#74549D", "#6D519E", "#674E9F", "#614BA0", 
              "#5B48A1", "#5445A2", "#4E42A3", "#483FA4",
              "#423CA5", "#3C3AA6")

# set plotting theme
theme_set(theme_minimal())

# read in images needed for the plot
birds <- readPNG("broadway_04_28/birds.png")
birds <- rasterGrob(birds, interpolate = TRUE)

tree <- readPNG("broadway_04_28/tree.png")
tree <- rasterGrob(tree, interpolate = TRUE)

elephants <- readPNG("broadway_04_28/elephants.png")
elephants <- rasterGrob(elephants, interpolate = TRUE)

# week_ending number on x-axis. avg. ticket price on y-axis
# fill according to the set gradient by year
final <- ggplot(plot_data, aes(x = as.numeric(Week), y = Freq,
                    fill = Year)) + 
geom_point(aes(x = 30, y = 3000), size = 100,
           colour = 'thistle1',
           alpha = 1/10) + # this is my sun (a GIANT point)
geom_area(alpha = 0.8, size = 0.5, colour = "white") + 
theme(legend.position = 'none',
      panel.background = element_rect(fill = alpha('sienna1',0.5)),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(family = "Andale Mono"),
      axis.title.x = element_text(family = "Andale Mono")) +
scale_x_continuous(limits = c(1,52), expand = c(0,0)) +
scale_y_continuous(limits = c(0, 4500), expand = c(0,0)) +
scale_fill_manual(values = rev(gradient)) +
annotation_custom(grob=birds, xmin=1, xmax=15, 
                  ymin=2500, ymax=4500) + 
annotation_custom(grob = elephants, xmin = 10, xmax = 52,
                  ymin = 0, ymax = 1000) +
annotation_custom(grob = tree, xmin = 1, xmax = 19,
                  ymin = 10, ymax = 1200) + 
geom_hline(yintercept = 0, size = 10, color = 'gray7') +
xlab("Week Number")

# begin exporting the plot
png(filename = "broadway_04_28/lion_king.png", type = "cairo",
    units = "in", width = 9.25, height = 6.5, pointsize = 10, res = 400)

final + plot_annotation(title = "The Cycle of Price", 
        subtitle = "From its opening in 1997, the average ticket price for The Lion King follows the same seasonal\ncycle every year, with the lowest price typically occurring on the 5th week of the year and the\nhighest price on the 52nd, or the last week, of the year. The stacked area plot below shows the\nweekly ticket price fluctuations for each full year of the show (1998 - 2019).",
        caption = "Visualization: @LinhKTP\nData Source: #TidyTuesday Week 18\nImages: clipartkey.com & pinclipart.com",
        theme = theme(plot.title = element_text(size = 18),
              plot.caption = element_text(size = 12),
              text = element_text('mono')))

dev.off()

# converting imputed data into a time series format
lion_ts <- ts(imputed$avg_ticket_price, 
              freq=365.25/7, 
              start=decimal_date(ymd("1997-10-19")))

# make a little song out of the time series data!
song <- sonify(lion_ts, duration = 5,
               flim = c(250, 500), play = FALSE)

# export the song as a wav format
writeWave(song, "broadway_04_28/cycle_of_price.wav")