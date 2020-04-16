# loading the tbi_age data
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')

# loading necessary packages
library(dplyr)
library(ggpubr)
library(gganimate)
library(gifski)

# cleaning of the tbi_age data
# 1. remove data for age group 0-17 and total (redundant for this plot) &
#    keep data if type is "Deaths"
# 2. relevel the age group so that ggplot knows to show them in the proper order
# 3. group the data by age group
# 4. within each age group, filter for the maximum injury rate
dat <- tbi_age %>% filter(age_group != "0-17" &
                   age_group != "Total" &
                   type == "Deaths") %>%
             mutate(age_group = factor(age_group, 
                                levels = c("0-4", "5-14", "15-24", "25-34",
                               "35-44", "45-54", "55-64","65-74", "75+"))) %>%
             group_by(age_group) %>% 
             filter(rate_est == max(rate_est, na.rm = TRUE)) 
             
p <- ggbarplot(dat,x = "age_group", y = "rate_est", # make barplot with x as age_group and y as the injury rate from the cleaned data
          fill = "injury_mechanism",                # fill the bars with same color for same injury type
          color = "white") + theme_minimal() +      # keep bar borders as white and use theme minimal
ggtitle("Largest Types of Traumatic Brain Injuries \nResulting in Deaths (2014)") + # make plot title
xlab("Age Group") + ylab("Injuries per 100,000 People") + # make axis titles
theme(legend.position = "none", # remove legend 
      plot.title = element_text(face = "bold", hjust = 0.5), # make plot title bold and centered
      axis.title.x = element_text(face = "bold"), # make x-axis title bold
      axis.title.y = element_text(face = "bold")) + # make y-axis title bold
geom_bracket(xmin = "5-14", xmax = "15-24", # add bracket for motor crashes
             y.position = 8, label =        # bracket starts at xmin and end at xmax. y-position of bracket is specified
             "Motor Vehicle Crashes") +     # label the bracket with its name
geom_bracket(xmin = "25-34", xmax = "55-64",
             y.position = 10, label =
             "Intentional Self-Harm") + 
geom_bracket(xmin = "65-74", xmax = "75+",
             y.position = 55, label =
             "Unintentional Falls") +
annotate(geom = "text", x = "0-4", 
         y = 3, label = "Assault") + 
transition_manual(factor(injury_mechanism, # use transition_manual to show the bars progressively, by injury mechanism
                         levels = c("Assault", # at the levels that are specified here
                                    "Motor Vehicle Crashes",
                                    "Intentional self-harm",
                                    "Unintentional Falls")),
                   cumulative = TRUE) # cumulative = FALSE would only show one animation frame at a time (i.e. 
                                      # the bars would be shown, then disappear before the next set of bars appear)

# save the plot as a gif file
anim_save("03_27.gif", p)
