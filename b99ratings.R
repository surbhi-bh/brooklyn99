
#Load packages
library(ggplot2)
library(tidyverse)
library(ggtext)
library(extrafont)

imdbdata <- read.csv("imdbb99.csv", header = TRUE)
imdbdata$series_ep <- 1:nrow(imdbdata)

b99data <- imdbdata %>%  
  group_by(season) %>% 
    mutate(mean_rating = mean(rating),
           label_pos = case_when(
           season == 1 ~ 11,
           season == 2 ~ 36,
           season == 3 ~ 59,
           season == 4 ~ 80,
           season == 5 ~ 102,
           season == 6 ~ 122,
           season == 7 ~ 137,
           season == 8 ~ 150
           ))

top.bottom <- do.call(rbind,lapply(unique(b99data$season), function(f){
    ab <- subset(b99data, season == f)
    ab <- ab[order(-ab$rating),]
    top <- head(ab,2)
    bottom <- tail(ab, 2)
    rbind(top, bottom)
    }))

background <- "#F0E578"
mean_lines <- "#180570"

theme_set(theme_minimal())

#Plot data
ggplot(b99data) +
    geom_segment(aes(series_ep,
                     xend=series_ep,
                     y=mean_rating,
                     yend=rating), 
               size = .4, color = mean_lines, linetype = 2) +
    geom_line(aes(series_ep,mean_rating), color = mean_lines, size = 1.8) +
    scale_y_continuous(limits = c(4.5, 10),
                       breaks = seq(4.5, 10, 0.5)) +
    scale_x_continuous(expand = c(0.05,0)) + 
    geom_point(aes(series_ep,
                   y=rating,
                   fill = factor(season)), colour= mean_lines,
               pch=21, size = 3.5) +
    ylab("IMDb rating") +
    geom_text(data=subset(top.bottom, rating > 8.4),
              aes(series_ep+2,rating+0.1,label=epname)) +
     geom_text(data=subset(top.bottom, rating < 7.5),
               aes(series_ep-1,rating-0.1,label=epname)) +
    geom_curve(aes(x = 120, y = 6.1, xend = 117, yend = 6.3),
               arrow = arrow(length = unit(0.3, "cm")),
                             size = 0.4,
               color = mean_lines, curvature = -1.9) +
    annotate("text", x = 120, y = 6.1 , size = 3.5,
             hjust = 0, color = mean_lines,
             family = "Cuckoo",
             label = paste("Gina returns to \n the Nine-nine")) +
    geom_text(data =  b99data,
              aes(y = mean_rating - .08,
                  x =  label_pos,
                  label = glue::glue(" Season {season} ")),
              family = "Univers", 
            size = 4, color = mean_lines) +
     theme(
         legend.position = "none",
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
         panel.grid.major.y = element_line(color = "gray45",
                                           linetype = 2, size = .2),
         plot.background = element_rect(fill = background,
                                        color = background),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank(),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.title.y = element_text(family = "Cuckoo",
                                     size = 20, colour= mean_lines),
         axis.text.y = element_text(size = 15, colour= mean_lines),
         plot.title = element_markdown(size = 18,
                                       colour = mean_lines,
                                       family = "Univers"),
         plot.subtitle = element_markdown(size = 12,
                                       colour = "black",
                                       family = "Univers")) +
     labs(title = "It's a nine-nine!",
       subtitle = "Brooklyn 99 has been a consistent favourite, and halloween heist episodes are recurring highlights",
       caption = "\nSleeping lines represent average rating per season.\ndataviz: @surbhaai | source: IMDb") 

ggsave("brooklyn99rating.png", width = 15, height = 9, dpi = 100)
dev.off()

## Fin! ##
