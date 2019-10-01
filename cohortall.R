
library(tidyverse)
library(here)
library(ggrepel)
library(ggthemes)
library(scales)


sbac.all <- read_rds( here("data", "sbac-all.rds"))


test <-      "ELA" #  "Math" #
folder <- "alldistricts"
placename <- "Monterey County"


bk.nest <- sbac.all %>% 
    filter(#`District Code` == "00000",
           `School Code` ==   "0000000"    , # "6102925" ,    #"0122911",
           TestID == test,
           `Subgroup ID` == "1") %>%
    mutate(Grade = factor(Grade, levels = c("3","4","5","6","7","8","11","Overall"))) %>%
    select(Grade, starts_with("Percentag"), `District Code`, `District Name`) %>%
    gather(key = "Year", value = "Percent", -Grade, -`District Code` , -`District Name` ) %>% 
    mutate(Year = Year %>% recode("Percentage Standard Met and Above.15" = "2015" ,
                                  "Percentage Standard Met and Above.16" = "2016",
                                  "Percentage Standard Met and Above.17" = "2017",
                                  "Percentage Standard Met and Above.18" = "2018",
                                  "Percentage Standard Met and Above.19" = "2019")) %>%
    mutate(cohort = as.numeric(Year) - as.numeric(as.character(Grade))) %>% 
    mutate(graphname = `District Name`) %>%
    group_by(`District Code`, graphname) %>%
    nest()


####  Slopegraph by grade ---- 


bk.nest <- bk.nest %>% 
    mutate(graph.grade = data %>%
    map2(graphname,~ ggplot(data = .x %>% filter(Year %in% c("2015", "2016" ,"2017","2018", "2019")) , aes(x = Year, y = Percent, group = Grade)) +
    geom_line(aes(color = Grade, alpha = 1), size = 1) +
    geom_text_repel(data = .x %>% filter(Year == "2015"), 
                    aes(label = Grade) , 
                    hjust = "left", 
                    segment.size = .2,
                    segment.color = "grey",
                    size = 3, 
                    nudge_x = -.45, 
                    direction = "y") +
    geom_text_repel(data = .x %>% filter(Year == "2019"), 
                    aes(label = Grade) , 
                    hjust = "right", 
                    segment.size = .2,
                    segment.color = "grey",
                    fontface = "bold", 
                    size = 3, 
                    nudge_x = .5, 
                    direction = "y") +
    geom_label(aes(label = Percent), 
               size = 2.5, 
               label.padding = unit(0.05, "lines"), 
               label.size = 0.0) +
    theme_hc() +  # Remove the legend
    theme(axis.text.y      = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    scale_x_discrete(position = "top") +
    theme(legend.position = "none") +
    labs(title = paste0(test," Percentage Met or Exceeded Standards \nOver Five Years for ", .y ),
         x = "",
         y= "Percentage Met or Exceeded Standards")
),
graph.cohort = data %>%
           map2(graphname,~ ggplot(data = .x %>% filter(Year %in% c("2015", "2016" ,"2017","2018", "2019")) , aes(x = Year, y = Percent, group = cohort)) +
                    geom_line(aes(color = factor(cohort), alpha = 1), size = 1) +
                    geom_text_repel(data = .x %>% filter(Year == "2015"), 
                                    aes(label = Grade) , 
                                    hjust = "left", 
                                    segment.size = .2,
                                    segment.color = "grey",
                                    #                fontface = "bold", 
                                    size = 3, 
                                    nudge_x = -.45, 
                                    direction = "y") +
                    geom_text_repel(data = .x %>% filter(Year == "2019"), 
                                    aes(label = Grade) , 
                                    hjust = "right", 
                                    segment.size = .2,
                                    segment.color = "grey",
                                    fontface = "bold", 
                                    size = 3, 
                                    nudge_x = .5, 
                                    direction = "y") +
                    geom_label(aes(label = Percent), 
                               size = 2.5, 
                               label.padding = unit(0.05, "lines"), 
                               label.size = 0.0) +
                    theme_hc() +  # Remove the legend
                    theme(axis.text.y      = element_blank()) +
                    theme(panel.grid.major.y = element_blank()) +
                    theme(panel.grid.minor.y = element_blank()) +
                    theme(axis.ticks       = element_blank()) +
                    scale_x_discrete(position = "top") +
                    theme(legend.position = "none") +
                    labs(title = paste0(test," Percentage Met or Exceeded Standards by Cohort \nOver Five Years for ", .y ),
                         x = "",
                         y= "Percentage Met or Exceeded Standards")
           ),
grade.file = paste0( graphname, "/",  paste0(test, " Percentage Met or Exceeded Standards Over Five Years for ", graphname,".png") ), # to create path and filename

 cohort.file = paste0( graphname, "/",  paste0(test, " Percentage Met or Exceeded Standards by Cohort Over Five Years for ", graphname,".png"))  # to create path and filename

)



loc <- "alldistricts"

dir.create(here(loc))
# Makes the district level folders
walk(bk.nest$graphname, ~ dir.create(here(loc, .x)))
#  Saves the graphs in the current teachers folder
walk2(bk.nest$grade.file, bk.nest$graph.grade , ~ggsave(filename = here(loc, .x  ), plot = .y, height = 5, width = 8) ) 
walk2(bk.nest$cohort.file, bk.nest$graph.cohort , ~ggsave(filename = here(loc, .x  ), plot = .y, height = 5, width = 8) ) 




