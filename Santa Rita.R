
library(tidyverse)
library(here)
library(ggrepel)
library(ggthemes)
library(scales)

 Soledad 27754400000000
 


test <-   "Math" #  "ELA" # 
folder <- "Spreckles"
placename <- "Spreckles"


bk <- sbac.all %>% 
    filter(`District Code` == "66167",
        `School Code` ==   "0000000"    , # "6102925" ,    #"0122911",
        TestID == test) %>%
    mutate(Grade = factor(Grade, levels = c("3","4","5","6","7","8","11","Overall")))


####  Slopegraph by grade ---- 

graphthis <- bk %>%
    filter(`Subgroup ID` == "1") %>%
    select(Grade, starts_with("Percentag")) %>%
    gather(key = "Year", value = "Percent", -Grade) %>%
    mutate(Year = Year %>% recode("Percentage Standard Met and Above.15" = "2015" ,
                                  "Percentage Standard Met and Above.16" = "2016",
                                  "Percentage Standard Met and Above.17" = "2017",
                                  "Percentage Standard Met and Above.18" = "2018",
                                  "Percentage Standard Met and Above.19" = "2019"))


ggplot(data = graphthis %>% filter(Year %in% c("2015", "2016" ,"2017","2018", "2019")) , aes(x = Year, y = Percent, group = Grade)) +
    geom_line(aes(color = Grade, alpha = 1), size = 1) +
    #  geom_point(aes(color = Type, alpha = .1), size = 4) +
    geom_text_repel(data = graphthis %>% filter(Year == "2015"), 
                    aes(label = Grade) , 
                    hjust = "left", 
                    segment.size = .2,
                    segment.color = "grey",
                    #                fontface = "bold", 
                    size = 3, 
                    nudge_x = -.45, 
                    direction = "y") +
    geom_text_repel(data = graphthis %>% filter(Year == "2019"), 
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
    labs(title = paste0(test," Percentage Met or Exceeded Standards Over Five Years for ", placename ),
         x = "",
         y= "Percentage Met or Exceeded Standards")


ggsave(here(folder, paste0(test, " Percentage Met or Exceeded Standards Over Five Years for ", placename,".png")), width = 8, height = 5)


### Slopegraph by Cohort ----


graphthis <- bk %>%
    filter(`Subgroup ID` == "1") %>%
    select(Grade, starts_with("Percentag")) %>%
    gather(key = "Year", value = "Percent", -Grade) %>%
    mutate(Year = Year %>% recode("Percentage Standard Met and Above.15" = "2015" ,
                                  "Percentage Standard Met and Above.16" = "2016",
                                  "Percentage Standard Met and Above.17" = "2017",
                                  "Percentage Standard Met and Above.18" = "2018",
                                  "Percentage Standard Met and Above.19" = "2019")) %>%
    mutate(cohort = as.numeric(Year) - as.numeric(as.character(  Grade)))


ggplot(data = graphthis %>% filter(Year %in% c("2015", "2016" ,"2017","2018", "2019")) , aes(x = Year, y = Percent, group = cohort)) +
    geom_line(aes(color = factor(cohort), alpha = 1), size = 1) +
    #  geom_point(aes(color = Type, alpha = .1), size = 4) +
    geom_text_repel(data = graphthis %>% filter(Year == "2015"), 
                    aes(label = Grade) , 
                    hjust = "left", 
                    segment.size = .2,
                    segment.color = "grey",
                    #                fontface = "bold", 
                    size = 3, 
                    nudge_x = -.45, 
                    direction = "y") +
    geom_text_repel(data = graphthis %>% filter(Year == "2019"), 
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
    labs(title = paste0(test," Percentage Met or Exceeded Standards by Cohort Over Five Years for ", placename ),
         x = "",
         y= "Percentage Met or Exceeded Standards")


ggsave(here(folder, paste0(test, " Percentage Met or Exceeded Standards by Cohort Over Five Years for ", placename,".png")), width = 8, height = 5)






## Mean Scale Scores by Grade ----

graphthis <- bk %>%
    filter(`Subgroup ID` == "1",
           Grade != "Overall") %>%
    select(Grade, starts_with("Mean Scale")) %>%
    gather(key = "Year", value = "Score", -Grade) %>%
    mutate(Year = Year %>% recode("Mean Scale Score.15" = "2015" ,
                                  "Mean Scale Score.16" = "2016",
                                  "Mean Scale Score.17" = "2017",
                                  "Mean Scale Score.18" = "2018",
                                  "Mean Scale Score.19" = "2019"))


ggplot(data = graphthis %>% filter(Year %in% c("2015", "2016" ,"2017","2018", "2019")) , aes(x = Year, y = Score, group = Grade)) +
    geom_line(aes(color = Grade, alpha = 1), size = 1) +
    #  geom_point(aes(color = Type, alpha = .1), size = 4) +
    geom_text_repel(data = graphthis %>% filter(Year == "2015"), 
                    aes(label = Grade) , 
                    hjust = "left", 
                    segment.size = .2,
                    segment.color = "grey",
                    #                fontface = "bold", 
                    size = 3, 
                    nudge_x = -.45, 
                    direction = "y") +
    geom_text_repel(data = graphthis %>% filter(Year == "2019"), 
                    aes(label = Grade) , 
                    hjust = "right", 
                    segment.size = .2,
                    segment.color = "grey",
                    fontface = "bold", 
                    size = 3, 
                    nudge_x = .5, 
                    direction = "y") +
    geom_label(aes(label = Score), 
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
    labs(title = paste0(test," Mean Scale Score Over Five Years for ", placename ),
         x = "",
         y= "Mean Scale Score")

ggsave(here(folder, paste0(test, " Mean Scale Score Over Five Years for ", placename,".png")), width = 8, height = 5)


####  Slopegraph by subgroup ---- 


slopegraph.sr <- function(df, groupie, title){ 
    
    
    
    tab <- df %>% 
        filter(Grade == "Overall") %>%
        select(`Demographic Name`,`Student Group`, TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange) %>% 
        filter(`Student Group` == {{groupie}}) %>%    #  Change this name for other subgroups 
        gather(key = "Year", value = "Percent", -`Demographic Name`) %>%
        mutate(Year = Year %>% recode("Percentage Standard Met and Above.15" = "2015" ,
                                      "Percentage Standard Met and Above.16" = "2016",
                                      "Percentage Standard Met and Above.17" = "2017",
                                      "Percentage Standard Met and Above.18" = "2018",
                                      "Percentage Standard Met and Above.19" = "2019"),
               Percent = as.numeric(Percent))%>%
        mutate(demo = `Demographic Name`) 
    
    ggplot(data = tab  %>% filter(Year %in% c("2015", "2016" ,"2017","2018", "2019"))
           , aes(x = Year, y = Percent, group = demo)) +
        geom_line(aes(color = demo, alpha = 1), size = 1) +
        geom_text_repel(data = tab %>% filter(Year == "2015"), 
                        aes(label = demo) , 
                        hjust = "left", 
                        segment.size = .2,
                        segment.color = "grey",
                        size = 3, 
                        nudge_x = -.4, 
                        direction = "y") +
        geom_text_repel(data = tab %>% filter(Year == "2019"), 
                        aes(label = demo) , 
                        hjust = "right", 
                        segment.size = .2,
                        segment.color = "grey",
                        fontface = "bold", 
                        size = 3, 
                        nudge_x = .4, 
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
        labs(title = title, 
             y = paste0("Percent Meeting or Exceeding Standard"  ),
             x = "")
    
}


slopegraph.sr(df = bk, groupie = "Ethnicity",  title = paste0(test," Percentage Met or Exceeded Standards \nOver Five Years for ", placename," by Ethnicity"))
ggsave(here(folder, paste0(test, " Percentage Met or Exceeded Standards Over Five Years for ", placename," by Ethnicity.png")), width = 8, height = 5)


slopegraph.sr(df = bk, groupie = "Gender",  title = paste0(test," Percentage Met or Exceeded Standards \nOver Five Years for ", placename," by Gender"))
ggsave(here(folder, paste0(test, " Percentage Met or Exceeded Standards Over Five Years for ", placename," by Gender.png")), width = 8, height = 5)


slopegraph.sr(df = bk, groupie = "Parent Education",  title = paste0(test," Percentage Met or Exceeded Standards \nOver Five Years for ", placename," by Parent Education"))
ggsave(here(folder, paste0(test, " Percentage Met or Exceeded Standards Over Five Years for ", placename," by Parent Education.png")), width = 8, height = 5)


slopegraph.sr(df = bk, groupie = "English-Language Fluency",  title = paste0(test," Percentage Met or Exceeded Standards \nOver Five Years for ", placename," by English-Language Fluency"))
ggsave(here(folder, paste0(test, " Percentage Met or Exceeded Standards Over Five Years for ", placename," by English-Language Fluency.png")), width = 8, height = 5)


slopegraph.sr(df = bk, groupie = "Economic Status",  title = paste0(test," Percentage Met or Exceeded Standards \nOver Five Years for ", placename," by Economic Status"))
ggsave(here(folder, paste0(test, " Percentage Met or Exceeded Standards Over Five Years for ", placename," by Economic Status.png")), width = 8, height = 5)


slopegraph.sr(df = bk, groupie = "Disability Status",  title = paste0(test," Percentage Met or Exceeded Standards \nOver Five Years for ", placename," by Disability Status"))
ggsave(here(folder, paste0(test, " Percentage Met or Exceeded Standards Over Five Years for ", placename," by Disability Status.png")), width = 8, height = 5)


### Claims ------


graphthis <- bk %>%
    filter(`Subgroup ID` == "1") %>%
    select(Grade, starts_with("Percentag")) %>%
    gather(key = "Year", value = "Percent", -Grade) %>%
    separate(Year, into = c("range","year"), sep = "([\\.\\?\\:])") %>%
    mutate(year = paste0("20",year)) %>%
    filter(range!= "Percentage Standard Met and Above")


graphthis %>% 
    ggplot() + 
    geom_col(aes(x = year, y = Percent, fill = range), position = "fill") + 
    #   geom_text(aes(x = `Test Year`, y = value, label = value), position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    facet_wrap(~Grade) +
    theme_hc() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste0(test," Percentage per Standards by Grade Over Five Years for ", placename),
         x = "",
         y = "")

ggsave(here(folder, paste0(test, " Percentage Met or Exceeded Standards by Grade Over Five Years for ", placename,".png")), width = 8, height = 5)



graphthis <- bk %>%
    filter(`Subgroup ID` == "1") %>%
    select(Grade, starts_with("Area")) %>%
    gather(key = "Year", value = "Percent", -Grade) %>%
    separate(Year, into = c("range","year"), sep = "([\\.\\?\\:])") %>%
    separate(range, into = c("claim","level"), sep = "Percentage") %>%
    mutate(year = paste0("20",year)) %>%
    filter(!str_detect(claim, "Area 4")) %>%
    mutate(level = recode(level, " At or Near Standard" = " Near Standard" ) %>% factor()) %>%
    mutate(level = fct_relevel(level, " Above Standard", " Near Standard", " Below Standard" )) %>%
    mutate(claim = case_when(
        claim == "Area 1 " ~ "Concepts & \nProcedures",
        claim ==  "Area 2 " ~ "Problem Solving, \nModeling & \nData Analysis",
        claim ==  "Area 3 " ~ "Communicating \nReasoning"
    )) %>%
    mutate(claim = fct_relevel(claim, "Concepts & \nProcedures", "Problem Solving, \nModeling & \nData Analysis", "Communicating \nReasoning" )) 

ggplot(graphthis) + 
    geom_col(aes(x = year, y = Percent, fill = level), position = "fill") + 
    #   geom_text(aes(x = `Test Year`, y = value, label = value), position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(claim~Grade) +
    theme_hc() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste0(test," Achievement Level by Claim by Grade Over Five Years"),
         x = "",
         y = "Claims",
         fill = "")

ggsave(here(folder, paste0(test, " Achievement Level by Claim by Grade Over Five Years for ", placename,".png")), width = 8, height = 5)

