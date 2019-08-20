
library(tidyverse)
library(here)
library(ggrepel)
library(ggthemes)
library(scales)

sbac2015 <- read_delim(here("data", "sb_ca2015_all_27_csv_v3.txt"), delim = ",") %>%
  rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".15"))

sbac2016 <- read_delim(here("data", "sb_ca2016_all_27_csv_v3.txt"), delim = ",")%>%
  rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".16"))

sbac2017 <- read_delim(here("data", "sb_ca2017_all_27_csv_v2.txt"), delim = ",")%>%
  rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".17"))

sbac2018 <- read_delim(here("data", "sb_ca2018_all_27_csv_v3.txt"), delim = ",") %>%
  rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".18"))

sbac2019 <- read_delim(here("data", "sb_ca2019_all_27_csv_v1.txt"), delim = ",") %>%
  rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".19"))


entities <- read_delim(here("data","sb_ca2019entities_csv.txt"), delim = ",") 

# sbac.all <- sbac2019 %>%
#   left_join(sbac2018, by = c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id"), suffix = c(".19", ".18"))  %>%
#   left_join(sbac2017, by = c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id"), suffix = c("", ".17")) %>%
#   left_join(sbac2016, by = c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id"), suffix = c("", ".16")) %>%
#   left_join(sbac2015, by = c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id"), suffix = c("", ".15")) %>%
#   left_join(entities, by = c("County Code", "District Code", "School Code")) %>%
#   mutate(TestID = `Test Id` %>% recode(  `1`= "ELA" , `2` = "Math"   )) %>%
#   mutate_at(vars(`Total Tested At Entity Level.19`:`Area 4 Percentage Below Standard.15`), list(as.numeric)) %>%
#   mutate(`District Name` = case_when(`District Code` == "00000" ~ "Monterey County",
#                                      TRUE ~ `District Name`)) %>%
#   mutate(`School Name` = case_when(`District Code` == "00000" & `School Code` == "0000000" ~ "Monterey County",
#                                    `School Code` == "0000000" ~ "District",
#                                      TRUE ~ `District Name`))

sbac.all <- list(sbac2019, sbac2018, sbac2017, sbac2016, sbac2015) %>%
  reduce(left_join, by = c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id") )%>%
  left_join(entities, by = c("County Code", "District Code", "School Code")) %>%
  mutate(TestID = `Test Id` %>% recode(  `1`= "ELA" , `2` = "Math"   )) %>%
  mutate_at(vars(`Total Tested At Entity Level.19`:`Area 4 Percentage Below Standard.15`), list(as.numeric)) %>%
  mutate(`District Name` = case_when(`District Code` == "00000" ~ "Monterey County",
                                     TRUE ~ `District Name`)) %>%
  mutate(`School Name` = case_when(`District Code` == "00000" & `School Code` == "0000000" ~ "Monterey County",
                                   `School Code` == "0000000" ~ "District",
                                   TRUE ~ `School Name`))



filtered <- sbac.all %>% 
    filter( `School Code`== "0000000",
           Grade == "13",
           `Subgroup ID` == "1") %>%
  mutate(OneYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.18`,
         FourYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.15`,
           PercTotalScored = (as.numeric(`Total Tested with Scores.19`) - as.numeric(`Total Tested with Scores.18`))*100/ as.numeric(`Total Tested with Scores.18`)) 
           


table <- filtered %>% 
    select(`District Name`,`School Name`, TestID, OneYrChange) %>%
    spread(key = TestID, value = OneYrChange) # %>%
  #  select(`District Name`,`School Name`, "ELA" = `1`, "Math" = `2`)

table2 <- filtered %>% 
  select(`District Name`,`School Name`, TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange)
  


write_csv(table, "Preliminary Change in Percent Met or Exceeded by District.csv")



nested <- sbac.all %>% 
  group_by(`District Name`) %>%
  nest() %>%
  mut
  filter(`School Code`== "0000000",
         Grade == "13",
         `Subgroup ID` == "1") %>%
  mutate(`Percentage Standard Met and Above.19` = as.numeric(`Percentage Standard Met and Above.19`),
         `Percentage Standard Met and Above.18` = as.numeric(`Percentage Standard Met and Above.18`),
         ChangeMetAbove = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.18`,
         PercTotalScored = (as.numeric(`Total Tested with Scores.19`) - as.numeric(`Total Tested with Scores.18`))*100/ as.numeric(`Total Tested with Scores.18`))






Rather than a table,what about two graphs per school with slopegraph charts and total percentage change in title.  Could be two or three years 

Also could consider heatmap of district and schools by grade .  Would be single year unless we want to look at the change instead of the scores.  

What do we want to know?
    Who improved the most? 
  Who improved? 
  Who is the highest? 
  
  
  
  
  ####  Slopegraph ---- 

graphthis <- table2 %>% 
  filter(TestID == "ELA") %>%
  select(`District Name`, starts_with("Percentag")) %>%
  gather(key = "Year", value = "Percent", -`District Name`)

  
ggplot(data = graphthis, aes(x = Year, y = Percent, group = `District Name`)) +
  geom_line(aes(color = `District Name`, alpha = 1), size = 1) +
  #  geom_point(aes(color = Type, alpha = .1), size = 4) +
  geom_text_repel(data = graphthis %>% filter(Year == "Percentage Standard Met and Above.15"), 
                  aes(label = `District Name`) , 
                  hjust = "left", 
  #                fontface = "bold", 
                  size = 3, 
                  nudge_x = -.45, 
                  direction = "y") +
  geom_text_repel(data = graphthis %>% filter(Year == "Percentage Standard Met and Above.19"), 
                  aes(label = `District Name`) , 
                  hjust = "right", 
                  fontface = "bold", 
                  size = 3, 
                  nudge_x = .5, 
                  direction = "y") +
  geom_label(aes(label = Percent), 
             size = 2.5, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0) +
  scale_x_discrete(position = "top") +
  theme_hc() +  # Remove the legend
  theme(legend.position = "none") 



#### Heatmap -----


table3 <- sbac.all %>% 
  filter(# `School Code`== "0000000",
         # Grade == "13",
          `Subgroup ID` == "1") %>%
  mutate(OneYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.18`,
         FourYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.15`)  %>% 
  select(`District Name`,`School Name`, Grade ,TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange)



ggplot(table3 %>%
         filter(TestID == "ELA",
                str_detect(`District Name`, "Alisal") ),
       aes( factor( Grade)      , fct_rev( `School Name`),   fill = `Percentage Standard Met and Above.19` )) + 
  geom_tile(colour = "white") +
  geom_text(aes(label= percent( `Percentage Standard Met and Above.19`/100)), size = 3) +
 # scale_x_discrete(labels = xlabs) +
  theme_hc() +
  scale_fill_gradient( low = "light yellow", high = "blue" )+
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(colour = 'white'),
 #   axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x="Grade",
       y="School",
       title = paste0("ELA Percentage Meeting and Exceeding by School and Grade in 2018-19"), 
       subtitle="", 
       fill="")






heat.nest <- sbac.all %>% 
  filter(# `School Code`== "0000000",
    # Grade == "13",
    `Subgroup ID` == "1") %>%
  mutate(OneYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.18`,
         FourYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.15`)  %>% 
  select(`District Name`,`School Name`, Grade ,TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange) %>%
  mutate(dd = `District Name`) %>%
  group_by(`District Name`) %>%
  nest() %>%
  mutate(heatmap.ELA = data %>%
           map(~ ggplot(data = .x %>%  filter(TestID == "ELA"),
                      aes( factor( Grade)      , fct_rev( `School Name`),   fill = `Percentage Standard Met and Above.19` )) + 
                 geom_tile(colour = "white") +
                 geom_text(aes(label= percent( `Percentage Standard Met and Above.19`/100)), size = 3) +
                 theme_hc() +
                 scale_fill_gradient( low = "light yellow", high = "blue" )+
                 theme(
                   legend.position = "none",
                   axis.ticks.x = element_blank(),
                   strip.background = element_rect(fill = "black"),
                   strip.text = element_text(colour = 'white'),
                 ) +
                 labs(x="Grade",
                      y="School",
                      title = paste0(.x$dd[1] ," ELA Percentage Meeting and Exceeding by School and Grade in 2018-19"), 
                      subtitle="", 
                      fill="")
  )) %>%
  mutate(heatmap.Math = data %>%
           map(~ ggplot(data = .x %>%  filter(TestID == "Math"),
                        aes( factor( Grade)      , fct_rev( `School Name`),   fill = `Percentage Standard Met and Above.19` )) + 
                 geom_tile(colour = "white") +
                 geom_text(aes(label= percent( `Percentage Standard Met and Above.19`/100)), size = 3) +
                 theme_hc() +
                 scale_fill_gradient( low = "light yellow", high = "blue" )+
                 theme(
                   legend.position = "none",
                   axis.ticks.x = element_blank(),
                   strip.background = element_rect(fill = "black"),
                   strip.text = element_text(colour = 'white'),
                 ) +
                 labs(x="Grade",
                      y="School",
                      title = paste0(.x$dd[1] ," Math Percentage Meeting and Exceeding by School and Grade in 2018-19"), 
                      subtitle="", 
                      fill="")
           ))

