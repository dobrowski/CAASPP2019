
library(tidyverse)
library(here)

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
    filter(# `School Code`== "0000000",
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
  
