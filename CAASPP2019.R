
library(tidyverse)
library(here)

sbac2015 <- read_delim(here("data", "sb_ca2015_all_27_csv_v3.txt"), delim = ",")
sbac2016 <- read_delim(here("data", "sb_ca2016_all_27_csv_v3.txt"), delim = ",")
sbac2017 <- read_delim(here("data", "sb_ca2017_all_27_csv_v2.txt"), delim = ",")
sbac2018 <- read_delim(here("data", "sb_ca2018_all_27_csv_v3.txt"), delim = ",") # %>%
  #  select(CountyCode = `County Code`,DistrictCode = `District Code`,SchoolCode = `School Code`, SubgroupID = `Subgroup ID`, 4:32)
sbac2019 <- read_delim(here("data", "sb_ca2019_all_27_csv_v1.txt"), delim = ",") # %>%
  #  select(CountyCode = `County Code`,DistrictCode = `District Code`,SchoolCode = `School Code`, SubgroupID = `Subgroup ID`, 4:32)

entities <- read_delim(here("data","sb_ca2019entities_csv.txt"), delim = ",") 

sbac.all <- sbac2019 %>%
  left_join(sbac2018, by = c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id"), suffix = c(".19", ".18")) %>%
  left_join(sbac2017, by = c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id"), suffix = c("", ".17")) %>%
  left_join(sbac2016, by = c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id"), suffix = c("", ".16")) %>%
  left_join(sbac2015, by = c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id"), suffix = c("", ".15")) %>%
  left_join(entities, by = c("County Code", "District Code", "School Code")) %>%
  mutate(TestID = `Test Id` %>% recode(  `1`= "ELA" , `2` = "Math"   )) %>%
  mutate_at(vars(`Total Tested At Entity Level.19`:`Area 4 Percentage Below Standard.15`), list(as.numeric))



filtered <- sbac.all %>% 
    filter(`School Code`== "0000000",
           Grade == "13",
           `Subgroup ID` == "1") 
    
mutate(`Percentage Standard Met and Above.19` = as.numeric(`Percentage Standard Met and Above.19`),
           `Percentage Standard Met and Above.18` = as.numeric(`Percentage Standard Met and Above.18`),
           ChangeMetAbove = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.18`,
           PercTotalScored = (as.numeric(`Total Tested with Scores.19`) - as.numeric(`Total Tested with Scores.18`))*100/ as.numeric(`Total Tested with Scores.18`))
           


table <- filtered %>% 
    select(`District Name`,`School Name`, `Test Id`, ChangeMetAbove) %>%
    spread(key = `Test Id`, value = ChangeMetAbove) %>%
    select(`District Name`,`School Name`, "ELA" = `1`, "Math" = `2`)

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
  
