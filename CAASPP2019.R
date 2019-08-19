
library(tidyverse)
library(here)


sbac2018 <- read_delim(here("data", "sb_ca2018_all_27_csv_v3.txt"), delim = ",") # %>%
  #  select(CountyCode = `County Code`,DistrictCode = `District Code`,SchoolCode = `School Code`, SubgroupID = `Subgroup ID`, 4:32)
sbac2019 <- read_delim(here("data", "sb_ca2019_all_27_csv_v1 2.txt"), delim = ",") # %>%
  #  select(CountyCode = `County Code`,DistrictCode = `District Code`,SchoolCode = `School Code`, SubgroupID = `Subgroup ID`, 4:32)

entities <- read_delim(here("data","sb_ca2019entities_csv.txt"), delim = ",") 

sbac.all <- sbac2019 %>%
    left_join(sbac2018, by = c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id"), suffix = c(".19", ".18")) %>%
    left_join(entities, by = c("County Code", "District Code", "School Code"))



filtered <- sbac.all %>% 
    filter(# `School Code`== "0000000",
           Grade == "13",
           `Subgroup ID` == "1") %>%
    mutate(`Percentage Standard Met and Above.19` = as.numeric(`Percentage Standard Met and Above.19`),
           `Percentage Standard Met and Above.18` = as.numeric(`Percentage Standard Met and Above.18`),
           ChangeMetAbove = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.18`,
           PercTotalScored = (as.numeric(`Total Tested with Scores.19`) - as.numeric(`Total Tested with Scores.18`))*100/ as.numeric(`Total Tested with Scores.18`))
           


table <- filtered %>% 
    select(`District Name`,`School Name`, `Test Id`, ChangeMetAbove) %>%
    spread(key = `Test Id`, value = ChangeMetAbove) %>%
    select(`District Name`,`School Name`, "ELA" = `1`, "Math" = `2`)

write_csv(table, "Preliminary Change in Percent Met or Exceeded by District.csv")

