
library(tidyverse)
library(here)
library(ggrepel)
library(ggthemes)
library(scales)
library(readxl)

round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg
}


### Load files ------

sbac2015 <- read_delim(here("data", "sb_ca2015_all_27_csv_v3.txt"), delim = ",") %>%
  rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".15"))

sbac2016 <- read_delim(here("data", "sb_ca2016_all_27_csv_v3.txt"), delim = ",")%>%
  rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".16"))

sbac2017 <- read_delim(here("data", "sb_ca2017_all_27_csv_v2.txt"), delim = ",")%>%
  rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".17"))

sbac2018 <- read_delim(here("data", "sb_ca2018_all_27_csv_v3.txt"), delim = ",") %>%
  rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".18"))

# sbac2019 <- read_delim(here("data", "sb_ca2019_all_27_csv_v1.txt"), delim = ",") %>%
 sbac2019 <- read_delim(here("data", "preview2" ,"sb_ca2019_all_27_csv_v2.txt"), delim = ",") %>%
  rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".19"))


entities <- read_delim(here("data","sb_ca2019entities_csv.txt"), delim = ",") 

subgroups <- read_delim(here("data","Subgroups.txt"), delim = ",") 


sbac2019.ca <- read_delim(here("data", "sb_ca2019_1_csv_v2.txt"), delim = ",")




EL.schools <-read.delim(here("data",  "LtelDownload1819.txt"))

EL.schools <- EL.schools %>% 
  mutate_at(vars(ends_with("Code")), funs(as.double(.) ) ) %>%
  mutate_at(vars(ends_with("Code")), funs(if_else( is.na(.), 0, .) ) ) %>%
  # filter(str_detect(SchoolName, "Monte Bella")) %>%
  mutate(cds = str_c( str_pad(  as.character(CountyCode) , width = 2, side = "left", pad = "0"  ) ,
                      str_pad(  as.character(DistrictCode), width = 5, side = "left", pad = "0"  ) ,
                      str_pad( as.character(SchoolCode), width = 7, side = "left", pad = "0"  )  )
  ) %>%  # current EL
  filter(Gender == "ALL") %>%
  #        filter(str_detect(CountyName , "Monterey") ) %>%
  group_by(cds) %>%
  mutate(sumEL = sum(EL),
         sumTotal = sum(TotalEnrollment),
         ELpercent = sumEL/sumTotal) %>%
  select(CountyCode,DistrictCode,SchoolCode,DistrictName,SchoolName, Charter, cds, ELpercent) %>%
  ungroup() %>%
  distinct() 


frpm <- read_excel(here("data", "frpm1819.xlsx"), sheet = "FRPM School-Level Data ", range = "A2:AB10477") %>% 
  mutate(cds = str_c(`County Code`,`District Code`,`School Code`)) %>%
  select(`County Code`,`District Code`,`School Code`, cds, starts_with("Percent"), `High Grade`   ) %>%
  select(cds, 6, `High Grade` ) 

colnames(frpm) <- (c("cds", "frpm", "highgrade"))


school.EL.FRPM <- EL.schools %>% left_join(frpm) # %>% mutate(cds = as.numeric(cds))








###  Merge files and massage -----

sbac.all <- list(sbac2019, sbac2018, sbac2017, sbac2016, sbac2015) %>%
  reduce(left_join, by = c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id") )%>%
  left_join(entities, by = c("County Code", "District Code", "School Code")) %>%
  left_join(subgroups, by = c("Subgroup ID" = "Demographic ID Num"  )) %>%
  mutate(TestID = `Test Id` %>% recode(  `1`= "ELA" , `2` = "Math"   )) %>%
  mutate_at(vars(`Total Tested At Entity Level.19`:`Area 4 Percentage Below Standard.15`), list(as.numeric)) %>%
  mutate(`District Name` = case_when(`District Code` == "00000" ~ "Monterey County",
                                     TRUE ~ `District Name`)) %>%
  mutate(`School Name` = case_when(`District Code` == "00000" & `School Code` == "0000000" ~ "Monterey County",
                                   `School Code` == "0000000" ~ " District",
                                   TRUE ~ `School Name`)) %>%
  mutate(Grade = case_when(Grade == "13" ~ "Overall",
                           TRUE ~ as.character(Grade))) %>%
  mutate(OneYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.18`,
         FourYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.15`)


write_rds(sbac.all, here("data", "sbac-all.rds"))

### Selecting part of the file -----

filtered <- sbac.all %>% 
    filter( `School Code`== "0000000",
           Grade == "Overall",
           `Subgroup ID` == "1") 
# 
# table <- filtered %>% 
#     select(`District Name`,`School Name`, TestID, OneYrChange) %>%
#     spread(key = TestID, value = OneYrChange) # %>%
#   #  select(`District Name`,`School Name`, "ELA" = `1`, "Math" = `2`)

table2 <- filtered %>% 
  select(`District Name`,`School Name`, TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange) %>%
  arrange(desc(`School Name`) ,`District Name`, TestID)
  

write_csv(table2, "Local Districts Summary.csv")

### Districts above California -----

above.CA <- sbac.all %>% 
  filter( `School Code`== "0000000",
          Grade == "Overall",
          `Subgroup ID` == "1")  %>% 
  select(`District Name`, TestID, `Percentage Standard Met and Above.19`) %>%
  spread(key = TestID, value = `Percentage Standard Met and Above.19`) %>%
  filter(ELA > 50.87,
         Math > 39.73) %>%
  mutate(Sentence = paste0(`District Name`, ": ",round(ELA), " percent in English Language Arts/Literacy and ", round(Math), " percent in Mathematics."  )) %>%
  arrange(desc(ELA))

clipr::write_clip(above.CA$Sentence)




growth.1yr <- sbac.all %>% 
  filter(# `School Code`== "0000000",
          Grade == "Overall",
          `Subgroup ID` == "1")  %>% 
  select(`District Name`, `School Name`, TestID, `Percentage Standard Met and Above.19`,`Percentage Standard Met and Above.18`, OneYrChange) %>%
#  spread(key = TestID, value = `Percentage Standard Met and Above.19`) %>%
  filter(OneYrChange > 10 ) %>% 
  filter(!str_detect(`School Name`, "Graves|San Antonio|Uplift") ) %>%
  mutate(Sentence = if_else(str_detect(`School Name`, "District") ,
                             paste0( `District Name`, " School District grew ",
                           round(OneYrChange, digits = 1), " percentage points in ", TestID,
                           " from ", round(`Percentage Standard Met and Above.18`, digits = 1), " percent to ",
                           round(`Percentage Standard Met and Above.19`, digits = 1)," percent."  ),
                           paste0(`School Name`," in " , `District Name`, " grew ",
                                   round(OneYrChange, digits = 1), " percentage points in ", TestID,
                                   " from ", round(`Percentage Standard Met and Above.18`, digits = 1), " percent to ",
                                   round(`Percentage Standard Met and Above.19`, digits = 1)," percent."  )
                           ))         %>%
  arrange(desc(OneYrChange))

clipr::write_clip(growth.1yr$Sentence)



growth.4yr <- sbac.all %>% 
  filter(# `School Code`== "0000000",
    Grade == "Overall",
    `Subgroup ID` == "1")  %>% 
  select(`District Name`, `School Name`, TestID, `Percentage Standard Met and Above.19`,`Percentage Standard Met and Above.15`, FourYrChange) %>%
  #  spread(key = TestID, value = `Percentage Standard Met and Above.19`) %>%
  filter(FourYrChange > 20 ) %>% 
  filter(!str_detect(`School Name`, "Graves|San Antonio|Uplift|San Lucas|Chualar|Lagunita") ) %>%
  mutate(Sentence = if_else(str_detect(`School Name`, "District") ,
                            paste0( `District Name`, " School District grew ",
                                    round(FourYrChange, digits = 1), " percentage points in ", TestID,
                                    " from ", round(`Percentage Standard Met and Above.15`, digits = 1), " percent to ",
                                    round(`Percentage Standard Met and Above.19`, digits = 1)," percent."  ),
                            paste0(`School Name`," in " , `District Name`, " grew ",
                                   round(FourYrChange, digits = 1), " percentage points in ", TestID,
                                   " from ", round(`Percentage Standard Met and Above.15`, digits = 1), " percent to ",
                                   round(`Percentage Standard Met and Above.19`, digits = 1)," percent."  )
  ))         %>%
  arrange(desc(FourYrChange))

clipr::write_clip(growth.4yr$Sentence)



#### Press Release Data Tables ------

# Grade ELA

pr1 <- sbac.all %>% 
  filter( `District Name` == "Monterey County",
          #  `School Code`== "0000000",
          #       Grade == "Overall",
          `Subgroup ID` == "1") %>%
  select(Grade, TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange) %>% 
  filter(TestID == "ELA") %>%
  transmute(Grade = Grade,
    `2015` = round(`Percentage Standard Met and Above.15`,digits = 1 ), 
         `2016` = round2(`Percentage Standard Met and Above.16`,digits = 1 ), 
         `2017` = round2(`Percentage Standard Met and Above.17`,digits = 1 ), 
         `2018` = round2(`Percentage Standard Met and Above.18`,digits = 1 ), 
         `2019` = round2(`Percentage Standard Met and Above.19`,digits = 1 ), 
    `1 Year Growth` = round2(OneYrChange,digits = 1 ),
    `4 Year Growth` = round2(FourYrChange,digits = 1 ),
  )

clipr::write_clip(pr1)


# Grade Math

pr2 <- sbac.all %>% 
  filter( `District Name` == "Monterey County",
          #  `School Code`== "0000000",
          #       Grade == "Overall",
          `Subgroup ID` == "1") %>%
  select(Grade, TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange) %>% 
  filter(TestID == "Math") %>%
  transmute(Grade = Grade,
            `2015` = round2(`Percentage Standard Met and Above.15`,digits = 1 ), 
            `2016` = round2(`Percentage Standard Met and Above.16`,digits = 1 ), 
            `2017` = round2(`Percentage Standard Met and Above.17`,digits = 1 ), 
            `2018` = round2(`Percentage Standard Met and Above.18`,digits = 1 ), 
            `2019` = round2(`Percentage Standard Met and Above.19`,digits = 1 ), 
            `1 Year Growth` = round2(OneYrChange,digits = 1 ),
            `4 Year Growth` = round2(FourYrChange,digits = 1 ),
  )

clipr::write_clip(pr2)


# Subgroup ELA

pr3 <- sbac.all %>% 
  filter( `District Name` == "Monterey County",
          #  `School Code`== "0000000",
                 Grade == "Overall",
          # `Subgroup ID` == "1"
          ) %>%
  select(`Student Group`, `Demographic Name` , TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange) %>% 
  filter(TestID == "ELA") %>%
  transmute(`Student Group` = `Student Group`,
            `Demographic Name` = `Demographic Name`,
            `2015` = round2(`Percentage Standard Met and Above.15`,digits = 1 ), 
            `2016` = round2(`Percentage Standard Met and Above.16`,digits = 1 ), 
            `2017` = round2(`Percentage Standard Met and Above.17`,digits = 1 ), 
            `2018` = round2(`Percentage Standard Met and Above.18`,digits = 1 ), 
            `2019` = round2(`Percentage Standard Met and Above.19`,digits = 1 ), 
            `1 Year Growth` = round2(OneYrChange,digits = 1 ),
            `4 Year Growth` = round2(FourYrChange,digits = 1 ),
  ) %>%
  arrange(`Student Group`, `2019`)

clipr::write_clip(pr3)

# Subgroup Math

pr4 <- sbac.all %>% 
  filter( `District Name` == "Monterey County",
          #  `School Code`== "0000000",
          Grade == "Overall",
          # `Subgroup ID` == "1"
  ) %>%
  select(`Student Group`, `Demographic Name` , TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange) %>% 
  filter(TestID == "Math") %>%
  transmute(`Student Group` = `Student Group`,
            `Demographic Name` = `Demographic Name`,
            `2015` = round2(`Percentage Standard Met and Above.15`,digits = 1 ), 
            `2016` = round2(`Percentage Standard Met and Above.16`,digits = 1 ), 
            `2017` = round2(`Percentage Standard Met and Above.17`,digits = 1 ), 
            `2018` = round2(`Percentage Standard Met and Above.18`,digits = 1 ), 
            `2019` = round2(`Percentage Standard Met and Above.19`,digits = 1 ), 
            `1 Year Growth` = round2(OneYrChange,digits = 1 ),
            `4 Year Growth` = round2(FourYrChange,digits = 1 ),
  ) %>%
  arrange(`Student Group`, `2019`)

clipr::write_clip(pr4)
# write_csv(table, "Preliminary Change in Percent Met or Exceeded by District.csv")


####  Districts gone down ------

descreasing <- sbac.all %>% 
  filter( `School Code`== "0000000",
          Grade == "Overall",
          `Subgroup ID` == "1") %>%
  select(`District Name`,`School Name`, TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange) %>%
  filter(OneYrChange < 0) %>%
  mutate(descreasing = paste0(`District Name`,TestID))


background <- sbac.all %>% 
  mutate(drop = paste0(`District Name`,TestID)) %>%
  filter(drop %in% descreasing$descreasing,
    `School Code` != "0000000",
     #     Grade == "Overall",
          `Subgroup ID` == "1") %>%
  select(`District Name`,`School Name`, TestID,Grade ,starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange) %>%
  group_by(`District Name`) %>%
  mutate(single = if_else( n() %in% c(7,14) , TRUE, FALSE )  ) %>%
  ungroup() %>%
  filter(single == FALSE & Grade == "Overall" | single == TRUE ) %>%
  arrange(`District Name`,TestID,`School Name`, Grade, desc(`Percentage Standard Met and Above.19`) )


write_excel_csv(background,"Districts with Decrease.xls")


### Nesting -----


## This doesn't work yet 

nested <- sbac.all %>% 
  group_by(`District Name`) %>%
  nest() %>%
  mutate(data2 = data %>%
           map(~ .x %>% purrr::discard(Grade == "3") ) ) %>%
  mutate(`Percentage Standard Met and Above.19` = as.numeric(`Percentage Standard Met and Above.19`),
         `Percentage Standard Met and Above.18` = as.numeric(`Percentage Standard Met and Above.18`),
         ChangeMetAbove = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.18`,
         PercTotalScored = (as.numeric(`Total Tested with Scores.19`) - as.numeric(`Total Tested with Scores.18`))*100/ as.numeric(`Total Tested with Scores.18`))





# 
# Rather than a table,what about two graphs per school with slopegraph charts and total percentage change in title.  Could be two or three years 
# 
# Also could consider heatmap of district and schools by grade .  Would be single year unless we want to look at the change instead of the scores.  
# 
# What do we want to know?
#     Who improved the most? 
#   Who improved? 
#   Who is the highest? 
  
  
  
  
  ####  Slopegraph ---- 

graphthis <- table2 %>% 
  filter(TestID == "Math") %>%
  select(`District Name`, starts_with("Percentag")) %>%
  gather(key = "Year", value = "Percent", -`District Name`) %>%
  mutate(Year = Year %>% recode("Percentage Standard Met and Above.15" = "2015" ,
                                "Percentage Standard Met and Above.16" = "2016",
                                "Percentage Standard Met and Above.17" = "2017",
                                "Percentage Standard Met and Above.18" = "2018",
                                "Percentage Standard Met and Above.19" = "2019"))

  
ggplot(data = graphthis %>% filter(Year %in% c("2017","2018", "2019")) , aes(x = Year, y = Percent, group = `District Name`)) +
  geom_line(aes(color = `District Name`, alpha = 1), size = 1) +
  #  geom_point(aes(color = Type, alpha = .1), size = 4) +
  geom_text_repel(data = graphthis %>% filter(Year == "2017"), 
                  aes(label = `District Name`) , 
                  hjust = "left", 
                  segment.size = .2,
                  segment.color = "grey",
  #                fontface = "bold", 
                  size = 3, 
                  nudge_x = -.45, 
                  direction = "y") +
  geom_text_repel(data = graphthis %>% filter(Year == "2019"), 
                  aes(label = `District Name`) , 
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
  labs(title = "Change on Math over three years for all districts in Monterey County",
       x = "")


ggsave(here("figs","Change on Math over three years for all districts in Monterey County.png"), width = 7, height = 8)

## Slopegraph countywide by grade

graphthis <- sbac.all %>% 
  filter( `District Name` == "Monterey County",
  #  `School Code`== "0000000",
   #       Grade == "Overall",
          `Subgroup ID` == "1") %>%
  select(Grade, TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange) %>% 
  filter(TestID == "ELA") %>%
  gather(key = "Year", value = "Percent", -Grade) %>%
  mutate(Year = Year %>% recode("Percentage Standard Met and Above.15" = "2015" ,
                                "Percentage Standard Met and Above.16" = "2016",
                                "Percentage Standard Met and Above.17" = "2017",
                                "Percentage Standard Met and Above.18" = "2018",
                                "Percentage Standard Met and Above.19" = "2019"))


ggplot(data = graphthis %>% filter(Year %in% c("2018", "2019")), aes(x = Year, y = Percent, group = Grade)) +
  geom_line(aes(color = Grade, alpha = 1), size = 1) +
  geom_text_repel(data = graphthis %>% filter(Year == "2018"), 
                  aes(label = Grade) , 
                  hjust = "left", 
                  segment.size = .2,
                  segment.color = "grey",
                  size = 3, 
                  nudge_x = -.4, 
                  direction = "y") +
  geom_text_repel(data = graphthis %>% filter(Year == "2019"), 
                  aes(label = Grade) , 
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
  labs(title = "Most grades in Monterey County showed improvement\n from 17-18 to 18-19 on ELA",
       y = "Percent Meeting or Exceeding Standard by Grade",
       x = "")

ggsave(here("figs","Most grades in Monterey County showed improvement\n from 17-18 to 18-19 on ELA.png"), width = 7, height = 8)

## Slopegraph countywide by subgroup

graphthis <- sbac.all %>% 
  filter( `District Name` == "Monterey County",
          #  `School Code`== "0000000",
                 Grade == "Overall",
          #`Subgroup ID` == "1"
          ) %>%
  mutate(OneYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.18`,
         FourYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.15`,
         #   PercTotalScored = (as.numeric(`Total Tested with Scores.19`) - as.numeric(`Total Tested with Scores.18`))*100/ as.numeric(`Total Tested with Scores.18`)
  ) %>% 
  select(`Demographic Name`,`Student Group`, TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange) %>% 
  filter(TestID == "Math",
         `Student Group` == "Parent Education") %>%
  gather(key = "Year", value = "Percent", -`Demographic Name`) %>%
  mutate(Year = Year %>% recode("Percentage Standard Met and Above.15" = "2015" ,
                                "Percentage Standard Met and Above.16" = "2016",
                                "Percentage Standard Met and Above.17" = "2017",
                                "Percentage Standard Met and Above.18" = "2018",
                                "Percentage Standard Met and Above.19" = "2019"))


ggplot(data = graphthis %>% filter(Year %in% c("2018", "2019")), aes(x = Year, y = Percent, group = `Demographic Name`)) +
  geom_line(aes(color = `Demographic Name`, alpha = 1), size = 1) +
  geom_text_repel(data = graphthis %>% filter(Year == "2018"), 
                  aes(label = `Demographic Name`) , 
                  hjust = "left", 
                  segment.size = .2,
                  segment.color = "grey",
                  size = 3, 
                  nudge_x = -.4, 
                  direction = "y") +
  geom_text_repel(data = graphthis %>% filter(Year == "2019"), 
                  aes(label = `Demographic Name`) , 
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
  labs(title = "Students regardless of Parent Education in Monterey County showed improvement from 17-18 to 18-19 on Math",
       y = "Percent Meeting or Exceeding Standard by Subgroup",
       x = "")

ggsave(here("figs","Students regardless of Parent Education in Monterey County showed improvement from 17-18 to 18-19 on Math.png"), width = 7, height = 8)


## Slopegraph formula

slopegraph <- function(df, groupie, test, title){ 

ggplot(data = df %>% filter(Year %in% c("2018", "2019")), aes(x = Year, y = Percent, group = {{groupie}})) +
  geom_line(aes(color = {{groupie}}, alpha = 1), size = 1) +
  geom_text_repel(data = df %>% filter(Year == "2018"), 
                  aes(label = {{groupie}}) , 
                  hjust = "left", 
                  segment.size = .2,
                  segment.color = "grey",
                  size = 3, 
                  nudge_x = -.4, 
                  direction = "y") +
  geom_text_repel(data = df %>% filter(Year == "2019"), 
                  aes(label = {{groupie}}) , 
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


graphthis <- sbac.all %>% 
  filter( `District Name` == "Monterey County",
          #  `School Code`== "0000000",
          Grade == "Overall",
          #`Subgroup ID` == "1"
          ) %>%
  select(`Demographic Name`,`Student Group`, TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange) %>% 
  filter(TestID == "Math",
         `Student Group` == "Parent Education") %>%    #  Change this name for other subgroups 
  gather(key = "Year", value = "Percent", -`Demographic Name`) %>%
  mutate(Year = Year %>% recode("Percentage Standard Met and Above.15" = "2015" ,
                                "Percentage Standard Met and Above.16" = "2016",
                                "Percentage Standard Met and Above.17" = "2017",
                                "Percentage Standard Met and Above.18" = "2018",
                                "Percentage Standard Met and Above.19" = "2019"),
         Percent = as.numeric(Percent))%>%
  mutate(demo = `Demographic Name`)

slopegraph(df = graphthis, groupie = demo, test = "Math", title = "All students improved on Math regardless of parents' level of education")

ggsave(here("figs","Students regardless of Parent Education in Monterey County showed improvement from 17-18 to 18-19 on Math.png"), width = 7, height = 8)



graphthis <- sbac.all %>% 
  filter( `District Name` == "Monterey County",
          #  `School Code`== "0000000",
          Grade == "Overall",
          #`Subgroup ID` == "1"
  ) %>%
  select(`Demographic Name`,`Student Group`, TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange) %>% 
  filter(TestID == "Math",
         `Student Group` == "Ethnicity") %>%    #  Change this name for other subgroups 
  gather(key = "Year", value = "Percent", -`Demographic Name`) %>%
  mutate(Year = Year %>% recode("Percentage Standard Met and Above.15" = "2015" ,
                                "Percentage Standard Met and Above.16" = "2016",
                                "Percentage Standard Met and Above.17" = "2017",
                                "Percentage Standard Met and Above.18" = "2018",
                                "Percentage Standard Met and Above.19" = "2019"),
         Percent = as.numeric(Percent))%>%
  mutate(demo = `Demographic Name`)

slopegraph(df = graphthis, groupie = demo, test = "Math", title = "Most students improved on Math regardless of Race/Ethnicity")

ggsave(here("figs","Students regardless of Ethnicity in Monterey County showed improvement from 17-18 to 18-19 on Math.png"), width = 7, height = 8)



#### Heatmap -----


# table3 <- sbac.all %>% 
#   filter(# `School Code`== "0000000",
#          # Grade == "13",
#           `Subgroup ID` == "1") %>%
#   mutate(OneYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.18`,
#          FourYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.15`)  %>% 
#   select(`District Name`,`School Name`, Grade ,TestID, starts_with("Percentage Standard Met and Above")  ,OneYrChange, FourYrChange)
# 
# 
# 
# ggplot(table3 %>%
#          filter(TestID == "ELA",
#                 str_detect(`District Name`, "Alisal") ),
#        aes( factor( Grade)      , fct_rev( `School Name`),   fill = `Percentage Standard Met and Above.19` )) + 
#   geom_tile(colour = "white") +
#   geom_text(aes(label= percent( `Percentage Standard Met and Above.19`/100)), size = 3) +
#  # scale_x_discrete(labels = xlabs) +
#   theme_hc() +
#   scale_fill_gradient( low = "light yellow", high = "blue" )+
#   theme(
#  #   legend.position = "none",
#     axis.ticks.x = element_blank(),
#     strip.background = element_rect(fill = "black"),
#     strip.text = element_text(colour = 'white'),
#  #   axis.text.x = element_text(angle = 45, hjust = 1)
#   ) +
#   labs(x="Grade",
#        y="School",
#        title = paste0("ELA Percentage Meeting and Exceeding by School and Grade in 2018-19"), 
#        subtitle="", 
#        fill="")






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
           map(~ ggplot(data = .x %>%
                          filter(TestID == "ELA"),
                      aes( factor( Grade)      , fct_rev( `School Name`),   fill = `Percentage Standard Met and Above.19` )) + 
                 geom_tile(colour = "white") +
                 geom_text(aes(label= percent( `Percentage Standard Met and Above.19`/100)), size = 3) +
                 theme_hc() +
                 scale_fill_gradient( low = "light yellow", high = "blue" )+
                 theme(
                  # legend.position = "none",
                   plot.title = element_text(size=22),
                   axis.ticks.x = element_blank(),
                   strip.background = element_rect(fill = "black"),
                   strip.text = element_text(colour = 'white'),
                 ) +
                 labs(x="Grade",
                      y="School",
                      title = paste0(.x$dd[1] ," ELA Percentage Meeting and Exceeding by School and Grade in 2018-19"), 
                      subtitle="", 
                      fill="Legend")
  )) %>%
  mutate(heatmap.Math = data %>%
           map(~ ggplot(data = .x %>%
                          filter(TestID == "Math"),
                        aes( factor( Grade)      , fct_rev( `School Name`),   fill = `Percentage Standard Met and Above.19` )) + 
                 geom_tile(colour = "white") +
                 geom_text(aes(label= percent( `Percentage Standard Met and Above.19`/100)), size = 3) +
                 theme_hc() +
                 scale_fill_gradient( low = "light yellow", high = "blue")+
                 theme(
                  # legend.position = "none",
                   plot.title = element_text(size=22),
                   axis.ticks.x = element_blank(),
                   strip.background = element_rect(fill = "black"),
                   strip.text = element_text(colour = 'white'),
                 ) +
                 labs(x="Grade",
                      y="School",
                      title = paste0(.x$dd[1] ," Math Percentage Meeting and Exceeding by School and Grade in 2018-19"), 
                      subtitle="", 
                      fill="Legend")
           ))

#### Math ------



### Selecting part of the file -----

filtered <- sbac.all %>% 
  filter( `School Code` != "0000000",
          Grade == "Overall",
          `Subgroup ID` == "1",
          TestID == "Math") %>%
  mutate(cds = str_c( str_pad(  as.character(`County Code`) , width = 2, side = "left", pad = "0"  ) ,
                      str_pad(  as.character(`District Code`), width = 5, side = "left", pad = "0"  ) ,
                      str_pad( as.character(`School Code`), width = 7, side = "left", pad = "0"  )  )
  ) %>%
  left_join(school.EL.FRPM, by = c("cds" = "cds")) 


# 
# table <- filtered %>% 
#     select(`District Name`,`School Name`, TestID, OneYrChange) %>%
#     spread(key = TestID, value = OneYrChange) # %>%
#   #  select(`District Name`,`School Name`, "ELA" = `1`, "Math" = `2`)

table2 <- filtered %>%
  mutate(ThreeYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.16`) %>%
 #  filter(OneYrChange > 0) %>%
  select(`District Name`,`School Name`, TestID, Charter, ELpercent, frpm , `Percentage Standard Met and Above.19` , OneYrChange, ThreeYrChange) %>%
  filter(ThreeYrChange >14,
         Charter == "N") %>%
  mutate(ELpercent = round(ELpercent *100, 1),
         frpm = round(frpm *100, 1) ) %>%
  arrange(desc(ThreeYrChange) ,`District Name`, TestID)


table3 <- filtered %>% 
  mutate(TwoYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.17`) %>%
  filter(TwoYrChange >0,
         str_detect(`School Name`, "High"),
         !str_detect(`School Name`, "Highland")) %>%
  #  filter(OneYrChange > 0) %>%
  select(`District Name`,`School Name`, TestID, frpm, ELpercent, starts_with("Percentage Standard Met and Above")  ,OneYrChange, TwoYrChange) %>%
  arrange(desc(TwoYrChange) ,`District Name`, TestID)


temp <- sbac.all %>%
  filter(Grade == "11",
         `Subgroup ID` == "1",
         str_detect(`District Name`, "Salinas"),
         str_detect(`School Name`, "District")) 
  




write_csv(table2, "Schools with positive three year change in Math more than 14.csv")



write_csv(table3, "High Schools with  positive two year change in Math.csv")
### Af-Am ------


AfAm <- sbac.all %>% 
  filter( #`School Code`== "0000000",
#    Grade == "11",
    `Subgroup ID` == "74",
    `Percentage Standard Exceeded.19` >= 1
#    TestID == "Math",
    ) 



