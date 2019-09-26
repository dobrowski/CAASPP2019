

library(tidyverse)
library(here)
library(ggrepel)
library(ggthemes)
library(scales)
library(readxl)



sbac2019.multi <- read_delim(here("data", "preview2", "sb_ca2019_all_27_csv_v2.txt"), delim = ",") %>%
    rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".19"))%>%
    mutate(cds = str_c( str_pad(  as.character(`County Code`) , width = 2, side = "left", pad = "0"  ) ,
                        str_pad(  as.character(`District Code`), width = 5, side = "left", pad = "0"  ) ,
                        str_pad( as.character(`School Code`), width = 7, side = "left", pad = "0"  )  )
    ) 


for(i in c(10,19,30,33,34,35,37,38,42,43,44,49,54)){

    
    sbac2019.load  <- read_delim(here("data", "preview2", paste0("sb_ca2019_all_",i,"_csv_v2.txt")), delim = ",") %>%
    rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".19"))  %>%
        mutate(cds = str_c( str_pad(  as.character(`County Code`) , width = 2, side = "left", pad = "0"  ) ,
                            str_pad(  as.character(`District Code`), width = 5, side = "left", pad = "0"  ) ,
                            str_pad( as.character(`School Code`), width = 7, side = "left", pad = "0"  )  )
        ) 

sbac2019.multi <- bind_rows(sbac2019.multi, sbac2019.load)
    
}
    
sbac2017.multi <- read_delim(here("data", "preview2", "sb_ca2017_all_csv_v2.txt"), delim = ",") %>%
    rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".17"))  %>%
    mutate(cds = str_c( str_pad(  as.character(`County Code`) , width = 2, side = "left", pad = "0"  ) ,
                        str_pad(  as.character(`District Code`), width = 5, side = "left", pad = "0"  ) ,
                        str_pad( as.character(`School Code`), width = 7, side = "left", pad = "0"  )  )
    ) 


sbac2018.multi <- read_delim(here("data", "preview2", "sb_ca2018_all_csv_v3.txt"), delim = ",") %>%
    rename_at(vars(-c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id")), ~ paste0(., ".18"))  %>%
    mutate(cds = str_c( str_pad(  as.character(`County Code`) , width = 2, side = "left", pad = "0"  ) ,
                        str_pad(  as.character(`District Code`), width = 5, side = "left", pad = "0"  ) ,
                        str_pad( as.character(`School Code`), width = 7, side = "left", pad = "0"  )  )
    ) 



### Supplemental Data ----


entities <- read_delim(here("data","sb_ca2019entities_csv.txt"), delim = ",") 

subgroups <- read_delim(here("data","Subgroups.txt"), delim = ",") 







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

sbac.all.multi <- list(sbac2019.multi, sbac2018.multi, sbac2017.multi) %>%
    reduce(left_join, by = "cds") %>% #c("County Code", "District Code", "School Code", "Subgroup ID", "Grade", "Test Id") )%>%
    left_join(entities, by = c("County Code", "District Code", "School Code")) %>%
    left_join(subgroups, by = c("Subgroup ID" = "Demographic ID Num"  )) %>%
    mutate(TestID = `Test Id` %>% recode(  `1`= "ELA" , `2` = "Math"   )) %>%
    mutate_at(vars(`Total Tested At Entity Level.19`:`Area 4 Percentage Below Standard.17`), list(as.numeric)) %>%
    mutate(`District Name` = case_when(`District Code` == "00000" ~ "the County",
                                       TRUE ~ `District Name`)) %>%
    mutate(`School Name` = case_when(`District Code` == "00000" & `School Code` == "0000000" ~ "the County",
                                     `School Code` == "0000000" ~ " District",
                                     TRUE ~ `School Name`)) %>%
    mutate(Grade = case_when(Grade == "13" ~ "Overall",
                             TRUE ~ as.character(Grade))) %>%
    mutate(OneYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.18`,
            TwoYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.17`)



### Filtered down ----



final.hs.2019 <- sbac2019.multi %>% 
    left_join(entities, by = c("County Code", "District Code", "School Code")) %>%
    left_join(subgroups, by = c("Subgroup ID" = "Demographic ID Num"  )) %>%
    mutate(TestID = `Test Id` %>% recode(  `1`= "ELA" , `2` = "Math"   )) %>%
    mutate_at(vars(`Total Tested At Entity Level.19`:`Area 4 Percentage Below Standard.19`), list(as.numeric)) %>%
    mutate(`District Name` = case_when(`District Code` == "00000" ~ "the County",
                                       TRUE ~ `District Name`)) %>%
    mutate(`School Name` = case_when(`District Code` == "00000" & `School Code` == "0000000" ~ "the County",
                                     `School Code` == "0000000" ~ " District",
                                     TRUE ~ `School Name`)) %>%
    mutate(Grade = case_when(Grade == "13" ~ "Overall",
                             TRUE ~ as.character(Grade))) %>%
    # mutate(OneYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.18`,
    #        TwoYrChange = `Percentage Standard Met and Above.19`- `Percentage Standard Met and Above.17`)
    filter(Grade == "11",
           TestID == "Math",
           `Subgroup ID` == "1") 


### End ----
