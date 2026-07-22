library(tidyverse)

counties.regions <- read_csv("/Users/kellyasche/Library/CloudStorage/GoogleDrive-kasche@ruralmn.org/My Drive/Data Prep/R Projects/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Name = str_replace(Name, "Mcleod", "McLeod"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc) ,
         edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))


counties.regions.1 <- counties.regions %>%
  mutate(statefp = "27",
         project.pr = ifelse(edr %in% c("EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 4 - West Central"), "Northwest",
                             ifelse(edr == "EDR 3 - Arrowhead", "Northeast",
                                    ifelse(edr %in% c("EDR 5 - North Central", "EDR 7E- East Central", "EDR 7W- Central"), "Central",
                                           ifelse(edr %in% c("EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 8 - Southwest"), "Southwest",
                                                  ifelse(edr %in% c("EDR 9 - South Central", "EDR 10 - Southeast"), "Southern", as.character(planning.region)))))),
         project.pr = fct_relevel(project.pr, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southern"))




# Prep data ---------------------------------------------------------------

fed.jobs.original <- read_csv("Data/No MN Emp Records Data/QCEWResults.csv")

names(fed.jobs.original)

fed.jobs.data <- fed.jobs.original %>%
  select(periodyear, ownertitle, empYear) %>%
  group_by(periodyear) %>%
  summarise(pct.fed = empYear[ownertitle == "Federal Government"] / empYear[ownertitle == "Total, All Ownerships"]) %>%
  ungroup() %>%
  summarize(avg.fed.emp = mean(pct.fed)) %>%
  mutate(region = "Northeast")
  
names(fed.jobs.data)

lf.participant.original <- read_csv("Data/No MN Emp Records Data/nhgis0144_csv/nhgis0144_ts_nominal_county.csv")

names(lf.participant.original)

lf.participant.data <- lf.participant.original %>%
  select(COUNTYFP, 
         A64AG105, A64AG115, A64AG125, A64AG135, A64AG145, A64AG155, A64AG165, A64AG175, A64AG185, A64AG195, A64AG205, A64AG215, A64AG225, A64AG235, A64AG245,
         A64AH105, A64AH115, A64AH125, A64AH135, A64AH145, A64AH155, A64AH165, A64AH175, A64AH185, A64AH195, A64AH205, A64AH215, A64AH225, A64AH235, A64AH245,
         A64AL105, A64AL115, A64AL125, A64AL135, A64AL145, A64AL155, A64AL165, A64AL175, A64AL185, A64AL195, A64AL205, A64AL215, A64AL225, A64AL235, A64AL245, 
         A64AM105, A64AM115, A64AM125, A64AM135, A64AM145, A64AM155, A64AM165, A64AM175, A64AM185, A64AM195, A64AM205, A64AM215, A64AM225, A64AM235, A64AM245,
         A64AN105, A64AN115, A64AN125, A64AN135, A64AN145, A64AN155, A64AN165, A64AN175, A64AN185, A64AN195, A64AN205, A64AN215, A64AN225, A64AN235, A64AN245,
         A64AR105, A64AR115, A64AR125, A64AR135, A64AR145, A64AR155, A64AR165, A64AR175, A64AR185, A64AR195, A64AR205, A64AR215, A64AR225, A64AR235, A64AR245,
         A64AS105, A64AS115, A64AS125, A64AS135, A64AS145, A64AS155, A64AS165, A64AS175, A64AS185, A64AS195, A64AS205, A64AS215, A64AS225, A64AS235, A64AS245,
         A64AT105, A64AT115, A64AT125, A64AT135, A64AT145, A64AT155, A64AT165, A64AT175, A64AT185, A64AT195, A64AT205, A64AT215, A64AT225, A64AT235, A64AT245,
         A64AX105, A64AX115, A64AX125, A64AX135, A64AX145, A64AX155, A64AX165, A64AX175, A64AX185, A64AX195, A64AX205, A64AX215, A64AX225, A64AX235, A64AX245) %>%
  pivot_longer(names_to = "age", values_to = "people", 2:ncol(.)) %>%
  mutate(year = str_sub(age, -3, -1),
         year = ifelse(year == "105", "2010",
                       ifelse(year == "115", "2011",
                              ifelse(year == "125", "2012",
                                     ifelse(year == "135", "2013",
                                            ifelse(year == "145", "2014",
                                                   ifelse(year == "155", "2015",
                                                          ifelse(year == "165", "2016",
                                                                 ifelse(year == "175", "2017",
                                                                        ifelse(year == "185", "2018",
                                                                               ifelse(year == "195", "2019",
                                                                                      ifelse(year == "205", "2020",
                                                                                             ifelse(year == "215", "2021",
                                                                                                    ifelse(year == "225", "2022",
                                                                                                           ifelse(year == "235", "2023",
                                                                                                                  ifelse(year == "245", "2024", year))))))))))))))),
         cat = str_sub(age, 4,5),
         age = str_sub(age, 1,3)) %>%
  left_join(counties.regions.1, by = c("COUNTYFP" = "countyfp")) %>%
  group_by(project.pr, year, cat) %>%
  summarize(people = sum(people)) %>%
  ungroup() %>%
  filter(project.pr == "Northeast") %>%
  mutate(cat = ifelse(cat %in% c("AG", "AM","AS"), "In labor force", cat),
         cat = ifelse(cat %in% c("AH", "AN", "AT"), "Military", cat),
         cat = ifelse(cat %in% c("AL", "AR", "AX"), "Not in labor force", cat)) %>%
  group_by(year, cat) %>%
  summarize(people = sum(people)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total.pop.20_34 = sum(people[cat %in% c("In labor force", "Not in labor force")])) %>%
  ungroup() %>%
  mutate(pct = people / total.pop.20_34) %>%
  group_by(cat) %>%
  summarize(avg = mean(pct)) %>%
  ungroup() %>%
  pivot_wider(names_from = cat, values_from = avg) %>%
  mutate(region = "Northeast")

names(lf.participant.data)

nes.original <-  read_csv("Data/No MN Emp Records Data/NES/Master-nonemployer-jobs-county.csv")

nes.data <- nes.original %>%
  filter(planning.region == "Northeast",
         year > 2012) %>%
  group_by(year) %>%
  summarize(total.jobs = sum(total.jobs),
            nes.jobs = sum(nes.jobs)) %>%
  ungroup() %>%
  mutate(pct.nes.jobs = nes.jobs / total.jobs) %>%
  summarize(mean.nes.jobs = mean(pct.nes.jobs)) %>%
  mutate(region = "Northeast")

names(nes.data)

# Combine data ------------------------------------------------------------

data <- fed.jobs.data %>%
  left_join(lf.participant.data, by = "region") %>%
  left_join(nes.data, by = "region") %>%
  select(region, avg.fed.emp, 3:ncol(.)) %>%
  rename(Region = region,
         `Average federal employment - pct` = avg.fed.emp,
         `Average labor force participation rate, 20-34` = `In labor force`,
         `Average percent in military, 20-34` = `Military`,
         `Average percent no in labor force, 20-34` = `Not in labor force`,
         `Average non-employers as a percent of total jobs` = mean.nes.jobs)

names(data)

write_csv(data, "Data/Claude/no-mn-emp-record-data.csv")
