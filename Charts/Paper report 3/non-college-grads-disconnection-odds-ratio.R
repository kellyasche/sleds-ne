# library -----------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggrepel)
library(scales)
library(ggiraph)
library(rmapshaper)
library(cowplot)
library(RColorBrewer)
library(readxl)
library(lubridate)
library(readxl)
library(systemfonts)
reset_font_cache()
library(ggtext)
library(janitor)
library(cowplot)
library(ggalluvial)
library(ggsankey)
library(broom)
library(forcats)
library(stringr)


rm(list = ls())

# themes ------------------------------------------------------------------
theme_bar <- theme_bw() +
  theme(panel.grid.major = element_line(color = "grey70", size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(1, "lines"),
        legend.margin = margin(0,0,0,0),
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(l = 2)),
        text = element_text(family = "Avenir") ,
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

theme_line <- theme_bw() +
  theme(legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.text = element_text(margin = margin(l = 2)),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey70", size = 0.1),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        legend.margin = margin(0,0,0,0),
        legend.key.size = unit(1, "lines"),
        text = element_text(family = "Avenir") ,
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))


theme_sf <- theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(l = 2)),
        legend.margin = margin(0,0,0,0),
        legend.key.size = unit(1, "lines"),
        text = element_text(family = "Avenir") ,
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))

theme_all <- theme_bw() +
  theme(axis.ticks=element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(l = 2)),
        legend.margin = margin(0,0,0,0),
        legend.key.size = unit(1, "lines"),
        text = element_text(family = "Avenir") ,
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"))


regions <- read_csv("/Users/kellyasche/Library/CloudStorage/GoogleDrive-kasche@ruralmn.org/My Drive/Data Prep/R Projects/Join docs/county_regions.csv") %>%
  select(5,6) %>%
  unique() %>%
  mutate(edr = str_replace(edr, "  ", " "),
         planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))

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

color.ruca <- c("Entirely rural" = "#009933", "Town/rural mix" = "#99CC33", "Urban/town/rural mix" = "#CC9966", "Entirely urban" = "#754C29", "Minnesota" = "black")

color.pr <- c("Northwest" = "#4575b4","Northeast" = "grey", "Central" = "#fee090", "Seven County Mpls-St Paul" = "#d73027", "Southwest" = "#91bfdb", "Southeast" = "#fc8d59", "Minnesota" = "black")

color.edr <- c("EDR 1 - Northwest" = "#b3cde3", "EDR 2 - Headwaters" = "#8c96c6", "EDR 3 - Arrowhead" = "#fe9929", "EDR 4 - West Central" = "#8856a7", "EDR 5 - North Central" = "#810f7c", "EDR 6E- Southwest Central" = "#e5f5f9", "EDR 6W- Upper Minnesota Valley" = "#bdc9e1", "EDR 7E- East Central" = "#99d8c9", "EDR 7W- Central" = "#2ca25f", "EDR 8 - Southwest" = "#74a9cf", "EDR 9 - South Central" = "#0570b0", "EDR 10 - Southeast" = "#d7301f", "EDR 11 - 7 County Twin Cities" = "#d8b365", "Minnesota" = "black")

color.pr.edr <- c ("Northwest" = "#4575b4","Northeast" = "#e0f3f8", "Central" = "#fee090", "Seven County Mpls-St Paul" = "#d73027", "Southwest" = "#91bfdb", "Southeast" = "#fc8d59", "Minnesota" = "black", "EDR 1 - Northwest" = "#b3cde3", "EDR 2 - Headwaters" = "#8c96c6", "EDR 3 - Arrowhead" = "#fe9929", "EDR 4 - West Central" = "#8856a7", "EDR 5 - North Central" = "#810f7c", "EDR 6E- Southwest Central" = "#e5f5f9", "EDR 6W- Upper Minnesota Valley" = "#bdc9e1", "EDR 7E- East Central" = "#99d8c9", "EDR 7W- Central" = "#2ca25f", "EDR 8 - Southwest" = "#74a9cf", "EDR 9 - South Central" = "#0570b0", "EDR 10 - Southeast" = "#d7301f", "EDR 11 - 7 County Twin Cities" = "#d8b365")

color.six <- c("#009933", "#4575b4", "grey", "#fee090", "#fc8d59", "#d73027")

mn_counties <- st_read("/Users/kellyasche/Library/CloudStorage/GoogleDrive-kasche@ruralmn.org/My Drive/Data Prep/R Projects/Shapefiles/County shapefiles/MNCounties_MNDOT.shp", quiet = TRUE) %>%
  ms_simplify(keep = .01, keep_shapes = TRUE) %>%
  rename("countyfp" = "FIPS_CODE")

counties.regions.1 <- counties.regions %>%
  mutate(statefp = "27",
         project.pr = ifelse(edr %in% c("EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 4 - West Central"), "Northwest",
                             ifelse(edr == "EDR 3 - Arrowhead", "Northeast",
                                    ifelse(edr %in% c("EDR 5 - North Central", "EDR 7E- East Central", "EDR 7W- Central"), "Central",
                                           ifelse(edr %in% c("EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 8 - Southwest"), "Southwest",
                                                  ifelse(edr %in% c("EDR 9 - South Central", "EDR 10 - Southeast"), "Southern", as.character(planning.region)))))),
         project.pr = fct_relevel(project.pr, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southern"))


# Prep data ---------------------------------------------------------------

gender <- read_csv("Data/SLEDS/Masters/Master-report-dataset.csv") %>%
  select(PersonID, Gender)

original <- read_csv("Data/SLEDS/Masters/Master-disconnected.csv") %>%
  left_join(gender, by = "PersonID")

names(original)

data <- original %>%
  select(RaceEthnicity, economic.status, SpecialEdStatus, Gender, Dem_Desc, avg.wages.pct.state, avg.unemp.rate, cte.achievement, pseo.participant, attended.ps, disconnected.cat) %>%
  mutate(RaceEthnicity = fct_relevel(RaceEthnicity, "White"),
         economic.status = ifelse(economic.status == 1, "FRL", "Non-FRL"),
         economic.status = fct_relevel(economic.status, "Non-FRL"),
         SpecialEdStatus = ifelse(SpecialEdStatus == 1, "Special Ed", "Non-Special Ed"),
         SpecialEdStatus = fct_relevel(SpecialEdStatus, "Non-Special Ed"),
         Gender = ifelse(Gender == "F", "Female", "Male"),
         Gender = fct_relevel(Gender, "Male"),
         Dem_Desc = fct_relevel(Dem_Desc, "Entirely rural"),
         pseo.participant = ifelse(pseo.participant == 1, "PSEO",
                                   ifelse(pseo.participant == 0, "No PSEO", pseo.participant)),
         pseo.participant = fct_relevel(pseo.participant, "No PSEO"),
         cte.achievement = fct_relevel(cte.achievement, "No CTE"),
         attended.ps = fct_relevel(attended.ps, "No"),
         disconnected.cat = ifelse(disconnected.cat %in% c("Broadly disconnected", "Persistently disconnected"), 1, 0))

model <- glm(
  disconnected.cat ~
    RaceEthnicity +
    economic.status +
    SpecialEdStatus +
    Gender +
    Dem_Desc +
    avg.wages.pct.state +
    avg.unemp.rate +
    pseo.participant +
    cte.achievement +
    attended.ps,
  family = binomial,
  data = data
)

model

write_csv(data, "Data/Claude/non-completers-disconnection.csv")
# Create chart ------------------------------------------------------------

or_plot <- tidy(model, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # Make labels nicer (edit these replacements to match your exact factor labels)
    term = str_replace_all(term, c(
      "economic.status" = "Economic status: ",
      "SpecialEdStatus" = "Special Ed: ",
      "pseo.participant" = "PSEO: ",
      "cte.achievement" = "CTE: ",
      "MCA.M" = "MCA Math",
      "highest.cred.level" = "Highest credential"
    )),
    # Put most important effects near the top (optional: tweak ordering later)
    term = fct_reorder(term, estimate),
    color = ifelse(estimate < 1, "Green", "Brown")
  )

ggplot(data = filter(or_plot, term %in% c("RaceEthnicityBlack", "RaceEthnicityHispanic", "RaceEthnicityHispanic", "RaceEthnicityAsian/PI", "Special Ed: Special Ed", "RaceEthnicityAI", "GenderFemale")), aes(x = estimate, y = term, color = color)) +
 geom_vline(xintercept = 1, linetype = "dashed") +
geom_point() +
  annotate(geom = "segment",
           x = 1,
           xend = 1.5,
           y = .6,
           yend = .6,
           arrow = arrow(length = unit(.1, "cm"))) +
  annotate(geom = "text",
           x = 1.9,
           y = .6,
           label = "More likely to be disconnected",
           size = 2) +
  scale_color_manual(values = c("#2E7C63", "#8B601F")) +
  scale_y_discrete(labels = c("Female", "American Indian", "Special Education", "Asian/Pacific Islander", "Hispanic", "Black")) +
  scale_x_continuous(limits =  c(.5, 3),
                     breaks = c(.5, 1, 1.5, 2, 2.5, 3),
                     labels = c("0.5", "1.0\nNo difference", "1.5", "2.0", "2.5", "3.0")) +
  theme_line +
  labs(
    x = "",
    y = NULL,
    title = "Odds of Being Persistently or Broadly Disconnected from the Labor\nForce",
    subtitle = "Race, special education status, and gender are all associated with higher likelihood of\nbeing persistently or broadly disconnected."
  ) +
  theme(legend.position = "none")

ggsave(filename = "Charts/Paper report 3/non-college-grads-disconnection-odds-ratio.pdf", device = cairo_pdf, dpi = "print", width = 6, height = 4.5)

ggsave(filename = "Charts/Paper report 3/non-college-grads-disconnection-odds-ratio.png",  dpi = "print", width = 6, height = 4.5)
