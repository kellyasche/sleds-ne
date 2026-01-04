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

original <- read_csv("Data/SLEDS/Masters/Master-report-dataset.csv") 

names(original)

data.highest.cred <- original %>%
  select(grad.year.5, highest.cred.level) %>%
  filter(grad.year.5 != "After 2023") %>%
  filter(grad.year.5 != "Attending ps") %>%
  filter(!highest.cred.level %in% c("Attending ps")) %>%
  mutate(highest.cred.level = ifelse(highest.cred.level %in% c("Associate degree", "Less than Associate Degree"), "Associate degree or lesser credential", highest.cred.level)) %>%
  group_by(highest.cred.level, grad.year.5) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(highest.cred.level) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()  %>%
  mutate(grad.year.5 = str_replace(grad.year.5, "No MN emp record, not attending ps", "No MN emp record"),
         grad.year.5 = str_replace(grad.year.5, "Not meaningful, not attending ps", "Not meaningful"),
         grad.year.5 = fct_relevel(grad.year.5, "Meaningful WF - Northeast", "Meaningful WF - MN", "Not meaningful", "No MN emp record"),
         highest.cred.level = fct_relevel(highest.cred.level, "Associate degree or lesser credential", "Bachelor degree or higher",  "Did not graduate ps")) 

data.InstitutionSector <- original %>%
  select(grad.year.5, ps.grad.InstitutionSector) %>%
  filter(grad.year.5 != "After 2023") %>%
  filter(grad.year.5 != "Attending ps") %>%
  filter(!ps.grad.InstitutionSector %in% c("Attending ps", "Did not grad", "Never attended ps")) %>%
  mutate(ps.grad.InstitutionSector = ifelse(ps.grad.InstitutionSector == "1", "4-year public",
                                            ifelse(ps.grad.InstitutionSector == "4", "2-year public",
                                                   ifelse(ps.grad.InstitutionSector == "11", "Other college", ps.grad.InstitutionSector))),
         ps.grad.InstitutionSector = as.factor(ps.grad.InstitutionSector)) %>%
  group_by(ps.grad.InstitutionSector, grad.year.5) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(ps.grad.InstitutionSector) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year.5 = str_replace(grad.year.5, "No MN emp record, not attending ps", "No MN emp record"),
         grad.year.5 = str_replace(grad.year.5, "Not meaningful, not attending ps", "Not meaningful"),
         grad.year.5 = fct_relevel(grad.year.5, "Meaningful WF - Northeast", "Meaningful WF - MN", "Not meaningful", "No MN emp record"))


# Create chart ------------------------------------------------------------
names(data.highest.cred)

highest.cred.plot <- ggplot(data= filter(data.highest.cred, grad.year.5 == "Meaningful WF - Northeast"), aes(highest.cred.level, pct, fill = highest.cred.level)) +
  geom_col(position = "dodge") +
  geom_label(aes(label = percent(pct, accuracy = .1)), show.legend = FALSE, position = position_dodge(width = .9), color = "black", size = 4) +
  labs(x="", y = "", subtitle = "Highest credential earned\n")+
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels = c("Associate degree\nor lesser", "Bachelor\ndegree or higher", "Did not\ngraduate ps")) +
  scale_fill_manual(values = c("#4d9221", "#4575b4", "#8c510a")) +
  theme_bar+
  theme(legend.position = "none",
        text = element_text(size = 14),
        plot.subtitle = element_text(size = 10),
        axis.text.x = element_text(size = 8))

highest.cred.plot

InstitutionSector.plot <- ggplot(data= filter(data.InstitutionSector, grad.year.5 == "Meaningful WF - Northeast"), aes(ps.grad.InstitutionSector, pct, fill = ps.grad.InstitutionSector)) +
  geom_col(position = "dodge") +
  geom_label(aes(label = percent(pct, accuracy = .1)), show.legend = FALSE, position = position_dodge(width = .9), color = "black", size = 4) +
  labs(x="", y = "", subtitle = "Type of institution from which highest credential\nwas earned")+
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = c("#4d9221", "#4575b4", "#8c510a")) +
  theme_bar+
  theme(legend.position = "none",
        text = element_text(size = 14),
        plot.subtitle = element_text(size = 10),
        axis.text.x = element_text(size = 8))

InstitutionSector.plot


title <- ggdraw() +
  draw_label("Has meaningful workforce participation in the region 5-years\nafter high school",
             fontfamily = "Avenir",
             fontface = "bold",
             x = 0,
             hjust = 0,
             size = 16)

top <- cowplot::plot_grid(highest.cred.plot, InstitutionSector.plot, ncol = 2, rel_widths = c(1,1))

top

cowplot::plot_grid(title, top, ncol = 1, rel_heights = c(.15, 1))

ggsave(filename = "Charts/Paper report/highest-cred-InstitutionSector-regional-wf.pdf", device = cairo_pdf, dpi = "print", width = 6.5, height = 5)

ggsave(filename = "Charts/Paper report/highest-cred-InstitutionSector-regional-wf.png", dpi = "print", width = 6.5, height = 5)

