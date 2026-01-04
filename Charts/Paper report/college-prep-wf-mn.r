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

data.act <- original %>%
  select(grad.year.5, took.ACT) %>%
  filter(grad.year.5 != "After 2023") %>%
  filter(grad.year.5 != "Attending ps") %>%
  filter(!is.na(took.ACT)) %>%
  group_by(took.ACT, grad.year.5) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(took.ACT) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year.5 = str_replace(grad.year.5, "No MN emp record, not attending ps", "No MN emp record"),
         grad.year.5 = str_replace(grad.year.5, "Not meaningful, not attending ps", "Not meaningful"),
         grad.year.5 = str_replace(grad.year.5, "Meaningful WF - Northeast", "Meaningful WF - NE"),
         grad.year.5 = fct_relevel(grad.year.5, "Meaningful WF - NE", "Meaningful WF - MN", "Not meaningful", "No MN emp record")) 

names(data.act)

data.ACTCompositeScore <- original %>%
  select(grad.year.5, ACTCompositeScore) %>%
  filter(grad.year.5 != "After 2023") %>%
  filter(grad.year.5 != "Attending ps") %>%
  filter(!is.na(ACTCompositeScore)) %>%
  mutate(ACTCompositeScore.cat = ifelse(ACTCompositeScore < 18, "Below college readiness",
                                        ifelse(ACTCompositeScore > 17 & ACTCompositeScore < 24, "College ready",
                                               ifelse(ACTCompositeScore > 23, "Exceeds college readiness", ACTCompositeScore)))) %>%
  group_by(ACTCompositeScore.cat, grad.year.5) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(ACTCompositeScore.cat) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year.5 = str_replace(grad.year.5, "No MN emp record, not attending ps", "No MN emp record"),
         grad.year.5 = str_replace(grad.year.5, "Not meaningful, not attending ps", "Not meaningful"),
         grad.year.5 = str_replace(grad.year.5, "Meaningful WF - Northeast", "Meaningful WF - NE"),
         grad.year.5 = fct_relevel(grad.year.5, "Meaningful WF - NE", "Meaningful WF - MN", "Not meaningful", "No MN emp record")) 

data.pseo <- original %>%
  select(grad.year.5, pseo.participant) %>%
  filter(grad.year.5 != "After 2023") %>%
  filter(grad.year.5 != "Attending ps") %>%
  filter(!is.na(pseo.participant)) %>%
  mutate(pseo.participant = ifelse(pseo.participant == 1, "Yes", "No")) %>%
  group_by(pseo.participant, grad.year.5) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(pseo.participant) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year.5 = str_replace(grad.year.5, "No MN emp record, not attending ps", "No MN emp record"),
         grad.year.5 = str_replace(grad.year.5, "Not meaningful, not attending ps", "Not meaningful"),
         grad.year.5 = str_replace(grad.year.5, "Meaningful WF - Northeast", "Meaningful WF - NE"),
         grad.year.5 = fct_relevel(grad.year.5, "Meaningful WF - NE", "Meaningful WF - MN", "Not meaningful", "No MN emp record")) 
  

data.ACTCompositeScore$ACTCompositeScore.cat
# Create chart ------------------------------------------------------------
plot.took.ACT <- ggplot(data = filter(data.act, grad.year.5 == "Meaningful WF - MN"), aes(took.ACT, pct, fill = took.ACT)) +
  geom_col(position = "dodge") +
  geom_label(aes(label = percent(pct, accuracy = .1)), show.legend = FALSE, position = position_dodge(width = .9), color = "white", size = 4) +
  geom_label(aes(x = 2, y = .15, label = "Significantly\nhigher"), color = "white", fill = "transparent") +
  labs(x="", 
       y = "", 
       subtitle = "Took ACT") +
  scale_y_continuous(labels=scales::percent,
                     limits = c(0, .25)) +
  scale_fill_manual(values = c("#1a9641", "#0571b0", "#a6611a", "grey")) +
  theme_bar+
  theme(legend.position = "none",
        text = element_text(size = 14),
        plot.subtitle = element_text(size = 10))

plot.took.ACT

plot.ACTCompositeScore.cat <- ggplot(data = filter(data.ACTCompositeScore, grad.year.5 == "Meaningful WF - MN"), aes(ACTCompositeScore.cat, pct, fill = ACTCompositeScore.cat)) +
  geom_col(position = "dodge") +
  geom_label(aes(label = percent(pct, accuracy = .1)), show.legend = FALSE, position = position_dodge(width = .9), color = "white", size = 4) +
  annotate(geom = "segment",
           x = .85,
           xend = 2.5,
           y = .2,
           yend = .28,
           arrow = arrow(length = unit(.25, "cm"))) +
  labs(x="", 
       y = "", 
       subtitle = "ACT Composite Score") +
  scale_y_continuous(labels=scales::percent,
                     limits = c(0, .3)) +
  scale_x_discrete(labels = c("Below\ncollege\nreadiness", "College\nready", "Exceeds\ncollege\nreadiness")) +
  scale_fill_manual(values = c("#1a9641", "#0571b0", "#a6611a", "grey")) +
  theme_bar+
  theme(legend.position = "none",
        text = element_text(size = 14),
        plot.subtitle = element_text(size = 10))

plot.ACTCompositeScore.cat

plot.pseo <- ggplot(data = filter(data.pseo, grad.year.5 == "Meaningful WF - MN"), aes(pseo.participant, pct, fill = pseo.participant)) +
  geom_col(position = "dodge") +
  geom_label(aes(label = percent(pct, accuracy = .1)), show.legend = FALSE, position = position_dodge(width = .9), color = "white", size = 4) +
  annotate(geom = "label",
           x = 2,
           y = .15,
           label = "Significantly\nhigher",
           fill = "transparent",
           color = "white") +
  labs(x="", 
       y = "", 
       subtitle = "PSEO participant") +
  scale_y_continuous(labels=scales::percent,
                     limits = c(0, .25)) +
  scale_fill_manual(values = c("#1a9641", "#0571b0", "#a6611a", "grey")) +
  theme_bar+
  theme(legend.position = "none",
        text = element_text(size = 14),
        plot.subtitle = element_text(size = 10))

plot.pseo

top <- plot_grid(plot.took.ACT, plot.ACTCompositeScore.cat, ncol = 2, rel_widths = c(1,1))

top

bottom <- plot_grid(NULL, plot.pseo, NULL, ncol = 3, rel_widths = c(.1,.8,.1))

bottom

title <- ggdraw() +
  draw_label("Proportion with meaningful workforce participation in\nMinnesota but outside region",
             fontface = "bold",
             x = 0,
             hjust = 0,
             fontfamily = "Avenir",
             size = 14)

plot_grid(title, top, bottom, rel_heights = c(.1, .45, .45), ncol = 1)

ggsave(filename = "Charts/Paper report/college-prep-wf-mn.pdf", device = cairo_pdf, dpi = "print", width = 6, height = 5)

ggsave(filename = "Charts/Paper report/college-prep-wf-mn.png", dpi = "print", width = 6, height = 5)
