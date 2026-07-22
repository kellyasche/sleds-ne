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

primary_colors <- c("#012623", "#2E7C63", "#8B601F", "#9B3F24", "grey")

alt_colors     <- c("#012623", "#9B3F24", "#E6A762", "#8B601F", "#222222")

expanded_colors <- c("#012623",   # Deep Field Green
                     "#9B3F24",   # Earth Red
                     "#2E7C63",   # Community Green
                     "#E6A762",   # Warm Accent
                     "#8B601F",   # Heritage Brown
                     "#0F5952",   # Secondary Green
                     "#222222",   # Dark Neutral
                     "grey")      # Light-mid neutral


# Prep data ---------------------------------------------------------------

original <- read_csv("Data/SLEDS/Masters/States/annual-seven-states.csv")

data <- original %>%
  mutate(states = ifelse(states %in% c("Meaningful emp County", "Meaningful emp EDR"), "Sustained WF - NE", states),
         states = ifelse(states == "Meaningful emp MN", "Sustained WF - MN", states),
         states = ifelse(states == "No MN emp record, not attending ps", "No MN emp record", states),
         states = ifelse(states == "Not meaningful, not attending ps", "Non-sustained WF", states),
         states = as.factor(states)) %>%
  select(grad.year, states) %>%
  filter(states != "After 2023") %>%
  group_by(grad.year, states) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(grad.year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year = str_sub(grad.year, -2, -1),
         grad.year = str_replace(grad.year, "[.]", ""),
         grad.year = as.numeric(grad.year),
         states = fct_relevel(states, "Sustained WF - NE", "Sustained WF - MN", "No MN emp record", "Non-sustained WF", "Attending ps"),
         data_id = seq(n())) %>%
  group_by(grad.year) %>%
  arrange(desc(states)) %>%
  mutate(cum.pct = cumsum(share),
         x.loc = cum.pct - ((cum.pct - lag(cum.pct)) / 2),
         x.loc = ifelse(states == "Attending ps", cum.pct/2, x.loc),
         x.loc = ifelse(states == "Non-sustained WF" & is.na(x.loc), cum.pct /2, x.loc)) %>%
  ungroup()


# Create chart ------------------------------------------------------------
names(data)

ggplot(data = filter(data, states != "Attending ps"), aes(grad.year, share, fill = states, group = states)) +
  facet_wrap(~states, ncol = 2) +
  geom_col(position = "dodge") +
  geom_label(data = filter(data, states != "Attending ps", grad.year %in% c(1,5,10, 15)), aes(label = percent(share, accuracy = .1)), show.legend = FALSE, position = position_dodge(width = .9), color = "white", size = 4) +
  labs(x="", y = "", title = "Pathways taken by share of graduates X years after graduating\nhigh school")+
  scale_y_continuous(labels=scales::percent) +
  theme_bar+
  scale_fill_manual(values = primary_colors,
                    guide = guide_legend(ncol = 2)) +
  theme(legend.position = "none")

ggsave(filename = "Charts/Draft 4/states.pdf", device = cairo_pdf, dpi = "print", width = 6, height = 5)

ggsave(filename = "Charts/Draft 4/states.png", dpi = "print", width = 6, height = 5)

names(data)

ggplot(data, aes(grad.year, share, fill = states, group = states)) +
  geom_area_interactive(aes(data_id = data_id, tooltip = paste("State: ", states, "\n", percent(share, accuracy = .1), sep = ""))) +
  geom_label(data = filter(data, grad.year %in% c(1, 5, 7, 10)), aes(y = x.loc, x = grad.year, label = percent(share, accuracy = .1)), show.legend = FALSE) +
  labs(x="Years after high school", y = "Proportion of high school graduates", color="", title = "Workforce outcomes by years after graduating high school")+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  theme_bar+
  scale_fill_manual(values = brewer.pal(n = 7, "RdYlBu"),
                    guide = guide_legend(ncol = 3)) +
  theme(legend.position = "bottom")

ggsave(filename = "Charts/Paper report/states.pdf", device = cairo_pdf, dpi = "print", width = 6.5, height = 5)

ggsave(filename = "Charts/Paper report/states.png", dpi = "print", width = 6.5, height = 5)
