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

data.ps.grad <- original %>%
  filter(ps.grad == "Y") %>%
  select(grad.year.5, SpecialEdStatus) %>%
  filter(!grad.year.5 %in% c("Attending ps", "After 2023")) %>%
  mutate(SpecialEdStatus = ifelse(SpecialEdStatus == 1, "Received special ed", "No special ed")) %>%
  group_by(grad.year.5, SpecialEdStatus) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(SpecialEdStatus) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year.5 = fct_relevel(grad.year.5, "Meaningful WF - Northeast", "Meaningful WF - MN", "Not meaningful, not attending ps", "No MN emp record, not attending ps"))

data.not.ps.grad <- original %>%
  filter(ps.grad == "No") %>%
  select(grad.year.5, SpecialEdStatus) %>%
  filter(!grad.year.5 %in% c("Attending ps", "After 2023")) %>%
  mutate(SpecialEdStatus = ifelse(SpecialEdStatus == 1, "Received special ed", "No special ed")) %>%
  group_by(grad.year.5, SpecialEdStatus) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(SpecialEdStatus) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(grad.year.5 = fct_relevel(grad.year.5, "Meaningful WF - Northeast", "Meaningful WF - MN", "Not meaningful, not attending ps", "No MN emp record, not attending ps"))



# Create chart ------------------------------------------------------------
names(data)

ps.grad.plot <- ggplot(data = filter(data.ps.grad, SpecialEdStatus == "Received special ed"), aes(grad.year.5, pct, fill = grad.year.5)) +
  geom_col(position = "dodge") +
  geom_label(aes(label = percent(pct, accuracy = .1)), show.legend = FALSE, position = position_dodge(width = .9), color = "white", size = 3) +
  labs(x="", y = "", subtitle = "College graduates")+
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels = c("Meaningful\nWF NE", "Meaningful\nWF MN", "Not\nmeaningful", "No MN\nemployment")) +
  theme_bar+
  scale_fill_manual(values = c("#1a9641", "#0571b0", "#a6611a", "grey"),
                    guide = guide_legend(ncol = 2)) +
  theme(legend.position = "none",
        text = element_text(size = 14),
        plot.subtitle = element_text(size = 10),
        axis.text.x = element_text(size = 8))

ps.grad.plot

not.ps.grad.plot <- ggplot(data = filter(data.not.ps.grad, SpecialEdStatus == "Received special ed"), aes(grad.year.5, pct, fill = grad.year.5)) +
  geom_col(position = "dodge") +
  geom_label(aes(label = percent(pct, accuracy = .1)), show.legend = FALSE, position = position_dodge(width = .9), color = "white", size = 3) +
  annotate(geom = "label",
           x = 3.5,
           y = .2,
           label = "Significantly higher\nthan college grads",
           color = "black",
           fill = "white",
           size = 3) +
  annotate(geom = "label",
           x = 1.5,
           y = .05,
           label = "Significantly lower\nthan college grads",
           color = "black",
           fill = "white",
           size = 3) +
  labs(x="", y = "", subtitle = "Non-college graduates")+
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels = c("Meaningful\nWF NE", "Meaningful\nWF MN", "Not\nmeaningful", "No MN\nemployment")) +
  theme_bar+
  scale_fill_manual(values = c("#1a9641", "#0571b0", "#a6611a", "grey"),
                    guide = guide_legend(ncol = 2)) +
  theme(legend.position = "none",
        text = element_text(size = 14),
        plot.subtitle = element_text(size = 10),
        axis.text.x = element_text(size = 8))

not.ps.grad.plot

title <- ggdraw() +
  draw_label("Received special education - college and non-college grads by\nworkforce participation category",
             fontfamily = "Avenir",
             fontface = "bold",
             x = 0,
             hjust = 0,
             size = 14)

top <- cowplot::plot_grid(ps.grad.plot, not.ps.grad.plot, ncol = 2, rel_widths = c(1,1))

top

cowplot::plot_grid(title, top, ncol = 1, rel_heights = c(.1, .9))

ggsave(filename = "Charts/Paper report/received-special-ed-ps-grad-wf-cat.pdf", device = cairo_pdf, dpi = "print", width = 6.5, height = 5)

ggsave(filename = "Charts/Paper report/received-special-ed-ps-grad-wf-cat.png", dpi = "print", width = 6.5, height = 5)

