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


# Card data: side, label, and vertical position (stacked top to bottom)
cards <- tibble::tribble(
  ~side,     ~label,                     ~row,
  "region",  "American Indian",          1,
  "region",  "Low-income",               1.5,
  "region",  "CTE",                      2,
  "region",  "Female",                   2.5,
  "outside", "EDR 2 & 4",                1,
  "outside", "Higher academic engagement", 1.5,
  "outside", "Black or Asian/PI",        2
) %>%
  mutate(
    x0 = if_else(side == "region", 0, 5.2),   # left column starts at 0, right at 5.2
    ymax = -(row - 1) * 1.15,                  # stack downward
    ymin = ymax - .5,
    accent_color = if_else(side == "region", "#012623", "#9B3F24")
  )

headers <- tibble::tribble(
  ~side,     ~label,
  "region",  "Completing College in Region",
  "outside", "Completing College outside Region"
) %>%
  mutate(
    x0 = if_else(side == "region", 0, 5.2),
    accent_color = if_else(side == "region", "#012623", "#9B3F24")
  )

card_width <- 4


# Create chart ------------------------------------------------------------



ggplot() +
  # cards (tan background)
  geom_rect(data = cards,
            aes(xmin = x0 + 0.08, xmax = x0 + card_width, ymin = ymin, ymax = ymax),
            fill = "#F0E5D6") +
  # left border accent
  geom_rect(data = cards,
            aes(xmin = x0, xmax = x0 + 0.08, ymin = ymin, ymax = ymax, fill = accent_color)) +
  scale_fill_identity() +
  # card labels
  geom_text(data = cards,
            aes(x = x0 + 0.35, y = (ymin + ymax) / 2, label = label),
            hjust = 0, size = 3, fontface = "bold", color = "#222222") +
  # column headers
  geom_text(data = headers,
            aes(x = x0, y = 0.55, label = label),
            hjust = 0, size = 4, fontface = "bold", color = "#0b0b0b") +
  # underline beneath each header
  geom_segment(data = headers,
               aes(x = x0, xend = x0 + card_width, y = 0.15, yend = 0.15, color = accent_color),
               linewidth = 1) +
  scale_color_identity() +
  # title text (plot-level, not data-driven)
  annotate("text", x = 0, y = 1.5, label = "HIGH SCHOOL EXPERIENCES & COLLEGE LOCATION",
           hjust = 0, size = 4, fontface = "bold", color = "#2E7C63") +
  annotate("text", x = 0, y = 1.1,
           label = "What high school experiences were associated with where\ngraduates went to college?",
           hjust = 0, size = 5, fontface = "bold", color = "#0b0b0b", lineheight = 0.9) +
  coord_cartesian(xlim = c(0, 9.7), ylim = c(-2.1, 1.5), expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.margin = margin(20, 20, 20, 20))

ggsave(filename = "Charts/Paper report/hs-experiences-regional-college.pdf", device = cairo_pdf, dpi = "print", width = 6, height = 4)

ggsave(filename = "Charts/Paper report/hs-experiences-regional-college.png", dpi = "print", width = 6, height = 4)

