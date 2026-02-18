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

original <- read_csv("Data/SLEDS/Masters/Master-report-dataset.csv") 

data <- original %>%
  filter(ps.grad == "No") %>%
  select(grad.year.5, cte.achievement) %>%
  filter(grad.year.5 != "After 2023") %>%
  filter(grad.year.5 != "Attending ps") %>%
  group_by(cte.achievement, grad.year.5) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(cte.achievement) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(cte.achievement = str_replace(cte.achievement, "CTE Concentrator or Completor", "CTE Concentrator"))

cte.achievement.order <- c( "CTE Concentrator", "CTE Participant","No CTE")

grad.year.5.order <- c("Sustained WF - NE", "Sustained WF - MN", "Non-sustained WF", "No MN emp record")

cte.achievement.order.plot <- c( "CTE\nConcentrator", "CTE\nParticipant","No CTE")

grad.year.5.order.plot <- c("Sustained\nWF - NE", "Sustained\nWF - MN", "Non-sustained\nwf", "No MN\nemp record")

df_lodes <- data %>%
  mutate(
    cte.achievement = factor(cte.achievement, levels = cte.achievement.order),
    grad.year.5 = factor(grad.year.5, levels = grad.year.5.order)
  ) %>%
  to_lodes_form(
    axes = c("cte.achievement", "grad.year.5"),
    key = "axis",
    value = "stratum",
    id = "flow_id"
  ) %>%
  group_by(flow_id) %>%
  mutate(
    highlight = if_else(
      any(
        axis == "grad.year.5" &
          stratum %in% c("Non-sustained WF", "No MN emp record")
      ),
      "highlight",
      "base"
    )
  ) %>%
  ungroup() %>%
  group_by(flow_id) %>%
  mutate(
    cte.achievement = first(stratum[axis == "cte.achievement"])
  ) %>%
  ungroup()

labels <- df_lodes %>%
  filter(axis == "cte.achievement", highlight == "highlight") %>%
  arrange(cte.achievement) %>%
  select(flow_id, stratum, pct) %>%
  mutate(cumsum = ifelse(stratum == "CTE Concentrator", 2,
                         ifelse(stratum == "CTE Participant", 1, 0))) %>%
  group_by(stratum) %>%
  mutate(cumpct = rev(cumsum(pct))) %>%
  ungroup() %>%
  mutate(half.pct = pct / 2,
         yloc = ifelse(flow_id %in% c(4,8, 12), cumsum + half.pct, cumsum + cumpct - half.pct),
         highlight = "highlight") %>%
  select(-pct)

master <- df_lodes %>%
  left_join(labels, by = c("stratum", "highlight", "flow_id"))

names(labels)
# Create chart ------------------------------------------------------------

ggplot(
  master,
  aes(
    x = axis,
    stratum = stratum,
    alluvium = flow_id,
    y = pct,
    fill = cte.achievement,     # left-side colors
    alpha = highlight           # highlight flows
  )
) +
  geom_flow(width = 0.4) +
  geom_stratum(width = 0.4, aes(fill = stratum, color = stratum)) +
  geom_text(
    stat = "stratum",
    label = c(rev(cte.achievement.order.plot), rev(grad.year.5.order.plot)),
    size = 3
  ) +
  geom_text(aes(x = 1.35, y = yloc, label = percent(pct, accuracy = .1)), fontface = "bold") +
  scale_alpha_manual(
    values = c(highlight = 1, base = 0.25),
    guide = "none"
  ) +
  scale_x_discrete(
    limits = c("cte.achievement", "middle", "grad.year.5"),
    labels = c("CTE", "", "5 years after\nhigh school")
  ) +
  labs(x = "", 
       y = "",
       title = "Limited CTE Engagement and Much Weaker Labor Force Attachment for Non-College Grads",
       subtitle = "Non-college grads with limited CTE exposure are even more frequently represented among\nnon-meaningful and missing Minnesota workforce participation outcomes") +
  theme_all +
  theme(legend.position = "none")

ggsave(filename = "Charts/Paper report/sankey-cte-acheivement-non-college-grads-non-sustained-wf.pdf", device = cairo_pdf, dpi = "print", width = 6, height = 4.5)

ggsave(filename = "Charts/Paper report/sankey-cte-acheivement-non-college-grads-non-sustained-wf.png", dpi = "print", width = 6, height = 4.5)
