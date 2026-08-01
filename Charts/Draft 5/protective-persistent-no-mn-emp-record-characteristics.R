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

original <- read_csv("Data/SLEDS/Masters/Master-non-college-grads-disconnection.csv") 


master.disc <- original %>%
  filter(grad.year > 2010,
         !is.na(pseo.participant)) %>%
  mutate(
    Gender              = fct_relevel(Gender, "M"),
    RaceEthnicity       = fct_relevel(RaceEthnicity, "White"),
    economic.status = as_factor(economic.status),
    economic.status     = fct_relevel(economic.status, "0"),
    SpecialEdStatus = as_factor(SpecialEdStatus),
    SpecialEdStatus     = fct_relevel(SpecialEdStatus, "0"),
    english.learner = as_factor(english.learner),
    english.learner     = fct_relevel(english.learner, "0"),
    Dem_Desc            = fct_relevel(Dem_Desc, "Entirely rural"),
    pseo.participant = as_factor(pseo.participant),
    pseo.participant    = fct_relevel(pseo.participant, "0"),
    took.ACT            = fct_relevel(took.ACT, "No"),
    ap.exam = as_factor(ap.exam),
    ap.exam             = fct_relevel(ap.exam, "0"),
    cte.achievement     = fct_relevel(cte.achievement, "No CTE"),
    attended.ps         = fct_relevel(attended.ps, "No"),
    
    # ACT missing-indicator recode - confirmed safe: missingness fully
    # explained by took.ACT (9,228 No/missing, 5,941 Yes/present, zero mismatches)
    ACTCompositeScore.filled = ifelse(is.na(ACTCompositeScore), 0, ACTCompositeScore),
    
    # Dependent variables: binary persistence indicators
    persistent.non_sustained = ifelse(non_sustained.cat == "Persistently non-sustained", 1, 0),
    persistent.no_record     = ifelse(no_record.cat == "Persistently no-record", 1, 0)
  )

model3_no_record <- glm(
  persistent.no_record ~
    Gender +
    RaceEthnicity +
    economic.status +
    SpecialEdStatus +
    english.learner +
    Dem_Desc +
    avg.wages.pct.state +
    avg.unemp.rate +
    pseo.participant +
    took.ACT +
    ACTCompositeScore.filled +
    ap.exam +
    cte.achievement +
    attended.ps,
  family = binomial,
  data = master.disc
)

summary(model3_no_record)


# ---- Organizational color palette ----
color_baseline <- "#012623"  # Deep Field Green
color_highlight <- "#2E7C63" # Community Green

# ---- Helper: average marginal effect (marginal standardization) ----
amp <- function(model, data, var, value) {
  data[[var]] <- value
  mean(predict(model, newdata = data, type = "response"))
}

# ---- Chart: Protective factors for NO-RECORD status ----

dumbbell_norecord <- tibble(
  characteristic = c(
    "Attended Postsecondary\n(vs. Never Attended)",
    "Took the ACT\n(vs. Non-Participant)",
    "CTE Concentrator/Completer\n(vs. No CTE)",
    "CTE Participant\n(vs. No CTE)",
    "Economic Disadvantage\n(vs. Not Disadvantaged)",
    "PSEO Participation\n(vs. Non-Participant)",
    "Female\n(vs. Male)"
  ),
  baseline_pct = c(
    amp(model3_no_record, master.disc, "attended.ps", "No"),
    amp(model3_no_record, master.disc, "took.ACT", "No"),
    amp(model3_no_record, master.disc, "cte.achievement", "No CTE"),
    amp(model3_no_record, master.disc, "cte.achievement", "No CTE"),
    amp(model3_no_record, master.disc, "economic.status", "0"),
    amp(model3_no_record, master.disc, "pseo.participant", "0"),
    amp(model3_no_record, master.disc, "Gender", "M")
  ),
  higher_pct = c(
    amp(model3_no_record, master.disc, "attended.ps", "Yes"),
    amp(model3_no_record, master.disc, "took.ACT", "Yes"),
    amp(model3_no_record, master.disc, "cte.achievement", "CTE Concentrator or Completor"),
    amp(model3_no_record, master.disc, "cte.achievement", "CTE Participant"),
    amp(model3_no_record, master.disc, "economic.status", "1"),
    amp(model3_no_record, master.disc, "pseo.participant", "1"),
    amp(model3_no_record, master.disc, "Gender", "F")
  )
) %>%
  mutate(
    gap = higher_pct - baseline_pct,
    characteristic = factor(characteristic, levels = characteristic[order(-gap)])
  )

ggplot(dumbbell_norecord, aes(y = characteristic)) +
  geom_segment(aes(x = baseline_pct, xend = higher_pct, yend = characteristic),
               color = "#B0B0B0", linewidth = 1.5, alpha = 0.7) +
  geom_point(aes(x = baseline_pct, color = "Without characteristic"), size = 4) +
  geom_point(aes(x = higher_pct, color = "With characteristic"), size = 4) +
  geom_text(aes(x = baseline_pct, label = percent(baseline_pct, accuracy = 1)),
            vjust = -1.6, size = 3, color = color_baseline) +
  geom_text(aes(x = higher_pct, label = percent(higher_pct, accuracy = 1)),
            vjust = -1.6, size = 3, color = color_highlight, fontface = "bold") +
  scale_color_manual(
    name = NULL,
    values = c("Without characteristic" = color_baseline, 
               "With characteristic" = color_highlight),
    breaks = c("Without characteristic", "With characteristic")
  ) +
  scale_x_continuous(labels = percent, limits = c(0, 1),
                     expand = expansion(mult = c(0.02, 0.1))) +
  labs(
    x = "Predicted likelihood of persistent no-record status",
    y = NULL,
    title = "Predicted Likelihood of Persistent No-Record Status by Protective\nCharacteristics",
    subtitle = "Attempting postsecondary education, ACT and PSEO participation, CTE engagement,\neconomic disadvantage, and Gender are all independently associated with lower\npersistent no-record status."
  ) +
  theme_bar +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 7))

ggsave(filename = "Charts/Draft 5/protective-persistent-no-mn-emp-record-characteristics.pdf", device = cairo_pdf, dpi = "print", width = 6, height = 5)

ggsave(filename = "Charts/Draft 5/protective-persistent-no-mn-emp-record-characteristics.png", dpi = "print", width = 6, height = 5)
