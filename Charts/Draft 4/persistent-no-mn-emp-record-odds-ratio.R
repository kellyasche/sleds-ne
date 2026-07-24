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
library(broom)


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
master.original <- read_csv("Data/SLEDS/Masters/Master-non-college-grads-disconnection.csv") 

master.disc <- master.original %>%
  filter(grad.year > 2010) %>%
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
    
    # MCA Math missing-indicator recode - applied here specifically because
    # English learner status is central to this confirmatory analysis, and
    # MCA missingness has previously been shown to correlate with EL status
    # and mask its true relationship with the outcome
    MCA.M.missing = ifelse(is.na(MCA.M), 1, 0),
    MCA.M.filled  = ifelse(is.na(MCA.M), 0, MCA.M),
    
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
    MCA.M.filled +
    MCA.M.missing +
    attended.ps,
  family = binomial,
  data = master.disc
)


or_plot <- tidy(model3_no_record, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, estimate),
         significant = p.value < 0.05,
         color = case_when(
           !significant ~ "Not statistically significant",
           estimate < 1  ~ "Lower odds",
           TRUE          ~ "Higher odds"))

ggplot(data = filter(or_plot, term %in% c("RaceEthnicityBlack", "SpecialEdStatus1", "RaceEthnicityAI", "GenderF", "RaceEthnicityAsian/PI", "RaceEthnicityHispanic")), aes(x = estimate, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = c(0.5, 0.75, 1.25, 1.5, 2),
             color = "grey85") +
  geom_point() +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0.2
  ) +
  scale_x_log10(
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2),
    labels = c("0.5", "0.75", "1.0", "1.25", "1.5", "2.0")
  ) +
  theme_line +
  labs(
    x = "Odds ratio (log scale)",
    y = NULL
  )

ggplot(data = filter(or_plot, term %in% c("RaceEthnicityBlack", "SpecialEdStatus1", "RaceEthnicityAI", "GenderF", "RaceEthnicityAsian/PI", "RaceEthnicityHispanic")), aes(x = estimate, y = term, color = color)) +
  annotate(geom = "segment",
           x = 1,
           xend = .3,
           y = .2,
           yend = .2,
           arrow = arrow(length = unit(.25, "cm"))) +
  annotate(geom = "segment",
           x = 1,
           xend = 1.7,
           y = .2,
           yend = .2,
           arrow = arrow(length = unit(.25, "cm"))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = c(0.5, 0.75, 1.25, 1.5, 2),
             color = "grey85") +
  annotate(geom = "text",
           x = .5,
           y = .6,
           label = "Less likely to persistently\nhave no MN emp record",
           size = 2) +
  annotate(geom = "text",
           x = 1.5,
           y = .6,
           label = "More likely to persistently\nhave no MN emp record",
           size = 2) +
  geom_point() +
  scale_color_manual(values = c("Higher odds" = "#8B601F",
                                "Lower odds" = "#2E7C63",
                                "Not statistically significant" = "grey70")) +
  scale_y_discrete(labels = c("Female", "Special Education", "Black", "American Indian", "Hispanic", "Asian/PI"),
                   expand = expansion(add = c(1, .25))) +
  theme_line +
  labs(
    x = NULL,
    y = NULL,
    color = "",
    title = "Odds of having persistent no Minnesota employment record first\nfive years after formal education",
    subtitle = "One group faces meaningfully higher odds of disappearing from Minnesota's\nemployment record entirely — and one faces meaningfully lower odds."
  ) +
  theme(legend.position = "bottom")

ggsave(filename = "Charts/Draft 4/persistent-no-mn-emp-record-odds-ratio.pdf", device = cairo_pdf, dpi = "print", width = 6, height = 4)

ggsave(filename = "Charts/Draft 4/persistent-no-mn-emp-record-odds-ratio.png", dpi = "print", width = 6, height = 4)
