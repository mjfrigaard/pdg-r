#=====================================================================#
# File name: gtrendsR.R
# This is code to create: Google trends for BioMarin App
# Authored by and feedback to: mjfrigaard
# Last updated: 2020-12-13
# MIT License
# Version: 1.0
#=====================================================================#

# description -------------------------------------------------------------
# download Google trend data in R and export at two .csv files


# packages ----------------------------------------------------------------
# install.packages("gtrendsR")
library(gtrendsR)
library(tidyverse)

# locate country geos -----
data(countries)
# countries[grepl("^UNITED KINGDOM", countries$name), ]
# countries[grepl("^HONG KONG", countries$name), ]
# countries[grepl("^JAPAN", countries$name), ]
# countries[grepl("^IRELAND", countries$name), ]
# countries[grepl("^BRAZIL", countries$name), ]

# trend data --------------------------------------------------------------
BmrnGoogle <- gtrendsR::gtrends(keyword = c("BioMarin", "BMRN"),
                                time = "today 12-m",
        # get locations for United States, Hong Kong, Great Britain, Ireland
                                geo = c("US", "HK", "GB", "IE"))
# BmrnGoogle %>% glimpse()

# export BmrnGoogle -----------------------------------------------------
# fs::dir_ls("docs/data")
write_rds(x = BmrnGoogle, file = paste0("docs/data/",
                              noquote(lubridate::today()),
                              "-BmrnGoogle.rds"))

# wrangle interest over time -------------------------------------------------
# interest over time
BmrnGoogleIOT <- BmrnGoogle$interest_over_time
BmrnGoogleIOT %>% glimpse()

# geo
BmrnGoogleIOT %>% count(geo)

# location
BmrnGoogleIOT <- BmrnGoogleIOT %>%
  mutate(Location = case_when(
    geo == "US" ~ "United States",
    geo == "HK" ~ "Hong Kong",
    geo == "GB" ~ "Great Britain",
    geo == "IE" ~ "Ireland",))
BmrnGoogleIOT %>% count(Location)

# keyword
BmrnGoogleIOT %>% count(keyword)

# plot interest over time -----------------------------------------------------
BmrnGoogleIOT %>%
  ggplot(aes(x = date, y = hits, color = Location, group = keyword)) +
  geom_line(aes(group = Location)) +
  facet_wrap(keyword ~ Location) +
  theme_minimal()

# export BmrnGoogleIOT -----------------------------------------------------
write_csv(x = BmrnGoogleIOT, file = paste0("docs/data/",
                              noquote(lubridate::today()),
                              "-BmrnGoogleIOT.csv"))

