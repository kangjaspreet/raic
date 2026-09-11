options(scipen = 999)

# Load packages ==============================

library(dplyr) # data wrangling
library(readr) # Read csv files
library(readxl) # Read xlsx files
library(tidyr) # data pivoting

# Read data files ========================================================================

data_10 <- read_csv("data/raw/2010 series/cc-est2020int-alldata-06.csv") # 2010-2019 data
data_20 <- read_csv("data/raw/2020 series/cc-est2025-alldata-06.csv") # 2020-recent year data

# Read linkage files ======================================================================

link_raic   <- read_excel("info/censusVintageLink.xlsx", sheet = "raicLink")
link_age    <- read_excel("info/censusVintageLink.xlsx", sheet = "ageLink") |> select(-age_standard)
link_year_10 <- read_excel("info/censusVintageLink.xlsx", sheet = "yearLink10")
link_year_20 <- read_excel("info/censusVintageLink.xlsx", sheet = "yearLink20")

# Process data ===========================================================================

# Remove years not needed
# Combine 2010 and 2020 series data

data_all <- bind_rows(
  # Remove 4/1/2010 population estimates base & 4/1/2020 Census population
  data_10 |> filter(!YEAR %in% c(1, 12)) |> left_join(link_year_10, by = c("YEAR" = "census")), 
  
  # Remove 4/1/2020 Census population
  data_20 |> filter(YEAR != 1) |> left_join(link_year_20, by = c("YEAR" = "census")) 
)

# Transform data from wide to long
# Add California
# Add total sex
# Group by. Summarise population
data_proc <- data_all %>%
  select(year, 
         county_lhj = CTYNAME, 
         census_age = AGEGRP, 
         all_of(link_raic$census) # Select all columns associated with race/ethnicity
         ) |> 
  pivot_longer(-c("year", "county_lhj", "census_age"), names_to = "census", values_to = "population") |> # pivot r/e columns from wide to long
  left_join(link_raic, by = "census") |> # convert r/e columns -> r/e label, sex, raic, hispanic
  left_join(link_age, by = c("census_age" = "census")) |> # Convert age codes to age labels
  mutate(county_lhj = gsub(" County.*", "", county_lhj)) %>%  # Remove " County" string
  bind_rows(mutate(., county_lhj = "California")) # Add California
  
# Add Total Sex
data_final <- data_proc |> 
  filter(census != "TOT_POP") %>% 
  bind_rows(mutate(., sex = "Total")) %>% 
  bind_rows(filter(., census == "TOT_POP")) |> 
  summarise(population = sum(population), .by = c("year", "county_lhj", "sex", "age_group", "race_eth", "raic", "hispanic")) # group_by -> summarise

# Validate ====================================

if (F) {
  
  # Check that processed pop estimates = raw pop estimates
  
  check_raw <- data_all |> 
    select(year, CTYNAME, AGEGRP, all_of(link_raic$census)) |> 
    arrange(year, CTYNAME, AGEGRP)
  
  # Converting back to raw format (wide)
  check_proc <- data_final |> 
    left_join(link_raic, by = c("sex", "race_eth", "raic", "hispanic")) |> 
    left_join(rename(link_age, census_age = census), by = "age_group") |> 
    filter(!is.na(census), county_lhj != "California") |> 
    mutate(county_lhj = paste(county_lhj, "County")) |> 
    select(year, CTYNAME = county_lhj, AGEGRP = census_age, census, population) |> 
    pivot_wider(names_from = census, values_from = population) |> 
    select(all_of(names(check_raw))) |> 
    arrange(year, CTYNAME, AGEGRP)
  
  identical(check_raw, check_proc) # Should return TRUE
  all.equal(check_raw, check_proc) # Should return TRUE
  
  # Check frequencies: All should be 53808 or 0
  table(data_final$race_eth, data_final$raic, data_final$hispanic, useNA = "ifany")
  
  # RaceNH + RaceHisp population = RaceAnyHisp population
  check_h <- data_final %>% 
    filter(!race_eth %in% c("Total", "Multi-Race", "Latino")) %>%  
    pivot_wider(names_from = hispanic, values_from = population) %>% 
    mutate(eq = h + nh == any)
  
  all(check_h$eq) # Should return TRUE
  
  # Female + Male = Total
  check_sex <- data_final |> 
    pivot_wider(names_from = sex, values_from = population) |> 
    mutate(eq = Female + Male == Total)
  
  all(check_sex$eq) # Should return TRUE
  
  # sum(county) = California for every strata
  check_county <- data_final |> 
    filter(county_lhj != "California") |> 
    mutate(county_lhj = "California") |> 
    summarise(population = sum(population), 
              .by = c("year", "county_lhj", "sex", "age_group", "race_eth", "raic", "hispanic")) |> 
    arrange(year, county_lhj, sex, age_group, race_eth, raic, hispanic)
  
  check_ca <- data_final |> 
    filter(county_lhj == "California") |>
    arrange(year, county_lhj, sex, age_group, race_eth, raic, hispanic)
  
  identical(check_county, check_ca) # Should return TRUE
  
  # Total Population ~ 39M in recent years
  data_final %>% 
    filter(race_eth == "Total", county_lhj == "California", age_group == "Total") %>% 
    pivot_wider(names_from = sex, values_from = population)
  
  # sum(RaceNH) + Hisp = Total Population
  check_race <- data_final %>% 
    filter(raic %in% c(NA, "alone"), hispanic %in% c(NA, "nh")) %>% 
    select(-raic, -hispanic) %>% 
    pivot_wider(names_from = race_eth, values_from = population) %>% 
    mutate(eq = `AI/AN` + Asian + Black + Latino + `Multi-Race` + `NH/PI` + White == Total)
  
  all(check_race$eq) # Should return TRUE
  
  # Visual Checks
  
  library(ggplot2)
  library(scales)
  
  # Race alone, NH population
  data_final %>% 
    filter(county_lhj == "California", age_group == "Total", raic %in% c(NA, "alone"), hispanic %in% c(NA, "nh")) %>% 
    ggplot(aes(x = year, y = population, color = sex)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = 2020) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(minor_breaks = min(data_final$year):max(data_final$year), 
                       breaks = min(data_final$year):max(data_final$year),
                       labels = min(data_final$year):max(data_final$year),
    ) +
    facet_wrap(~race_eth, scales = "free") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  
  # Race Alone, NH
  # Race Alone, H
  # Race Alone
  # Race AIC, NH
  # Race AIC, H
  # Race AIC
  data_final |> 
    filter(county_lhj == "California", age_group == "Total", sex == "Total", race_eth != "Total") |> 
    mutate(raic = ifelse(race_eth == "Latino", "any", raic), 
           hispanic = ifelse(race_eth == "Latino", "any", hispanic)) |> 
    ggplot(aes(x = year, y = population, color = raic, linetype = hispanic)) +
    geom_line() +
    geom_vline(xintercept = 2020) +
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    scale_x_continuous(minor_breaks = min(data_final$year):max(data_final$year), 
                       breaks = min(data_final$year):max(data_final$year),
                       labels = min(data_final$year):max(data_final$year),
    ) +
    facet_wrap(~race_eth, scales = "free") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  
  # Age Group
  data_final %>% 
    filter(county_lhj == "California", age_group != "Total", race_eth == "Total") %>% 
    mutate(age_group = factor(age_group, levels = link_age$age_group)) %>% 
    ggplot(aes(x = year, y = population, color = sex)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = 2020) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(minor_breaks = min(data_final$year):max(data_final$year), 
                       breaks = min(data_final$year):max(data_final$year),
                       labels = min(data_final$year):max(data_final$year),
    ) +
    facet_wrap(~age_group, scales = "free") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
}

# Save data ======================================================

write_csv(data_final, file = "data/processed/county-pop-age-raic-sex.csv")
saveRDS(data_final, file = "data/processed/county-pop-age-raic-sex.RDS")
