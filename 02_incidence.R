# =============================================================================
# 02_incidence.R  --  critical illness incidence rates from the cleaned data
# =============================================================================
# CI incidence rate = all admitted CI claims (major and early-stage) / policy-years
# of exposure, per 1,000 policy-years. Exposure is split by calendar year so rates
# can be cut by year or by attained age.
#
# Uses the tables saved by 01_clean.R (output/_clean.rds), with the Excel headers.
# Run from the project root after 01_clean.R:   Rscript scripts/02_incidence.R
# Saves: output/incidence_by_year.csv, output/incidence_by_age.csv, output/plots/*.png
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
options(scipen = 999)

cleaned <- readRDS("output/_clean.rds")
policy  <- cleaned$policy
claims  <- cleaned$claims

AGE_BREAKS <- c(0, 30, 40, 50, 60, 100)
AGE_LABELS <- c("<30", "30-39", "40-49", "50-59", "60+")
age_band <- function(date_of_birth, date)
  cut(floor(as.numeric(date - date_of_birth) / 365.25), AGE_BREAKS, right = FALSE, labels = AGE_LABELS)

# Adds a copy of every row labelled "All products", so each table gets an all-products line
with_all_products <- function(table) bind_rows(table, mutate(table, Product = "All products"))


# =============================================================================
# 1. EXPOSURE: one row per policy per calendar year
# =============================================================================
# e.g. in force 1 Jul 2018 - 31 Mar 2020  ->  2018: 0.50, 2019: 1.00, 2020: 0.25 policy-years
exposure <- policy %>%
  filter(is.na(`Policy Exclusion`), `Exposure Days` > 0) %>%
  mutate(`Calendar Year` = mapply(seq, year(`Exposure Start`), year(`Exposure End`), SIMPLIFY = FALSE)) %>%
  unnest(`Calendar Year`) %>%
  mutate(from = pmax(`Exposure Start`, make_date(`Calendar Year`, 1, 1)),
         to   = pmin(`Exposure End`,   make_date(`Calendar Year` + 1, 1, 1)),
         `Policy Years` = as.numeric(to - from) / 365.25,
         `Age Band`     = age_band(`Date of Birth`, from + (to - from) / 2),   # age at the middle of the piece
         Product        = `Plan Code (Dread Disease)`) %>%
  filter(`Policy Years` > 0) %>%
  with_all_products()


# =============================================================================
# 2. CLAIMS: all admitted CI claims (major and early-stage), with the same groupings
# =============================================================================
counted <- claims %>%
  filter(`Claim Included`) %>%
  mutate(`Calendar Year` = year(`Date of Diagnosis`),
         `Age Band`      = age_band(`Date of Birth`, `Date of Diagnosis`),   # age at diagnosis
         Product         = `Plan Code (Dread Disease)`) %>%
  with_all_products()


# =============================================================================
# 3. RATES: claims / policy-years x 1,000, for any grouping
# =============================================================================
rates_by <- function(groups) {
  years        <- exposure %>% count(across(all_of(groups)), wt = `Policy Years`, name = "Policy Years")
  claim_counts <- counted  %>% count(across(all_of(groups)), name = "Claims")
  left_join(years, claim_counts, by = groups) %>%
    mutate(Claims           = coalesce(Claims, 0L),
           `Rate per 1,000` = round(1000 * Claims / `Policy Years`, 3),
           `Policy Years`   = round(`Policy Years`, 1))
}


# =============================================================================
# 4. HEADLINE RATE by product
# =============================================================================
cat("\n=== CI INCIDENCE BY PRODUCT ===\n")
print(as.data.frame(rates_by("Product")))


# =============================================================================
# 4. SAVED: CI incidence overall (by product), by calendar year and by age band
# =============================================================================
overall <- rates_by("Product")
by_year <- rates_by(c("Product", "Calendar Year"))
by_age  <- rates_by(c("Product", "Age Band"))
write.csv(overall, "output/incidence_overall.csv", row.names = FALSE)
write.csv(by_year, "output/incidence_by_year.csv", row.names = FALSE)
write.csv(by_age,  "output/incidence_by_age.csv",  row.names = FALSE)
cat("\n=== CI INCIDENCE OVERALL ===\n");          print(as.data.frame(overall))
cat("\n=== CI INCIDENCE BY CALENDAR YEAR ===\n"); print(as.data.frame(by_year))
cat("\n=== CI INCIDENCE BY AGE BAND ===\n");      print(as.data.frame(by_age))