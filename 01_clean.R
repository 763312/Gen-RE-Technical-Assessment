# =============================================================================
# Task 1
# =============================================================================
# How it works:
#   1. COLUMN_TYPES says what type every column is (id, date, category, number)
#   2. Each type has a rule:
#        id        must match its format        (ID_FORMAT,   e.g. P + 6 digits)
#        date      must be a real date after cleaning
#        category  must be in its allowed list  (ALLOWED,     e.g. Gender = M, F)
#        number    must be inside its range     (NUMBER_RANGE)
#   3. The data is cleaned using the same map
#   4. check_columns() then checks every column against its rule and prints how
#      many values in each are blank or wrong
#
# Column names are the original Excel headers. Headers with spaces or brackets
# are written in backticks (policy$`Date of Birth`) or quotes ("Date of Birth").
#
# Steps:  1 load -> 2 clean (incl. plan/product lookup and entry age) -> 3 check clean
#         -> 4 validate claims and work out exposure -> 5 write files
#
# Run from the project root:   Rscript scripts/01_clean.R
# Outputs (output/):  policy_clean.csv, claims_clean.csv
#   (to read the CSVs back with the same headers: read.csv(..., check.names = FALSE))
#
# Assumptions:
#   One person per policy
# =============================================================================


# =============================================================================
# LIBRARIES
# =============================================================================
library("readxl")
library("dplyr")
library("stringr")
library("lubridate")
options(scipen = 999)   # write numbers in full (500000), never as 5e+05


# =============================================================================
# SETTINGS
# =============================================================================
VALUATION_DATE      <- as.Date("2025-12-31")   # the extracts run to this date
EXCEL_ORIGIN        <- as.Date("1899-12-30")   # Excel counts dates as days since this
WAITING_PERIOD_DAYS <- 90                      # Provisions clause C1

EARLY_STAGE_CAUSES <- c("Angioplasty", "Carcinoma in situ (early stage)", "Early stage cancer")
MAJOR_CAUSES <- c("Blindness", "Cancer", "Coronary Artery By-pass Surgery", "Deafness",
                  "Heart Attack (Myocardial Infarction)", "Kidney Failure", "Major Organ Transplant",
                  "Multiple Sclerosis", "Paralysis", "Stroke")
NOT_A_CI_CAUSES <- c("Common Cold", "Fracture")
WAITING_PERIOD_CAUSES <- c("Cancer", "Heart Attack (Myocardial Infarction)",
                           "Coronary Artery By-pass Surgery", EARLY_STAGE_CAUSES)   # clause C1 conditions

PRODUCT_NAMES <- c(VC20 = "VitalCare Critical Illness Plan", LS30 = "LifeSecure Critical Illness Rider")


# =============================================================================
# THE EXCEL HEADERS (the script stops if a file's headers are different)
# =============================================================================
POLICY_COLUMNS <- c("Policy Number", "Plan Code (Basic Plan)", "Plan Code (Dread Disease)", "Product Name",
                    "Gender", "Date of Birth", "Policy Issue Date", "Entry Age", "Smoker Status",
                    "Marital Status", "Education Level", "Occupation Class", "Height (cm)", "Weight (kg)",
                    "Annual Income", "Sum Assured", "Annual Premium", "Premium Frequency", "Region",
                    "Policy Status", "Status Date")
CLAIMS_COLUMNS <- c("Claim Number", "Policy Number", "Plan Code (Dread Disease)", "Product Name", "Gender",
                    "Date of Birth", "Claim Cause / Condition", "Claim Type", "Date of Diagnosis",
                    "Claim Status", "Claim Amount")


# =============================================================================
# COLUMN TYPE MAP
# =============================================================================
COLUMN_TYPES <- list(
  id       = c("Policy Number", "Claim Number"),
  date     = c("Date of Birth", "Policy Issue Date", "Status Date", "Date of Diagnosis"),
  category = c("Gender", "Smoker Status", "Marital Status", "Education Level", "Occupation Class",
               "Claim Cause / Condition", "Plan Code (Basic Plan)", "Plan Code (Dread Disease)",
               "Product Name", "Premium Frequency", "Region", "Policy Status", "Claim Type", "Claim Status"),
  number   = c("Entry Age", "Height (cm)", "Weight (kg)", "Annual Income", "Sum Assured",
               "Annual Premium", "Claim Amount"))


# =============================================================================
# THE RULES FOR EACH TYPE
# =============================================================================

# ---- ID: allowed format ----
ID_PREFIX <- c("Policy Number" = "P", "Claim Number" = "C")
ID_FORMAT <- setNames(paste0("^", ID_PREFIX, "\\d{6}$"), names(ID_PREFIX))   # P + 6 digits, C + 6 digits

# ---- DATE: for two-digit years the century is added: birth dates 19xx, other dates 20xx ----
DATE_CENTURY <- c("Date of Birth" = 1900, "Policy Issue Date" = 2000, "Status Date" = 2000,
                  "Date of Diagnosis" = 2000)

# ---- CATEGORY: allowed lists ----
ALLOWED <- list(
  "Gender"                    = c("M", "F"),
  "Smoker Status"             = c("Non-smoker", "Smoker"),
  "Marital Status"            = c("Single", "Married", "Divorced", "Widowed"),
  "Education Level"           = c("Secondary", "Diploma", "Bachelor", "Master", "Doctorate"),
  "Occupation Class"          = c("1", "2", "3", "4", "5"),
  "Claim Cause / Condition"   = c(MAJOR_CAUSES, EARLY_STAGE_CAUSES, NOT_A_CI_CAUSES, "Unspecified"),
  "Plan Code (Basic Plan)"    = c("TL10", "WL05"),
  "Plan Code (Dread Disease)" = c("VC20", "LS30"),
  "Product Name"              = unname(PRODUCT_NAMES),
  "Premium Frequency"         = c("Annual", "Semi-annual", "Quarterly", "Monthly"),
  "Region"                    = c("Central", "East", "North", "South", "West"),
  "Policy Status"             = c("Inforce", "Lapsed", "CI Claim", "Died", "Matured"),
  "Claim Type"                = c("Major", "Early-stage"),
  "Claim Status"              = c("Paid", "Declined", "Pending"))

# Value to use after cleaning when a category cannot be recognised (otherwise left blank)
UNKNOWN_LABEL <- c("Smoker Status" = "Unknown", "Marital Status" = "Unknown",
                   "Education Level" = "Unknown", "Claim Status" = "Missing")

# ---- NUMBER: allowed range ----
NUMBER_RANGE <- list(
  "Entry Age"      = c(15, 80),
  "Height (cm)"    = c(100, 230),
  "Weight (kg)"    = c(30, 250),
  "Annual Income"  = c(1000, 1e7),
  "Sum Assured"    = c(50000, 500000),
  "Annual Premium" = c(50, 50000),
  "Claim Amount"   = c(1, 1e7))


# =============================================================================
# CLEANING MAP: known wrong spellings -> allowed value (wrong spelling in lower case)
# =============================================================================
SPELLINGS <- list(
  "Gender"          = c(male = "M", "1" = "M", female = "F", "2" = "F"),
  "Smoker Status"   = c("0" = "Non-smoker", n = "Non-smoker", "non smoker" = "Non-smoker", ns = "Non-smoker",
                        s = "Smoker", y = "Smoker"),
  "Marital Status"  = c(s = "Single", maried = "Married", m = "Married", divorce = "Divorced"),
  "Education Level" = c(bachelors = "Bachelor", bsc = "Bachelor", degree = "Bachelor", dip = "Diploma",
                        masters = "Master", msc = "Master", phd = "Doctorate",
                        "high school" = "Secondary", "o-level" = "Secondary"),
  "Claim Cause / Condition" = c("999" = "Unspecified", unknown = "Unspecified"))


# =============================================================================
# HELPER: compare every column with the rule for its type, and count the problems
# =============================================================================
check_columns <- function(this_table, title) {
  summary_rows <- list()
  
  for (type in names(COLUMN_TYPES)) {
    for (column in intersect(COLUMN_TYPES[[type]], names(this_table))) {
      values <- this_table[[column]]
      blank  <- is.na(values) | as.character(values) %in% ""
      
      if (type == "id") {
        wrong <- !grepl(ID_FORMAT[[column]], values)
      } else if (type == "date") {
        wrong <- rep(FALSE, length(values))                # after cleaning, a date is either real or blank
      } else if (type == "category") {
        allowed <- ALLOWED[[column]]
        if (column %in% names(UNKNOWN_LABEL)) allowed <- c(allowed, UNKNOWN_LABEL[[column]])
        wrong <- !values %in% allowed
      } else if (type == "number") {
        number <- suppressWarnings(as.numeric(values)); range <- NUMBER_RANGE[[column]]
        wrong  <- is.na(number) | number < range[1] | number > range[2]
      }
      
      summary_rows[[column]] <- data.frame(column = column, type = type,
                                           blank = sum(blank), wrong_value = sum(wrong & !blank, na.rm = TRUE))
    }
  }
  
  summary <- do.call(rbind, summary_rows)
  summary$pct_of_rows <- round(100 * (summary$blank + summary$wrong_value) / nrow(this_table), 1)
  
  cat(sprintf("\n======== %s  (%d rows) ========\n", title, nrow(this_table)))
  unmapped <- setdiff(names(this_table), unlist(COLUMN_TYPES))
  if (length(unmapped) > 0) cat("Not checked (no type in COLUMN_TYPES):", paste(unmapped, collapse = ", "), "\n")
  print(summary, row.names = FALSE, right = FALSE)
  if (all(summary$blank + summary$wrong_value == 0)) cat("Every value matches its allowed list or format.\n")
}


# =============================================================================
# HELPERS: cleaning
# =============================================================================

# ---- category: wrong case or known spelling -> allowed value ----
standardise <- function(values, column) {
  allowed <- ALLOWED[[column]]
  result  <- allowed[match(str_to_lower(values), str_to_lower(allowed))]     # e.g. "inforce" -> "Inforce"
  if (!is.null(SPELLINGS[[column]]))
    result <- coalesce(result, unname(SPELLINGS[[column]][str_to_lower(values)]))
  if (column %in% names(UNKNOWN_LABEL)) result[is.na(result)] <- UNKNOWN_LABEL[[column]]
  result                                                                     # anything else -> blank
}

# ---- date: tries each format in turn and keeps the first that works ----
read_dates <- function(values, century) {
  serial <- grepl("^\\d+$", values)
  dates  <- coalesce(
    EXCEL_ORIGIN + suppressWarnings(as.numeric(ifelse(serial, values, NA))),  # 42223
    ymd(values, quiet = TRUE),                                                # 2016-12-29
    mdy(values, quiet = TRUE),                                                # 05/31/2016, 03/04/2010
    dmy(values, quiet = TRUE))                                                # 14/06/1959, 07-Dec-80
  two_digit_year <- grepl("-\\d{2}$", values)                                 # 07-Dec-80: set the century
  year(dates[two_digit_year]) <- century + year(dates[two_digit_year]) %% 100
  dates
}

# ---- number: strip text, fix obvious typing errors, blank out the impossible ----
clean_number <- function(values, column) {
  number <- suppressWarnings(as.numeric(str_replace_all(values, "[S$,]", "")))
  if (column == "Height (cm)")
    number <- ifelse(!is.na(number) & number >= 1 & number <= 2.5, number * 100, number)   # metres -> cm
  if (column %in% c("Weight (kg)", "Sum Assured", "Claim Amount")) number <- abs(number)  # wrong sign
  range <- NUMBER_RANGE[[column]]
  ifelse(!is.na(number) & number >= range[1] & number <= range[2], number, NA)
}


# =============================================================================
# 1. LOAD
# =============================================================================
policy_raw <- read_excel("Policy Data.xlsx", col_types = "text")
claims_raw <- read_excel("Claims Data.xlsx", col_types = "text")
names(policy_raw) <- str_squish(names(policy_raw)); names(claims_raw) <- str_squish(names(claims_raw))

missing_headers <- c(setdiff(POLICY_COLUMNS, names(policy_raw)), setdiff(CLAIMS_COLUMNS, names(claims_raw)))
if (length(missing_headers) > 0) stop("Header not found in the Excel file: ", paste(missing_headers, collapse = " | "))

policy_raw[] <- lapply(policy_raw, str_squish); claims_raw[] <- lapply(claims_raw, str_squish)
dir.create("output", showWarnings = FALSE)


# =============================================================================
# 2. CLEAN
# =============================================================================
cleaned <- list(policy = distinct(policy_raw), claims = distinct(claims_raw))    # remove exact duplicate rows

for (table_name in names(cleaned)) {
  this_table <- cleaned[[table_name]]
  
  # ---- ids: restore a lost prefix (123456 -> P123456); drop a policy written twice ----
  for (column in intersect(COLUMN_TYPES$id, names(this_table))) {
    digits_only <- !is.na(this_table[[column]]) & grepl("^\\d{6}$", this_table[[column]])
    this_table[[column]][digits_only] <- paste0(ID_PREFIX[[column]], this_table[[column]][digits_only])
  }
  if (table_name == "policy") this_table <- this_table[!duplicated(this_table$`Policy Number`), ]
  
  # ---- dates ----
  for (column in intersect(COLUMN_TYPES$date, names(this_table)))
    this_table[[column]] <- read_dates(this_table[[column]], DATE_CENTURY[[column]])
  
  # ---- categories ----
  for (column in intersect(COLUMN_TYPES$category, names(this_table)))
    this_table[[column]] <- standardise(this_table[[column]], column)
  
  # ---- numbers ----
  for (column in intersect(COLUMN_TYPES$number, names(this_table)))
    this_table[[column]] <- clean_number(this_table[[column]], column)
  
  cleaned[[table_name]] <- this_table
}

policy <- cleaned$policy
claims <- cleaned$claims

# ---- Plan and product come from the policy, looked up by policy number ----
# (a claim whose policy is not in the policy file keeps its own plan code)
policy_row <- match(claims$`Policy Number`, policy$`Policy Number`)
claims$`Plan Code (Dread Disease)` <- coalesce(policy$`Plan Code (Dread Disease)`[policy_row],
                                               claims$`Plan Code (Dread Disease)`)
policy$`Product Name` <- unname(PRODUCT_NAMES[policy$`Plan Code (Dread Disease)`])
claims$`Product Name` <- unname(PRODUCT_NAMES[claims$`Plan Code (Dread Disease)`])

# ---- Entry age: issue date - date of birth, in whole years (age last birthday) ----
policy$`Entry Age` <- floor(as.numeric(policy$`Policy Issue Date` - policy$`Date of Birth`) / 365.25)


# =============================================================================
# 3. CHECK THE CLEANED DATA AGAINST THE SAME MAP
# =============================================================================
cat("\n=== CHECK: CLEANED DATA ===\n")
cat("(anything left over is genuinely missing; the rules in step 4 exclude those records)\n")
check_columns(policy, "POLICY DATA (cleaned)")
check_columns(claims, "CLAIMS DATA (cleaned)")


# =============================================================================
# 4. VALIDATE  --  which policies and claims can be used for incidence?
# =============================================================================
# ---- policies: can the dates be trusted? -----------------------------------
policy$`Policy Exclusion` <- case_when(
  is.na(policy$`Date of Birth`) | is.na(policy$`Policy Issue Date`) ~ "Missing DOB or issue date",
  policy$`Entry Age` < 15 | policy$`Entry Age` > 80                  ~ "Entry age outside 15-80",   # incl. born after issue
  policy$`Policy Issue Date` > VALUATION_DATE                        ~ "Issue date after 31-Dec-2025",
  policy$`Status Date` < policy$`Policy Issue Date`                  ~ "Status date before issue date",
  TRUE ~ NA_character_)

# ---- policies: exposure window ---------------------------------------------
# Starts at issue; ends at the status date (lapse, death, claim) or the valuation
# date, whichever is first. VitalCare also ends at age 75 (clause B5).
policy$`Exposure Start` <- policy$`Policy Issue Date`
policy$`Exposure End`   <- pmin(policy$`Status Date`, VALUATION_DATE)
is_vitalcare <- policy$`Plan Code (Dread Disease)` %in% "VC20"
policy$`Exposure End`[is_vitalcare] <- pmin(policy$`Exposure End`[is_vitalcare],
                                            policy$`Date of Birth`[is_vitalcare] %m+% years(75), na.rm = TRUE)

# ---- claims: is each one admissible under the Provisions? ------------------
claims <- claims %>%
  left_join(select(policy, `Policy Number`, `Policy Gender` = Gender, `Policy Date of Birth` = `Date of Birth`,
                   `Policy Issue Date`, `Policy Status`, `Status Date`, `Policy Exclusion`),
            by = "Policy Number") %>%
  arrange(`Policy Number`, `Date of Diagnosis`, `Claim Number`) %>%
  group_by(`Policy Number`) %>% mutate(`Claim Sequence` = row_number()) %>% ungroup()
claims$Gender          <- coalesce(claims$`Policy Gender`, claims$Gender)                    # policy is master
claims$`Date of Birth` <- coalesce(claims$`Policy Date of Birth`, claims$`Date of Birth`)

# A claim gets the FIRST reason below that applies
claims$`Claim Exclusion` <- case_when(
  !claims$`Policy Number` %in% policy$`Policy Number`             ~ "Policy not found in policy data",
  claims$`Claim Cause / Condition` %in% NOT_A_CI_CAUSES           ~ "Not a critical illness",
  is.na(claims$`Date of Diagnosis`)                               ~ "Missing diagnosis date",
  claims$`Date of Diagnosis` > VALUATION_DATE                     ~ "Diagnosis after 31-Dec-2025",
  !is.na(claims$`Policy Exclusion`)                               ~ "Policy excluded (bad dates)",
  claims$`Date of Diagnosis` < claims$`Policy Issue Date`         ~ "Diagnosis before policy issue",
  claims$`Date of Diagnosis` - claims$`Policy Issue Date` < WAITING_PERIOD_DAYS &
    claims$`Claim Cause / Condition` %in% WAITING_PERIOD_CAUSES   ~ "Within 90-day waiting period",
  claims$`Date of Diagnosis` > claims$`Status Date` &
    claims$`Policy Status` != "Inforce"                           ~ "Diagnosis after policy ended",
  claims$`Plan Code (Dread Disease)` == "LS30" &
    claims$`Claim Type` == "Early-stage"                          ~ "Early-stage not covered by LS30",
  claims$`Plan Code (Dread Disease)` == "LS30" &
    claims$`Claim Sequence` > 1                                   ~ "2nd claim on single-claim LS30 rider",
  claims$`Claim Status` == "Declined"                             ~ "Claim declined",
  TRUE ~ NA_character_)
claims$`Claim Included`        <- is.na(claims$`Claim Exclusion`) & claims$`Claim Status` != "Pending"
claims$`Pending (Sensitivity)` <- is.na(claims$`Claim Exclusion`) & claims$`Claim Status` == "Pending"

# LifeSecure is a single-claim rider: exposure stops at its first admitted claim
first_lifesecure_claim <- claims %>%
  filter(`Claim Included`, `Plan Code (Dread Disease)` == "LS30") %>%
  group_by(`Policy Number`) %>% summarise(`First LS30 Claim` = min(`Date of Diagnosis`))
policy <- left_join(policy, first_lifesecure_claim, by = "Policy Number")
policy$`Exposure End`  <- pmin(policy$`Exposure End`, policy$`First LS30 Claim`, na.rm = TRUE)
policy$`Exposure Days` <- ifelse(is.na(policy$`Policy Exclusion`),
                                 pmax(as.numeric(policy$`Exposure End` - policy$`Exposure Start`), 0), NA)

cat("\n=== CLAIMS EXCLUDED FROM THE INCIDENCE COUNT ===\n")
print(as.data.frame(sort(table(claims$`Claim Exclusion`), decreasing = TRUE), responseName = "claims") %>%
        rename(reason = Var1), row.names = FALSE)
cat(sprintf("\nPending (shown as a sensitivity only): %d\n", sum(claims$`Pending (Sensitivity)`)))
cat("Policies excluded from exposure:\n"); print(table(policy$`Policy Exclusion`))


# =============================================================================
# 5. WRITE
# =============================================================================
policy_out <- policy %>% select(all_of(POLICY_COLUMNS), `Policy Exclusion`,
                                `Exposure Start`, `Exposure End`, `Exposure Days`)
claims_out <- claims %>% select(all_of(CLAIMS_COLUMNS), `Policy Issue Date`, `Claim Exclusion`,
                                `Claim Included`, `Pending (Sensitivity)`)
write.csv(policy_out, "output/policy_clean.csv", row.names = FALSE, na = "")
write.csv(claims_out, "output/claims_clean.csv", row.names = FALSE, na = "")