##################################################################################
# Pilot project
#
# Construct income series for pilots using ACS data
# 
##################################################################################

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

library(dplyr)
library(stringr)
library(tidyr)
library(janitor)
library(haven)
library(data.table)
library(arrow)
library(foreach)
library(doParallel)
library(ggplot2)


# Read in the ACS data
ddi <- read_ipums_ddi("data/raw/usa_00032.xml")
data <- read_ipums_micro(ddi) |> clean_names()


pilots <- data |> 
  # subset to piolots
  filter(occ2010 == 9030) |> 
  # general restrictions
  filter(
    # non-institutionalized
    gq != 3,  
    # 50 states + DC
    statefip <= 56,
    # non-farm
    farm == 1,
    # employed
    empstat == 1,
    # works for salary
    classwkr == 2,
    # full-time
    uhrswork >= 35, 
    wkswork2 >= 5
  ) 
 
pilots |> 
  summary()

pilots |> 
  group_by(year, statefip) |> 
  summarise(
    ave_incwage = weighted.mean(incwage, w = perwt)
  ) |> 
  ungroup() |> 
  ggplot(aes(x = ave_incwage)) +
  geom_histogram() +
  facet_wrap(~year)

state_year_wage <- pilots |>
  group_by(year, statefip) |>
  summarise(
    ave_incwage = weighted.mean(incwage, w = perwt, na.rm = TRUE),
    .groups = "drop"
  )

year_summary <- state_year_wage |>
  group_by(year) |>
  summarise(
    n_states = n(),
    mean_state_wage   = mean(ave_incwage, na.rm = TRUE),
    sd_state_wage     = sd(ave_incwage, na.rm = TRUE),
    min_state_wage    = min(ave_incwage, na.rm = TRUE),
    p10_state_wage    = quantile(ave_incwage, 0.10, na.rm = TRUE),
    p25_state_wage    = quantile(ave_incwage, 0.25, na.rm = TRUE),
    median_state_wage = median(ave_incwage, na.rm = TRUE),
    p75_state_wage    = quantile(ave_incwage, 0.75, na.rm = TRUE),
    p90_state_wage    = quantile(ave_incwage, 0.90, na.rm = TRUE),
    max_state_wage    = max(ave_incwage, na.rm = TRUE),
    .groups = "drop"
  )



# Clear environment
rm(list = ls())

system.time({
  
  if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
  
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(janitor)
  library(haven)
  library(data.table)
  library(arrow)
  library(foreach)
  library(doParallel)
  
  
  # Parallel forloop setup --------------------------------------------------
  
  # How many cores my CPU have
  n_cores <- detectCores()
  
  # Register cluster
  cluster <- makeCluster(n_cores - 1)
  registerDoParallel(cluster)
  
  
  # Load data ---------------------------------------------------------------
  
  # Read in the ACS data
  ddi <- read_ipums_ddi("data/raw/usa_00032.xml")
  data <- read_ipums_micro(ddi) |> clean_names()
  
  
  # Only keep variables relevant for taxsim
  hh_data <- data |> 
    # subset to non-institutionalized, non-farm
    filter(gq != 3, farm == 1) |> 
    group_by(sample, serial) |> 
    filter(
      any(occ2010 == 9030)
      ) |> 
    ungroup() |> 
    select(
      # basics
      year, sample, serial, pernum, relate, sploc, momloc, poploc, perwt, hhwt, 
      # migration
      statefip, migrate1, migplac1, pwstate2, puma, migpuma1, pwpuma00,
      # demographics
      sex, age, marst, school, 
      # work
      empstat, tranwork, wkswork2, uhrswork,
      # income
      incwage, inctot, incbus00, incss, incwelfr, incinvst, incretir, incsupp, incother, incearn, 
      # housing
      movedin, ownershp, mortamt1, taxincl, proptx99, rent, mortgage, valueh, spmmort
    ) 
  
  
  
  # Helper: compute dependent ages quickly with data.table -----------
  get_dep_ages <- function(dt) {
    setDT(dt)
    dt[qual_child == TRUE,
       .(depx = .N,
         dep_age1 = if (.N >= 1) sort(p1age)[1] else NA_integer_,
         dep_age2 = if (.N >= 2) sort(p1age)[2] else NA_integer_,
         dep_age3 = if (.N >= 3) sort(p1age)[3] else NA_integer_),
       by = .(sample, serial, tax_unit_pernum)]
  }
  
  
  # Clean year by year to improve speed
  
  # Split into a named list of data.frames by year
  hh_data_list <- split(hh_data, hh_data$year)
  
  # Initialize list to hold yearly collapsed outputs
  collapsed_list <- foreach (
    dt = hh_data_list,
    .packages = c("dplyr", "data.table", "arrow", "stringr", "tidyr", "haven"), 
    .export = c("get_dep_ages")
  ) %dopar% {
    
    yr <- unique(dt$year)   # what year this chunk belongs to
    hh_year <- dt           # rename for clarity
    
    
    # Main cleaning -------------------------------------------------------------
    
    all_hh <- hh_year |> 
      mutate(
        # Qualifying child rule 
        # Be under age 19 or under 24 if a full-time student 
        qual_child = (age <= 18) | ((age > 18 & age < 24) & school == 2),
        
        # Create new variables for momloc, poploc, and sploc
        # Strip labels and replace 0 with NA
        momloc_int = na_if(as.integer(momloc), 0L),  
        poploc_int = na_if(as.integer(poploc), 0L),
        sploc_int = na_if(as.integer(sploc), 0L),
        
        # add cleaned incwage and incbus00 variables for later identifying the the primary taxpayer among married couples
        cleaned_incwage = ifelse((incwage == 999998 | incwage == 999999), 0, incwage),
        cleaned_incbus00 = ifelse((incbus00 == 999999), 0, incbus00)
      )
    
    ## Look up spousal variables -------------------------------------------------------------
    # Create lookup: spouse's variables by (sample, serial, pernum)
    spouse_lookup <- all_hh |>
      select(
        # spouse ID
        sample, serial, pernum, 
        # characteristics
        age, sex, empstat, tranwork,
        # migration
        statefip, migrate1, pwpuma00,
        # income
        inctot, incwage, incss, incsupp, incwelfr, incretir, incother, incbus00, incinvst,
        # for identifying primary earner
        cleaned_incwage, cleaned_incbus00
      ) |>
      rename(
        # spouse ID
        sploc_int = pernum, 
        
        # characteristics
        p2age = age,
        p2sex = sex,
        p2empstat = empstat,
        p2tranwork = tranwork,
        
        # migration
        p2statefip = statefip, 
        p2migrate1 = migrate1, 
        p2pwpuma00 = pwpuma00,
        
        # income
        p2inctot = inctot,
        p2incwage = incwage,
        p2incss = incss,
        p2incsupp = incsupp,
        p2incwelfr = incwelfr,
        p2incretir = incretir,
        p2incother = incother,
        p2incbus00 = incbus00,
        p2incinvst = incinvst,
        
        # for identifying primary earner
        p2cleaned_incwage = cleaned_incwage,
        p2cleaned_incbus00 = cleaned_incbus00
      )
    
    
    ## Merge and identify the primary earner between married spouses -----------
    all_hh_with_spouse_var <- all_hh |> 
      left_join(spouse_lookup, by = c("sample", "serial", "sploc_int")) |> 
      # rename variables to match these in taxsimclean.do
      rename(
        # characteristics
        p1age = age,
        p1sex = sex,
        p1empstat = empstat,
        p1tranwork = tranwork,
        
        # income
        p1inctot = inctot,
        p1incwage = incwage,
        p1incss = incss,
        p1incsupp = incsupp,
        p1incwelfr = incwelfr,
        p1incretir = incretir,
        p1incother = incother,
        p1incbus00 = incbus00,
        p1incinvst = incinvst,
        
        # for identifying primary earner
        p1cleaned_incwage = cleaned_incwage,
        p1cleaned_incbus00 = cleaned_incbus00
      ) |> 
      mutate(
        # if not married, all spousal variables should be NAs
        across(starts_with("p2"), ~ ifelse(marst != 1, NA, .))
      ) |> 
      mutate(
        # Compute the sum of wage and business income as earnings
        # for later identifying who's the primary earner
        p1earnings = p1cleaned_incwage + p1cleaned_incbus00,
        p2earnings = p2cleaned_incwage + p2cleaned_incbus00,
        
        # identify primary earner
        primary_earner_pernum = case_when(
          # my earnings higher than spouse's -> me
          (p1earnings > p2earnings) ~ pernum,
          
          # my spouse's earnings higher than mine -> spouse
          (p2earnings > p1earnings) ~ sploc_int,
          
          # tie break: heterosexual -> man
          (p1earnings == p2earnings) & (p1sex == 1) & (p2sex == 2) ~ pernum,
          (p1earnings == p2earnings) & (p1sex == 2) & (p2sex == 1) ~ sploc_int,
          
          # tie break: same-sex -> whoever is older
          (p1earnings == p2earnings) & (p1sex == p2sex) & (p1age > p2age) ~ pernum,
          (p1earnings == p2earnings) & (p1sex == p2sex) & (p2age > p1age) ~ sploc_int,
          # if same age, whoever has the smaller pernum
          (p1earnings == p2earnings) & (p1sex == p2sex) & (p2age == p1age) ~ pmin(pernum, sploc_int),
          
          # otherwise: if not married, p2 vars are all missings, so this should also be missing
          .default = NA
        )
      )
    
    
    # Create lookup for identifying the pernum of the primary earner for QCs whose parents are married
    parent_primary_lookup <- all_hh_with_spouse_var |>
      filter(marst == 1) |> 
      select(sample, serial, pernum, primary_earner_pernum)
    
    
    all_hh_with_tax_unit <- all_hh_with_spouse_var |>
      # Joining twice on both mom and dad bc
      # even though both parents have the same primary_earner_pernum,
      # for a child of a same-sex couple, it could be either momloc or poploc
      left_join(
        parent_primary_lookup,
        by = c("sample", "serial", "momloc_int" = "pernum"),
        suffix = c("", "_frommom")
      ) |>
      left_join(
        parent_primary_lookup,
        by = c("sample", "serial", "poploc_int" = "pernum"),
        suffix = c("", "_fromdad")
      ) |>
      mutate(
        # Assign tax unit pernum
        tax_unit_pernum = case_when(
          # qualifying child of married couple -> defined primary earner
          qual_child & !is.na(primary_earner_pernum_frommom) ~ primary_earner_pernum_frommom,
          qual_child & !is.na(primary_earner_pernum_fromdad) ~ primary_earner_pernum_fromdad,
          
          # qualifying child, no married parents, but only dad present
          qual_child & is.na(primary_earner_pernum_fromdad) & (!is.na(poploc_int) & is.na(momloc_int)) ~ poploc_int,
          
          # qualifying child, no married parents, but only mom present
          qual_child & is.na(primary_earner_pernum_frommom) & (!is.na(momloc_int) & is.na(poploc_int)) ~ momloc_int,
          
          # qualifying child, no married parents, but both present -> mom;
          qual_child & is.na(primary_earner_pernum_frommom) & (!is.na(momloc_int) & !is.na(poploc_int)) ~ momloc_int,
          
          # qualifying child, neither parents present, not hh head -> hh head
          qual_child & is.na(momloc_int) & is.na(poploc_int) & (relate != 1) ~ 1,
          
          # qualifying child, neither parents present, hh head -> NA
          qual_child & is.na(momloc_int) & is.na(poploc_int) & (relate == 1) ~ NA_integer_,
          
          # single adult -> own pernum
          (qual_child == FALSE) & (marst != 1) ~ pernum,
          
          # married adult -> primary earner
          # both spouses collapse to the same ID
          (qual_child == FALSE) & (marst == 1) ~ primary_earner_pernum,
          
          # default is own pernum
          .default = pernum
        )
      ) |>
      select(
        -primary_earner_pernum_frommom,
        -primary_earner_pernum_fromdad
      )
    
    ## compute number of dependents and identify three youngest dependent ages -----------------------------------------------------------------------
    
    dep_ages_all_hh <- get_dep_ages(all_hh_with_tax_unit)
    
    # merge in the dependent variables
    all_hh_with_tax_unit_kids <- all_hh_with_tax_unit |> 
      left_join(dep_ages_all_hh, by = c("sample","serial","tax_unit_pernum")) |> 
      mutate(
        # recode all 0s to 1s bc in TAXSIM infant age should be 1s,
        # 0 means no dependent in TAXSIM
        dep_age1 = if_else(dep_age1 == 0, 1, dep_age1),
        dep_age2 = if_else(dep_age2 == 0, 1, dep_age2),
        dep_age3 = if_else(dep_age3 == 0, 1, dep_age3)
        
      ) 
    
    # Save yearly outputs
    final_data_year <- all_hh_with_tax_unit_kids |> 
      zap_labels()
    
    # keep the full dataset in memory for later combination
    final_data_year
  }
  
  # stop the cluster
  stopCluster(cl = cluster)
  
  # Combine outputs across years
  final_data_all <- bind_rows(collapsed_list)
  
  
  #### Final processing (to one row per tax return)--------------------------------------------------------
  
  final_collapsed_all <- final_data_all |> 
    filter(
      # keep the primary tax filer
      (pernum == tax_unit_pernum),
      # drop the qualifying children
      (qual_child != 1)
    ) 
  
  
  ### Save as dta ------------------------------------------------------------------
  write_dta(final_collapsed_all, "data/derived_ipums/acs_for_taxsimclean_collapsed.dta")
  
})

