# Shared accounting preparation for the consolidated report.
# Baseline: revenue_report_fy26.qmd. No dependency on either legacy QMD.
# Migrated from agency report, original line 16


knitr::opts_chunk$set(warning = FALSE, message = FALSE)

library(tidyverse)
library(formatR)
library(lubridate)
library(scales)
library(kableExtra)
library(ggplot2)
library(readxl)
library(janitor)
library(cmapplot)

alea_theme <- function() {
  font <-"sans"
  
  ggplot2::theme(
    legend.position = "right",
    legend.title = element_blank(),
    
    panel.background = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.minor.y = element_line(color = "grey", 
                                      linetype = "dashed"),
    # panel.grid.major.x = ggplot2::element_blank(),
    axis.ticks = element_line(color = "gray"),
    axis.ticks.x = element_blank()
  )
  
}

theme_set(alea_theme())

# Custom billion format
label_billions <- function(digits = 1) {
  function(x) {
    number_format(accuracy = 10^-digits, suffix = "B")(
      x / 1e9
    )
  }
}

scale_y_billions <- function(..., digits = 1) {
  scale_y_continuous(labels = label_billions(digits), ...)
}

move_to_last <- function(df, n) df[c(setdiff(seq_len(nrow(df)), n), n), ]


current_year <- report_year

# fiscal year, not calendar year
past_year=current_year-1

rev_temp <- read_csv(file.path(data_dir, "rev_temp.csv"), show_col_types = FALSE)

exp_temp <- read_csv(file.path(data_dir, "exp_temp.csv"), show_col_types = FALSE)

# A historical render must not incorporate later observations in graphs or CAGR.
rev_temp <- rev_temp |> filter(fy <= current_year)
exp_temp <- exp_temp |> filter(fy <= current_year)

# Preserve original identifiers before accounting recodes. Pension identification
# never changes in_ff or the original agency.
exp_temp <- exp_temp |> mutate(
  original_agency = str_pad(as.character(agency), 3, pad = "0"),
  original_object = str_pad(as.character(object), 4, pad = "0"),
  original_in_ff = in_ff,
  pension_kind = case_when(
    original_object == "1298" & fy %in% c(2010, 2011) & fund %in% c("0477", "0479", "0481") ~ 3L,
    original_object == "4431" ~ 1L,
    original_object %in% as.character(1160:1165) & !fund %in% c("0183", "0193") ~ 2L,
    fund == "0319" ~ 4L,
    TRUE ~ 0L),
  is_pension = pension_kind > 0L)


# Migrated from agency report, original line 82


update_recessions <- function(url = NULL, quietly = FALSE){

  # Use default URL if user does not override
  if (is_null(url) | missing(url)) {
    url <- "https://data.nber.org/data/cycles/business_cycle_dates.json"
  }
  
   # locally bind variable names
  start_char <- end_char <- start_date <- end_date <- ongoing <- index <- peak <- trough <- NULL

  return(
    # attempt to download and format recessions table
    tryCatch({
      recessions <- jsonlite::fromJSON(url) |>
        # drop first row trough
        dplyr::slice(-1) |>
        # convert peaks and troughs...
        dplyr::mutate(
          # ...to R dates
          start_date = as.Date(peak),
          end_date = as.Date(trough),
          # ... and clean char strings
          start_char = format(start_date, "%b %Y"),
          end_char = format(end_date, "%b %Y")) |>
        # confirm ascending and create row number
        dplyr::arrange(start_date) |>
        mutate(index = row_number()) |>
        mutate(
          # Flag unfinished recessions
          ongoing = case_when(
            is.na(end_date) & index == max(.$index) ~ T,
            TRUE ~ F),
          # set ongoing recession to arbitrary future date
          end_date = case_when(
            ongoing ~ as.Date("2200-01-01"),
            TRUE ~ end_date),
          # mark ongoing recession in char field
          end_char = case_when(
            ongoing ~ "Ongoing",
            TRUE ~ end_char)
          ) |>
        # clean up
        select(start_char, end_char, start_date, end_date, ongoing)

      if (!quietly) {message("Successfully fetched from NBER")}

      # Return recessions
      recessions
    },
    error = function(cond){
      if (!quietly) message("WARNING: Fetch or processing failed. `NULL` returned.")
      return(NULL)
    }
    )
  )
}

recessions <- update_recessions()


# Migrated from agency report, original line 181


tax_refund_long <- exp_temp |>           # fund != "0401" # removes State Trust Funds
  filter(fund != "0401" &
           (object == "9900" | object=="9910"|object=="9921"|object=="9923"|object=="9925")) |>
  # keeps these objects which represent revenue, insurance, treasurer,and financial and professional reg tax refunds
  mutate(refund = case_when(
    object == "9900" & fund == "0278" ~ "FY23_Rebates",
    fund=="0278" & sequence == "00" ~ "02", # for income tax refund
    fund=="0278" & sequence == "01" ~ "03", # tax administration and enforcement and tax operations become corporate income tax refund
        fund=="0380" ~ "03", # corporate franv tax refund

     fund == "0278" & sequence == "02" ~ "02",
    object=="9921" ~ "21",                # inheritance tax and estate tax refund appropriation
    object=="9923" ~ "09",                # motor fuel tax refunds
    obj_seq_type == "99250055" ~ "06",    # sales tax refund
    fund=="0378" & object=="9925" ~ "24", # insurance privilege tax refund
   (fund=="0001" & object=="9925") | (object=="9925" & fund == "0384" & fy == 2023) ~ "35", # all other taxes
   # fund=="0001" & object=="9925" ~ "35", # all other taxes
       fund %in% c("0946", "0912", "0671")  ~ "35", # cannabis, aviation, rental purchase tax refund
    T ~ "CHECK"))                       # if none of the items above apply to the observations, then code them as CHECK 

    
exp_temp <- left_join(exp_temp, tax_refund_long) |>
  mutate(refund = ifelse(is.na(refund),"not refund", as.character(refund)))

tax_refund <- tax_refund_long |> 
  group_by(refund, fy)|>
  summarize(refund_amount = sum(expenditure, na.rm = TRUE)) |>
  pivot_wider(names_from = refund, values_from = refund_amount, names_prefix = "ref_") |>
  mutate_all(replace_na, 0) |>
  arrange(fy)

tax_refund |>
  pivot_longer(c(ref_06:ref_35, ref_FY23_Rebates), names_to = "Refund Type", values_to = "Amount") |>
  ungroup() |>
  ggplot()+
  geom_line(aes(x=fy, y=Amount, group = `Refund Type`, color = `Refund Type`))+
    scale_y_billions() +
  labs(title = "Refund Types") +
  labs(title = "Tax refunds",
       caption = "Rev_type codes: 02=income taxes, 03=corporate income taxes, 06=sales tax, 09=motor fuel tax,
       24=insurance taxes and fees, 35 = all other tax refunds.",
       y="Dollars", x = element_blank()
       ) 


# Migrated from agency report, original line 233

tax_refund_long <- exp_temp |>           # fund != "0401" # removes State Trust Funds
  filter(fund != "0401" &
           (object == "9900" | object=="9910"|object=="9921"|object=="9923"|object=="9925")) |>
  # keeps these objects which represent revenue, insurance, treasurer,and financial and professional reg tax refunds
  mutate(refund = case_when(
    object == "9900" & fund == "0278" ~ "FY23_Rebates",
    fund=="0278" & sequence == "00" ~ "02", # for income tax refund
    fund=="0278" & sequence == "01" ~ "03", # tax administration and enforcement and tax operations become corporate income tax refund
        fund=="0380" ~ "03", # corporate franv tax refund

     fund == "0278" & sequence == "02" ~ "02",
    object=="9921" ~ "21",                # inheritance tax and estate tax refund appropriation
    object=="9923" ~ "09",                # motor fuel tax refunds
    obj_seq_type == "99250055" ~ "06",    # sales tax refund
    fund=="0378" & object=="9925" ~ "24", # insurance privilege tax refund
   (fund=="0001" & object=="9925") | (object=="9925" & fund == "0384" & fy == 2023) ~ "35", # all other taxes
   # fund=="0001" & object=="9925" ~ "35", # all other taxes
       fund %in% c("0946", "0912", "0671")  ~ "35", # cannabis, aviation, rental purchase tax refund
    T ~ "CHECK"))                       # if none of the items above apply to the observations, then code them as CHECK 

    
exp_temp <- left_join(exp_temp, tax_refund_long) |>
  mutate(refund = ifelse(is.na(refund),"not refund", as.character(refund)))

tax_refund <- tax_refund_long |> 
  group_by(refund, fy)|>
  summarize(refund_amount = sum(expenditure, na.rm = TRUE)) |>
  pivot_wider(names_from = refund, values_from = refund_amount, names_prefix = "ref_") |>
  mutate_all(replace_na, 0) |>
  arrange(fy)
 
tax_refund |>
  pivot_longer(c(ref_06:ref_35), names_to = "Refund Type", values_to = "Amount") |>
  ggplot()+
  theme_classic()+
  geom_line(aes(x=fy,y=Amount, group = `Refund Type`, color = `Refund Type`))+
  labs(title = "Refund Types") +
  labs(title = "Tax refunds without FY23 Abatements",
       caption = "Rev_type codes: 02=income taxes, 03=corporate income taxes, 06=sales tax, 09=motor fuel tax,
       24=insurance taxes and fees, 35 = all other tax refunds.",
       ) +
  scale_y_billions()


# Migrated from agency report, original line 284

tax_refund_long |> 
  summarize(expenditure = sum(expenditure, na.rm=TRUE), .by = c(fy, in_ff) ) |>
  ggplot() + 
  geom_line(aes(x=fy, y = expenditure, group = factor(in_ff), color = factor(in_ff)))+
  labs(title = "Excluded Refund Expenditures", y = "Dollars", x = element_blank())


# Migrated from agency report, original line 297

tax_refund_long_rev <- rev_temp |>          
  mutate(refund = case_when(
    fund == "0121" ~ "35", # Estate tax refund
    fund=="0278" ~ "02", # for income tax refunds (individual and corporate)
    fund=="0380" ~ "03", # corporate franchise tax refund
    fund=="0378" ~ "24", # insurance privilege tax refund
    fund %in% c("0946", "0912", "0671")  ~ "35", # cannabis, aviation, rental purchase tax refund
    T ~ "CHECK")) |>                      # if none of the items above apply to the observations, then code them as CHECK 
  filter(refund != "CHECK")

tax_refund_rev <- tax_refund_long_rev |> 
  group_by(refund, fy)|>
  summarize(allocated_for_refunds = sum(receipts, na.rm = TRUE)/1000000) |>
  pivot_wider(names_from = refund, values_from = allocated_for_refunds, names_prefix = "ref_") |>
  mutate_all(replace_na, 0) |>
  arrange(fy)
 
tax_refund_rev|>
  pivot_longer(c(ref_02:ref_35), names_to = "Refund Type", values_to = "Amount") |>
  ggplot()+
  geom_line(aes(x=fy,y=Amount, group = `Refund Type`, color = `Refund Type`))+
  labs(title = "Refund Types") +
  labs(title = "Revenue Allocated to Tax Refund Funds ",
       caption = "Rev_type codes: 02=income taxes, 03=corporate income taxes, 06=sales tax, 09=motor fuel tax,
       24=insurance taxes and fees, 35 = all other tax refunds.",
       y = "Millions of $", x = element_blank()) + 
    scale_x_continuous(expand = c(0,0), limits = c(1998, current_year+.5), breaks = c(1998, 2005, 2010, 2015, 2020, current_year))



# Migrated from agency report, original line 337

# manually adds the abatements as expenditure item and keeps on expenditure side.
# otherwise ignored since it is in fund 0278, which is coded as in_ff=0
# all other income tax refunds are excluded from fiscal gap calculations

exp_temp <- exp_temp |>
  mutate(in_ff = ifelse(object == 9900 & fund == "0278", 1, in_ff))



# Migrated from agency report, original line 384

pension_agencies <- c("589", "593", "594", "693", "275", "131" )

pension_objects <- c(4431, 1160:1165)

exp_temp |> 
  filter(
     fy == 2024 &
       object %in% pension_objects) |>
  mutate(group = ifelse(!agency %in% pension_agencies, "901", agency),
         group_label = case_when(
           group == "593" | group == "594" ~ "TRS",
           group == "589" ~ "SERS",
           group == "693" ~ "SURS",
           group == "275" ~ "JRS",
           group == "131" ~ "GARS",
          group == "901" ~ "Other Pension Costs",

           
           TRUE ~ "CHECK ME")) |>
 #   group = ifelse(object %in% 1160:1165, "901", as.character(agency))) |> 
  group_by(group, group_label) |>
  summarize(Expenditures = sum(expenditure, na.rm=TRUE)/1000000000)
 

exp_temp |> 
  filter(
     fy == 2025 &
       object %in% pension_objects) |>
  mutate(group = ifelse(!agency %in% pension_agencies, "901", agency),
         group_label = case_when(
           group == "901" ~ "Other Pensions",
           group == "593"  ~ "K-12 Education",
           group == "594" ~ "Chi. TPF ",
           group == "589" ~ "SERS",
           group == "693" ~ "Higher Education",
           group == "275" ~ "Judicial",
           group == "131" ~ "Legislative",
           
           TRUE ~ "CHECK ME")) |>
 #   group = ifelse(object %in% 1160:1165, "901", as.character(agency))) |> 
  group_by(group, group_label) |>
  summarize(Expenditures = sum(expenditure, na.rm=TRUE)/1000000000)

exp_temp |> 
  filter(
     fy == 2024 &
       object %in% pension_objects) |>
  mutate(group = ifelse(!agency %in% pension_agencies, "901", agency),
         group_label = case_when(
           group == "901" ~ "Other Department Pensions",
           group == "593" ~ "K-12 Education",
           group == "594" ~ "Chi. TPF ",
           group == "589" ~ "Other Department Pensions",
           group == "693" ~ "Higher Education",
           group == "275" ~ "Judicial",
           group == "131" ~ "Legislative",
           
           TRUE ~ "CHECK ME")) |>
 #   group = ifelse(object %in% 1160:1165, "901", as.character(agency))) |> 
  group_by(group, group_label) |>
  summarize(Expenditures = sum(expenditure, na.rm=TRUE)/1000000000)

# in billions 
exp_temp |> 
  filter( fy == 2025 &
            agency!="494" &
            object %in% pension_objects ) |>
  summarize(`Pension Expenditures` = sum(expenditure/1000000000, na.rm=TRUE))


# Migrated from agency report, original line 460
pension_totals <- exp_temp |> arrange(fund) |> mutate(pension = pension_kind)

table(pension_totals$pension) 



pension_totals %>% 
  filter(pension != 0) %>%
  mutate(pension = as.factor(pension))%>%
  group_by(fy, pension) %>% 
  summarize(expenditure = sum(expenditure/1000000000, na.rm = TRUE)) %>%
  ggplot(aes(x=fy, y = expenditure, group=pension)) + 
  theme_classic()+
  geom_col(aes(fill = pension)) + 

  labs (title = "Pension expenditures", 
  caption = "1 = State contributions INTO pension funds.  
  2 = Employer Contributions.   
  3 = Purchase of Investments anomaly in 2010 and 2011. 
  4 = pension stabilization fund")+
    theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::dollar, name = "$ Billions")



# Migrated from agency report, original line 510


exp_temp <-  exp_temp |> 
  arrange(fund) |>
  mutate(pension_accounting_adjustment = case_when( 
    
    ## Commented out line below:
    # (object=="4431") ~ 1, # 4431 = easy to find pension_accounting_adjustment payments INTO fund
    

    
    (object=="1298" &  # Purchase of Investments, Normally excluded
       (fy==2010 | fy==2011) & 
       (fund=="0477" | fund=="0479" | fund=="0481")) ~ 3, #judges retirement OUT of fund
    # state borrowed money from pension_accounting_adjustment funds to pay for core services during 2010 and 2011. 
    # used to fill budget gap and push problems to the future. 
    
    fund == "0319" ~ 4, # pension_accounting_adjustment stabilization fund
    TRUE ~ 0) )


# Migrated from agency report, original line 535


# special accounting of pension_accounting_adjustment obligation bond (POB)-funded contributions to JRS, SERS, GARS, TRS 

exp_temp <- exp_temp |> 
  # change object for 2010 and 2011, retirement expenditures were bond proceeds and would have been excluded
  mutate(object = ifelse((pension_accounting_adjustment > 0 & in_ff == "0"), "4431", object)) |> 
  # changes weird teacher & judge retirement system  pensions object to normal pension_accounting_adjustment object 4431
  mutate(pension_accounting_adjustment =  ifelse(pension_accounting_adjustment > 0 & in_ff == "0", 6, pension_accounting_adjustment)) |> # coded as 6 if it was supposed to be excluded. 
  mutate(in_ff = ifelse(pension_accounting_adjustment > 0, "1", in_ff))




# Migrated from agency report, original line 570

exp_temp |>
  filter(fy==2024) |>
  filter((appr_org=="01" | appr_org == "65" | appr_org=="88") & (object=="4900" | object=="4400") ) |> 
  group_by(agency, agency_name) |> # separates CHIP from health and human services and saves it as Medicaid
  summarize(expenditure = sum(expenditure))



# Migrated from agency report, original line 586

transfers_drop <- exp_temp |> filter(
  agency == "799" | # statutory transfers
           object == "1993" |  # interfund cash transfers
           object == "1298") # purchase of investments
transfers_drop # items being dropped, 

# always check to make sure you aren't accidentally dropping something of interest.

exp_temp <- anti_join(exp_temp, transfers_drop)



# Migrated from agency report, original line 620

exp_temp |> filter(org_name == "BUREAU OF BENEFITS") |> 
  group_by(fy, agency) |>
  summarize(expenditure = sum(expenditure) ) |>
  pivot_wider(names_from = "fy", values_from = "expenditure")

exp_temp |> 
  filter(org_name == "BUREAU OF BENEFITS") |> 
  group_by(fy, agency) |>
  summarize(expenditure = sum(expenditure) ) |>
  ggplot() + 
  geom_line(aes(x=fy, y=expenditure, group= agency, color = agency)) + 
  scale_y_continuous(labels = scales::dollar) +
  labs(title="Bureau of Benefits Expenditures")

exp_temp |> 
  filter(org_name == "BUREAU OF BENEFITS") |> 
  group_by(fy, object) |>
  summarize(expenditure = sum(expenditure) ) |>
  ggplot() + 
  geom_line(aes(x=fy, y=expenditure, group= object, color = object)) + 
  scale_y_continuous(labels = scales::dollar) +
  labs(title="Bureau of Benefits Expenditures")


# Migrated from agency report, original line 649


#if observation is a group insurance contribution, then the expenditure amount is set to $0 (essentially dropped from analysis)

# pretend eehc is named group_insurance_contribution or something like that
# eehc coded as zero implies that it is group insurance
# if eehc=0, then expenditures are coded as zero for group insurance to avoid double counting costs


exp_temp <- exp_temp |> 
  mutate(eehc = ifelse(
    # group insurance contributions for 1998-2005 and 2013-present
   fund == "0001" & (object == "1180" | object =="1900") & agency == "416" & appr_org=="20", 0, 1) )|> 
  
  mutate(eehc = ifelse(
    # group insurance contributions for 2006-2012
    fund == "0001" & object == "1180" & agency == "478" 
    & appr_org=="80", 0, eehc) )|>
    
   # group insurance contributions from road fund
  # coded with 1900 for some reason??
    mutate(eehc = ifelse(
      fund == "0011" & object == "1900" & 
        agency == "416" & appr_org=="20", 0, eehc) ) |>
  
  mutate(expenditure = ifelse(eehc=="0", 0, expenditure)) |>

  mutate(agency = case_when(
    ## turns specific items into State Employee Healthcare (agency=904)
      fund=="0907" & (agency=="416" & appr_org=="20") ~ "904",   # central management Bureau of benefits using health insurance reserve
      fund=="0907" & (agency=="478" & appr_org=="80") ~ "904",   # agency = 478: healthcare & family services using health insurance reserve - stopped using this in 2012
       TRUE ~ as.character(agency))) |>
  
  mutate(agency_name = ifelse(
    agency == "904", "STATE EMPLOYEE HEALTHCARE", as.character(agency_name)),
         in_ff = ifelse(agency == "904", 1, in_ff),
         group = ifelse(agency == "904", "904", as.character(agency)))  
# creates group variable

# Default group = agency number

healthcare_costs <- exp_temp |> filter(group == "904")


# Migrated from agency report, original line 698


exp_temp <- exp_temp |> mutate(
  agency = case_when(fund=="0515" & object=="4470" & type=="08" ~ "971", # income tax to local governments
                     fund=="0515" & object=="4491" & type=="08" & sequence=="00" ~ "971", # object is shared revenue payments
                     fund=="0802" & object=="4491" ~ "972", #pprt transfer
                     fund=="0515" & object=="4491" & type=="08" & sequence=="01" ~ "976", #gst to local
                     fund=="0627" & object=="4472"~ "976" , # public transportation fund but no observations exist
                     fund=="0648" & object=="4472" ~ "976", # downstate public transportation, but doesn't exist
                     fund=="0515" & object=="4470" & type=="00" ~ "976", # object 4470 is grants to local governments
                    object=="4491" & (fund=="0188"|fund=="0189") ~ "976",
                     fund=="0187" & object=="4470" ~ "976",
                     fund=="0186" & object=="4470" ~ "976",
                    object=="4491" & (fund=="0413"|fund=="0414"|fund=="0415")  ~ "975", #mft to local
                  fund == "0952"~ "975", # Added Sept 29 2022 AWM. Transportation Renewal MFT
                    TRUE ~ as.character(agency)),
  
  agency_name = case_when(agency == "971"~ "INCOME TAX 1/10 TO LOCAL",
                          agency == "972" ~ "PPRT TRANSFER TO LOCAL",
                          agency == "975" ~ "MFT TO LOCAL",
                          agency == "976" ~ "GST TO LOCAL",
                          TRUE~as.character(agency_name)),
  group = ifelse(agency>"970" & agency < "977", as.character(agency), as.character(group)))


# Migrated from agency report, original line 729

transfers_long <- exp_temp |> 
  filter((group == "971" |group == "972" | group == "975" | group == "976")) 
#  fund == "0325")

transfers_long |> 
  group_by(agency_name, group, fy) |> 
  summarize(expenditure = sum(expenditure, na.rm=TRUE) )|> 
  ggplot() + 
  geom_line(aes(x=fy, y = expenditure, color=agency_name)) + 
  alea_theme() + 
  scale_x_continuous(expand = c(0,0), limits = c(1998, current_year+.5), breaks = c(1998, 2005, 2010, 2015, 2020, current_year)) +
  
  labs(title = "Transfers to Local Governments", caption = "Data Source: Illinois Office of the Comptroller")


# Migrated from agency report, original line 752
transfers_long <- exp_temp |> 
  filter(group == "971" |group == "972" | group == "975" | group == "976")


transfers <- transfers_long |>
  group_by(fy, group ) |>
  summarize(sum_expenditure = sum(expenditure)/1000000) |>
  pivot_wider(names_from = "group", values_from = "sum_expenditure", names_prefix = "exp_" )

exp_temp <- anti_join(exp_temp, transfers_long)


dropped_inff_0 <- exp_temp |> filter(in_ff == 0)

exp_temp <- exp_temp |> filter(in_ff == 1) # drops in_ff = 0 funds AFTER dealing with net-revenue above


# Migrated from agency report, original line 775

debt_drop <- exp_temp |> 
  filter(object == "8841" |  object == "8811")  
# escrow  OR  principle

#debt_drop |> group_by(fy) |> summarize(sum = sum(expenditure)) |> arrange(-fy)


debt_keep <- exp_temp |> 
  filter(fund != "0455" & (object == "8813" | object == "8800" )) 
# examine the debt costs we want to include

#debt_keep |> group_by(fy) |> summarize(sum = sum(expenditure)) |> arrange(-fy) 


exp_temp <- anti_join(exp_temp, debt_drop) 
exp_temp <- anti_join(exp_temp, debt_keep)

debt_keep <- debt_keep |>
  mutate(
    agency = ifelse(fund != "0455" & (object == "8813" | object == "8800"), "903", as.character(agency)),
    group = ifelse(fund != "0455" & (object == "8813" | object == "8800"), "903", as.character(group)),
    in_ff = ifelse(group == "903", 1, as.character(in_ff)))

debt_keep_yearly <- debt_keep |> 
  group_by(fy, group) |> 
  summarize(debt_cost = sum(expenditure,na.rm=TRUE)/1000000) |> 
  select(-group)


# Migrated from agency report, original line 808


tollway_exp <- exp_temp |> filter(fund == "0455") |> group_by(fy) |> summarize(expenditure = sum(expenditure))
                                                                                                    #tollway_exp |> ggplot() + geom_line(aes(x=fy, y=expenditure)) + labs(title = "Fund 0455 from Expenditure: All Tollway Expenditures", caption = "Data from IOC Expenditure Files. Fund 0455 is the IL State Tollway")


# all tollway revenues, not just bond proceeds
alltollway<-rev_temp |> filter(fund == "0455" & source != "0571") |> group_by(fy) |> summarize(sum = sum(receipts, na.rm = TRUE))


# tollway bond proceeds
tollway_bondproc <- rev_temp |> filter(fund == "0455" & source == "0571" ) |> group_by(fy) |> summarize(sum = sum(receipts, na.rm = TRUE))

#alltollway |>  ggplot() + geom_line(aes(x=fy, y=sum)) + labs(title = "Fund 0455 - All Tollway Revenue", caption = "Data from IOC Revenue Files. Fund 0455 is the IL State Tollway Revenue") 

#tollway_bondproc |> ggplot() + geom_line(aes(x=fy, y=sum)) + labs(title = "Fund 0455 - Tollway Revenue: Tollway Bond Proceeds", caption = "Data from IOC Revenue Files. Fund 0455 is the IL State Tollway Revenue")
  
#ggplot() + geom_line(data=tollway_bondproc, aes(x=fy, y=sum)) + labs(title = "Fund 0455 - Tollway Revenue: Tollway Bond Proceeds", caption = "Data from IOC Revenue Files. Fund 0455 is the IL State Tollway Revenue")

#tollwaydebt |> ggplot() + geom_line(aes(x=fy, y=sum)) + labs(title = "Tollway Debt Service", caption = "Debt service includes principal and interest for the Illinois Tollway. Object = 8800 and fund = 0455")


#tollway debt principal and interest
tollwaydebt <- exp_temp |>filter(object == "8800" & fund == "0455") |> group_by(fy) |> summarize(sum=sum(expenditure)) 

# Tollway agency expenditures = SAME as filtering by fund == 0455
#tollway<-exp_temp |> filter(agency == "557")
#exp_temp |> filter(agency == "557") |> group_by(fy) |> summarize(sum = sum(expenditure)) |> arrange(-fy)

# contributions and benefits paid comparison
ggplot()+
  scale_x_continuous(expand = c(0,0), limits = c(1998, current_year+.5), breaks = c(1998, 2005, 2010, 2015, 2020, current_year)) +
    geom_line(data=tollway_bondproc, aes(x=fy, y=sum, color='Bond Proceeds')) +
  geom_line(data= tollwaydebt, aes(x=fy, y = sum, color = 'Debt Service'))+ 
  geom_line(data= tollway_exp, aes(x=fy, y = expenditure, color = 'Tollway Expenditures'))+ 
  geom_line(data= alltollway, aes(x=fy, y = sum, color = "Tollway Revenue"))+ 
  scale_color_manual(values = c(
    'Bond Proceeds' = 'darkblue',
    'Debt Service' = 'red',
    'Tollway Expenditures' = 'orange',
    'Tollway Revenue' = 'light green')) +
  labs(title="Tollway bond procreeds, debt service, revenue, and expenditures.", 
       caption = "Tollway revenue + bond proceeds should be roughly equal to tollway expenditures + debt service.", 
       y = "Dollars")

# Migrated from agency report, original line 865


exp_temp <- exp_temp |>
  #mutate(agency = as.numeric(agency) ) |>
  # arrange(agency)|>
  mutate(
    group = case_when(
      agency>"100"& agency<"200" ~ "910", # legislative
      
      agency == "528"  | (agency>"200" & agency<"300") ~ "920", # judicial 
      
      ######################################################
      # Not used if we are not separating pension_accounting_adjustment costs!!
      # pension_accounting_adjustment > 0  ~ "901", # pensions
      
      ## New CODE: April 23rd, 2025:
      agency == "593" ~ "959", #  TRS becomes part of K-12 costs
      agency == "594" ~ "959",   # TRS
      agency == "589" ~ "589", # SERS becomes part of "Other Agencies"
      agency == "693" ~ "960", # SURS becomes part of group 960
      agency == "275" ~ "920",  # JRS becomes part of group 920
      agency == "131" ~  "910", # GARS becomes part of Group 910
      ######################################################
      
      (agency>"309" & agency<"400") ~ "930",    # elected officers: Governor, lt gov, attorney general, sec. of state, comptroller, treasurer
      
      agency == "586" ~ "959", # create new K-12 group

      agency=="402" | agency=="418" | agency=="478" | agency=="444" | agency=="482" ~ as.character(agency), # aging, CFS, HFS, human services, public health
      T ~ as.character(group))
    ) |>      

  
  mutate(group = case_when(
    agency=="478" & (appr_org=="01" | appr_org == "65" | appr_org=="88") & (object=="4900" | object=="4400") ~ "945", # separates CHIP from health and human services and saves it as Medicaid
    
    agency == "586" & fund == "0355" ~ "945",  # 586 (Board of Edu) has special education which is part of medicaid
    
    # OLD CODE: agency == "586" & appr_org == "18" ~ "945", # Spec. Edu Medicaid Matching
    
    agency=="425" | agency=="466" | agency=="546" | agency=="569" | agency=="578" | agency=="583" | agency=="591" | agency=="592" | agency=="493" | agency=="588" ~ "941", # public safety & Corrections
    
    agency=="420" | agency=="494" |  agency=="406" | agency=="557" ~ as.character(agency), # econ devt & infra, tollway
    
    agency=="511" | agency=="554" | agency=="574" | agency=="598" ~ "946",  # Capital improvement
    
    agency=="422" | agency=="532" ~ as.character(agency), # environment & nat. resources
    
    agency=="440" | agency=="446" | agency=="524" | agency=="563"  ~ "944", # business regulation
    
    agency=="492" ~ "492", # revenue
    
    agency == "416" ~ "416", # central management services
    agency=="448" & fy > 2016 ~ "416", #add DoIT to central management 
    
    T ~ as.character(group))) |>
  
  
  mutate(group = case_when(
    # agency=="684" | agency=="691"  ~ as.character(agency), # moved under higher education in next line. 11/28/2022 AWM
    
    agency=="692" | agency == "693" | agency=="695" | agency == "684" |agency == "691" | (agency>"599" & agency<"677") ~ "960", # higher education
    
    agency=="427"  ~ as.character(agency), # employment security
    
############################ 
# Leaving these agencies as their own agency number for now. Had been coded to "Other departments" Group 948
# - GOMB (507)  
# - Human Rights (442)  
# - Illinois Power Agency (445)  
# - Labor (452)   
# - State Lottery (458)   
# - Veteran's Affairs (497) 

       agency=="507" | agency=="442" | agency=="445" | agency=="452" |agency=="458" | agency=="497" ~ as.character(agency), # Were included within "other departments"
    
#    agency=="507"|  agency=="442" | agency=="445" | agency=="452" |agency=="458" | agency=="497" ~ "948", # other departments

###########################################
    

# other boards & Commissions
    agency=="503" | agency=="509" | agency=="510" | agency=="565" |agency=="517" | agency=="525" | agency=="526" | agency=="529" | agency=="537" | agency=="541" | agency=="542" | agency=="548" |  agency=="555" | agency=="558" | agency=="559" | agency=="562" | agency=="564" | agency=="568" | agency=="579" | agency=="580" | agency=="587" | agency=="590" | agency=="527" | agency=="585" | agency=="567" | agency=="571" | agency=="575" | agency=="540" | agency=="576" | agency=="564" | agency=="534" | agency=="520" | agency=="506" | agency == "533" ~ "949", 
    
# Other Departments
  #   Before pensions were included back with the original agency that spent the money, remaining non-pension_accounting_adjustment expenditures from agencies that deal with pensions were included with Other Departments 
  #   agency=="131" |
  #   agency=="275" | #JRS
  #   agency=="589" | #SERS
  #   agency=="593"|  # TRS
  #   agency=="594"| # Also TRS
  #   agency=="693"   #SURS
  #  ~ "948",
    
    T ~ as.character(group))) |>
  
  mutate(group_name = 
           case_when(
             group == "416" ~ "Central Management",
             group == "442" ~ "Human Rights",
             group == "445" ~ "IL Power Agency",
             group == "452" ~ "Labor",
             group == "458" ~ "State Lottery",
             group == "589" ~ "SERS",
             group == "478" ~ "Healthcare and Family Services",
             group == "482" ~ "Public Health",
             group == "901" ~ "State Pension Contributions", ## Split up into GARS, SERS, etc. now
             group == "903" ~ "Debt Service",
             group == "910" ~ "Legistlative"  ,
             group == "920" ~ "Judicial" ,
             group == "930" ~ "Elected Officers" , 
             group == "941" ~ "Public Safet" ,
             group == "942" ~ "Econ Development & Infrastructure" ,
             group == "943" ~ "Central Services",
             group == "944" ~ "Business & Professional Regulation" ,
             group == "945" ~ "Medicaid" ,
             group == "946" ~ "Capital Improvement" , 
             group == "948" ~ "Other Departments" ,
             group == "949" ~ "Other Boards & Commissions" ,
             group == "959" ~ "K-12 Education" ,
             group == "960" ~ "University Education" ,
             group == agency ~ as.character(agency_name),
             TRUE ~ "Check name"),
         year = fy)


exp_temp <- exp_temp |>
    mutate(fund_cat_name =
           case_when(
             fund_cat_name == "General Fund" ~ "General Funds",
             fund_cat_name == "REVOLVING FUNDS" ~ "Revolving Funds",
             T ~ fund_cat_name
           ),
         federal_funded = case_when(
           fund_cat_name == "Federal Trust Funds" ~ "Federal Funds",
           group_name == "MEDICAID" & fund_cat_name == "General Funds" ~ "Federal Funds",
          T ~ "State Funds"
           
         ))


exp_temp |> filter(group_name == "Check name")



# Migrated from agency report, original line 1019

# recodes old agency numbers to consistent agency number
rev_temp <- rev_temp |> 
  mutate(agency = case_when(
    (agency=="438"| agency=="475" |agency == "505") ~ "440",
    # financial institution &  professional regulation &
     # banks and real estate  --> coded as  financial and professional reg
    agency == "473" ~ "588", # nuclear safety moved into IEMA
    (agency =="531" | agency =="577") ~ "532", # coded as EPA
    (agency =="556" | agency == "538") ~ "406", # coded as agriculture
    agency == "560" ~ "592", # IL finance authority (fire trucks and agriculture stuff)to state fire marshal
    agency == "570" & fund == "0011" ~ "494",   # city of Chicago road fund to transportation
    TRUE ~ (as.character(agency)))) |>
  mutate(fund_cat_name =
           case_when(
             fund_cat_name == "General Fund" ~ "General Funds",
             fund_cat_name == "REVOLVING FUNDS" ~ "Revolving Funds",
             T ~ fund_cat_name
           ))

# Migrated from agency report, original line 1044

#rev_temp <- rev_temp |> filter(in_ff==1)

rev_temp <- rev_temp |> 
  mutate(
    rev_type = ifelse(rev_type=="57" & agency=="478" & (source=="0618"|source=="2364"|source=="0660"|source=="1552"| source=="2306"| source=="2076"|source=="0676"|source=="0692"), "58", rev_type),
    rev_type_name = ifelse(rev_type=="58", "Federal Medicaid Reimbursements", rev_type_name),
    rev_type = ifelse(rev_type=="57" & agency=="494", "59", rev_type),
    rev_type_name = ifelse(rev_type=="59", "Federal Transportation", rev_type_name),
    rev_type_name = ifelse(rev_type=="57", "Federal - Other", rev_type_name),
    rev_type = ifelse(rev_type=="6", "06", rev_type),
    rev_type = ifelse(rev_type=="9", "09", rev_type)) 

rev_temp |> 
  filter(rev_type == "58" | rev_type == "59" | rev_type == "57") |> 
  group_by(fy, rev_type, rev_type_name) |> 
  summarise(receipts = sum(receipts, na.rm = TRUE)/1000000) |> 
  ggplot() +
  geom_line(aes(x=fy, y=receipts,color=rev_type_name)) +
  scale_y_continuous(labels = comma)+
  labs(title = "Federal to State Transfers", 
       y = "Millions of Dollars", x = "") + 
  theme(legend.position = "bottom", legend.title = element_blank()  )


# Migrated from agency report, original line 1072

rev_temp <- rev_temp |> mutate(covid_dollars = ifelse(source_name_AWM == "FEDERAL STIMULUS PACKAGE",1,0))

rev_temp |> filter(source_name_AWM == "FEDERAL STIMULUS PACKAGE") |>
  group_by(fy) |> summarize(Received = sum(receipts))
  

# Migrated from agency report, original line 1087

medicaid_cost_total <- exp_temp |> 
  filter(agency=="478" & (appr_org=="01" | appr_org == "65" | appr_org=="88") & (object=="4900" | object=="4400")) |> 
  group_by(fy) |> 
  summarize(sum=sum(expenditure, na.rm=TRUE))

medicaid_cost <- exp_temp |> 
  filter(#agency=="478" & 
          # (appr_org=="01" | appr_org == "65" | appr_org=="88") & 
           (object=="4900" | object=="4400")) |> 
  group_by(fy, agency) |> 
  summarize(sum=sum(expenditure, na.rm=TRUE))

ggplot()+
  geom_line(data=medicaid_cost_total, aes(x=fy, y = sum, color = "Expenditures"), lwd = 1) +
    geom_line(data=medicaid_cost, aes(x=fy, y = sum, color = agency)) + 

  scale_x_continuous(n.breaks = 6) +
  labs(title = "Medicaid expenditures", 
       caption = "Medicaid expenditures include funds provided to medical providers.", 
       color = element_blank()
       )

# Migrated from agency report, original line 1130

#collect optional insurance premiums to fund 0907 for use in eehc expenditure  
rev_temp <- rev_temp |> 
  mutate(
    employee_premiums = ifelse(fund=="0907" & (source=="0120"| source=="0121"| (source>"0345" & source<"0357")|(source>"2199" & source<"2209")), 1, 0),
    
    # adds more rev_type codes
    rev_type = case_when(
      fund =="0427" ~ "12", # pub utility tax
      fund == "0742" | fund == "0473" ~ "24", # insurance and fees
      fund == "0976" ~ "36",# receipts from rev producing
      fund == "0392" |fund == "0723" ~ "39", # licenses and fees
      fund == "0656" ~ "78", #all other rev sources
      TRUE ~ as.character(rev_type)))
# if not mentioned, then rev_type as it was

# Migrated from agency report, original line 1150


# # optional insurance premiums = employee insurance premiums

# emp_premium <- rev_temp |>
#   group_by(fy, employee_premiums) |>
#   summarize(employee_premiums_sum = sum(receipts)/1000000) |>
#   filter(employee_premiums == 1) |>
#   rename(year = fy) |> 
#   select(-employee_premiums)

emp_premium_long <- rev_temp |>  filter(employee_premiums == 1)
emp_premium_long

# drops employee premiums from revenue
# rev_temp <- rev_temp |> filter(employee_premiums != 1)
# should be dropped in next step since rev_type = 51



# Migrated from agency report, original line 1183

rev_temp <- rev_temp |> 
  filter(in_ff == 1) |> 
  mutate(local = ifelse(is.na(local), 0, local)) |> # drops all revenue observations that were coded as "local == 1"
  filter(local != 1)

# 1175 doesnt exist?
in_from_out <- c("0847", "0867", "1175", "1176", "1177", "1178", "1181", "1182", "1582", "1592", "1745", "1982", "2174", "2264")

# what does this actually include:
# all are items with rev_type = 75 originally. 
in_out_df <- rev_temp |>
  mutate(infromout = ifelse(source %in% in_from_out, 1, 0)) |>
  filter(infromout == 1)

rev_temp <- rev_temp |> 
   mutate(rev_type_new = ifelse(source %in% in_from_out, "76", rev_type))
# if source contains any of the codes in in_from_out, code them as 76 (all other rev).
# I end up excluding rev_76 in later steps


# Migrated from agency report, original line 1207


# revenue types to drop
drop_type <- c("32", "45", "51", 
               "66", "72", "75", "76", "79", "98", "99")

# drops Blank, Student Fees, Retirement contributions, proceeds/investments,
# bond issue proceeds, interagency receipts, cook IGT, Prior year refunds.


rev_temp <- rev_temp |> filter(!rev_type_new %in% drop_type)
# keep observations that do not have a revenue type mentioned in drop_type

table(rev_temp$rev_type_new)


# Migrated from agency report, original line 1233

rev_temp |> filter(is.na(rev_type))


# Migrated from agency report, original line 1241

ff_rev <- rev_temp |> 
  group_by(rev_type_new, fy) |> 
  summarize(sum_receipts = sum(receipts, na.rm=TRUE)/1000000 ) |>
  pivot_wider(names_from = "rev_type_new", values_from = "sum_receipts", names_prefix = "rev_")

 
ff_rev <- mutate_all(ff_rev, replace_na, 0)


# Migrated from agency report, original line 1255

# DOCUMENTATION PURPOSES ONLY: If refunds on the expenditure side are subtracted from the revenue side, it was done like this. The refunds were identified in the "Tax Refunds" section of the code. 

# OLD way of doing refunds ##
# ff_rev <- ff_rev |>
#   mutate(rev_02 = rev_02 - ref_02,
#          rev_03 = rev_03 - ref_03,
#          rev_06 = rev_06 - ref_06,
#          rev_09 = rev_09 - ref_09,
#          rev_21 = rev_21 - ref_21,
#          rev_24 = rev_24 - ref_24,
#          rev_35 = rev_35 - ref_35
# 
#       #   rev_78new = rev_78 #+ pension_amt #+ eehc
#          ) |> 
#   select(-c(ref_02:ref_35, rev_99, rev_NA, rev_76
#             #, ref_CHECK#, pension_amt , rev_76,
#           #  , eehc
#             ))
# 
# ff_rev



#noproblem <- c(0)  # if ref_CHECK = $0, then there is no problem. :) 
# 
# if((sum(ff_rev$ref_CHECK) == 0 )){
# 
# ff_rev <- ff_rev |>
#   
#   mutate(rev_02 = rev_02 - ref_02,
#          rev_03 = rev_03 - ref_03,
#          rev_06 = rev_06 - ref_06,
#          rev_09 = rev_09 - ref_09,
#          rev_21 = rev_21 - ref_21,
#          rev_24 = rev_24 - ref_24,
#          rev_35 = rev_35 - ref_35
#          ) |> 
#   select(-c(ref_02:ref_35, rev_99, rev_76, ref_CHECK )) 
# }else{"You have a problem! Check what revenue items did not have rev codes (causing it to be coded as rev_NA) or the check if there were refunds that were not assigned revenue codes (tax_refunds_long objects)"}


# Migrated from agency report, original line 1302

# Since I already pivot_wider()ed the table in the previous code chunk, I now change each column's name by using rename() to set new variable names. Ideally the final dataframe would have both the variable name and the variable label but I have not done that yet.


aggregate_rev_labels <- ff_rev |>
  rename("INDIVIDUAL INCOME TAXES, gross of local, net of refunds" = rev_02,
         "CORPORATE INCOME TAXES, gross of PPRT, net of refunds" = rev_03,
         "SALES TAXES, gross of local share" = rev_06 ,
         "MOTOR FUEL TAX, gross of local share, net of refunds" = rev_09 ,
         "PUBLIC UTILITY TAXES, gross of PPRT" = rev_12,
         "CIGARETTE TAXES" = rev_15 ,
         "LIQUOR GALLONAGE TAXES" = rev_18,
         "INHERITANCE TAX" = rev_21,
         "INSURANCE TAXESFEES&LICENSES, net of refunds" = rev_24 ,
         "CORP FRANCHISE TAXES & FEES" = rev_27,
        "HORSE RACING TAXES & FEES" = rev_30,  # in Other
         "MEDICAL PROVIDER ASSESSMENTS" = rev_31 ,
         # "GARNISHMENT-LEVIES " = rev_32 , # dropped
         "LOTTERY RECEIPTS" = rev_33 ,
         "OTHER TAXES" = rev_35,
         "RECEIPTS FROM REVENUE PRODUCNG" = rev_36, 
         "LICENSES, FEES & REGISTRATIONS" = rev_39 ,
         "MOTOR VEHICLE AND OPERATORS" = rev_42 ,
         #  "STUDENT FEES-UNIVERSITIES" = rev_45,   # dropped
         "RIVERBOAT WAGERING TAXES" = rev_48 ,
         # "RETIREMENT CONTRIBUTIONS " = rev_51, # dropped
         "GIFTS AND BEQUESTS" = rev_54, 
         "FEDERAL OTHER" = rev_57 ,
         "FEDERAL MEDICAID" = rev_58, 
         "FEDERAL TRANSPORTATION" = rev_59 ,
         "OTHER GRANTS AND CONTRACTS" = rev_60, #other
        "INVESTMENT INCOME" = rev_63, # other
         # "PROCEEDS,INVESTMENT MATURITIES" = rev_66 , #dropped
         # "BOND ISSUE PROCEEDS" = rev_72,  #dropped
         # "INTER-AGENCY RECEIPTS" = rev_75,  #dropped
       # "TRANSFER IN FROM OUT FUNDS" = rev_76,  # dropped
         "ALL OTHER SOURCES" = rev_78,
         # "COOK COUNTY IGT" = rev_79, #dropped
         # "PRIOR YEAR REFUNDS" = rev_98 #dropped
  ) 

aggregate_rev_labels |> mutate_all(round, digits = 0)



# Presentation categories: provisional non-pension treatment is identical.
exp_temp <- exp_temp |> mutate(
  group_agency = as.character(group),
  group_separate_pensions = if_else(is_pension, "901", group_agency))

# REVIEW-NONPENSION: legacy separate version sends these residual costs to 948.
retirement_review <- exp_temp |>
  filter(original_agency %in% c("131", "275", "589", "593", "594", "693"), !is_pension) |>
  group_by(fy, original_agency, group_agency) |>
  summarize(Dollars = sum(expenditure, na.rm = TRUE), .groups = "drop") |>
  mutate(legacy_separate_group = if_else(original_agency == "693", "960", "948"),
         decision = if_else(group_agency == legacy_separate_group,
                            "Same effective category in both legacy versions",
                            "Pending; retained in agency category in both views"))

# Records the broader identification flag without using it to expand inclusion.
pension_inclusion_review <- dropped_inff_0 |> filter(is_pension) |>
  group_by(fy, original_agency, pension_kind) |>
  summarize(Dollars = sum(expenditure, na.rm = TRUE), .groups = "drop")
