# One aggregation function for both pension presentations.
aggregate_view <- function(shared, view) {
  e <- new.env(parent = shared)
  e$current_year <- shared$current_year
  e$allow_uncategorized <- isTRUE(shared$allow_uncategorized)
  e$exp_temp <- shared$exp_temp
  e$exp_temp$group <- if (view == "agency") e$exp_temp$group_agency else e$exp_temp$group_separate_pensions
  evalq({
for (nm in paste0("exp_", c(971, 972, 975, 976))) {
  if (!nm %in% names(transfers)) transfers[[nm]] <- 0
}

ff_exp <- exp_temp |> 
  group_by(fy, group) |> 
  summarize(sum_expenditures = sum(expenditure, na.rm=TRUE)/1000000 ) |>
  pivot_wider(names_from = "group", values_from = "sum_expenditures", names_prefix = "exp_")|>
  
    left_join(debt_keep_yearly) |>
  rename(exp_903 = debt_cost) |>

 #  join local transfers and create exp_970
  left_join(transfers) |>
  mutate(exp_970 = rowSums(across(all_of(c("exp_971", "exp_972", "exp_975", "exp_976"))), na.rm = TRUE))

 ff_exp<- ff_exp |> 
  select(-any_of(paste0("exp_", 971:976))) # drop unwanted columns that are already included in exp_970

ff_exp # not labeled


rev_long <- pivot_longer(ff_rev, starts_with("rev_"), names_to = c("type","Category"), values_to = "Dollars", names_sep = "_") |> 
  rename(Year = fy) |>
  mutate(Category_name = case_when(
    Category == "02" ~ "INDIVIDUAL INCOME TAXES" ,
    Category == "03" ~ "CORPORATE INCOME TAXES" ,
    Category == "06" ~ "SALES TAXES" ,
    Category == "09" ~ "MOTOR FUEL TAX" ,
    Category == "12" ~ "PUBLIC UTILITY TAXES" ,
    Category == "15" ~ "CIGARETTE TAXES" ,
    Category == "18" ~ "LIQUOR GALLONAGE TAXES" ,
    Category == "21" ~ "INHERITANCE TAX" ,
    Category == "24" ~ "INSURANCE TAXES, FEES & LICENSES" ,
    Category == "27" ~ "CORP FRANCHISE TAXES & FEES" ,
    Category == "30" ~ "HORSE RACING TAXES & FEES", 
    Category == "31" ~ "MEDICAL PROVIDER ASSESSMENTS" ,
    Category == "32" ~ "GARNISHMENT-LEVIES" , # dropped
    Category == "33" ~  "LOTTERY RECEIPTS" ,
    Category == "35" ~  "OTHER TAXES" ,
    Category == "36" ~  "RECEIPTS FROM REVENUE PRODUCING", 
    Category == "39" ~  "LICENSES, FEES & REGISTRATIONS" ,
    Category == "42" ~  "MOTOR VEHICLE AND OPERATORS" ,
    Category == "45" ~  "STUDENT FEES-UNIVERSITIES",   # dropped
    Category == "48" ~  "RIVERBOAT WAGERING TAXES" ,
    Category == "51" ~  "RETIREMENT CONTRIBUTIONS" , # dropped
    Category == "54" ~ "GIFTS AND BEQUESTS", 
    Category == "57" ~  "FEDERAL OTHER" ,
    Category == "58" ~  "FEDERAL MEDICAID", 
    Category == "59" ~  "FEDERAL TRANSPORTATION" ,
    Category == "60" ~  "OTHER GRANTS AND CONTRACTS", 
    Category == "63" ~  "INVESTMENT INCOME", 
    Category == "66" ~ "PROCEEDS, INVESTMENT MATURITIES" , #dropped
    Category == "72" ~ "BOND ISSUE PROCEEDS",  #dropped
    Category == "75" ~  "INTER-AGENCY RECEIPTS ",  #dropped
    Category == "76" ~  "TRANSFER IN FROM OUT FUNDS",  
    Category == "78" ~  "ALL OTHER SOURCES" ,
    Category == "79" ~   "COOK COUNTY IGT", #dropped
    Category == "98" ~  "PRIOR YEAR REFUNDS", #dropped
    T ~ "Check Me!"
    
  ) )|> 
  mutate(Category_name = str_to_title(Category_name))


exp_long <- pivot_longer(ff_exp, starts_with("exp_") , names_to = c("type", "Category"), values_to = "Dollars", names_sep = "_") |> 
  rename(Year = fy ) |> 
  mutate(Category_name = 
           case_when(
             
             Category == "131" ~ "GARS",    # should be in Legislative Group already
             Category == "275" ~ "JRS",     # should be in Judicial Category already
             Category == "402" ~ "Aging",
             Category == "406" ~ "Agriculture",   # agriculture
             Category == "416" ~ "Central Management", ## contains DoIT also
             Category == "418" ~ "Children & Family Services", 
             Category == "420" ~ "Commerce & Economic Opportunity",
             Category == "422" ~ "Natural Resources" ,
             Category == "426" ~ "Corrections",
             Category == "427" ~ "Employment Security" ,
             
             Category == "442" ~ "Human Rights" ,  # sometimes included in "Other Departments" when trying to have fewer expenditure categories
             
             Category == "444" ~ "Human Services" ,  
             Category == "445" ~ "IL Power Agency" ,    # IL Power Agency
             Category == "448" ~ "Innovation & Technology",   # should be in Central Management already
             
             Category == "452" ~ "Labor" ,   # Sometimes included in "Other Departments when trying to have fewer categories
             
             Category == "458" ~ "State Lottery" ,   # State Lottery is sometimes included as "Other Departments when trying to have fewer expenditure categories
             
             Category == "478" ~ "Family Services (net Medicaid)",
             Category == "480" ~ "Early Childhood",
             Category == "482" ~ "Public Health", 
             Category == "492" ~ "Revenue", 
             Category == "493" ~ "Teacher Retirement System (TRS)",  # Should be included in K-12 already
             Category == "494" ~ "Transportation" ,
             Category == "507" ~ "GOMB",  # GOMB     # GOMB is sometimes included as "Other Departments when trying to have fewer expenditure categories
             Category == "497" ~ "Veterans' Affairs",  # Veterans' Affairs  is sometimes included as "Other Departments when trying to have fewer expenditure categories
             Category == "532" ~ "Environmental Protection Agency" ,
             Category == "557" ~ "IL State Tollway" ,
             Category == "589" ~ "State Emp. Retirement System (SERS)",
             Category == "693" ~ "SURS",                             # should be in Higher Education already
             Category == "901" ~ "Pension Expenditure",
             Category == "903" ~ "Debt Service",
             Category == "904" ~ "State Employee Healthcare",
             Category == "910" ~ "Legislative"  ,
             Category == "920" ~ "Judicial" ,
             Category == "930" ~ "Elected Officers" , 
             Category == "941" ~ "Public Safety" ,
             Category == "943" ~ "Central Services",
             Category == "944" ~ "Business & Professional Regulation" ,
             Category == "945" ~ "Medicaid" ,
             Category == "946" ~ "Capital Improvements" , 
             Category == "948" ~ "Other Departments" ,   # Used when pre-grouping small agencies to group = 948. 
             Category == "949" ~ "Other Boards & Commissions" ,
             Category == "959" ~ "K-12 Education" ,
             Category == "960" ~ "University Education",
             Category == "970" ~ "Local Govt Transfers",
             T ~ "CHECK ME!")
           )   |>
  dplyr::mutate(Dollars = ifelse(is.na(Dollars), 0, Dollars))


# combine revenue and expenditures into one data frame
aggregated_totals_long <- rbind(rev_long, exp_long)

# Fail on either revenue or expenditure categories, including negative amounts.
uncategorized_review <- aggregated_totals_long |>
  filter(is.na(Category_name) | grepl("check me", Category_name, ignore.case = TRUE))
if (any(abs(uncategorized_review$Dollars) > 0, na.rm = TRUE) &&
    !isTRUE(shared$allow_uncategorized)) {
  stop("Uncategorized revenue or expenditures found. Assign categories, or use allow_uncategorized: true for preliminary estimates; see PENSION-CONSOLIDATION.md.")
}
aggregated_totals_long <- aggregated_totals_long |>
  mutate(Category_name = if_else(is.na(Category_name) |
    grepl("check me", Category_name, ignore.case = TRUE), "Uncategorized", Category_name))
rev_long <- aggregated_totals_long |> filter(type == "rev")
exp_long <- aggregated_totals_long |> filter(type == "exp")
aggregated_totals_long |> mutate(`Dollars (Millions)` = round(Dollars, digits = 0)) |> select(-Dollars) |>
  select(Year, Category_name, `Dollars (Millions)`, type, Category)


year_totals <- aggregated_totals_long |> 
  group_by(type, Year) |> 
  summarize(Dollars = sum(Dollars, na.rm = TRUE)) |> 
  pivot_wider(names_from = "type", values_from = Dollars) |> 

  rename(Expenditures = exp,
         Revenue = rev) |>  
  mutate(`Fiscal Gap` = Revenue - Expenditures)
  }, envir = e)
  e
}
