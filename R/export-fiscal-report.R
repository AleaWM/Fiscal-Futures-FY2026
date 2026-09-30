# Existing openxlsx export workflow, shared by both views.
export_fiscal_report <- function(e, view, suffix, output_dir, retirement_review,
                                 inclusion_review, reconciliation) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  stem <- paste0("FY", e$current_year, "_", suffix)
  metadata <- data.frame(Notes = c(
    paste("Fiscal Futures", e$current_year),
    paste("Pension presentation:", view),
    paste("Created:", Sys.Date()),
    paste("Preliminary mode (uncategorized amounts retained):", isTRUE(e$allow_uncategorized)),
    "Accounting inclusion follows revenue_report_fy26.qmd; presentation does not change inclusion.",
    "Non-pension retirement-agency spending remains in agency categories pending review.",
    "Pension identification: object 4431; objects 1160-1165 except funds 0183/0193; fund 0319; specified 2010/2011 pension-bond investments.",
    "Aggregate long data, DataHub tables and fiscal gap: millions of nominal dollars.",
    "Yearly change tables: billions of nominal dollars; CAGR sheets: percent.",
    "Retirement and pension inclusion review sheets: nominal dollars.",
    "https://igpa.uillinois.edu/igpa-data-hub",
    "https://github.com/igpa-uillinois/Fiscal-Futures"
  ))
  summary <- list(
    README = metadata,
    "Table 1" = e$revenue_change_majorcats,
    "Table 2" = e$expenditure_change_majorcats,
    "Appendix 1" = e$revenue_change2,
    "Appendix 2" = e$expenditure_change2,
    "CAGR Rev-MajorCats" = e$CAGR_revenue_majorcats_tot,
    "CAGR Exp-MajorCats" = e$CAGR_expenditures_majorcats_tot,
    "Fiscal Gap" = e$year_totals,
    aggregated_totals_long = e$aggregated_totals_long,
    "Retirement agency review" = retirement_review,
    "Pension inclusion review" = inclusion_review,
    "Uncategorized review" = e$uncategorized_review,
    Reconciliation = reconciliation)
  summary_file <- file.path(output_dir, paste0("summary_", stem, ".xlsx"))
  openxlsx::write.xlsx(summary, summary_file, overwrite = TRUE,
                      firstRow = TRUE, colWidths = "auto")
  datahub_file <- file.path(output_dir, paste0("IGPA_Datahub_", stem, ".xlsx"))
  openxlsx::write.xlsx(list(README = metadata, Expenditures = e$datahub_exp,
                           Revenues = e$datahub_rev), datahub_file,
                      overwrite = TRUE, firstRow = TRUE, colWidths = "auto")
  c(summary_file, datahub_file)
}
