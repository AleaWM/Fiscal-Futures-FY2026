# Fiscal Futures FY2026

## Current consolidated report

Use `revenue_report_fy26_consolidated.qmd` for the maintained report. It displays local pension-comparison tabs beside tables and figures, derives fiscal-year labels from `report_year`, and writes both presentations to `outputs/`. Run `./render-pension-reports.ps1` to build the comparison and both standalone reports. See [PENSION-CONSOLIDATION.md](PENSION-CONSOLIDATION.md) for parameters, the preliminary uncategorized-data override, and accounting decisions. The two original QMDs are retained as legacy references; the descriptions below document that original workflow.

This project contains the report-specific code and rendered output for the FY2026 Fiscal Futures work. It uses the shared datasets maintained in the neighboring **Fiscal-Future-Topics** project.

The data are stored centrally so that separate annual projects do not accumulate copied datasets that can diverge. The FY2026 project reads the shared files directly, and its spreadsheet exports are also saved in the shared annual data folder.

## Folder structure

The two projects should remain next to each other under the same parent folder:

```text
Fiscal Futures IGPA/
├── Fiscal-Future-Topics/                 # Shared data and researcher website
│   ├── Fiscal-Future-Topics.Rproj
│   ├── code-cleaning.qmd                 # Imports, combines, and recodes IOC data
│   ├── inputs/
│   │   ├── ioc_data_received/
│   │   │   ├── DATA_RAW/                 # Original IOC deliveries
│   │   │   ├── revenue/
│   │   │   └── expenditures/
│   │   ├── funds_ab_in.xlsx              # Fund recodes and inclusion decisions
│   │   └── ioc_source.xlsx               # Revenue source lookup
│   ├── data/
│   │   ├── FY2025 Files/
│   │   └── FY2026 Files/
│   │       ├── allrevfiles_2026.csv       # Combined revenue records
│   │       ├── allexpfiles_2026.csv       # Combined expenditure records
│   │       ├── rev_temp.csv              # Revenue input read by FY2026 report code
│   │       ├── exp_temp.csv              # Expenditure input read by FY2026 report code
│   │       ├── summary_file_FY2026_<date>.xlsx
│   │       └── IGPA_Datahub_Download_<date>.xlsx
│   └── docs/                            # Rendered researcher website
├── Fiscal Futures FY2026/                # This project
│   ├── README.md
│   ├── Fiscal Futures FY2026.Rproj
│   ├── revenue_report_fy26.qmd
│   ├── revenue_report_fy26_w_pension_cat.qmd
│   ├── revenue_report_fy26.html
│   └── revenue_report_fy26_files/        # Supporting rendered assets
├── Fiscal Futures FY2025/
├── Fiscal Futures FY2024/
└── ...                                  # Earlier annual projects and reference material
```

This is a simplified map, not an exhaustive inventory. `<date>` represents the date included in an output filename. The `FY2026 Files` folder identifies the data/run vintage; its files can contain historical observations across many fiscal years, not just FY2026.

## Opening the project and accessing shared data

Open **Fiscal Futures FY2026.Rproj** in RStudio. Relative paths below assume that the working directory is the FY2026 project folder. Use `getwd()` to check it when running code interactively.

From this folder, `..` means “go up to Fiscal Futures IGPA.” Then `Fiscal-Future-Topics` selects the neighboring project. For example:

```r
"../Fiscal-Future-Topics/data/FY2026 Files/rev_temp.csv"
```

A compact way to read the shared inputs is:

```r
current_year <- 2026
past_year <- current_year - 1

shared_project <- file.path("..", "Fiscal-Future-Topics")
shared_data <- file.path(shared_project, "data",
                         paste0("FY", current_year, " Files"))

revenue_file <- file.path(shared_data, "rev_temp.csv")
expenditure_file <- file.path(shared_data, "exp_temp.csv")

stopifnot(file.exists(revenue_file), file.exists(expenditure_file))

rev_temp <- readr::read_csv(revenue_file)
exp_temp <- readr::read_csv(expenditure_file)
```

This example documents the path convention; it does not change the report scripts. Their current input paths use the longer equivalent form:

```r
paste0("../../Fiscal Futures IGPA/Fiscal-Future-Topics/data/FY",
       current_year, " Files/rev_temp.csv")
```

Under the folder structure shown above, both forms resolve to the same file. The shorter `../Fiscal-Future-Topics/...` form does not depend on the parent folder being named `Fiscal Futures IGPA`; the longer form does. Keep the sibling project names and relative positions consistent, or update the paths if folders move.

These are local filesystem paths, not GitHub downloads. Anyone using the FY2026 project on another computer also needs the shared project and the required data files available locally. Downloading only the annual report project is not sufficient.

## Where data preparation and report calculations happen

1. IOC deliveries and lookup workbooks are maintained in `Fiscal-Future-Topics/inputs/`.
2. The shared project's `code-cleaning.qmd` imports, combines, labels, and recodes the records, writing annual intermediate files under `data/FY2026 Files/`.
3. The FY2026 report reads `rev_temp.csv` and `exp_temp.csv` from that location and applies its report-specific calculations, inclusion rules, and presentation choices.
4. Rendered report HTML and supporting assets are kept with the annual project, while spreadsheet exports are written back to the shared annual data folder.

The researcher website describes what exists in the broader dataset. An item appearing in that documentation is not necessarily included in Fiscal Futures report totals. Likewise, the intermediate CSVs are inputs to the report calculations, not final aggregate report tables.

## Where outputs are saved

The main report currently writes these workbook names under `../Fiscal-Future-Topics/data/FY2026 Files/` when `current_year` remains `2026`:

| Output | Purpose |
|---|---|
| `summary_file_FY2026_<date>.xlsx` | Report tables, growth calculations, fiscal-gap totals, and supporting aggregate data |
| `IGPA_Datahub_Download_<date>.xlsx` | Formatted workbook for the IGPA Data Hub |

The pension-category variant uses a `_withpensioncat` suffix on its summary export. Inspect each script's active export chunks for the exact destinations: not every output is uniquely named by calculation variant, and the Data Hub export uses `overwrite = TRUE`. An unformatted Data Hub export is present as reference code in a disabled chunk.

Rendering the report executes calculations and file-writing chunks; it is not only an HTML formatting step. This README documents the existing arrangement and does not certify a complete render or the report totals.

## Updating and reproducing the report

- Update shared data in `Fiscal-Future-Topics` and read it from there instead of creating another working copy in the annual project.
- Confirm the IOC delivery date and whether revenue and expenditure files are preliminary or final. The FY2026 additions were initially recorded in Git as preliminary expenditure data.
- Check `current_year`, explicit year-specific paths, and the selected report variant before running. In the September 23, 2026 inspection, the pension-category variant started at FY2026 but also contained a later `current_year <- 2025` assignment, plus references to FY2025 supporting analyses. These need review before treating it as a consistent FY2026 run.
- Updating shared inputs can change a later rerun of this report. Central storage prevents divergent working copies, but does not automatically preserve the exact data used for a published edition.
- For a final report, record the code version, input/lookup versions, data status, calculation variant, and output filenames. Preserve a deliberate release snapshot or immutable references so the published results remain reproducible.

For broader project history and proposed report archiving, see the shared project's [project guide](../Fiscal-Future-Topics/project-guide.qmd) and [archive proposal](../Fiscal-Future-Topics/report-archives.qmd). These links assume the same sibling folder layout and may not resolve when viewing an annual repository by itself on GitHub.

*Folder structure and code paths checked September 23, 2026.*
