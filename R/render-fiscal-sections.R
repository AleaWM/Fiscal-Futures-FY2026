# Knit blocks in persistent view environments; compare only explicitly marked
# outputs. Calculations are maintained once in the shared child file.
render_fiscal_sections <- function(template, views, labels) {
  starts <- grep("^```\\{r", template)
  ends <- vapply(starts, function(i) i + which(template[(i + 1):length(template)] == "```")[1], 1L)
  cursor <- 1L
  for (j in seq_along(starts)) {
    if (starts[j] > cursor) cat(template[cursor:(starts[j] - 1)], sep = "\n")
    block <- template[starts[j]:ends[j]]
    compare <- any(grepl("^# pension-compare: true$", block))
    results <- lapply(names(views), function(view) {
      text <- block
      # Shared outputs appear once, but their calculations still run in both
      # environments so downstream expenditure tables and exports remain valid.
      if (!compare && view != names(views)[1]) {
        text <- text[!grepl("^#\\| include:", text)]
        text <- append(text, "#| include: false", after = 1)
      }
      label_line <- grep("^#\\| label:", text)
      label <- if (length(label_line)) sub("^#\\| label: *", "", text[label_line]) else paste0("block-", j)
      label <- if (grepl("^(fig|tbl)-", label)) sub("^([^-]+)-", paste0("\\1-", view, "-"), label) else paste0(view, "-", label)
      if (length(label_line)) text[label_line] <- paste("#| label:", label)
      else text <- append(text, paste("#| label:", label), after = 1)
      paste(knitr::knit_child(text = text, envir = views[[view]], quiet = TRUE), collapse = "\n")
    })
    if (!compare) results <- results[1]
    if (any(nzchar(trimws(unlist(results))))) {
      tabbed <- compare && length(views) > 1
      if (tabbed) cat("\n\n::: {.panel-tabset}\n\n")
      for (k in seq_along(results)) {
        if (tabbed) cat("\n##### ", labels[[names(views)[k]]], " {.unnumbered .unlisted}\n\n", sep = "")
        cat(results[[k]], "\n\n")
      }
      if (tabbed) cat("\n:::\n\n")
    }
    cursor <- ends[j] + 1L
  }
  if (cursor <= length(template)) cat(template[cursor:length(template)], sep = "\n")
}
