
motor_data = function() {
  main_url = "https://www.bea.gov/data/gdp/gross-domestic-product#collapse86"
  url = "https://apps.bea.gov/national/xls/gap_hist.xlsx"
  destfile = file.path(tempdir(), basename(url))
  if (!file.exists(destfile)) {
    curl::curl_download(url, destfile, quiet = FALSE)
  }
  sn = readxl::excel_sheets(destfile)
  readme = motor_readme_toc(destfile, sheet = "Readme")
  # dfs = lapply(sn, read_motor_table, destfile = destfile)
  # names(dfs) = sn
  sheet = NULL
  if (length(readme) > 0) {
    heavy = grepl("heavy", readme$description, ignore.case = TRUE)
    if (sum(heavy) == 1) {
      sheet = readme$sheet_name[heavy]
    }
  }
  if (is.null(sheet)) {
    warning("Guessing sheet 5")
    sheet = "Table 5"
  }
  df = read_motor_table(destfile, sheet)
  df = df %>%
    dplyr::mutate(
      month = factor(month, levels = month.name),
      month_num = as.numeric(month),
      year_fac = factor(year),
      modern = dplyr::case_when(
        year >= 2019 ~ ">= 2019",
        TRUE ~ "< 2019")
    )
  # ddf = df %>%
  #   filter(year >= 2010)
  # g = ddf %>%
  #   ggplot(
  #     aes(x = month_num,
  #         colour = year_fac,
  #         alpha = modern,
  #         y = `Seasonally adjusted (Thousands)`)) +
  #   geom_line() +
  #   scale_alpha_manual(values = c(">= 2019" = 1, "< 2019" = 0.25)) +
  #   guides(colour = "none", alpha = "none")
  # plotly::ggplotly(g, tooltip = c("colour", "y"))
  df
}

guess_motor_skip = function(destfile, sheet) {
  df = readxl::read_excel(destfile, sheet = sheet,
                          col_names = FALSE,
                          n_max = 10)
  if (ncol(df) >= 3) {
    indices = 3:ncol(df)
  } else {
    indices = 1:ncol(df)
  }
  # - 1 because no colnames
  skipper = apply(df[indices], 2, function(x) {
    which.min(is.na(x)) - 1
  })
  skipper = names(sort(table(skipper), decreasing = TRUE))[1]
  skipper = as.integer(skipper)
  skipper
}
read_motor_table = function(destfile, sheet) {
  skip = guess_motor_skip(destfile, sheet)
  df = readxl::read_excel(destfile,
                          sheet = sheet,
                          col_names = TRUE,
                          skip = skip)
  if (all(grepl("^[.][.]", colnames(df)[1:2]))) {
    colnames(df)[1:2] = c("month", "year")
  }
  df = remove_all_na_rows(df)
  colnames(df) = remove_double_space(colnames(df))
  df
}


motor_readme_toc = function(destfile, sheet) {
  sn = readxl::excel_sheets(destfile)
  if (!sheet %in% sn) {
    return(NULL)
  }
  df = readxl::read_excel(destfile, sheet = sheet)
  rep_list = rep("", ncol(df))
  names(rep_list) = colnames(df)
  df = tidyr::replace_na(df, replace = as.list(rep_list))
  df = apply(df, 1, paste, collapse = " ")
  df = trimws(df)
  df = df[ !df %in% ""]
  ind = grep("table\\s*of\\s*contents", df, ignore.case = TRUE)
  if (length(ind) == 0) return(NULL)
  df = df[seq(ind, length(df))]
  df = df[ grepl("table\\s*\\d.*", df, ignore.case = TRUE)]
  n = gsub("(table \\d*).*", "\\1", df, ignore.case = TRUE)
  df = gsub("(table \\d*)(.*)", "\\2", df, ignore.case = TRUE)
  df = trimws(df)
  df = dplyr::tibble(
    sheet_name = n,
    description = df
  )
  df


}
