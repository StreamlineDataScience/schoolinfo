remove_all_na_rows = function(df) {
  all_na = rowSums(is.na(df)) == ncol(df)
  df = df[!all_na, ]
}

remove_double_space = function(x) {
  gsub("\\s+", " ", x)
}
