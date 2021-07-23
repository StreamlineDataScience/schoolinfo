
school_df = function() {
  main_url = paste0("https://fsapartners.ed.gov/knowledge-center/",
                    "library/resource-type/",
                    "Federal%20School%20Code%20Lists")


  file = paste0("https://fsapartners.ed.gov/sites/default/files",
                "/attachments/2020-11/",
                "2122FedSchoolCodeList.xlsx")
  destfile = file.path(tempdir(), basename(file))
  if (!file.exists(destfile)) {
    curl::curl_download(file, destfile, quiet = FALSE)
  }
  sn = readxl::excel_sheets(destfile)
  # what universities have the shortest trips to the airport??
  df = readxl::read_excel(destfile)
  df = df %>%
    dplyr::filter(!SchoolCode %in% "End of table")
  df
}


school_default_rates = function() {
  main_url = paste0("https://www2.ed.gov/offices/OSFAP/",
                    "defaultmanagement/cdr.html")

  file = paste0("https://www2.ed.gov/offices/",
                "OSFAP/defaultmanagement/peps300.xlsx")
  destfile = file.path(tempdir(), basename(file))
  if (!file.exists(destfile)) {
    curl::curl_download(file, destfile, quiet = FALSE)
  }
  sn = readxl::excel_sheets(destfile)
  # what universities have the shortest trips to the airport??
  df = readxl::read_excel(destfile)
}



