agency_default_data = function(years = 2015:2017) {
  main_url = paste0("https://www2.ed.gov/offices/OSFAP/",
                    "defaultmanagement/",
                    "lga3yr.html")
  codebook_url = paste0("https://www2.ed.gov/offices/OSFAP/",
                        "defaultmanagement/",
                        "lgainstructions.html")
  files = paste0("https://www2.ed.gov/offices/OSFAP/",
                 "defaultmanagement/", "lga", years, ".zip")
  names(files) = years
  # need to push the .zip files to google store
  # if doesn't exist, then download and upload
  # if it does exist, then download from bucket
  destfiles = lapply(files, function(file) {
    destfile = file.path(tempdir(), basename(file))
    if (!file.exists(destfile)) {
      curl::curl_download(file, destfile, quiet = FALSE)
    }
    res = utils::unzip(zipfile = destfile, exdir = tempdir())
    res = res[!grepl("__MACOSX", res)]
    stopifnot(length(res) == 1)
    res
  })
  destfiles
}


agency_default_rates = function(...) {
  destfiles = agency_default_data(...)
  scrub_cn = function(df) {
    cn = colnames(df)
    cn = gsub("\r", " ", cn)
    cn = gsub("\n", " ", cn)
    cn = gsub("\\s+", " ", cn)
    cn
    colnames(df) = cn
    df
  }
  lenders = lapply(destfiles, function(destfile) {
    sn = readxl::excel_sheets(destfile)
    # print(sn)
    sn = sn[grepl("LEND", toupper(sn))]
    stopifnot(length(sn) == 1)
    df = readxl::read_excel(destfile, sheet = sn)
    df = scrub_cn(df)
  })
  lenders = dplyr::bind_rows(lenders)

  # ga = guarantor
  ga = lapply(destfiles, function(destfile) {
    sn = readxl::excel_sheets(destfile)
    sn = sn[grepl(" GA ", toupper(sn))]
    stopifnot(length(sn) == 1)
    df = readxl::read_excel(destfile)
    df = scrub_cn(df)
  })
  ga = dplyr::bind_rows(ga)
  L = list(
    lenders = lenders,
    guarantors = ga
  )
}

clean_agency = function(...) {
  L = agency_default_rates(...)
  lenders = L$lenders
  info = lenders %>%
    dplyr::select(LID, Name, Address, City,
           State, `Zip Code`, `Zip Ext`) %>%
    dplyr::distinct()

  ga = L$guarantors
  info = ga %>%
    dplyr::select(`GA Code`, Name, Address, City,
           State, `Zip Code`, `Zip Ext`) %>%
    dplyr::distinct()

  L = list(
    lenders = lenders,
    guarantors = ga
  )
}

clean_address = function(x) {
  x = toupper(x)
  x = gsub("\\s+", " ", x)
  x = gsub("P[.]\\s*O[.] ", "PO", x)
  x = gsub("P\\s*O\\s*BOX", "PO BOX", x)
  x = gsub("AV[.]", "AVE", x)
  x = gsub("STE[.]", "STREET", x)
  # probably want to convert AVE to AVENUE
  x = gsub("\\s(AVE|BLVD|FL|FT|HWY|RD|PKWY|OPS)[.]", " \\1", x)
  x = gsub("\\s(DEPT|DR|ST)[.]", " \\1", x)
  x = gsub("U[.]S[.]", "US", x)
  x = gsub(" E[.]", " EAST", x)
  x = gsub(" W[.]", " WEST", x)
  x = gsub(" N[.]", " NORTH", x)
  x = gsub(" S[.]", " SOUTH", x)


  potential_direction = grepl(" (E|W|N|S) ", x)
  x = gsub(" W[.]", " WEST", x)
  x = gsub(" N[.]", " NORTH", x)
  x = gsub(" S[.]", " SOUTH", x)

  y = unlist(strsplit(x, " "))
  table(y[grepl("[.]$", y)])
}

