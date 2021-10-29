#' @importFrom readr cols col_character col_double col_logical
parking_fines = function() {

  main_url = paste0("https://data.baltimorecity.gov/datasets/",
                    "baltimore::parking-and-moving-citations/",
                    "about")
  # file = paste0("https://opendata.baltimorecity.gov/egis/rest",
  #               "/services/NonSpatialTables/ParkingFines/FeatureServer",
  #               "/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
  # out = httr::GET(file, httr::progress())

  # not updated link
  file = paste0(
    "https://opendata.arcgis.com/api/",
    "v3/",
    "datasets/",
    "5a0d5557ca104a8595b8ba86b85d97e3_0/",
    "downloads/data?", "format=csv&spatialRefId=4326")
  destfile = file.path(tempdir(),
                       "Parking_and_Moving_Citations.csv")
  if (!file.exists(destfile)) {
    curl::curl_download(file, destfile, quiet = FALSE)
  }
  specification =
    cols(
      .default = col_character(),
      ViolFine = col_double(),
      Balance = col_double(),
      OpenFine = col_double(),
      OpenPenalty = col_double(),
      CouncilDistrict = col_double()
    )
  df = readr::read_csv(destfile, col_types = specification)
  readr::stop_for_problems(df)
  df
}

balt_service_url = function() {
  "https://opendata.baltimorecity.gov/egis/rest/services"
}

balt_query_url = function(service = "RealProperty") {
  paste0(balt_service_url(),
         "/NonSpatialTables/",
         service,
         "/FeatureServer/0/query")
}

bad_drivers = function(df) {
  df = df %>%
    dplyr::select(Tag, State, ViolFine, Balance,
                  ViolDate, Description, NoticeDate)
  df = df %>%
    dplyr::filter(!is.na(Tag), !is.na(State)) %>%
    dplyr::filter(!Tag %in% c("NOTAGS T", "NO TAGST", "NOTAGSDI"))
  stopifnot(!anyNA(df$ViolFine))
  stopifnot(!anyNA(df$ViolDate))
  stopifnot(!anyNA(df$Balance))
  first_date = lubridate::as_date("1970-01-01")
  n_missing_date = sum(df$ViolDate <= first_date)
  df = df %>%
    dplyr::mutate(
      ViolDate = dplyr::if_else(
        ViolDate == first_date,
        NoticeDate,
        ViolDate))
  n_missing_date2 = sum(df$ViolDate <= first_date)
  high_tags = df %>%
    dplyr::group_by(Tag, State) %>%
    dplyr::summarise(n = dplyr::n(),
              total_fine = sum(ViolFine),
              total_balance = sum(Balance),
              latest_violation_date = max(ViolDate),
              n_outstanding = sum(Balance > 0))
  # high_tags = high_tags %>%
  #   arrange(desc(n))
  high_tags = high_tags %>%
    dplyr::arrange(dplyr::desc(total_balance))
}


parking_fines_update = function()  {
  main_url = paste0("https://data.baltimorecity.gov/",
                    "datasets/",
                    "baltimore::parking-and-moving-citations/api")

  url = paste0("https://opendata.baltimorecity.gov/",
               "egis/rest/services/NonSpatialTables/",
               "ParkingFines/FeatureServer/0/query?",
               "where=1%3D1&",
               "outFields=*&",
               "returnGeometry=false&",
               "outSR=4326&",
               "f=json")
}

datetime_format = function(x) {
  x = lubridate::as_datetime(x)
  x = format(x, format = "%Y-%m-%d %H:%M:%S")
}

flatten_content = function(x) {
  jsonlite::fromJSON(
    httr::content(x, as = "text"),
    flatten = TRUE)
}

updated_fines = function(
  start_date = "2021-07-20",
  end_date = Sys.time()
) {
  start_date = datetime_format(start_date)
  end_date = datetime_format(end_date)


  library(httr)
  library(sf)
  library(lubridate)
  # see
  # https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm
  # library(tmap)
  url <- httr::parse_url(
    paste0("https://opendata.baltimorecity.gov/",
           "egis/rest/services/NonSpatialTables/",
           "ParkingFines/FeatureServer/0/query")
  )
  where=paste0("ViolDate >= TIMESTAMP '", start_date, "'")
  where=paste0(where, " AND ViolDate <= TIMESTAMP '",
               end_date,
               "'")
  url$query <- list(where = where,
                    outFields = "*",
                    # returnGeometry = "false",
                    outSR=4326,
                    resultType="standard",
                    # resultType = "results",
                    maxRecordCount = 10000,
                    # f = "geojson"
                    f = "json"
  )
  url = httr::build_url(url)
  # url = paste0(url, "&outFields=*")
  # url = paste0(
  # "https://opendata.baltimorecity.gov/egis/rest/services",
  # "/NonSpatialTables/ParkingFines/FeatureServer/0/query",
  # "?where=ViolDate=1323704820000&returnGeometry=false&f=json")
  # # request <- build_url(url)
  #
  # works_url = paste0("https://opendata.baltimorecity.gov/egis/rest/services",
  #                    "/NonSpatialTables/ParkingFines/FeatureServer/0/query",
  #                    "?where=ViolDate%20%3E%3D%20TIMESTAMP%20'2020-12-31%",
  #                    "2019%3A00%3A00'%20AND%20",
  #                    "ViolDate%20%3C%3D%20TIMESTAMP%20'",
  #                    "2021-12-31%2019%3A00%3A00'&",
  #                    "outFields=*&outSR=4326&f=json")
  # url = works_url
  # count = "returnCountOnly=true"
  # URLdecode(works_url)
  utils::URLdecode(url)
  count = httr::GET(paste0(url, "&returnCountOnly=true"),
                    httr::content_type_json())
  count = flatten_content(count)$count
  count
  res = httr::GET(url)
  x = flatten_content(res)
  nrow(x$features)

  res2 = httr::GET(paste0(url, "&resultOffset=1999"))
  x2 = flatten_content(res2)
  nrow(x2$features)


  # ESRI_OID

  # we can use citation
}

prop_info = function() {
  main_url = paste0(
    "https://data.baltimorecity.gov/datasets/",
    "real-property-information/explore")
  url = balt_query_url(service = "RealProperty")
  url = httr::parse_url(url)
  url$query = list(
    where = "1=1",
    outFields = "*",
    outSR=4326,
    resultType="standard",
    f = "json"
  )
  url = httr::build_url(url)
  utils::URLdecode(url)
  count = httr::GET(paste0(url, "&returnCountOnly=true"),
                    httr::content_type_json())
  count = flatten_content(count)$count
  count
  res = httr::GET(url)
  get_features = function(res) {
    x = flatten_content(res)
    nrow(x$features)
    result = list(
      data = x$features,
      fields = x$fields,
      nr = nrow(x$features),
      need_next = x$exceededTransferLimit %||% FALSE
    )
    rm(x)
    cn = colnames(result$data)
    cn = sub("attributes[.]", "", cn)
    colnames(result$data) = cn
    result
  }
  x = get_features(res)
  x$data$AsOfDate = lubridate::as_datetime(
    x$data$AsOfDate/1000,
    origin = "1970-01-01")


  res2 = httr::GET(paste0(url, "&resultOffset=", nrow(x$features)))
  x2 = flatten_content(res2)
  nrow(x2$features)

  # PropertyID
}

#NYC
nyc_parking_fines = function() {
  main_url = "https://data.cityofnewyork.us/City-Government/Open-Parking-and-Camera-Violations/nc67-uf89"
  # tfile = tempfile(fileext = ".json")
  # res = httr::GET(
  #   "https://data.cityofnewyork.us/resource/nc67-uf89.json",
  #           httr::content_type_json(),
  #   httr::write_disk(tfile),
  #   httr::progress())
  # API has limits
  # this is full data- who knows how big
  # url = "https://data.cityofnewyork.us/api/views/nc67-uf89/rows.csv?accessType=DOWNLOAD"
  # just one year
  # https://data.cityofnewyork.us/City-Government/Parking-Violations-Issued-Fiscal-Year-2022/pvqr-7yc4
  url = "https://data.cityofnewyork.us/api/views/pvqr-7yc4/rows.csv?accessType=DOWNLOAD"

  tfile = tempfile(fileext = ".csv")
  res = httr::GET(
    url,
    httr::content_type(type = "text/csv"),
    httr::write_disk(tfile),
    httr::progress())

}
# PHILA
philly_parking_finces = function() {
  #
  main_url = "https://www.opendataphilly.org/dataset/parking-violations"
  url = paste0("https://phl.carto.com/api/v2/sql?",
               "filename=parking_violations&format=csv&",
               "skipfields=cartodb_id,the_geom,the_geom_webmercator&q=SELECT%20*%20FROM%20parking_violations%20WHERE%20issue_datetime%20%3E=%20%272017-07-01%27%20AND%20issue_datetime%20%3C%20%272020-01-01%27"
  )
}
