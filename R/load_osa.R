#' Get test info from the OSA filename
#'
#' @param file Path to an OSA data file.
#'
#' @returns A single row data frame
#'
#' @examples

.osa_get_test_info = function(file) {

  # determine the test station
  if (stringr::str_detect(file, "_OSA[.]xlsx$")) {
    test_station = "MOIV1"
  } else if (stringr::str_detect(file, "_OSA[.]csv$")) {
    test_station = "MOIV3"
  } else if (stringr::str_detect(file, "-OSA[.]csv$")) {
    test_station = "ETS01"
  } else {
    test_station = "???"
    stop("Could not determine the OSA test station from the file name!")
  }

  # extract test info from file name and put in a data frame
  df_cond = tibble::tibble(
    work_order = stringr::str_extract(file, "WO\\d{2}-\\d{4}"),
    test_station = test_station,
    fc_id = dplyr::case_when(
      test_station == "ETS01" ~ stringr::str_extract(basename(file), "^([^-]+-[^-]+-[^-]+-[^-]+).*[.]csv", group = 1),
      .default = stringr::str_extract(file, "\\d{2}FC\\d{5}")
    ),
    ch = stringr::str_extract(file, "CH([1-4])", group = 1),
    temperature = dplyr::case_when(
      test_station == "ETS01" ~ stringr::str_extract(file, "-(\\d{2})[.]?\\d?C-", group = 1),
      .default = stringr::str_extract(file, "_(\\d{2})[.]?\\d?C", group = 1),
    ),
    If = stringr::str_extract(file, "[-_](\\d{2,3})[.]?\\d?\\d?mA[-_]", group = 1) |> as.numeric() * 1e-3,
    test_id = dplyr::case_when(
      test_station == "ETS01" ~ stringr::str_extract(file, "(?<=-)([0-9]{2}(?:_.*)?)(?=-)-OSA[.]csv$", group = 1),
      .default = stringr::str_extract(file, "_(\\d{2})_OSA", group = 1),
    )
  )

  return(df_cond)

}


#' Load OSA files
#'
#' @param file Path to the OSA file
#'
#' @returns A data frame
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' filepath <- system.file("extdata",
#'    "DT7_25FC01497-CF5_B4_C6-CH1-WG-Pad-Stage_100mA_50C_NAV_00_OSA.xlsx",
#'    package = "loadr")
#' if (file.exists(filepath)) {
#'   df <- load_osa(filepath)
#'   head(df)
#' }

load_osa = function(file) {

  # get test info
  df_info = .osa_get_test_info(file)

  # read data
  if (df_info$test_station == "MOIV1") {

    df = readxl::read_excel(
      path = file,
      sheet = "DevID_NEW",
      skip = 35
    )

    # select and rename columns
    df = df |>
      dplyr::select(
        wavelength = .data$nm,
        power = .data$dBm,
      )

  } else if (df_info$test_station == "MOIV3") {

    df = data.table::fread(
      file = file,
      skip = 21
    ) |>
      tibble::as_tibble()

    # select and rename columns
    df = df |>
      dplyr::select(
        wavelength = .data$`wavelength[m]`,
        power = .data$`level[dBm]`
      )

    # convert to preferred units
    df = df |>
      dplyr::mutate(
        wavelength = .data$wavelength / 1e-9
      )
  } else if (df_info$test_station == "ETS01") {

    df = data.table::fread(file) |>
      tibble::as_tibble()

    # select and rename columns
    df = df |>
      dplyr::select(
        wavelength = .data$`wavelength[nm]`,
        power = .data$`power[dBm]`,
      )

  }

  # combine columns of test info data & measurement data
  df = dplyr::bind_cols(df_info, df) |>
    dplyr::rename(
      test_station_osa = .data$test_station,
      test_id_osa = .data$test_id
    )

  return(df)

}
