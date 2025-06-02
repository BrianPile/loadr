#' Get test info from the filename
#'
#' @param file Path to an LIV data file.
#'
#' @returns A single row data frame
#'
#' @examples

.get_test_info = function(file) {

  # determine the test station
  if (stringr::str_detect(file, "_LIV[.]xlsx$")) {
    test_station = "MOIV1"
  } else if (stringr::str_detect(file, "_LIV[.]csv$")) {
    test_station = "MOIV3"
  } else if (stringr::str_detect(file, "-LIV[.]csv$")) {
    test_station = "ETS01"
    message("this is ETS01")
  } else {
    test_station = "???"
    stop("Could not determine the test station from the file name!")
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
    test_id = dplyr::case_when(
      test_station == "ETS01" ~ stringr::str_extract(file, "-(\\d{2})-LIV", group = 1),
      .default = stringr::str_extract(file, "_(\\d{2})_LIV", group = 1),
    ),
    temperature = dplyr::case_when(
      test_station == "ETS01" ~ stringr::str_extract(file, "-(\\d{2})[.]?\\d?C-", group = 1),
      .default = stringr::str_extract(file, "_(\\d{2})[.]?\\dC?", group = 1),
    )
  )

  return(df_cond)

}


#' Load LIV files
#'
#' @param file Path to an LIV data file.
#'
#' @returns A data frame
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' filepath <- system.file("extdata",
#'    "DT7_25FC01497-CF5_B4_C6-CH1-Front-Pad-Stage_500mA_50C_NAV_00_LIV.xlsx",
#'    package = "loadr")
#' if (file.exists(filepath)) {
#'   df <- load_liv(filepath)
#'   head(df)
#' }

load_liv = function(file) {

  # get test info
  df_info = .get_test_info(file)

  # read data
  if (df_info$test_station == "MOIV1") {

    df = readxl::read_excel(
      path = file,
      skip = 33
    )

    # select and rename columns
    df = df |>
      dplyr::select(
        current = .data$`I/mA`,
        power = .data$`P/mW`,
        voltage = .data$`V/V`,
        mpd_current = .data$`Ch2I/mA`
      )

    # convert to SI units
    df = df |>
      dplyr::mutate(
        current = .data$current * 1e-3,
        power = .data$power * 1e-3,
        mpd_current = .data$mpd_current * 1e-3

      )
  } else if (df_info$test_station == "MOIV3") {

    df = data.table::fread(
      file = file,
      skip = 12
    ) |>
      tibble::as_tibble()

    # select and rename columns
    df = df |>
      dplyr::select(
        current = .data$`set_current[mA]`,
        power = .data$`power[mW]`,
        voltage = .data$`voltage[V]`,
      ) |>
      dplyr::mutate(mpd_current = NA) # MOIV3 does not have MPD capability?

    # convert to SI units
    df = df |>
      dplyr::mutate(
        current = .data$current * 1e-3,
        power = .data$power * 1e-3
      )
  } else if (df_info$test_station == "ETS01") {

    df = data.table::fread(file) |>
      tibble::as_tibble()

    # select and rename columns
    df = df |>
      dplyr::select(
        current = .data$`current[mA]`,
        power = .data$`power[mW]`,
        voltage = .data$`voltage[V]`,
        mpd_current = .data$`mpd_current[mA]`
      )

    # convert to SI units
    df = df |>
      dplyr::mutate(
        current = .data$current * 1e-3,
        power = .data$power * 1e-3,
        mpd_current = .data$mpd_current * 1e-3
      )

  }

  # combine columns of test info data & measurement data
  df = dplyr::bind_cols(df_info, df)

  return(df)

}
