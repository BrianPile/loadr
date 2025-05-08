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

  # determine the test station
  if (stringr::str_detect(file, "_LIV[.]xlsx$")) {
    test_station = "MOIV1"
  } else if (stringr::str_detect(file, "_LIV[.]csv$")) {
    test_station = "MOIV3"
  } else {
    test_station = "???"
  }

  # extract dut info from file name
  work_order = stringr::str_extract(file, "WO\\d{2}-\\d{4}")
  fc_id = stringr::str_extract(file, "\\d{2}FC\\d{5}")
  ch = stringr::str_extract(file, "CH([1-4])", group = 1)
  dut_id = paste(sep = "-", fc_id, ch)
  test_id = stringr::str_extract(file, "_(\\d{2})_LIV", group = 1)
  temperature = stringr::str_extract(file, "_(\\d{2})[.]?\\dC?", group = 1)

  # read data
  if (test_station == "MOIV1") {

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
  } else if (test_station == "MOIV3") {

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
      dplyr::mutate(mpd_current = 0) # MOIV3 does not have MPD capability?

    # convert to SI units
    df = df |>
      dplyr::mutate(
        current = .data$current * 1e-3,
        power = .data$power * 1e-3
      )
  }


  # add dut info
  df = df |>
    dplyr::mutate(
      work_order = work_order,
      test_station = test_station,
      fc_id = fc_id,
      ch = ch,
      dut_id = dut_id,
      test_id = test_id,
      temperature = temperature,
      .before = .data$current
    )

  return(df)

}
