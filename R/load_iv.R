#' Load IV files
#'
#' @param file Path to the IV file
#'
#' @returns A data frame
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' filepath <- system.file("extdata", "25FC01497_CH1.csv", package = "loadr")
#' if (file.exists(filepath)) {
#'   df <- load_iv(filepath)
#'   head(df)
#' }

load_iv = function(file) {

  # determine the test station (unclear from file names / content)
  test_station = "?"

  # extract dut info from file name
  work_order = stringr::str_extract(file, "WO\\d{2}-\\d{4}")
  fc_id = stringr::str_extract(file, "\\d{2}FC\\d{1,5}")
  fc_numeric = stringr::str_remove(fc_id, "^\\d{2}FC") |> readr::parse_number()
  fc_char = sprintf(fmt = "%05d", fc_numeric)
  fc_id = paste0("25FC", fc_char)
  ch = stringr::str_extract(file, "CH([1-4])", group = 1)
  # dut_id = paste(sep = "-", fc_id, ch)
  temperature = stringr::str_extract(file, "_(\\d{2})[.]?\\dC?", group = 1)
  test_id = stringr::str_extract(file, "_(\\d{2})_LIV", group = 1)

  # detect header
  lines = readr::read_lines(file, n_max = 10)
  header_line = which(stringr::str_detect(lines, "Vanodf"))

  # read data
  df = data.table::fread(
    file = file,
    skip = header_line -1
  ) |>
    tibble::as_tibble()

  # select and rename columns
  df = df |>
    dplyr::select(
      current = .data$Ianodf,
      voltage = .data$Vanodf,
    )

  # add dut info
  df = df |>
    dplyr::mutate(
      work_order = work_order,
      test_station_iv = test_station,
      fc_id = fc_id,
      ch = ch,
      temperature = temperature,
      test_id_iv = test_id,
      .before = .data$current
    )

  return(df)

}
