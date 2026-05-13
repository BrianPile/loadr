#' Load LIV data from the Singapore 2026 database table
#'
#' Retrieves LIV measurements from the `LIV_SG_2026` database table for one or
#' more FC IDs. The function standardizes column names, converts current from
#' mA to A and power from mW to W, and returns the selected records as a local
#' data frame.
#'
#' When `make_unique = TRUE`, duplicate uploads are reduced in two steps:
#' first, only the earliest upload is kept for each unique `file_name`; second,
#' only the latest upload is kept for each `fc_id`, channel, and temperature
#' combination.
#'
#' @param fc_ids Character vector of FC IDs to load. Defaults to
#'   `c("26FC00933")`.
#' @param make_unique Logical scalar. If `TRUE`, keeps a single representative
#'   record set per file and per `fc_id`/channel/temperature combination. If
#'   `FALSE`, returns all matching records. Defaults to `FALSE`.
#'
#' @return A data frame containing LIV measurements with columns:
#'   `file_name`, `upload_time`, `file_created_time`, `work_order`,
#'   `measurement_step`, `int_cell_id`, `int_bar_id`, `int_die_id`, `fc_id`,
#'   `ch`, `temperature`, `current`, `power`, `voltage`, and `pd_current`.
#'
#' @details
#' The function opens a database connection using `connect_to_database()` and
#' closes it on exit with `DBI::dbDisconnect()`. Data manipulation is performed
#' using `dplyr` before the filtered result is collected into R.
#'
#' @examples
#' \dontrun{
#' db_load_liv()
#'
#' db_load_liv(fc_ids = c("26FC00933", "26FC00934"))
#'
#' db_load_liv(fc_ids = "26FC00933", make_unique = TRUE)
#' }
#' @export
db_load_liv = function(fc_ids = c("26FC00933"), make_unique = FALSE) {

  con = connect_to_database()
  on.exit(DBI::dbDisconnect(con))

  tbl0 = dplyr::tbl(con, "LIV_SG_2026") |>
    janitor::clean_names(case = "old_janitor")

  tbl1 = tbl0 |>
    dplyr::mutate(
      current = .data$current_ma * 1e-3,
      power = .data$power_mw * 1e-3
    ) |>
    dplyr::select(
      .data$file_name, .data$upload_time, .data$file_created_time,
      .data$work_order, .data$measurement_step,
      int_cell_id = .data$cell_id,
      int_bar_id = .data$bar_id,
      int_die_id = .data$die_id,
      .data$fc_id,
      ch = .data$channel,
      .data$temperature,
      .data$current,
      .data$power,
      voltage = .data$voltage_v,
      .data$pd_current
    )

  df = tbl1 |>
    dplyr::filter(
      .data$fc_id %in% fc_ids
    ) |>
    dplyr::collect() |>
    mutate(
      ch = readr::parse_number(ch) + 1,
      temperature = readr::parse_number(temperature)
    )

  if (make_unique) {
    df = df |>
      dplyr::filter( # for each unique file name, keep the one uploaded first
        .by = c(.data$file_name),
        .data$upload_time == dplyr::first(.data$upload_time, order_by = .data$upload_time)
      ) |>
      dplyr::filter( # for each FC_ID-Channel-temperature combination, keep the last file created
        .by = c(.data$fc_id, .data$ch, .data$temperature),
        .data$upload_time == dplyr::last(.data$upload_time, order_by = .data$upload_time)
      )
  }

  return(df)

}
