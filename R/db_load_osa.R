#' Load OSA metrics from the Singapore 2026 database table
#'
#' Retrieves optical spectrum analyzer (OSA) summary metrics from the
#' `OSA_SG_2026` database table for one or more FC IDs. The function
#' standardizes column names, selects the relevant metadata and OSA metric
#' columns, and returns the matching records as a local data frame.
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
#' @return A data frame containing OSA metrics with columns:
#'   `file_name`, `upload_time`, `file_created_time`, `work_order`,
#'   `measurement_step`, `int_cell_id`, `int_bar_id`, `int_die_id`, `fc_id`,
#'   `ch`, `temperature`, `If`, `Lp`, `SMSR`, and `Pp`.
#'
#' @details
#' The returned metric columns are:
#' \itemize{
#'   \item `If`: injection current.
#'   \item `Lp`: first peak wavelength, in nm.
#'   \item `SMSR`: side-mode suppression ratio, in dB.
#'   \item `Pp`: first peak power, in dBm.
#' }
#'
#' The function opens a database connection using `connect_to_database()` and
#' closes it on exit with `DBI::dbDisconnect()`. Data manipulation is performed
#' using `dplyr` before the filtered result is collected into R.
#'
#' @examples
#' \dontrun{
#' db_load_osa_metrics()
#'
#' db_load_osa_metrics(fc_ids = c("26FC00933", "26FC00934"))
#'
#' db_load_osa_metrics(fc_ids = "26FC00933", make_unique = TRUE)
#' }
#'
#' @export
db_load_osa_metrics = function(fc_ids = c("26FC00933"), make_unique = FALSE) {

  con = connect_to_database()
  on.exit(DBI::dbDisconnect(con))

  tbl0 = dplyr::tbl(con, "OSA_SG_2026") |>
    janitor::clean_names(case = "old_janitor")

  tbl1 = tbl0 |>
    dplyr::select(
      .data$file_name, .data$upload_time, .data$file_created_time,
      .data$work_order, .data$measurement_step,
      int_cell_id = .data$cell_id,
      int_bar_id = .data$bar_id,
      int_die_id = .data$die_id,
      .data$fc_id,
      ch = .data$channel,
      .data$temperature,
      If = .data$current,
      Lp = .data$x1st_peak_wavelength_nm,
      SMSR = .data$smsr_db,
      Pp = .data$x1st_peak_power_dbm
    )

  df = tbl1 |>
    dplyr::filter(
      .data$fc_id %in% fc_ids
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      ch = readr::parse_number(.data$ch) + 1,
      temperature = readr::parse_number(.data$temperature)
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
