#' Load OSA metrics from the Singapore 2026 database table
#'
#' Retrieves optical spectrum analyzer (OSA) summary metrics from the
#' `OSA_SG_2026` database table. Results can optionally be filtered by FC ID
#' and/or work order. The function standardizes column names, selects the
#' relevant metadata and OSA metric columns, and returns the matching records as
#' a local data frame.
#'
#' When `make_unique = TRUE`, duplicate uploads are reduced in two steps:
#' first, only the earliest upload is kept for each unique `file_name`; second,
#' only the latest upload is kept for each `fc_id`, channel, and temperature
#' combination.
#'
#' @param fc_ids Optional character vector of FC IDs to load. If `NULL`, no
#'   filtering by FC ID is applied. Defaults to `NULL`.
#' @param work_orders Optional character vector of work orders to load. If
#'   `NULL`, no filtering by work order is applied. Defaults to `NULL`.
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
#' db_load_osa_metrics(work_orders = "WO12345")
#'
#' db_load_osa_metrics(fc_ids = "26FC00933", work_orders = "WO12345")
#'
#' db_load_osa_metrics(fc_ids = "26FC00933", make_unique = TRUE)
#' }
#'
#' @export
db_load_osa_metrics = function(fc_ids = NULL, work_orders = NULL, make_unique = FALSE) {

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

  if (!is.null(fc_ids)) {
    tbl1 = tbl1 |>
      dplyr::filter(.data$fc_id %in% fc_ids)
  }

  if (!is.null(work_orders)) {
    tbl1 = tbl1 |>
      dplyr::filter(.data$work_order %in% work_orders)
  }

  df = tbl1 |>
    dplyr::collect() |>
    dplyr::mutate(
      ch = readr::parse_number(.data$ch) +
        dplyr::case_when(
          .data$work_order %in% c("WO26-0078", "WO26-0119", "WO26-0167", "WO26-0173") ~ 0,
          (.data$work_order == "WO26-0056") & (.data$fc_id %in% c("26FC02217", "26FC02218", "26FC02219", "26FC02220", "26FC02221")) ~ 0,
          .default = 1
        ),
      temperature = readr::parse_number(.data$temperature)
    )

  if (make_unique) {
    df = df |>
      dplyr::filter(
        .by = c(.data$file_name),
        .data$upload_time == dplyr::first(.data$upload_time, order_by = .data$upload_time)
      ) |>
      dplyr::filter(
        .by = c(.data$fc_id, .data$ch, .data$temperature, .data$If),
        .data$file_created_time == dplyr::last(.data$file_created_time, order_by = .data$file_created_time)
      )
  }

  return(df)

}






