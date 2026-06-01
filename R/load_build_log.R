# load ELS G2 build log (aka PT11B log)
# TODO: AI documentation

#' Load the ELS G2 Build List
#'
#' @param path A path to the build log
#'
#' @returns A data frame
#' @export
#'
# @examples
load_build_log = function(path) {
  df_build = readxl::read_xlsx(
    # path = here("../../Documents/ELS Gen2 project repository/test and measurement/assy info/PT11B build log.xlsx"),
    path = path,
    sheet = "fc_log"
  ) |>
    tidyr::pivot_longer(
      cols = tidyselect::matches("^ld_id_ch\\d$"),
      names_to = "ch",
      values_to = "ld_id",
      names_prefix = "ld_id_ch"
    ) |>
    dplyr::mutate(ch = as.numeric(.data$ch)) |>
    tidyr::drop_na(.data$ld_id)

  return(df_build)
}
