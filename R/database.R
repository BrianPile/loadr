#' Connect to the POET MariaDB database
#'
#' Opens a connection to the POET MariaDB database using `DBI::dbConnect()` and
#' the `RMariaDB` driver.
#'
#' @return A MariaDB database connection object of class `MariaDBConnection`.
#'
#' @details
#' The returned connection should be closed after use with
#' `DBI::dbDisconnect()`. Functions that call `connect_to_database()` should
#' typically use `on.exit(DBI::dbDisconnect(con))` to ensure the connection is
#' closed.
#'
#' @examples
#' \dontrun{
#' con <- connect_to_database()
#' DBI::dbDisconnect(con)
#' }
connect_to_database = function() {
  con = DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = "192.168.10.185",
    port = 3307,
    user = "poetuser",
    password = "P.Technol0g1es.Pt3Ltd",
    dbname = "poetuser_"
  )

  return(con)
}
