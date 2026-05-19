#' Connect to the POET Database
#'
#' Establishes a connection to the MariaDB database server used for the POET system.
#' It temporarily disables TLS peer verification to allow connections to the local/internal server.
#'
#' @details
#' The function sets the environmental variable `MARIADB_TLS_DISABLE_PEER_VERIFICATION = "1"`
#' before attempting the connection. It connects via specific hardcoded credentials and ports
#' (`3307`) designated for the internal `poetuser_` schema.
#'
#' @return A formal \code{\link[DBI:DBIConnection-class]{DBIConnection}} object
#'   used to interact with the MariaDB database.
#'
#' @seealso \code{\link[DBI]{dbConnect}}, \code{\link[RMariaDB]{MariaDB}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- connect_to_database()
#' # Perform database operations...
#' DBI::dbDisconnect(con)
#' }
connect_to_database = function() {
  Sys.setenv(MARIADB_TLS_DISABLE_PEER_VERIFICATION = "1")
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


