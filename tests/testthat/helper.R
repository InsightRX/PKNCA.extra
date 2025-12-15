local_set_global_plot_theme <- function(theme = "irx", env = parent.frame()) {
  set_global_plot_theme(theme)
  withr::defer(reset_global_plot_theme(), envir = env)
}

local_create_sqlite_db <- function(db = fs::file_temp(), env = parent.frame()) {
  conn <- withr::local_db_connection(
    DBI::dbConnect(RSQLite::SQLite(), db),
    .local_envir = env
  )
  DBI::dbWriteTable(conn, "mtcars", mtcars)
  DBI::dbWriteTable(conn, "trees", trees)
}
