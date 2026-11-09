make_sdtm <- function(route = "ORAL") {
  ids <- c("STUDY-001", "STUDY-002", "STUDY-003")
  times <- c(0, 1, 4)
  list(
    dm = data.frame(
      usubjid = ids,
      actarm = c("Drug Low", "Drug High", "Placebo")
    ),
    ex = data.frame(
      usubjid = ids,
      exdose = c(10, 20, 0),
      exdosu = "mg",
      exroute = route,
      visit = "DAY 1",
      visitnum = 1
    ),
    pc = data.frame(
      usubjid = rep(ids, each = length(times)),
      pcdtc = rep(paste0("2024-01-01T0", times, ":00"), length(ids)),
      pctptnum = rep(times, length(ids)),
      pctest = "DRUGX",
      pcspec = "PLASMA",
      pcstresc = c(0, 5, 2, 0, 10, 4, 0, 0, 0),
      pcstresu = "ng/mL",
      pclloq = 0.1,
      visitnum = 1
    )
  )
}
