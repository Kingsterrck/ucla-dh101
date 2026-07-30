# ---------------------------------------------------------------
# Wave totals by California county (raw counts, no normalization)
# Input : ca_covid_long.csv (from diff.R)
# Output: one row per county, cases + deaths summed per wave
#         -> join to the counties shapefile, divide by population there
# ---------------------------------------------------------------

long_path <- "ca_covid_long.csv"

waves <- data.frame(
  wave  = 1:5,
  label = c("Winter 2020-21", "Delta", "Omicron", "BA.2/BA.5", "Winter 2022-23"),
  start = as.Date(c("2020-11-01", "2021-06-01", "2021-12-01", "2022-04-01", "2022-11-01")),
  end   = as.Date(c("2021-02-28", "2021-10-31", "2022-02-28", "2022-08-31", "2023-02-28")),
  stringsAsFactors = FALSE
)
waves$days <- as.numeric(waves$end - waves$start) + 1

long <- read.csv(long_path, check.names = FALSE)
long$date <- as.Date(long$date)

gap <- waves[waves$start < min(long$date) | waves$end > max(long$date), ]
if (nrow(gap)) stop("Data does not cover wave(s): ", paste(gap$wave, collapse = ", "))

per_wave <- do.call(rbind, lapply(seq_len(nrow(waves)), function(i) {
  w   <- waves[i, ]
  sub <- long[long$date >= w$start & long$date <= w$end, ]

  agg <- aggregate(cbind(cases = sub$new_cases, deaths = sub$new_deaths),
                   by = list(county = sub$county), FUN = sum, na.rm = TRUE)

  # every county should contribute one row per day of the window
  n <- aggregate(list(n = sub$date), by = list(county = sub$county), FUN = length)
  short <- n$county[n$n != w$days]
  if (length(short)) warning("Wave ", w$wave, ": incomplete series for ",
                             paste(short, collapse = ", "))

  agg$wave  <- w$wave
  agg$label <- w$label
  agg$days  <- w$days
  agg
}))

per_wave <- per_wave[order(per_wave$wave, per_wave$county), ]
write.csv(per_wave, "ca_waves_long.csv", row.names = FALSE, na = "")

# --- wide: one row per county, ready for a 1:1 join -------------
# Field names kept under 10 characters so they survive a shapefile
# join. A file geodatabase allows 64, but the DBF format truncates.
counties <- sort(unique(long$county))
wide <- data.frame(county = counties, stringsAsFactors = FALSE)
for (i in seq_len(nrow(waves))) {
  w <- per_wave[per_wave$wave == i, ]
  w <- w[match(counties, w$county), ]
  wide[[paste0("w", i, "_cases")]]  <- w$cases
  wide[[paste0("w", i, "_deaths")]] <- w$deaths
}
write.csv(wide, "ca_waves_wide.csv", row.names = FALSE, na = "")

cat("\nwide: ", nrow(wide), " counties x ", ncol(wide), " fields\n", sep = "")
cat("\n--- Wave windows ---\n")
print(waves[, c("wave", "label", "start", "end", "days")], row.names = FALSE)

cat("\n--- Statewide totals ---\n")
tot <- aggregate(cbind(cases, deaths) ~ wave + label + days, per_wave, sum)
tot$per_day <- round(tot$cases / tot$days)
print(tot[order(tot$wave), c("wave", "label", "days", "cases", "deaths", "per_day")],
      row.names = FALSE)