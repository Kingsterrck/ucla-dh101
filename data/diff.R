# # library(tidyverse)
# confirmed <- read.csv("confirmed_ca.csv")
# death <- read.csv("death_ca.csv")
#
# confirmed_delta <- data.frame(
#     Date = confirmed["Date"],
#     matrix(nrow = 1143, ncol = 58)
# )
# colnames(confirmed_delta) <- colnames(confirmed)
#
# death_delta <- data.frame(
#     Date = death["Date"],
#     matrix(nrow = 1143, ncol = 58)
# )
# colnames(death_delta) <- colnames(death)
#
# for (i in 2:1143) {
#   for (j in 2:59) {
#     confirmed_delta[i, j] <- confirmed[i, j] - confirmed[i - 1, j]
#   }
# }
# for (i in 2:1143) {
#   for (j in 2:59) {
#     death_delta[i, j] <- death[i, j] - death[i - 1, j]
#   }
# }
# confirmed_delta[1, -1] <- int(0)
# death_delta[1, -1] <- int(0)
#
# write.csv(confirmed_delta, "confirmed_delta.csv", row.names = FALSE)
# write.csv(death_delta, "death_delta.csv", row.names = FALSE)

# ---------------------------------------------------------------
# Cumulative -> daily new counts, California counties (JHU CSSE)
# Input : Date column (M.D.YY) + one column per county, cumulative
# Output: wide deltas (same shape as input) + long/tidy for ArcGIS
# ---------------------------------------------------------------

# check.names = FALSE keeps "Los Angeles" from becoming "Los.Angeles",
# which is what has to match the shapefile's NAME field later.
confirmed <- read.csv("confirmed_ca.csv", check.names = FALSE)
death     <- read.csv("death_ca.csv",     check.names = FALSE)

to_daily <- function(df, value_name) {
  stopifnot(names(df)[1] == "Date")

  dates    <- as.Date(df$Date, format = "%m.%d.%y")
  counties <- as.matrix(df[, -1, drop = FALSE])
  storage.mode(counties) <- "double"

  if (anyNA(dates)) stop("Unparsed dates: ", paste(df$Date[is.na(dates)], collapse = ", "))
  if (is.unsorted(dates)) stop("Dates are not in ascending order; sort before differencing.")

  # Cumulative series should never decrease. Where it does, the source
  # revised its history. Carry the running maximum forward so the daily
  # figures stay >= 0 and still sum back to the final cumulative total.
  counties[is.na(counties)] <- 0
  monotone <- apply(counties, 2, cummax)

  # rbind(0, diff()) does the whole matrix at once: day 1 = 0 because
  # the JHU series starts at zero, and diff() handles every later row.
  delta <- rbind(0, diff(monotone))
  dimnames(delta) <- dimnames(counties)

  revisions <- sum(diff(counties) < 0, na.rm = TRUE)
  if (revisions > 0)
    message(value_name, ": ", revisions, " downward revisions flattened by cummax()")

  list(dates = dates, raw_date = df$Date, delta = delta)
}

confirmed_d <- to_daily(confirmed, "confirmed")
death_d     <- to_daily(death,     "deaths")

# --- wide output, same layout as the original ------------------
confirmed_delta <- data.frame(Date = confirmed_d$raw_date,
                              confirmed_d$delta, check.names = FALSE)
death_delta     <- data.frame(Date = death_d$raw_date,
                              death_d$delta, check.names = FALSE)

write.csv(confirmed_delta, "confirmed_delta.csv", row.names = FALSE)
write.csv(death_delta,     "death_delta.csv",     row.names = FALSE)

# --- long output, one row per county per day (for ArcGIS) ------
to_long <- function(x, value_name) {
  n <- nrow(x$delta)
  out <- data.frame(
    date   = rep(x$dates, times = ncol(x$delta)),
    county = rep(colnames(x$delta), each = n),
    v      = as.vector(x$delta)
  )
  names(out)[3] <- value_name
  out
}

long <- merge(to_long(confirmed_d, "new_cases"),
              to_long(death_d,     "new_deaths"),
              by = c("date", "county"))
long <- long[order(long$county, long$date), ]

# 7-day trailing mean, computed per county so it never spans a boundary
long$cases_7day <- ave(long$new_cases, long$county, FUN = function(v)
  as.numeric(stats::filter(v, rep(1/7, 7), sides = 1)))

# ISO dates so ArcGIS reads them as dates, not text
long$date <- format(long$date, "%Y-%m-%d")
write.csv(long, "ca_covid_long.csv", row.names = FALSE, na = "")

cat("wide:", nrow(confirmed_delta), "rows x", ncol(confirmed_delta), "cols\n")
cat("long:", nrow(long), "rows\n")