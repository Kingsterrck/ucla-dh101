library(tidyverse)

confirmed <- read.csv("time_series_covid19_confirmed_US.csv")

confirmed <- confirmed |>
    filter(Province_State == "California" & Admin2 != "Out of CA" & Admin2 != "Unassigned") |>
    select(Admin2, contains("."))
colnames(confirmed)[-1] <- substring(colnames(confirmed)[-1], 2)

confirmed <- confirmed |>
    pivot_longer(cols = -1, names_to = "Date", values_to = "value") |>
    pivot_wider(names_from = 1, values_from = value)
write.csv(confirmed, file = "confirmed_ca.csv", row.names = FALSE)


death <- read.csv("time_series_covid19_deaths_US.csv")

death <- death |>
    filter(Province_State == "California" & Admin2 != "Out of CA" & Admin2 != "Unassigned") |>
    select(Admin2, contains("."))
colnames(death)[-1] <- substring(colnames(death)[-1], 2)

death <- death |>
    pivot_longer(cols = -1, names_to = "Date", values_to = "value") |>
    pivot_wider(names_from = 1, values_from = value)
write.csv(death, file = "death_ca.csv", row.names = FALSE)