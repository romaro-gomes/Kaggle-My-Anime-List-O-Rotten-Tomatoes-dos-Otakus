fish_encounters
fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen)
