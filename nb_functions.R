# NB function

# Value

nb_value <- function(sch.w) {

  q30nb <- as.data.frame(svytable(~q30, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q30"
      ) %>%
    select(
      var, val = q30, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q30nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q46nb <- as.data.frame(svytable(~q46, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q46"
    ) %>%
    select(
      var, val = q46, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q46nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q51nb <- as.data.frame(svytable(~q51, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q51"
    ) %>%
    select(
      var, val = q51, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q51nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q36nb <- as.data.frame(svytable(~q36, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q36"
    ) %>%
    select(
      var, val = q36, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q36nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  nb_value <- bind_rows(q30nb, q46nb, q51nb, q36nb)

  nb_value <- nb_value %>%
    mutate(
      nb = (val5 + val4) - (val2 + val1)
    ) %>%
    summarize(
      val = "Value",
      nb = mean(nb)
      )

 return(nb_value)

}

# Life impact

nb_life <- function(sch.w) {

  q34nb <- as.data.frame(svytable(~q34, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q34"
    ) %>%
    select(
      var, val = q34, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q34nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q35nb <- as.data.frame(svytable(~q35, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q35"
    ) %>%
    select(
      var, val = q35, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q35nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q48nb <- as.data.frame(svytable(~q48, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q48"
    ) %>%
    select(
      var, val = q48, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q48nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q49nb <- as.data.frame(svytable(~q49, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q49"
    ) %>%
    select(
      var, val = q49, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q49nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q50nb <- as.data.frame(svytable(~q50, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q50"
    ) %>%
    select(
      var, val = q50, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q50nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")


  nb_life <- bind_rows(q34nb, q35nb, q48nb, q49nb, q50nb)

  nb_life <- nb_life %>%
    mutate(
      nb = (val5 + val4) - (val2 + val1)
    ) %>%
    summarize(
      var = "Life",
      nb = mean(nb)
      )

  return(nb_life)

}

# Career satisfaction

nb_career <- function(sch.w) {

  ### START HERE ###

  q34nb <- as.data.frame(svytable(~q34, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q34"
    ) %>%
    select(
      var, val = q34, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q34nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q35nb <- as.data.frame(svytable(~q35, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q35"
    ) %>%
    select(
      var, val = q35, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q35nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q48nb <- as.data.frame(svytable(~q48, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q48"
    ) %>%
    select(
      var, val = q48, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q48nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q49nb <- as.data.frame(svytable(~q49, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q49"
    ) %>%
    select(
      var, val = q49, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q49nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q50nb <- as.data.frame(svytable(~q50, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q50"
    ) %>%
    select(
      var, val = q50, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q50nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")


  nb_life <- bind_rows(q34nb, q35nb, q48nb, q49nb, q50nb)

  nb_life <- nb_life %>%
    mutate(
      nb = (val5 + val4) - (val2 + val1)
    ) %>%
    summarize(nb = mean(nb))

  return(nb_life)

}

# Skill dev

# Affinity

