# NB function

# Value

dnb_value <- function(sch.w) {

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
    )

 return(nb_value)

}

# Life impact

dnb_life <- function(sch.w) {

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
    )

  return(nb_life)

}

# Career satisfaction

dnb_career <- function(sch.w) {

  q33nb <- as.data.frame(svytable(~q33, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q33"
    ) %>%
    select(
      var, val = q33, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q33nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q47nb <- as.data.frame(svytable(~q47, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q47"
    ) %>%
    select(
      var, val = q47, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q47nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q53nb <- as.data.frame(svytable(~q53, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q53"
    ) %>%
    select(
      var, val = q53, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q53nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q31nb <- as.data.frame(svytable(~q31, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q31"
    ) %>%
    select(
      var, val = q31, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q31nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q32nb <- as.data.frame(svytable(~q32, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q32"
    ) %>%
    select(
      var, val = q32, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q32nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q52nb <- as.data.frame(svytable(~q52, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q52"
    ) %>%
    select(
      var, val = q52, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q52nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q102nb <- as.data.frame(svytable(~q102, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q102"
    ) %>%
    select(
      var, val = q102, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q102nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  nb_career <- bind_rows(q33nb, q47nb, q53nb, q31nb, q32nb, q52nb, q102nb)

  nb_career <- nb_career %>%
    mutate(
      nb = (val5 + val4) - (val2 + val1)
    )

  return(nb_career)

}

# Skill dev

dnb_skill <- function(sch.w) {

  q71nb <- as.data.frame(svytable(~q71, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q71"
    ) %>%
    select(
      var, val = q71, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q71nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q72nb <- as.data.frame(svytable(~q72, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q72"
    ) %>%
    select(
      var, val = q72, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q72nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q73nb <- as.data.frame(svytable(~q73, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q73"
    ) %>%
    select(
      var, val = q73, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q73nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q74nb <- as.data.frame(svytable(~q74, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q74"
    ) %>%
    select(
      var, val = q74, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q74nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q75nb <- as.data.frame(svytable(~q75, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q75"
    ) %>%
    select(
      var, val = q75, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q75nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q76nb <- as.data.frame(svytable(~q76, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q76"
    ) %>%
    select(
      var, val = q76, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q76nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q77nb <- as.data.frame(svytable(~q77, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q77"
    ) %>%
    select(
      var, val = q77, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q77nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q78nb <- as.data.frame(svytable(~q78, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q78"
    ) %>%
    select(
      var, val = q78, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q78nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q79nb <- as.data.frame(svytable(~q79, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q79"
    ) %>%
    select(
      var, val = q79, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q79nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q80nb <- as.data.frame(svytable(~q80, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q80"
    ) %>%
    select(
      var, val = q80, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q80nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q81nb <- as.data.frame(svytable(~q81, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q81"
    ) %>%
    select(
      var, val = q81, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q81nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  nb_skill <- bind_rows(q71nb, q72nb, q73nb, q74nb, q75nb, q76nb, q77nb, q78nb, q79nb, q80nb, q81nb)

  nb_skill <- nb_skill %>%
    mutate(
      nb = (val5 + val4) - (val2 + val1)
    )

  return(nb_skill)

}

# Affinity


dnb_affinity <- function(sch.w) {

  q119nb <- as.data.frame(svytable(~q119, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q119"
    ) %>%
    select(
      var, val = q119, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q119nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q120nb <- as.data.frame(svytable(~q120, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q120"
    ) %>%
    select(
      var, val = q120, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q120nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q121nb <- as.data.frame(svytable(~q121, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q121"
    ) %>%
    select(
      var, val = q121, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q121nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q122nb <- as.data.frame(svytable(~q122, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q122"
    ) %>%
    select(
      var, val = q122, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q122nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q123nb <- as.data.frame(svytable(~q123, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q123"
    ) %>%
    select(
      var, val = q123, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q123nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  q124nb <- as.data.frame(svytable(~q124, sch.w)) %>%
    mutate(
      perc = Freq/sum(Freq),
      var = "q124"
    ) %>%
    select(
      var, val = q124, perc
    ) %>%
    pivot_wider(
      names_from = val,
      values_from = perc
    )

  names(q124nb)[2:6] <- c("val1", "val2", "val3", "val4", "val5")

  nb_affinity <- bind_rows(q119nb, q120nb, q121nb, q122nb, q123nb, q124nb)

  nb_affinity <- nb_affinity %>%
    mutate(
      nb = (val5 + val4) - (val2 + val1)
    )

  return(nb_affinity)

}

