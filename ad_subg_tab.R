# Sub group function

ad_subg_tab <- function(x, sch.w) {

  tot <- as.data.frame(svytable(bquote(~.(as.name(x))), sch.w))
  tot$grp <- "Total"
  tot$var <- "Total"
  names(tot)[1] <- "val"
  tot <- tot %>% group_by(grp, var) %>%
    mutate(perc = Freq/sum(Freq)) %>%
    select(grp, var, val, Freq, perc)


  soc <- as.data.frame(svytable(bquote(~.(as.name(x))+soc), sch.w))
  soc$grp <- "soc"
  names(soc)[2] <- "var"
  names(soc)[1] <- "val"
  soc <- soc %>% group_by(grp, var) %>%
    mutate(perc = Freq/sum(Freq)) %>%
    select(grp, var, val, Freq, perc)

  fgen <- as.data.frame(svytable(bquote(~.(as.name(x))+fgen), sch.w))
  fgen$grp <- "fgen"
  names(fgen)[2] <- "var"
  names(fgen)[1] <- "val"
  fgen <- fgen %>% group_by(grp, var) %>%
    mutate(perc = Freq/sum(Freq)) %>%
    select(grp, var, val, Freq, perc)

  fem <- as.data.frame(svytable(bquote(~.(as.name(x))+q274), sch.w))
  fem$grp <- "fem"
  names(fem)[2] <- "var"
  names(fem)[1] <- "val"
  fem <- fem %>% group_by(grp, var) %>%
    mutate(perc = Freq/sum(Freq)) %>%
    select(grp, var, val, Freq, perc)

  ntrad <- as.data.frame(svytable(bquote(~.(as.name(x))+ntrad), sch.w))
  ntrad$grp <- "ntrad"
  names(ntrad)[2] <- "var"
  names(ntrad)[1] <- "val"
  ntrad <- ntrad %>% group_by(grp, var) %>%
    mutate(perc = Freq/sum(Freq)) %>%
    select(grp, var, val, Freq, perc)

  recg <- as.data.frame(svytable(bquote(~.(as.name(x))+recg), sch.w))
  recg$grp <- "recg"
  names(recg)[2] <- "var"
  names(recg)[1] <- "val"
  recg <- recg %>% group_by(grp, var) %>%
    mutate(perc = Freq/sum(Freq)) %>%
    select(grp, var, val, Freq, perc)

  subg <- bind_rows(tot, soc, fgen, fem, ntrad, recg)

  subg <- subg %>%
    filter(var %in% c(
      "Total", "Yes", "Not White", "Woman"
    )
    ) %>%
    filter(val %in% c("Strongly agree", "Somewhat agree")) %>%
    group_by(grp) %>%
    summarize(perc = sum(perc))

  subg$grp <- factor(subg$grp,
                     levels = c("Total", "soc", "fgen", "fem", "ntrad", "recg"),
                     labels = c("Total", "Students of color", "First generation", "Females", "Non-Traditional", "Recent graduate")
                     )

  return(subg)

  }
