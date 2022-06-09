# Fig styles

### Color pallettes ###

# Strada Blue, Bright blue, new navy, sea green
colr <- c("#0095C8", "#00AEFB", "gray", "#003F6B", "#00BF96")

# For NB details
rcolr <- c("#003F6B", "#0095C8", "#00BF96", "gray70", "gray30")

# For subg figs
colr2 <- c("#0095C8", "#00AEFB", "gray", "#10363D", "#003F6B", "#00BF96")


### Fig types ###

# Single var bar, flipped

strada1 <- function(tabl) {

  ggplot(tabl, aes(var, perc, label=scales::percent(perc, 1.0))) +
    geom_col(fill=colr[1]) +
    geom_text(
      color = "white",
      hjust = 1.3,
      size = 3
    ) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = rev) +
    theme_minimal() +
    labs(x="", y="") +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank()
    )
}

# 5pt stacked, color pallette 1

strada2 <- function(tabl, txt) {

  ggplot(tabl, aes(var2, perc, fill=var)) +
    geom_col() +
    geom_text(
      aes(label=scales::percent(perc, 1.0)),
      position = position_stack(vjust = .5),
      color = "white",
      size = 3
    )  +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = colr) +
    theme_minimal() +
    labs(x=txt, y="") +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      legend.title = element_blank()
    )
}

# Subg bars

strada3 <- function(tabl) {

  ggplot(tabl, aes(grp, perc, label=scales::percent(perc, 1.0))) +
    geom_col(fill=colr2) +
    geom_text(
      color = "white",
      hjust = 1.3,
      size = 3
    ) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = rev) +
    theme_minimal() +
    labs(x="", y="") +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank()
    )
}

# Net benefit

strada4 <- function(tabl) {

  ggplot(tabl, aes(var, nb)) +
    geom_col(fill=colr[1]) +
    geom_text(aes(label=round(nb*100, 0)),
      #position = position_stack(vjust = .5),
      color = "white",
      size = 3,
      vjust = 1.5
    )  +
    #scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = rev) +
    scale_fill_manual(values = colr) +
    theme_minimal() +
    labs(x="", y="") +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      #axis.text.x = element_blank(),
      legend.title = element_blank()
    )
}

strada5 <- function(t1, t2) {

  ggplot() +
    geom_col(data=t1, aes(x=str_wrap(label, 30), y=perc, fill = var22)) +
    geom_col(data=t2, aes(x=str_wrap(label, 30), y = dummy, ), fill = "white", color = "grey50", alpha = 0) +
    geom_text(data=t2, aes(y = pos, x=str_wrap(label, 30), label=round(nb*100, 0)), color="grey10") +
    geom_text(
      data=t1,
      aes(x=str_wrap(label, 30), y=perc, label=scales::percent(perc, 1.0), fill=var22),
      position = position_stack(vjust=0.5),
      color = "white",
      size = 3
    )  +
    scale_fill_manual(values = rcolr) +
    scale_y_continuous(expand = c(-0.1, .2)) +
    labs(x = "", y = "") +
    theme_minimal() +
    coord_flip() +
    theme(axis.text.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank()
    ) +
    guides(fill = guide_legend(reverse = TRUE))
}
