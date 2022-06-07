# Fig styles

### Color pallettes ###

# Strada Blue, Bright blue, new navy, sea green
colr <- c("#0095C8", "#00AEFB", "gray", "#003F6B", "#00BF96")

# Bright blue, slate gray, white
colr2 <- c("#00AEFB","#10363D", "#FFFFFF")

### Fig types ###

# Single var bar, flipped

strada1 <- function(tabl) {

  ggplot(tabl, aes(var, perc, label=scales::percent(perc, 1.0))) +
    geom_col(fill=colr[1]) +
    geom_text(
      hjust = -0.3,
      size = 2
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
