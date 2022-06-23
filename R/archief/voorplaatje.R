df <- read_csv("../data/H5_NSGL_NF1_scen_2.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = recode(
      compartment,
      "S" = "Susceptible",
      "I" = "Infected",
      "R" = "Removed"),
    compartment = fct_inorder(compartment)
  ) %>%
  filter(step_size != "h=0.625")


palette <- c("#e27c7c", "#6d4b4b", "#599e94")

plot <- df %>%
  ggplot(aes(x = t, y = value, color = step_size, group = step_size)) +
  geom_point(size = 2, show.legend = FALSE) +
  geom_line(size = 0.25, show.legend = FALSE) +
  scale_color_manual(values = palette) +
  facet_wrap(~ compartment) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    strip.text.x = element_blank()
  )
ggsave("plots/voorplaatje.png", plot, width = 9, height = 3)
