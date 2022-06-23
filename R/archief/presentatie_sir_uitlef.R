library(hrbrthemes)
library(latex2exp)
library(tidyverse)
library(ggpubr)
library(ggtext) 
library(glue)

theme_set(theme_ipsum())

palette <- c("#d11141", "#00b159", "#00aedb")

df <- read_csv("../data/../data/H3_sir_nonstandard.csv") %>%
  select(t, "Susceptible" = `S_h=1.25`, "Infected" = `I_h=1.25`, "Removed" = `R_h=1.25`)


plot <- df %>%
  ggplot(aes(x = t)) +
  geom_area(aes(y = 1), fill = "#8c9da9", alpha = 0.5) +
  geom_vline(aes(xintercept = 0), size = 1.5, linetype = 2) +
  scale_x_continuous(
    breaks = seq(0, 50, 10),
    labels = c("Week 1", paste("Week", seq(10, 50, 10))),
    expand = expansion(c(0, 0))
  ) +
  scale_y_continuous(expand = expansion(c(0, 0.075))) +
  theme(plot.title = element_markdown()) +
  theme(
    plot.title = element_markdown(),
    axis.text.x = element_text(size = 15, vjust = -0.5),
    axis.text.y = element_blank()
  ) +
  labs(
    x = NULL, y = NULL,
    title = "De totale <span style='color:#8c9da9;'>**Bevolking**</span> is constant"
  )
ggsave("plots/Presentatie/uitleg_sir_1.png", plot, width = 8, height = 3)

plot <- df %>%
  pivot_longer(-t) %>%
  mutate(name = name %>% fct_inorder() %>% fct_rev()) %>%
  ggplot(aes(x = t)) +
  geom_area(
    aes(y = value, fill = name, color = name), 
    alpha = 0.5, show.legend = FALSE
  ) +
  geom_vline(aes(xintercept = 0), size = 1.5, linetype = 2) +
  geom_curve(
    aes(x = 27.5, y = 0.31, xend = 25, yend = 0.12),
    arrow = arrow(length = unit(0.05, "npc"))
  ) +
  geom_label(
    aes(x = 27.5, y = 0.35, label = "Infectie sterft langzaam uit"),
    size = 5, hjust = 0, vjust = 1
  ) +
  scale_x_continuous(
    breaks = seq(0, 50, 10),
    labels = c("Week 1", paste("Week", seq(10, 50, 10))),
    expand = expansion(c(0, 0))
  ) +
  scale_y_continuous(expand = expansion(c(0, 0.075))) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette) +
  theme(
    plot.title = element_markdown(),
    axis.text.x = element_text(size = 15, vjust = -0.5),
    axis.text.y = element_blank()
  ) +
  labs(
    x = NULL, y = NULL,
    title = "SIR-model: <span style='color:#00aedb;'>Susceptible</span>, 
    <span style='color:#00b159;'>Infected</span> en 
    <span style='color:#d11141;'>Removed</span>"
  )
ggsave("plots/Presentatie/uitleg_sir_2.png", plot, width = 8, height = 3)
