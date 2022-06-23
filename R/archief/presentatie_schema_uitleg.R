library(hrbrthemes)
library(tidyverse)
library(latex2exp)
library(glue)

theme_set(theme_ipsum())

palette <- c("#e27c7c", "#a86464", "#6d4b4b", "#466964", "#599e94", "#6cd4c5")

LINE_COLOR = palette[1]

df <- tibble(t = seq(0, 10, 0.5)) %>%
  mutate(y = 0.5^(2 * t + 1))

plot <- ggplot() +
  scale_x_continuous(limits = c(0, 10), breaks = c(0, 10)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(size = 15, hjust = 0.5, vjust = -2),
    axis.title.y = element_text(size = 15, hjust = 0.5, vjust = 2)
  ) +
  labs(x = TeX("Tijd $t$"), y = TeX("$y(t)$"))
ggsave("plots/Presentatie/uitleg_schema_1.png", plot, width = 10, height = 7.6)

plot <- ggplot() +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 0.5)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    panel.grid.major.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(size = 15, hjust = 0.5, vjust = -2),
    axis.title.y = element_text(size = 15, hjust = 0.5, vjust = 2)
  ) +
  labs(x = TeX("Tijd $t$"), y = TeX("$y(t)$"))
ggsave("plots/Presentatie/uitleg_schema_2.png", plot, width = 10, height = 7.6)

for (i in 1:6) {
  plot <- df %>%
    slice(1:i) %>%
    ggplot(aes(x = t, y = y)) +
    geom_point(color = LINE_COLOR, size = 10) +
    geom_line(color = LINE_COLOR, size = 1) +
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 0.5)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme(
      panel.grid.major.x = element_line(size = 1),
      panel.grid.minor.x = element_blank(),
      axis.title.x = element_text(size = 15, hjust = 0.5, vjust = -2),
      axis.title.y = element_text(size = 15, hjust = 0.5, vjust = 2)
    ) +
    labs(x = TeX("Tijd $t$"), y = TeX("$y(t)$"))
  ggsave(glue("plots/Presentatie/uitleg_schema_{i+2}.png"), plot, width = 10, height = 7.6)
}

plot <- df %>%
  ggplot(aes(x = t, y = y)) +
  geom_point(color = LINE_COLOR, size = 10) +
  geom_line(color = LINE_COLOR, size = 1) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 0.5)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    panel.grid.major.x = element_line(size = 1),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(size = 15, hjust = 0.5, vjust = -2),
    axis.title.y = element_text(size = 15, hjust = 0.5, vjust = 2)
  ) +
  labs(x = TeX("Tijd $t$"), y = TeX("$y(t)$"))
ggsave("plots/Presentatie/uitleg_schema_9.png", plot, width = 10, height = 7.6)