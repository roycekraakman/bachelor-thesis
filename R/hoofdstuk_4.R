library(hrbrthemes)
library(latex2exp)
library(tidyverse)
library(glue)


palette <- c("#e27c7c", "#a86464", "#6d4b4b", "#466964", "#599e94", "#6cd4c5")


# Forward GL
df <- read_csv("../data/H4_forward_gl_decay.csv")
exact <- select(df, t, exact)
plot <- df %>%
  select(-exact) %>%
  pivot_longer(-t) %>%
  filter(!is.na(value)) %>%
  mutate(name = fct_inorder(name)) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(
    data = exact, aes(x = t, y = exact),
    linetype = 2
  ) +
  geom_line(aes(color = name), size = 0.5, show.legend = FALSE) +
  geom_point(aes(color = name), size = 4, show.legend = FALSE) +
  scale_color_manual(values = palette) +
  facet_wrap(~ name, scales = "free") +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15))
ggsave("plots/H4/voorwaartse_gl.png", plot, width = 12, height = 5.5)


# Backward GL
df <- read_csv("../data/H4_backward_gl_decay.csv")
exact <- select(df, t, exact)
plot <- df %>%
  select(-exact) %>%
  pivot_longer(-t) %>%
  filter(!is.na(value)) %>%
  mutate(name = fct_inorder(name)) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(
    data = exact, aes(x = t, y = exact),
    linetype = 2
  ) +
  geom_line(aes(color = name), size = 0.5, show.legend = FALSE) +
  geom_point(aes(color = name), size = 4, show.legend = FALSE) +
  scale_color_manual(values = palette) +
  facet_wrap(~ name, scales = "free") +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15))
ggsave("plots/H4/achterwaartse_gl.png", plot, width = 12, height = 5.5)
