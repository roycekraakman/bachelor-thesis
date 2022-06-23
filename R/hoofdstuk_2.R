library(hrbrthemes)
library(latex2exp)
library(tidyverse)
library(glue)


palette <- c("#e27c7c", "#a86464", "#6d4b4b", "#466964", "#599e94", "#6cd4c5")


# Forward Euler
df <- read_csv("../data/H2_logistic_equation_forward_euler.csv")
plot <- df %>%
  pivot_longer(-k) %>%
  filter(!is.na(value)) %>%
  mutate(name = fct_inorder(name)) %>%
  ggplot(aes(x = k, y = value)) +
  geom_line(aes(color = name), size = 1.2, show.legend = FALSE) +
  scale_color_manual(values = palette) +
  facet_wrap(~ name, scales = "free") +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15))
ggsave("plots/H2/voorwaartse_euler.png", plot, width = 12, height = 5)


# Central Difference
df <- read_csv("../data/H2_logistic_equation_central_difference.csv")
plot <- df %>%
  pivot_longer(-k) %>%
  filter(!is.na(value)) %>%
  mutate(name = fct_inorder(name)) %>%
  ggplot(aes(x = k, y = value)) +
  geom_line(aes(color = name), size = 1.2, show.legend = FALSE) +
  scale_color_manual(values = palette) +
  facet_wrap(~ name, scales = "free") +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15))
ggsave("plots/H2/centrale_differentie.png", plot, width = 12, height = 2.75)



# Exact Scheme
df <- read_csv("../data/H2_logistic_equation_exact.csv")
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
  geom_point(aes(color = name), size = 4, show.legend = FALSE) +
  scale_color_manual(values = palette) +
  facet_wrap(~ name, scales = "free") +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15))
ggsave("plots/H2/exact_schema.png", plot, width = 12, height = 5)


# Non-Local Central Difference
df <- read_csv("../data/H2_logistic_equation_non_local_central_difference.csv")
plot <- df %>%
  pivot_longer(-k) %>%
  filter(!is.na(value)) %>%
  mutate(name = fct_inorder(name)) %>%
  ggplot(aes(x = k, y = value)) +
  geom_line(aes(color = name), size = 1.2, show.legend = FALSE) +
  scale_color_manual(values = palette[c(1, 3)]) +
  facet_wrap(~ name, scales = "free") +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15))
ggsave("plots/H2/niet_lokale_centrale_differentie.png", plot, width = 12, height = 2.75)
