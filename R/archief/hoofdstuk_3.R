library(hrbrthemes)
library(latex2exp)
library(tidyverse)
library(glue)

list.files("../data")

palette <- c("#e27c7c", "#a86464", "#6d4b4b", 
             "#466964", "#599e94", "#6cd4c5")

# Forward Euler
df <- read_csv("../data/H3_sir_forward_euler.csv")
plot <- df %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment)
  ) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = compartment), size = 1.2) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = palette[c(1, 4, 5)]) +
  facet_wrap(~ step_size, scales = "free_x") +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "right",
    legend.justification = "top",
    legend.text = element_text(size = 15),
    legend.title = element_blank())
ggsave("plots/H3/SIR_voorwaartse_euler.png", plot, width = 12, height = 2.5)
plot <- plot +
  labs(
    title = "Voorwaartse Euler SIR-model",
    subtitle = TeX("Onjuist voor grotere $h$.")
  )
ggsave("plots/Presentatie/H3_voorwaartse_euler.png", plot, width = 12, height = 3.5)


df <- read_csv("../data/H3_sir_nonstandard.csv")
plot <- df %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment)
  ) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = compartment), size = 1.2) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = palette[c(1, 4, 6)]) +
  facet_wrap(~ step_size, scales = "free_x") +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "right",
    legend.justification = "top",
    legend.text = element_text(size = 15),
    legend.title = element_blank())
ggsave("plots/H3/SIR_nonstandard.png", plot, width = 12, height = 2.5)
plot <- plot +
  labs(
    title = "Niet-standaard schema SIR-model",
    subtitle = TeX("Voor grote $h$ een correcte oplossing.")
  )
ggsave("plots/Presentatie/H3_niet_standaard.png", plot, width = 12, height = 3.5)




fe <- read_csv("../data/H3_sir_forward_euler.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "Voorwaartse Euler"
  )
ns <- read_csv("../data/H3_sir_nonstandard.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "Niet-Standaard"
  ) 
df <- bind_rows(fe, ns)
df %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment)
  ) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = compartment), size = 1.2) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = palette[c(1, 4, 6)]) +
  facet_wrap(~ step_size, scales = "free_x") +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "right",
    legend.justification = "top",
    legend.text = element_text(size = 15),
    legend.title = element_blank())


# Double plot
plot <- df %>%
  mutate(
    name = fct_inorder(name),
    compartment = recode(
      compartment,
      "S" = "Susceptible",
      "I" = "Infected",
      "R" = "Recovered"
    )
  ) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = compartment), size = 1.2) +
  scale_y_continuous(limits = c(0, 1), position = "right") +
  scale_color_manual(values = palette[c(1, 4, 5)]) +
  scale_linetype_manual(values = c(1, 5, 2)) +
  facet_grid(
    rows = vars(name), cols = vars(step_size), 
    scales = "free", switch = "y"
  ) +
  labs(x = NULL, y = NULL) + 
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "top",
    legend.text = element_text(size = 15),
    legend.title = element_blank())
ggsave("plots/H3/h3_schemes.png", plot, width = 12, height = 6)
