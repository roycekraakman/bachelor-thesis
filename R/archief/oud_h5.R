library(hrbrthemes)
library(latex2exp)
library(tidyverse)
library(glue)

list.files("../data")

palette <- c("#e27c7c", "#a86464", "#6d4b4b", 
             "#466964", "#599e94", "#6cd4c5")

df <- read_csv("../data/H5_sir_nsgl_phase_portrets.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "alpha"), names_sep = "_") %>%
  mutate(
    alpha = str_split(alpha, "=") %>% map_chr(2),
    alpha = fct_inorder(alpha))

I_max <- df %>% 
  filter(compartment == "max") %>% 
  distinct(alpha, value)

plot <- df %>%
  filter(compartment != "max") %>%
  pivot_wider(names_from = "compartment", values_from = "value") %>%
  ggplot(aes(x = S, y = I, color = alpha)) +
  geom_abline(slope= -1, intercept= 1, linetype = 3) +
  geom_path(size = 2, show.legend = FALSE) +
  geom_vline(
    data = I_max,
    aes(xintercept = value, color = alpha), 
    linetype = 2, show.legend = FALSE) +
  xlim(c(0, 1)) +
  ylim(c(0, 1)) +
  scale_color_manual(values = palette) +
  facet_wrap(~ alpha) +
  labs(x = "Susceptible", y = "Infected") +
  theme_ipsum()
ggsave("plots/H5/SIR_nsgl_phase_portrets.png", plot, width = 12, height = 5)










# Scenario 1
scheme_nsgl <- read_csv("../data/H5_sir_gl_nonstandard_scen_1.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "NSGL"
  ) %>%
  select(name, everything())
scheme_gl <- read_csv("../data/H5_sir_forward_gl_scen_1.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "GL"
  ) %>%
  select(name, everything())
scheme_ns <- read_csv("../data/H5_sir_nonstandard_scen_1.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "NS"
  ) %>%
  select(name, everything())

df <- bind_rows(scheme_nsgl, scheme_gl, scheme_ns)

plot <- df %>%
  mutate(
    name = fct_inorder(name),
    compartment = recode(compartment,
                         "S" = "Susceptible",
                         "I" = "Infected",
                         "R" = "Recovered"
    )) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = name, linetype = name), size = 1.2) +
  scale_y_continuous(limits = c(0, 1), position = "right") +
  scale_color_manual(values = palette[c(1, 2, 5)]) +
  facet_grid(rows = vars(compartment), cols = vars(step_size), 
             scales = "free", switch = "y") +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "top",
    legend.text = element_text(size = 15),
    legend.title = element_blank())
ggsave("plots/H5/SIR_schemes_scen_1.png", plot, width = 10, height = 5.5)



# Scenario 2
scheme_nsgl <- read_csv("../data/H5_sir_nsgl_scen_2.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "NSGL"
  ) %>%
  select(name, everything())
scheme_nsgl_f1 <- read_csv("../data/H5_sir_nsgl_f1_scen_2.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "NSGL Noemerfunctie 1"
  ) %>%
  select(name, everything())
scheme_nsgl_f2 <- read_csv("../data/H5_sir_nsgl_f2_scen_2.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "NSGL Noemerfunctie 2"
  ) %>%
  select(name, everything())

df <- bind_rows(scheme_nsgl, scheme_nsgl_f1, scheme_nsgl_f2)

plot <- df %>%
  mutate(
    name = fct_inorder(name),
    compartment = recode(compartment,
                         "S" = "Susceptible",
                         "I" = "Infected",
                         "R" = "Recovered"
    )) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = name, linetype = name), size = 1.2) +
  scale_y_continuous(limits = c(0, 1), position = "right") +
  scale_color_manual(values = palette[c(1, 2, 5)]) +
  facet_grid(rows = vars(compartment), cols = vars(step_size), 
             scales = "free", switch = "y") +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "top",
    legend.text = element_text(size = 15),
    legend.title = element_blank())
ggsave("plots/H5/SIR_schemes_scen_2.png", plot, width = 10, height = 5.5)



# Scenario 3
df <- read_csv("../data/H5_sir_nsgl_scen_3.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "alpha"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    alpha = str_split(alpha, "=") %>% map_chr(2),
    alpha = as.numeric(alpha),
    compartment = fct_inorder(compartment),
    name = "NSGL"
  ) %>%
  select(name, everything())
plot <- df %>%
  mutate(
    name = fct_inorder(name),
    compartment = recode(compartment,
                         "S" = "Susceptible",
                         "I" = "Infected",
                         "R" = "Recovered"
    )) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = alpha, group = alpha), size = 1) +
  scale_y_continuous(limits = c(0, 1), position = "right") +
  scale_color_distiller(type = "div", palette = 5) +
  facet_wrap(~ compartment, ncol = 3, scales = "free", switch = "y") +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "right",
    legend.justification = "top",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15, hjust = 0.5)) +
  guides(color = guide_colorbar(TeX("$\\alpha$")))
ggsave("plots/H5/SIR_schemes_scen_3.png", plot, width = 10, height = 2.75)



# Scenario 4
df <- read_csv("../data/H5_sir_nsgl_scen_4.csv") %>%
  pivot_longer(-t, names_to = "alpha") %>%
  filter(!is.na(value)) %>%
  mutate(
    alpha = str_split(alpha, "=") %>% map_chr(2),
    alpha = as.numeric(alpha),
  )
plot <- df %>%
  mutate(
    group = if_else(alpha < 0.5, "<0.5", ">=0.5"),
    group = factor(group, levels = c(">=0.5", "<0.5"))
  ) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = alpha, group = alpha), size = 1) +
  scale_color_distiller(type = "div", palette = 5) +
  facet_wrap(~ group, scales = "free_x") +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "right",
    legend.justification = "top",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15, hjust = 0.5)) +
  guides(color = guide_colorbar(TeX("$\\alpha$")))
ggsave("plots/H5/SIR_schemes_scen_4.png", plot, width = 10, height = 2.75)

df <- read_csv("../data/H5_sir_nsgl_scen_4_2.csv") %>%
  pivot_longer(-t, names_to = "step_size") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = str_split(step_size, "=") %>% map_chr(2),
    step_size = as.numeric(step_size),
  )
plot <- df %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = step_size, group = step_size), size = 1) +
  scale_color_distiller(type = "div", palette = 5) +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "right",
    legend.justification = "top",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15, hjust = 0.5)) +
  guides(color = guide_colorbar(TeX("$h$")))
ggsave("plots/H5/SIR_schemes_scen_4_2.png", plot, width = 10, height = 2.75)
