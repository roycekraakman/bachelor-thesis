library(hrbrthemes)
library(latex2exp)
library(tidyverse)
library(glue)

theme_set(theme_ipsum())

palette <- c("#e27c7c", "#a86464", "#6d4b4b", "#466964", "#599e94", "#6cd4c5")



#' Scenario 1.
#' We bekijken wat er gebeurt als de afgeleide orde naar 1 gaat.
scheme_nsgl <- read_csv("../data/H5_NSGL_scen_1.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "NSGL"
  ) %>%
  select(name, everything())
scheme_gl <- read_csv("../data/H5_FGL_scen_1.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "FGL"
  ) %>%
  select(name, everything())
scheme_ns <- read_csv("../data/H5_NSE_scen_1.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "NSE"
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
    )
  ) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = name, linetype = name), size = 1.2) +
  scale_y_continuous(limits = c(0, 1), position = "right") +
  scale_color_manual(values = palette[c(1, 4, 6)]) +
  scale_linetype_manual(values = c(1, 5, 2)) +
  facet_grid(
    rows = vars(compartment), cols = vars(step_size), 
    scales = "free", switch = "y"
  ) +
  labs(x = NULL, y = NULL) + 
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "top",
    legend.text = element_text(size = 15),
    legend.title = element_blank())
ggsave("plots/H5/SIR_scen_1.png", plot, width = 12, height = 8)



#' Scenario 2.
#' We bekijken verschillende noemerfuncties
scheme_nsgl <- read_csv("../data/H5_NSGL_scen_2.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "NSGL"
  ) %>%
  select(name, everything())
scheme_gl <- read_csv("../data/H5_NSGL_NF1_scen_2.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "NSGL-NF1"
  ) %>%
  select(name, everything())
scheme_ns <- read_csv("../data/H5_NSGL_NF2_scen_2.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "step_size"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = fct_inorder(step_size),
    compartment = fct_inorder(compartment),
    name = "NSGL-NF2"
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
    )
  ) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = name, linetype = name), size = 1.2) +
  scale_y_continuous(limits = c(0, 1), position = "right") +
  scale_color_manual(values = palette[c(1, 4, 6)]) +
  scale_linetype_manual(values = c(1, 5, 2)) +
  facet_grid(
    rows = vars(compartment), cols = vars(step_size), 
    scales = "free", switch = "y"
  ) +
  labs(x = NULL, y = NULL) + 
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "top",
    legend.text = element_text(size = 15),
    legend.title = element_blank())
ggsave("plots/H5/SIR_scen_2.png", plot, width = 12, height = 8)



#' Scenario 3.
#' We bekijken verschillende perturbaties rondom een R0=1.
df <- read_csv("../data/H5_NSGL_scen_3.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "alpha", "perturbation"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    alpha = str_split(alpha, "=") %>% map_chr(2),
    alpha = fct_inorder(alpha),
    perturbation = str_split(perturbation, "=") %>% map_chr(2),
    perturbation = fct_inorder(perturbation)
  ) %>%
  mutate(
    above = if_else(value >= 0.2, 1, 0),
    epidemic = if_else(
      perturbation %in% c("0.1", "0.01", "0.001"), 
      "Epidemie", "Geen Epidemie"))

df <- df %>%
  mutate(
    alpha = factor(
      alpha, labels=c(
        "0.95" = parse(text=TeX("$\\alpha$$=0.95$")),
        "0.9" = parse(text=TeX("$\\alpha$$=0.9$")),
        "0.8" = parse(text=TeX("$\\alpha$$=0.8$")),
        "0.7" = parse(text=TeX("$\\alpha$$=0.7$"))
        )
    ),
    perturbation = factor(
      perturbation, labels=c(
        "0.1" = parse(text=TeX("$\\epsilon$$=0.1$")),
        "0.01" = parse(text=TeX("$\\epsilon$$=0.01$")),
        "0.001" = parse(text=TeX("$\\epsilon$$=0.001$")),
        "-0.1" = parse(text=TeX("$\\epsilon$$=-0.1$")),
        "-0.01" = parse(text=TeX("$\\epsilon$$=-0.01$")),
        "-0.001" = parse(text=TeX("$\\epsilon$$=-0.001$")))))

plot <- df %>%
  ggplot(aes(x = t, y = value, group = epidemic)) +
  geom_hline(aes(yintercept = 0.2), linetype = 2) +
  geom_line(aes(color = epidemic), size = 1.2) +
  scale_x_continuous(limits = c(0, 1.5)) +
  scale_y_continuous(limits = c(0.175, 0.215), position = "right") +
  scale_color_manual(values = c("#d72631", "#077b8a")) +
  facet_grid(
    rows = vars(alpha), cols = vars(perturbation), 
    scales = "free", switch = "y", labeller = label_parsed
  ) +
  labs(x = NULL, y = NULL) + 
  theme(
    panel.grid.minor = element_blank(),
    strip.text.y = element_text(hjust = 1),
    strip.text = element_text(size = 15),
    legend.position = "top",
    legend.text = element_text(size = 15),
    legend.title = element_blank()
  ) +
  guides(color = guide_legend(TeX("$\\epsilon$"), nrow = 1))
ggsave("plots/H5/SIR_scen_3.png", plot, width = 12, height = 10.5)



#' Scenario 4.
#' Faseportret S-I voor meerdere alpha.
df <- read_csv("../data/H5_NSGL_scen_4.csv") %>%
  pivot_longer(-t, names_to = c("compartment", "alpha", "s0"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    alpha = str_split(alpha, "=") %>% map_chr(2),
    alpha = fct_inorder(alpha),
    s0 = str_split(s0, "=") %>% map_chr(2),
    s0 = fct_inorder(s0)
  ) %>%
  pivot_wider(names_from = compartment, values_from = value)

df <- df %>%
  mutate(alpha = factor(
    alpha, labels=c(
      "0.95" = parse(text=TeX("$\\alpha$$=0.95$")),
      "0.9" = parse(text=TeX("$\\alpha$$=0.9$")),
      "0.8" = parse(text=TeX("$\\alpha$$=0.8$")),
      "0.7" = parse(text=TeX("$\\alpha$$=0.7$")))))

plot <- df %>%
  ggplot(aes(x = S, y = I)) +
  geom_path(aes(color = s0), size = 0.5) +
  geom_point(aes(color = s0), size = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~ alpha, labeller = label_parsed) +
  scale_color_manual(values = palette[c(1, 3, 5, 6)]) +
  labs(x = "Susceptible", y = "Infected") + 
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "top",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.title.y = element_text(size = 15, vjust = 2, hjust = 0.5),
    axis.title.x = element_text(size = 15, hjust = 0.5, vjust = -2)
  ) +
  guides(color = guide_legend(TeX("$S0$"), nrow = 1))
ggsave("plots/H5/SIR_scen_4.png", plot, width = 12, height = 5.5)


#' Scenario 5.
#' Wat gebeurt er met de totale populatie.
df <- read_csv("../data/H5_NSGL_scen_5_1.csv") %>%
  pivot_longer(-t, names_to = "alpha") %>%
  filter(!is.na(value)) %>%
  mutate(
    alpha = str_split(alpha, "=") %>% map_chr(2),
    alpha = fct_inorder(alpha))

df <- df %>%
  mutate(alpha = factor(
    alpha, labels=c(
      "0.99" = parse(text=TeX("$\\alpha$$=0.99$")),
      "0.95" = parse(text=TeX("$\\alpha$$=0.95$")),
      "0.9" = parse(text=TeX("$\\alpha$$=0.9$")),
      "0.85" = parse(text=TeX("$\\alpha$$=0.85$")),
      "0.75" = parse(text=TeX("$\\alpha$$=0.75$")),
      "0.5" = parse(text=TeX("$\\alpha$$=0.5$")),
      "0.25" = parse(text=TeX("$\\alpha$$=0.25$")),
      "0.05" = parse(text=TeX("$\\alpha$$=0.05$")),
      "0.01" = parse(text=TeX("$\\alpha$$=0.01$")))))

plot <- df %>%
  ggplot(aes(x = t, y = value, color = alpha)) +
  geom_line(color = palette[2], size = 2, show.legend = FALSE) +
  gghighlight::gghighlight(
    unhighlighted_params = list(size = 0.5),
    use_direct_label = FALSE
  ) +
  scale_color_brewer(type = "qual", palette = 3) +
  facet_wrap(~ alpha, labeller = label_parsed) +
  labs(x = NULL, y = NULL) + 
  theme_ipsum() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "right",
    legend.justification = "top",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15, hjust = 0.5))
ggsave("plots/H5/SIR_scen_5_1.png", plot, width = 12, height = 7.5)



options(scipen=999)
df <- read_csv("../data/H5_NSGL_scen_5_2.csv") %>%
  pivot_longer(-t, names_to = "step_size") %>%
  filter(!is.na(value)) %>%
  mutate(
    step_size = str_split(step_size, "=") %>% map_chr(2),
    step_size = as.numeric(step_size),
    step_size = round(step_size, 5),
    step_size = as.character(step_size),
    step_size = fct_inorder(step_size))
plot <- df %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = step_size, group = step_size), size = 1) +
  labs(x = NULL, y = NULL) + 
  scale_color_manual(values = palette[c(1, 3, 5, 6)]) +
  theme_ipsum() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15),
    legend.position = "right",
    legend.justification = "top",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 15, hjust = 0.5)) +
  guides(color = guide_legend(TeX("$h$")))
ggsave("plots/H5/SIR_scen_5_2.png", plot, width = 12, height = 2.75)

