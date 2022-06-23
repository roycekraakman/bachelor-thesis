library(hrbrthemes)
library(latex2exp)
library(tidyverse)
library(ggtext) 
library(glue)

theme_set(theme_ipsum())

palette <- c("#e27c7c", "#a86464", "#6d4b4b", "#466964", "#599e94", "#6cd4c5")

fe <- read_csv("../data/PRES_combustion_fe.csv")
ns <- read_csv("../data/PRES_combustion_ns.csv")
plot <- bind_rows(fe, ns) %>%
  pivot_longer(-c(t, scheme), names_to = c("y0", "h"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    y0 = fct_inorder(y0),
    h = fct_inorder(h),
    scheme = fct_inorder(scheme)
  ) %>%
  mutate(
    y0 = factor(
      y0, labels=c(
        "y0=0.25" = parse(text=TeX("$y_{0}=0.25$")),
        "y0=0.5" = parse(text=TeX("$y_{0}=0.5$")),
        "y0=0.75" = parse(text=TeX("$y_{0}=0.75$"))
        )
      ),
    h = factor(
      h, labels=c(
        "h=5" = parse(text=TeX("$h=5$")),
        "h=2.5" = parse(text=TeX("$h=2.5$")),
        "h=1.25" = parse(text=TeX("$h=1.25$")),
        "h=0.625" = parse(text=TeX("$h=0.6255$"))
      )
    )
  ) %>%
  ggplot(aes(x = t, y = value)) +
  geom_line(aes(color = scheme), size = 1.2) +
  scale_y_continuous(limits = c(0, 1.5), position = "right") +
  scale_color_manual(values = palette[c(5, 2)]) +
  facet_grid(
    rows = vars(y0), cols = vars(h), 
    switch = "y", labeller = label_parsed
  ) +
  theme_ipsum() +
  theme(
    plot.title = element_markdown(),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 15),
    strip.text = element_text(size = 15),
    strip.text.y = element_text(hjust = 1)) +
  labs(
    x = NULL, y = NULL,
    title = "<span style='color:#599e94;'>Standaard</span> vs. 
    <span style='color:#a86464;'>Niet-Standaard</span> voor Verbrandingsmodel
    ",
    subtitle = paste(
      "Standaard schema is instabiel voor een grote stapgrootte.",
      "Niet-Standaard schema convergeert altijd naar 1."
    )
  )

df_text <- tibble(
  t = 25,
  value = 1.35,
  y0 = factor("y0=0.25", labels = c(parse(text=TeX("$y_{0}=0.25$")))),
  h = factor("h=5", labels = c(parse(text=TeX("$h=5$"))))
)
plot <- plot + geom_label(
  data = df_text, 
  mapping = aes(x = t, y = value, label = "Naar Inf"))

df_text <- tibble(
  t = 25,
  value = 1.45,
  y0 = factor("y0=0.5", labels = c(parse(text=TeX("$y_{0}=0.5$")))),
  h = factor("h=5", labels = c(parse(text=TeX("$h=5$"))))
)
plot <- plot + geom_label(
  data = df_text, 
  mapping = aes(x = t, y = value, label = "Naar Inf"))

df_text <- tibble(
  t = 10,
  value = 1.45,
  y0 = factor("y0=0.75", labels = c(parse(text=TeX("$y_{0}=0.75$")))),
  h = factor("h=5", labels = c(parse(text=TeX("$h=5$"))))
)
plot <- plot + geom_label(
  data = df_text, 
  mapping = aes(x = t, y = value, label = "Naar Inf"))

ggsave("plots/Presentatie/verbrandingsmodel.png", plot, width = 10, height = 7.6)
