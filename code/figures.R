library(tidyverse)
library(here)
library(ggdist)
library(cowplot)
library(magick)

here::i_am("code/figures.R")

# Data Prep --------------------------------------------------------------

trial_data <- list()
trial_data$E1 <- read_rds(here("data/processed/exp1_confirmatory_data.rds"))
trial_data$E2 <- read_rds(here("data/processed/exp2_confirmatory_data.rds"))
trial_data$E2s <- read_rds(here("data/processed/exp2_supp_1_data.rds"))
trial_data$E3 <- read_rds(here("data/processed/exp3_confirmatory_data.rds"))

# Aggregate trial data into one dataframe
all_trial_data <- list_rbind(trial_data, names_to = "experiment")

# Calculate Cousineau–Morey corrected within-subject SEM
# per Cousineau & O'Brien (2014), Behav Res 46:1149-1151, Eqs. 2 and 5.
summary_data <- trial_data |>
  map(\(x) {
    x |>
      droplevels() |>
      summarise(rt = mean(rt), .by = c(p_id, statement_type)) |>
      mutate(grand_mean = mean(rt)) |>
      mutate(participant_mean = mean(rt), .by = p_id) |>
      mutate(rt_normalised = rt - participant_mean + grand_mean) |>
      summarise(
        n = n(),
        n_conditions = length(levels(statement_type)),
        mean_rt = mean(rt),
        se_within = sd(rt_normalised) /
          sqrt(n) *
          sqrt(n_conditions / (n_conditions - 1)),
        .by = statement_type
      )
  }) |>
  list_rbind(names_to = "experiment")


# Setting Theme Elements -------------------------------------------------
base_pt <- 7
theme_set(
  theme_classic(base_size = base_pt) +
    theme(
      axis.title = element_text(size = base_pt),
      axis.text = element_text(size = base_pt * 0.9),
      strip.text = element_text(size = base_pt, face = "bold"),
      strip.background = element_blank()
    )
)


# Define Plotting Function -----------------------------------------------

create_plot <- function(trial_data, summary_data) {
  # Base Layer
  ggplot(
    data = trial_data,
    aes(
      x = statement_type,
      y = rt,
      side = statement_type,
      fill = statement_type
    )
  ) +

    # Violin Plot
    stat_slab(slab_alpha = 1, scale = 0.5) +
    scale_side_mirrored(start = "left") +
    scale_fill_manual(
      values = c(Reality = "#386CB0", Knowledge = "#7FC97F", Belief = "#BEAED4")
    ) +

    # Slopegraph
    geom_line(
      aes(group = p_id),
      linewidth = 0.05,
      colour = "#d8d8d8"
    ) +
    geom_point(
      size = .5,
      colour = "#d8d8d8"
    ) +

    # Condition means
    geom_point(
      data = summary_data,
      aes(y = mean_rt),
      size = 2,
      colour = "#000000"
    ) +

    # Error bars - SE (Cousineau–Morey corrected)
    geom_errorbar(
      data = summary_data,
      aes(
        y = mean_rt,
        ymin = mean_rt - se_within,
        ymax = mean_rt + se_within
      ),
      width = .1,
      linewidth = .5,
      colour = "#000000"
    ) +

    scale_y_continuous(breaks = scales::breaks_width(0.5)) +

    theme(
      legend.position = "none",
      strip.background = element_blank()
    ) +
    labs(
      x = "Statement Type",
      y = "RT (Seconds)"
    ) +
    facet_grid(
      ~experiment,
      axes = "all_y",
      labeller = labeller(
        experiment = c(
          E1 = "Experiment 1",
          E2 = "Experiment 2",
          E2s = "Experiment 2",
          E3 = "Experiment 3"
        )
      )
    )
}


# Knowledge Figure -------------------------------------------------------

# Bottom Panel - Graphs
k_fig <- create_plot(
  all_trial_data |> filter(experiment %in% c("E1", "E2")),
  summary_data |> filter(experiment %in% c("E1", "E2"))
)

# Top left - Statements
statements <- tribble(
  ~label       , ~text                           , ~y    ,
  "Reality:"   , "In REALITY, there is ONE cube" , 0.5   ,
  "Knowledge:" , "Adam KNOWS there is ONE cube"  , 0.375 ,
  "Belief:"    , "Adam THINKS there is ONE cube" , 0.25
)

text_panel <- ggplot(statements) +
  annotate(
    "text",
    x = 0.35,
    y = 0.65,
    label = "Statements (Experiments 1 & 2)",
    hjust = 0,
    fontface = "bold",
    size = base_pt / .pt
  ) +
  geom_text(
    aes(x = 0.2, y = y, label = label),
    hjust = 0,
    size = base_pt / .pt
  ) +
  geom_text(
    aes(x = 0.35, y = y, label = text),
    hjust = 0,
    size = base_pt / .pt
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_void()


# Top Right - Stimuli Example
stimuli_example <- ggdraw() +
  draw_image(
    image_read(here("materials/TB-K-NB.png")),
    x = -.15,
    y = 0.05,
    width = 1,
    height = 1
  )

# Figure Assembly
top <- plot_grid(text_panel, stimuli_example, nrow = 1, rel_widths = c(1.7, 1))
final_k <- plot_grid(top, k_fig, ncol = 1, rel_heights = c(1, 2))

# Export
ggsave(
  here("outputs/figures/fig1_multipanel_knowledge.svg"),
  final_k,
  width = 180,
  height = 120,
  units = "mm"
)

# Ignorance Figure -------------------------------------------------------

# Bottom Panel - Graphs

ig_fig <- create_plot(
  all_trial_data |> filter(experiment %in% c("E2s", "E3")),
  summary_data |> filter(experiment %in% c("E2s", "E3"))
)

# Top left - Statements
statements <- tribble(
  ~block , ~header                   , ~label               , ~text                                     ,
       1 , 'Statements (Experiment 2)' , "Knowledge:"         , "Adam KNOWS there are TWO cubes"          ,
       1 , 'Statements (Experiment 2)' , "Belief:"            , "Adam THINKS there are TWO cubes"         ,
       2 , 'Statements (Experiment 3)' , "Negated Knowledge:" , "Adam does NOT KNOW there are TWO cubes"  ,
       2 , 'Statements (Experiment 3)' , "Negated Belief:"    , "Adam does NOT THINK there are TWO cubes"
)

# layout constants
line_gap <- 0.13 # between statement rows
block_gap <- 0.12 # extra space above each header
head_gap <- 0.10 # header to first statement

layout <- statements |>
  mutate(
    row = row_number(),
    new_block = block != lag(block, default = first(block)),
    y = .8 - cumsum(line_gap + block_gap * new_block)
  )

headers <- layout |>
  summarise(y = max(y) + head_gap, .by = c(block, header))

text_panel <- ggplot() +
  geom_text(
    data = headers,
    aes(x = 0.25, y = y, label = header),
    hjust = 0,
    fontface = "bold",
    size = base_pt / .pt
  ) +
  geom_text(
    data = layout,
    aes(x = 0.1, y = y, label = label),
    hjust = 0,
    size = base_pt / .pt
  ) +
  geom_text(
    data = layout,
    aes(x = 0.35, y = y, label = text),
    hjust = 0,
    size = base_pt / .pt
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_void()

# Top Right - Stimuli Example
stimuli_example <- ggdraw() +
  draw_image(
    image_read(here("materials/FB-Ig.png")),
    x = -.15,
    y = 0.05,
    width = 1,
    height = 1
  )

# Figure Assembly
top <- plot_grid(text_panel, stimuli_example, nrow = 1, rel_widths = c(1.7, 1))
final_ig <- plot_grid(top, ig_fig, ncol = 1, rel_heights = c(1, 2))

ggsave(
  here("outputs/figures/fig2_multipanel_ignorance.svg"),
  final_ig,
  width = 180,
  height = 120,
  units = "mm"
)

# Stimuli Example Grid ---------------------------------------------------
panel_names <- c("TB-K-NB", "FB-Ig", "TB-Ig-B", "TB-K-B")

panels <- panel_names |>
  set_names() |>
  map(\(x) ggdraw() + draw_image(here("materials", paste0(x, ".png")), scale = .95))

stim_examples <- plot_grid(plotlist = panels, labels = "auto", align = "v")

ggsave(
  here("outputs/figures/stimuli_examples.png"),
  stim_examples,
  width = 180,
  height = 120,
  units = "mm"
)
