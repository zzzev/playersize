library(tidyverse)
library(lubridate)
library(gifski)
library(gganimate)
library(showtext)

font_add_google("Lato", "lato")
font_add_google("Lato", "latol", regular.wt = 300)

showtext_auto()

nba <- read_rds("data/nba/rosters.rds") %>%
  transmute(year = parse_number(season), roster, league = "nba") %>%
  unnest %>%
  transmute(year, league, name = player, weight, height,
            id = map2_chr(league, url, ~str_glue(.x, .y)))
nfl <- read_rds("data/nfl/rosters.rds") %>%
  mutate(league = "nfl") %>%
  select(year, roster, league) %>%
  unnest %>%
  transmute(year, league, name = player_name, weight, height,
            id = map2_chr(league, player_url, ~str_glue(.x, .y)))
nhl <- read_rds("data/nhl/rosters.rds") %>%
  mutate(league = "nhl") %>%
  transmute(year = parse_number(year), roster, league) %>%
  unnest %>%
  transmute(year, league, height, weight = parse_number(weight), name = player,
            id = map2_chr(league, player_url, ~str_glue(.x, .y)))
mlb <- read_rds("data/mlb/rosters.rds") %>%
  mutate(league = "mlb") %>%
  transmute(year = parse_number(year), roster, league) %>%
  unnest %>%
  transmute(year, league, name, weight, height,
            id = map2_chr(league, url, ~str_glue(.x, .y)))

data <- bind_rows(nba, nfl, nhl, mlb)
height_data <- data %>%
  mutate(height_parts = str_split(height, "[ -]"),
         height_feet = height_parts %>% map_dbl(~first(.x) %>% parse_number),
         height_inches = height_parts %>% map_dbl(~last(.x) %>% parse_number)
         ) %>%
  select(-c(height_parts)) %>%
  mutate(height = height_feet * 12 + height_inches) %>%
  filter(height != 0, !is.na(height), !is.na(weight))

# Manually jitter weight and height. Do this manually because using geom_jitter
# will create a new, distracting jitter for each frame of the animation; we want
# to jitter only once per player.
jittered_height_data <- height_data %>%
  group_by(id) %>%
  mutate(height = height + runif(1, -0.5, 0.5),
         weight_precision = case_when(
           weight %% 10 == 0 ~ 10,
           weight %% 5 == 0 ~ 5,
           TRUE ~ 1
         ),
         weight = weight +
           runif(1, weight_precision / -2, weight_precision / 2)) %>%
  select(-weight_precision) %>%
  ungroup

years <- height_data %>% pull(year) %>% unique
num_years <- max(years) - min(years) + 1

height_data %>%
  count(year, league, sort = TRUE)

mean_height_data <- height_data %>%
  group_by(year, league) %>%
  summarise(weight = mean(weight),
            height = mean(height)) %>%
  mutate(id = "mean")

combined_data <- bind_rows(sample_frac(jittered_height_data, 1),
                           mean_height_data) %>%
  select(year, league, weight, height, id)

offblack <- "#333333"

guide_labels <- c("Single Player", "Avg. of All Players")
guide_single_v_mean <- guide_legend(
  override.aes = list(color = "white"),
  title = NULL
)

format_heights <- function(inches) {
  inches %>% map_chr(function(inches) {
    ft <- floor(inches / 12)
    str_glue(ft, "' ", inches - 12 * ft, "\"")
  })
}

baseplot <- combined_data %>%
  mutate(is_mean = id == "mean") %>%
  ggplot(mapping = aes(x = weight, y = height, color = league, group = id,
                       shape = is_mean, size = is_mean, alpha = is_mean,
                       fill = is_mean,
                       stroke = if_else(is_mean, 2, 0))) +
  geom_point(fill = "white") +
  scale_shape_manual(values = c(16, 21), labels = guide_labels) +
  scale_size_discrete(range = c(2, 8), labels = guide_labels) +
  scale_alpha_discrete(range = c(0.2, 1), labels = guide_labels) +
  scale_color_brewer(type = "qual", palette = 6, labels = str_to_upper) +
  scale_y_continuous(breaks = c(10:16 * 6), labels = format_heights) +
  labs(title = "Height & Weight of US Athletes", subtitle = "{floor(frame_time)}",
       caption = "Visualization by @zzzev    -    Data source: sports-reference.com",
       color = "League", x = "Weight (lbs.)", y = "Height (ft.)") +
  theme(legend.position = c(0.14, 0.8),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "black", color = "black"),
        panel.grid = element_line(color = offblack),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "white"),
        plot.background = element_rect(fill = "black", color = "black"),
        plot.title = element_text(family = "latol", hjust = 0.5, size = 20,
                                  margin = margin(t = 4)),
        plot.subtitle = element_text(margin = margin(t = 580, b = -600),
                                     hjust = 0.9, size = 50, color = "#666666",
                                     family = "latol"),
        plot.caption = element_text(hjust = 0.5),
        text = element_text(color = "white", family = "lato")) +
  guides(
    size = guide_single_v_mean,
    alpha = guide_single_v_mean,
    shape = guide_single_v_mean
  )

baseplot

anim <- baseplot +
    transition_time(year) +
    ease_aes("linear") +
    enter_appear() +
    exit_disappear()

animate(anim, duration = num_years / 2, fps = 10, end_pause = 60,
        width = 720, height = 720)
anim_save("athelete_height_weight.gif")

animate(anim, duration = num_years / 2, fps = 10, end_pause = 60,
        renderer = av_renderer(), width = 720, height = 720)
anim_save("athelete_height_weight.mp4")
