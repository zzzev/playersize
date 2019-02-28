library(tidyverse)
library(lubridate)
library(rvest)
library(httr)

read_html_nocomment <- function(url) url %>%
  GET %>%
  content %>%
  as.character() %>%
  str_remove_all("<!--") %>%
  str_remove_all("-->") %>%
  read_html

all_team_index <- read_html_nocomment("https://www.pro-football-reference.com/teams/")
destructure_link <- function(a) tibble(name = html_text(a),
                                       url = html_attr(a, "href"))
read_teams <- function(table) {
  table %>%
    html_nodes("tbody tr") %>%
    discard(~ length(html_nodes(.x, "th a")) == 0) %>%
    map_dfr(function(row) {
      from <- html_node(row, "td:nth-child(2)") %>% html_text
      to <- html_node(row, "td:nth-child(3)") %>% html_text
      row %>%
        html_nodes("th a") %>%
          map_dfr(destructure_link) %>%
          bind_cols(tibble(from, to))
    })
}
active_teams <- all_team_index  %>%
  html_node("#teams_active") %>%
  read_teams %>%
  mutate(franchise_active = TRUE)
inactive_teams <- all_team_index  %>%
  html_node("#teams_inactive") %>%
  read_teams %>%
  mutate(franchise_active = FALSE)
all_teams <- bind_rows(active_teams, inactive_teams)

read_roster <- function(id, year) {
  print(str_glue(id, " ", year))
  filename <- str_glue("data/nfl/", id, "/", year, ".rds")
  if (file.exists(filename)) return(read_rds(filename))
  Sys.sleep(0.5)
  if (!dir.exists(dirname(filename)))
    dir.create(dirname(filename), recursive = TRUE)
  url <- str_glue("https://www.pro-football-reference.com/teams/", id,
                  "/", year, "_roster.htm")
  url %>%
    read_html_nocomment %>%
    html_nodes("#games_played_team tbody tr") %>%
    map_dfr(function(row) {
      tds <- html_nodes(row, "td")
      texts <- html_text(tds)
      nums <- map(texts, quietly(parse_number)) %>% map_dbl("result")
      tibble(number = html_node(row, "th") %>% html_text,
             player_name = texts[[1]],
             player_url = html_node(tds[[1]], "a") %>% html_attr("href"),
             position = texts[[3]],
             games_played = nums[[4]],
             games_started = nums[[5]],
             weight = nums[[6]],
             height = texts[[7]],
             college = texts[[8]],
             birthdate = texts[[9]] %>% mdy,
             years = texts[[10]],
             approx_value = nums[[11]],
             drafted = texts[[12]],
             salary = if_else(length(nums) >= 13,
                              last(nums) %>% as.numeric,
                              as.numeric(NA)))
    }) %>%
    write_rds(filename)
}

all_teams %>%
  mutate(id = map(url, ~str_split(.x, "/")) %>% map_chr(~nth(first(.x), 3)),
         year = map2(from, to, ~ .x:.y)) %>%
  select(-c(from, to, url)) %>%
  unnest %>%
  mutate(roster = map2(id, year, read_roster)) %>%
  write_rds("data/nfl/rosters.rds")
