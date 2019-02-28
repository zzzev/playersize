library(tidyverse)
library(rvest)
library(httr)
library(lubridate)

read_html_nocomment <- function(url) url %>%
  GET %>%
  content %>%
  as.character() %>%
  str_remove_all("<!--") %>%
  str_remove_all("-->") %>%
  read_html

all_team_index <- read_html_nocomment("https://www.baseball-reference.com/teams/") %>%
  html_nodes("#teams_active,#teams_defunct") %>%
  html_nodes("tbody .left a") %>%
  map_dfr(function(link) {
    tibble(team = html_text(link),
           url = html_attr(link, "href"))
  })

read_team_index <- function(url) {
  print(url)
  url %>%
    map_chr(~ str_glue("https://www.baseball-reference.com", .x)) %>%
    read_html %>%
    html_nodes("th a") %>%
    map_dfr(function(link) {
      tibble(year = html_text(link),
             url = html_attr(link, "href"))
    })
}

all_team_index <- all_team_index %>%
  mutate(years = map(url, read_team_index))

read_team_year <- function(url) {
  id <- str_remove(url, "\\.s?html?$") %>% str_remove("/teams/")
  filename <- str_glue("data/mlb/", id, ".rds")
  print(filename)
  if (file.exists(filename)) return(read_rds(filename))
  Sys.sleep(0.5)
  if (!dir.exists(dirname(filename)))
    dir.create(dirname(filename), recursive = TRUE)
  raw <- map_chr(url, ~ str_glue("https://www.baseball-reference.com", .x)) %>%
    GET %>%
    content %>%
    as.character() %>%
    str_remove_all("<!--") %>%
    str_remove_all("-->") %>%
    read_html
  cols <- raw %>%
    html_nodes("#div_appearances thead th") %>%
    html_text()
  stat_names <- cols[-length(cols)][-(1:8)] %>%
    str_to_lower %>%
    map_chr(~ str_glue("stat_", .x))
  raw %>%
    html_nodes("#div_appearances tbody tr") %>%
    map_dfr(function(row) {
      a <- html_node(row, "a")
      tds <- html_nodes(row, "td")
      texts <- html_text(tds)
      nums <- map(texts, quietly(parse_number)) %>% map_dbl("result")
      stats <- tibble(stat = stat_names, value = nums[9:length(nums) - 1]) %>%
        spread(key = 1, value = 2)
      tibble(name = html_text(a),
             url = html_attr(a, "href"),
             age = nums[[1]],
             nationality = html_node(tds[[2]], "span") %>%
               html_text %>%
               str_to_upper,
             bats = texts[[3]],
             throws = texts[[4]],
             height = texts[[5]],
             weight = nums[[6]],
             birthdate = texts[[7]] %>% mdy,
             notes = texts[[length(texts)]]) %>%
        bind_cols(stats)
    }) %>%
    write_rds(filename)
}

all_team_index %>%
  select(-url) %>%
  unnest %>%
  mutate(roster = map(url, read_team_year)) %>%
  write_rds("data/mlb/rosters.rds")
