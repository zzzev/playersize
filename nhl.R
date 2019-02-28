library(tidyverse)
library(rvest)
library(lubridate)
library(httr)

read_html_nocomment <- function(url) url %>%
  GET %>%
  content %>%
  as.character() %>%
  str_remove_all("<!--") %>%
  str_remove_all("-->") %>%
  read_html

parse_team_row <- function(row) {
  link <- html_node(row, "a")
  tds <- html_nodes(row, "td") %>% html_text
  tibble(team = html_text(link),
         url = html_attr(link, "href"),
         league = tds[[1]],
         from = parse_number(tds[[2]]),
         to = parse_number(tds[[3]]))
}

# Get all teams + urls for each year they were active
raw <- read_html_nocomment("https://www.hockey-reference.com/teams/")
active_teams <- raw %>%
  html_nodes("#active_franchises tbody tr") %>%
  discard(~length(html_nodes(.x, "a")) == 0) %>%
  map_dfr(parse_team_row) %>%
  mutate(active = TRUE)
inactive_teams <- raw %>%
  html_nodes("#defunct_franchises tbody tr") %>%
  discard(~length(html_nodes(.x, "a")) == 0) %>%
  map_dfr(parse_team_row) %>%
  mutate(active = FALSE)
all_teams <- bind_rows(active_teams, inactive_teams) %>%
  mutate(id = map_chr(url, ~first(str_split(.x, "/")) %>%
                        nth(3)) %>%
                        str_to_lower,
         url = map_chr(url,
                       ~str_glue("https://www.hockey-reference.com", .x))) %>%
  mutate(year_url = map(url, function(url) {
    print(url)
    read_html(url) %>%
      html_nodes("th a") %>%
      map_dfr(~tibble(
        year = html_text(.x),
        url = str_glue("https://www.hockey-reference.com",
                       html_attr(.x, "href"))
      ))
  }))

basedir <- "data/nhl/"
if (!dir.exists(basedir)) dir.create(basedir, recursive = TRUE)

all_teams %>%
  write_rds(str_glue(basedir, "teams.rds"))

# Get rosters per year per team
read_team_year <- function(id, year, url) {
  filename <- str_glue(basedir, id, "/", year, ".rds")
  print(filename)
  if (file.exists(filename)) return(read_rds(filename))
  basedir <- dirname(filename)
  if (!dir.exists(basedir)) dir.create(basedir)
  url %>%
    read_html %>%
    html_nodes("#roster tbody tr") %>%
    map_dfr(function(row) {
      tds <- html_nodes(row, "td")
      texts <- html_text(tds)
      tibble(
        number = html_node(row, "th") %>% html_text,
        player = texts[[1]],
        player_url = html_node(tds[[1]], "a") %>% html_attr("href"),
        nationality = texts[[2]],
        position = texts[[3]],
        age = texts[[4]],
        height = texts[[5]],
        weight = texts[[6]],
        shoot_catch = texts[[7]],
        experience = texts[[8]],
        birthdate = texts[[9]] %>% mdy
      )
    }) %>%
    write_rds(filename)
}

rosters <- all_teams %>%
  arrange(desc(to - from)) %>%
  select(-c(url, from, to)) %>%
  unnest %>%
  mutate(roster = pmap(list(id, parse_number(year) + 1, url), read_team_year))

rosters %>%
  write_rds("data/nhl/rosters.rds")
