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

get_teams_index <- function() {
  teams_index <- read_html_nocomment("https://www.basketball-reference.com/teams/")
  active_team_links <- teams_index %>%
    html_nodes("#teams_active,#teams_defunct") %>%
    html_nodes("a")
  tibble(name = html_text(active_team_links),
         url = html_attr(active_team_links, "href"))
}

get_team_index <- function(url) {
  print(url)
  raw <- read_html(str_glue("https://www.basketball-reference.com", url))
  season <- raw %>%
    html_nodes("th a") %>%
    html_text()
  url <- raw %>%
    html_nodes("th a") %>%
    html_attr("href")
  league <- raw %>%
    html_nodes("td:nth-child(2)") %>%
    html_text()
  wins <- raw %>%
    html_nodes("td:nth-child(4)") %>%
    html_text() %>%
    parse_number()
  losses <- raw %>%
    html_nodes("td:nth-child(5)") %>%
    html_text() %>%
    parse_number()
  finish <- raw %>%
    html_nodes("td:nth-child(7)") %>%
    html_text() %>%
    parse_number()
  pace <- raw %>%
    html_nodes("td:nth-child(11)") %>%
    html_text() %>%
    parse_number()
  rel_pace <- raw %>%
    html_nodes("td:nth-child(11)") %>%
    html_text() %>%
    parse_number()
  o_rtg <- raw %>%
    html_nodes("td:nth-child(12)") %>%
    html_text() %>%
    parse_number()
  rel_o_rtg <- raw %>%
    html_nodes("td:nth-child(13)") %>%
    html_text() %>%
    parse_number()
  d_rtg <- raw %>%
    html_nodes("td:nth-child(14)") %>%
    html_text() %>%
    parse_number()
  rel_d_rtg <- raw %>%
    html_nodes("td:nth-child(15)") %>%
    html_text() %>%
    parse_number()
  playoffs <- raw %>%
    html_nodes("td:nth-child(17)") %>%
    html_text()
  coaches_raw <- raw %>%
    html_nodes("td:nth-child(18)") %>%
    as.character()
  top_ws_raw <- raw %>%
    html_nodes("td:nth-child(19)") %>%
    as.character()
  tibble(season, url, league, wins, losses, finish, pace, rel_pace,
         o_rtg, rel_o_rtg, d_rtg, rel_d_rtg, playoffs, coaches_raw,
         top_ws_raw)
}

get_roster <- function(year, id, url) {
  filename <- str_glue("data/nba/", id, "/", year, ".rds")
  if (file.exists(filename)) return(read_rds(filename))
  basedir <- dirname(filename)
  if (!dir.exists(basedir)) dir.create(basedir, recursive = TRUE)
  Sys.sleep(0.4)
  print(str_glue("fetching roster from ", url))
  raw <- read_html(str_glue("https://www.basketball-reference.com", url))
  number <- raw %>%
    html_nodes("#roster tbody th") %>%
    html_text()
  player <- raw %>%
    html_nodes("#roster td:nth-child(2) a") %>%
    html_text()
  url <- raw %>%
    html_nodes("#roster td:nth-child(2) a") %>%
    html_attr("href")
  position <- raw %>%
    html_nodes("#roster td:nth-child(3)") %>%
    html_text()
  height <- raw %>%
    html_nodes("#roster td:nth-child(4)") %>%
    html_text()
  weight <- raw %>%
    html_nodes("#roster td:nth-child(5)") %>%
    html_text() %>%
    parse_number()
  birthdate <- raw %>%
    html_nodes("#roster td:nth-child(6)") %>%
    html_text() %>%
    str_trim() %>%
    parse_date("%B %d, %Y")
  nationality <- raw %>%
    html_nodes("#roster td:nth-child(7)") %>%
    html_text()
  experience <- raw %>%
    html_nodes("#roster td:nth-child(8)") %>%
    html_text() %>%
    str_replace("R", "0") %>%
    parse_number()
  college <- raw %>%
    html_nodes("#roster td:nth-child(9)") %>%
    html_text()
  tibble(number, player, url, position, height, weight, birthdate, nationality,
         experience, college) %>%
    write_rds(filename)
}

teams_index <- get_teams_index()
per_team_index <- teams_index %>%
  mutate(per_season = map(url, get_team_index)) %>%
  write_rds("data/nba/teams.rds")
per_team_index %>%
  rename(team_url = url) %>%
  unnest() %>%
  select(c(name, season, url)) %>%
  mutate(year = parse_number(season),
         id = str_split(url, "/") %>% map_chr(~nth(.x, 3)),
         roster = pmap(list(year, id, url), get_roster)) %>%
  write_rds("data/nba/rosters.rds")
