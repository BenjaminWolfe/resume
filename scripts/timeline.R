#' ---
#' title: "Resume"
#' author: "Benjamin Wolfe"
#' date: "September 20, 2019"
#' output:
#'   html_document:
#'     code_folding: hide
#' ---
#' 
#+ message=FALSE
knitr::opts_chunk$set(
  fig.path = "../images/",
  dev.args = list(bg="transparent")
)

library(tidyverse)
library(lubridate)
library(scales)
library(here)
library(cowplot)
library(extrafont)

#' It's been helpful for me to write out my work history in code.
#' For one thing, it's highly transparent, and I value that in my career search.
#' For another, it allows me to commit dates and events to paper
#' that I might otherwise forget as I move on in my career.
today <- as.character(Sys.Date())

#' Locations:
#' 
#' * Most dates are approximate.
locations <- tribble(
  ~id,  ~start_date,    ~end_date,      ~location,
    1, "1996-08-01", "2000-12-22",  "Wheaton, IL",
    2, "2004-08-01", "2006-06-01",   "Nashua, NH",
    3, "2006-06-15", "2015-06-23", "Bay Area, CA",
    4, "2015-06-30",        today,   "Austin, TX",
) %>% 
  mutate_at(
    vars(ends_with("_date")),
    ymd
  ) %>% 
  print()

#' Employers:
#' 
#' * Dates are approximate until the end of Farmers / start of Esurance.
employers <- tibble(
  id = 1:3,
  start_date = ymd(c(
    "2004-08-01",
    "2006-10-01",
    "2011-10-17"
  )),
  end_date = ymd(c(
    "2006-06-01",
    "2011-10-14",
    today
  )),
  employer = c(
    "Nashua HS North",
    "Farmers Insurance Group",
    "Esurance"
  ),
  cities = c(
    "Nashua, NH",
    "Pleasanton, CA",
    "San Francisco; Austin"
  ),
  positions = c(
    "Spanish Teacher",
    "Marketing Specialist / Analyst",
    "Various Positions"
  )
) %>% 
  print()

#' Positions:
#' 
#' * Teaching dates are approximate.
#' * Technically I began at Farmers as a Marketing Trainee
#'   (for that matter I was a temp before that),
#'   and I believe I ended as a Senior Marketing Specialist.
#'   They did not have an analyst title at my location,
#'   but it was offered to me as they created a new team just before I left.
#' * Farmers start date is approximate; dates after that are accurate.
#' 
#' Esurance management titles are nuanced as well.
#' 
#' * I started _managing_ with my first hire, before being promoted to manager.
#'   (See _people managed_, below.)
#' * While I never had the title of "Manager of Data Science,"
#'   we handled data science work, and my reports were data scientists.
#'   With the hiring of Deepak Ramanathan on 2014-09-24,
#'   my team was subsumed under the newly formed Data Science Group.
positions <- 
  tribble(
    ~id, ~emp_id,  ~start_date,    ~end_date,  ~position,
      1,       1, "2004-08-01", "2006-06-01", "Spanish\nTeacher",
      2,       2, "2006-10-01", "2011-10-14", "Mktg.\nSpecialist\n/ Analyst",
      3,       3, "2011-10-17", "2012-11-07", "Marketing\nAnalyst",
      4,       3, "2012-11-07", "2014-04-01", "Sr. Marketing\nAnalyst",
      5,       3, "2014-04-01", "2015-09-23", "Manager,\nData Science",
      6,       3, "2015-09-23", "2019-04-18", "Manager,\nCX Analytics",
      7,       3, "2019-04-18",        today, "Lead\nCX Analyst"
  ) %>% 
  mutate_at(
    vars(ends_with("_date")),
    ymd
  ) %>% 
  print()

pad_ends <- function(positions,
                     pad_start = weeks(11),
                     pad_end = weeks(2)) {
  
  padded_start_dates <- 
    positions %>% 
    group_by(emp_id) %>% 
    filter(row_number(start_date) == 1) %>% 
    pull(id)
  
  padded_end_dates <- 
    positions %>% 
    group_by(emp_id) %>% 
    filter(row_number(desc(end_date)) == 1) %>% 
    pull(id)
  
  positions %>% 
    mutate(
      padded_start = start_date + 
        if_else(id %in% padded_start_dates, pad_start, days(0)),
      padded_end = end_date -
        if_else(id %in% padded_end_dates, pad_end, days(0))
    )
}

#' Teams I've managed:
teams_managed <- 
  tibble(
    id         = 1:2,
    start_date = ymd(c("2013-03-11", "2015-09-23")),
    end_date   = ymd(c("2015-09-23", "2019-04-18")),
    type       = c("Data Science", "CX Analytics"),
    label      = c("Led Marketing\nData Science Team\nSan Francisco, CA",
                   "Led CX Analytics Team:\nAustin, TX (remote)"),
    details    = c("built, handed over", "promoted to, flattened")
  ) %>% 
  print()

pad_teams <- function(teams_managed) {
  teams_managed %>% 
    mutate(
      end_date = end_date - if_else(id == 1, weeks(1), weeks(0)),
      start_date = start_date + if_else(id == 2, weeks(1), weeks(0))
    )
}

#' Positions I've managed:
positions_managed <- 
  tribble(
    ~id,                        ~position,
      1,    "Associate Marketing Analyst",
      2,              "Marketing Analyst",
      3,                "Website Analyst",
      4, "Assoc Customer Insight Analyst",
      5,       "Customer Insight Analyst",
      6, "Sr Customer Experience Analyst",
      7,   "Sr Sales and Service Analyst",
      8,                 "Data Scientist"
) %>% 
  print()

#' Direct reports, anonymized, with dates, positions, and major events.
#' I figure it's worthwhile to get this information as specifically as possible,
#' since I won't always be able to pull this up quite as easily. :)
people_managed <- 
  tribble(
    ~team, ~id, ~col,        ~date,             ~event, ~title,          ~memo,
        1,   1,    1, "2013-03-11",            "hired",      1,             NA,
        1,   1,    1, "2014-04-01",         "promoted",      2,             NA,
        1,   1,    1, "2014-11-17",         "promoted",      8,             NA,
        1,   1,    1, "2015-09-23", "transitioned out",     NA, "actually 7/2",
        1,   2,    2, "2013-05-28",            "hired",      1,             NA,
        1,   2,    2, "2014-04-01",         "promoted",      2,             NA,
        1,   2,    2, "2014-08-05",         "promoted",      8,             NA,
        1,   2,    2, "2014-08-06",           "exited",     NA,  "sister team",
        1,   3,    2, "2014-11-10",            "hired",      1,             NA,
        1,   3,    2, "2015-09-23", "transitioned out",     NA,             NA,
        2,   4,    2, "2015-09-23",  "transitioned in",      7,             NA,
        2,   4,    2, "2016-02-26",         "promoted",      6,             NA,
        2,   4,    2, "2017-05-27",           "exited",     NA,     "external",
        2,   5,    3, "2015-09-23",  "transitioned in",      7,             NA,
        2,   5,    3, "2016-02-26",         "promoted",      6,             NA,
        2,   5,    3, "2016-07-29",           "exited",     NA,           "BI",
        2,   6,    1, "2015-09-23",  "transitioned in",      3,             NA,
        2,   6,    1, "2015-12-21",           "exited",     NA,     "external",
        2,   7,    1, "2016-03-30",  "transitioned in",      4,             NA,
        2,   7,    1, "2016-08-01",         "promoted",      5,             NA,
        2,   7,    1, "2018-08-24",         "promoted",      6,             NA,
        2,   7,    1, "2019-04-18", "transitioned out",     NA,             NA,
        2,   8,    3, "2017-09-22",            "hired",      5,     "internal",
        2,   8,    3, "2018-05-08",           "exited",     NA,     "external",
        2,   9,    2, "2018-05-14",            "hired",      5,             NA,
        2,   9,    2, "2019-04-18", "transitioned out",     NA,             NA,
        2,  10,    3, "2018-10-22",            "hired",      5,             NA,
        2,  10,    3, "2019-04-18", "transitioned out",     NA,             NA
  ) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(
    padded_date = date +
      case_when(
        id == 1 & event == "hired"  ~ weeks( 4),
        event == "transitioned in"  ~ weeks( 2),
        event == "transitioned out" ~ weeks(-2),
        TRUE ~ weeks(0)
      )
  ) %>% 
  print()

#' Notes on graphics...
#' 
#' I tried for rounded rectangles:
#' 
#' * https://stackoverflow.com/a/52822082/573332
#' * https://github.com/hrbrmstr/statebins/blob/master/R/geom-rrect.r
#' 
#' But they didn't work right with a reversed y-axis.
#' 
#' Here's the StackOverflow link
#' to [flip a date y-axis](https://stackoverflow.com/a/43626186/573332).
#' 
#' Also under consideration...
#' When I started working with what languages, and what projects I worked on?
#' Awards, etc.?
#' For the management section, include a concise summary, perhaps?
#' 2 teams, 6 hires, 5 promoted?
#' Not sure what I think of my legend placement. Good for now.
#' Kind of like the idea of laying them horizontally at the top.
#+ timeline, fig.width = 385 / 96, fig.height = 799 / 96, dpi = 96
# reverse date transformation
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  
  a <- as.trans(a)
  b <- as.trans(b)
  
  name <- paste(a$name, b$name, sep = "-")
  
  trans <- function(x) a$trans(b$trans(x))
  inv   <- function(x) b$inverse(a$inverse(x))
  
  trans_new(name, trans, inverse = inv, breaks = breaks, format = format)
}

rev_date <- c_trans("reverse", "date")

# graphics parameters
timeline_x         <- 0.16
timeline_to_labels <- 0.03
timeline_to_team   <- 0.06
team_spacing       <- 0.05
right_edge         <- timeline_x + 0.4
edge_to_team_box   <- 0.05
box_to_name        <- 0.02

# default text parameters on timeline itself
update_geom_defaults("text", list(
  size       = 3.2,
  lineheight = 1,
  hjust      = 1,
  vjust      = 0.8,
  family     = "Calibri"
))

ggplot() +
  theme_nothing() + # thematic elements
  theme(
    axis.text.y = element_text(
      family        = "Calibri",
      face          = "italic",
      color         = "gray30",
      size          = 9,
      vjust         = 0.5,
      hjust         = 1,
      angle         = 0,
      lineheight    = 0.9,
      margin        = margin(0, 10, 0, -7),
      inherit.blank = FALSE
    ),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  ) +
  scale_y_continuous( # scales: y, alpha, fill, shape
    trans    = rev_date,
    breaks   = ymd("2005-01-01") + years(0:14),
    labels   = function(x) format(x, "'%y"),
    position = "right"
  ) +
  scale_alpha_identity() +
  scale_shape_manual(
    values = c(
      start             = 21,
      end               = 16,
      hired             = 21,
      promoted          = 16,
      `transitioned in` = 16
    )
  ) +
  geom_rect( # employers as rectangles
    aes(
      xmin = 0,
      xmax = right_edge,
      ymin = start_date,
      ymax = end_date
    ),
    color = "white",
    alpha = 0.2,
    data  = employers
  ) +
  geom_text( # employer names
    aes(
      x     = right_edge - box_to_name,
      y     = start_date + weeks(16),
      label = paste(employer, cities, sep = "\n")
    ),
    fontface = "bold",
    data     = employers
  ) +
  geom_segment( # positions as segments
    aes(
      y    = padded_start,
      yend = padded_end,
      x    = timeline_x,
      xend = timeline_x
    ),
    data = positions %>% pad_ends()
  ) +
  geom_point( # position end points
    aes(
      x     = timeline_x,
      y     = event_date,
      shape = date_type
    ),
    fill = "gray",
    data = positions %>% 
      pad_ends() %>% 
      pivot_longer(
        cols         = starts_with("padded_"),
        names_to     = "date_type",
        names_prefix = "padded_",
        values_to    = "event_date"
      ) %>% 
      group_by(event_date) %>% 
      filter(
        row_number(date_type) == 1,
        !(date_type == "end" & end_date %in% employers$end_date)
      )
  ) +
  geom_text( # position names
    aes(
      x = timeline_x - timeline_to_labels,
      y = padded_start + 
            case_when(
              id == 2 ~  weeks(3),
              id == 7 ~ -weeks(3),
              TRUE    ~  weeks(0)
            ),
      label = position
    ),
    data = pad_ends(positions)
  ) +
  geom_rect( # teams as rectangles
    aes(
      xmin = timeline_x + timeline_to_team,
      xmax = right_edge - edge_to_team_box,
      ymin = start_date,
      ymax = end_date
    ),
    alpha = 0.15,
    data  = pad_teams(teams_managed)
  ) +
  geom_text( # team labels
    aes(
      x     = right_edge - edge_to_team_box - box_to_name,
      y     = start_date + weeks(12),
      label = label
    ),
    color    = "gray20",
    fontface = "bold",
    hjust    = 0,
    vjust    = 1,
    angle    = -90,
    data     = teams_managed
  ) +
  geom_segment( # direct reports as segments
    aes(
      x    = timeline_x + timeline_to_team + col * team_spacing,
      xend = timeline_x + timeline_to_team + col * team_spacing,
      y    = start_date,
      yend = end_date
    ),
    color = "gray20",
    data  = people_managed %>% 
      group_by(col, id) %>% 
      summarise(
        start_date = min(date),
        end_date   = max(date)
      ) %>% 
      mutate(
        start_date = start_date +
          if_else(start_date %in% teams_managed$start_date, weeks(6), weeks(0)),
        end_date = end_date -
          if_else(end_date %in% teams_managed$end_date[1], weeks(2), weeks(0))
      )
  ) +
  geom_point( # direct report career events
    aes(
      x     = timeline_x + timeline_to_team + col * team_spacing,
      y     = date,
      shape = event
    ),
    color = "gray10",
    fill  = "gray",
    data  = people_managed %>% 
      filter(event %in% c("hired", "promoted", "transitioned in")) %>% 
      mutate(
        date = date +
          if_else(date %in% teams_managed$start_date, weeks(6), weeks(0)))
  ) +
  geom_segment( # legend: segments
    aes(
      x    = timeline_x + timeline_to_team + team_spacing * 0.5,
      xend = timeline_x + timeline_to_team + team_spacing * 1.25,
      y    = y,
      yend = y
    ),
    data = tibble(y = ymd(c("2008-07-01", "2008-11-01")))
  ) +
  geom_point( # legend: segment endpoints
    aes(
      x     = timeline_x + timeline_to_team + team_spacing * 0.5,
      y     = y,
      shape = type
    ),
    color  = "gray10",
    fill   = "gray",
    data   = tibble(
      y    = ymd(c("2008-07-01", "2008-11-01")),
      type = c("hired", "promoted")
    )
  ) +
  geom_text( # legend: labels
    aes(
      x     = timeline_x + timeline_to_team + team_spacing * 1.3,
      y     = y,
      label = label
    ),
    hjust = 0,
    vjust = 0.3,
    data    = tibble(
      y     = ymd(c("2008-07-01", "2008-11-01")),
      label = c("new hire", "promotion / lateral move")
    )
  ) +
  annotate( # explain direct reports
    "text",
    x     = timeline_x + timeline_to_team + team_spacing * 0.5,
    y     = ymd("2012-12-01"),
    label = "Direct Reports",
    hjust = 0
  )
