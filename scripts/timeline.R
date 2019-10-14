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
library(tidyverse)
library(lubridate)
library(scales)
library(here)
library(dichromat)
library(cowplot)

#' It's been helpful for me to write out my work history in code.
#' For one thing, it's highly transparent, and I value that in my career search.
#' For another, it allows me to commit dates and events to paper
#' that I might otherwise forget as I move on in my career.
today <- as.character(Sys.Date())
state_colors <- dichromat::colorschemes$SteppedSequential.5

#' Locations:
#' 
#' * Most dates are approximate.
#' * `dichromat` colors chosen for colorblind readers.
locations <- tribble(
  ~id,  ~start_date,    ~end_date,      ~location,         ~region, ~color,
    1, "1996-08-01", "2000-12-22",  "Wheaton, IL",      "Illinois",      2,
    2, "2004-08-01", "2006-06-01",   "Nashua, NH", "New Hampshire",      3,
    3, "2006-06-15", "2015-06-23", "Bay Area, CA",    "California",      5,
    4, "2015-06-30",        today,   "Austin, TX",         "Texas",      4,
) %>% 
  mutate_at(
    vars(ends_with("_date")),
    ymd
  ) %>% 
  mutate(
    map_fills = state_colors[(color - 1) * 5 + 4],
    saturated = state_colors[(color - 1) * 5 + 2]
  ) %>% 
  print()

work_locations <- tail(locations, -1) %>% 
  print()

#' Map and timeline colors:
map_fills <- setNames(
  c(locations[["map_fills"]], "gray" ),
  c(locations[["region"   ]], "Other")
) %>% 
  print()

saturated <- setNames(
  c(locations[["saturated"]], "white"),
  c(locations[["region"   ]], "Other")
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
    "Nashua",
    "Pleasanton",
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
      2,       2, "2006-10-01", "2011-10-14", "Mktg. Specialist\n/ Analyst",
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
    label      = c("Led Marketing\nData Science Team",
                   "Led CX\nAnalytics Team"),
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

#' Individuals I've managed, anonymized with dates, positions, and major events.
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

#' Legend of locations (maybe tie colors to other timeline & education? maybe.):
map_data("state") %>% 
  mutate(
    region = region %>% 
      str_to_title() %>% 
      fct_other(keep = locations[["region"]])
  ) %>% 
  ggplot() +
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = region),
    color = "white",
    size = 1
  ) +
  scale_fill_manual(values = map_fills) +
  coord_map(
    projection = "albers",
    lat0 = 39,
    lat1 = 45
  ) +
  theme_nothing()

#' Notes on graphics...
#' 
#' Need to experiment with the right numbers for graphics device,
#' so it doesn't show ugly lines (if I even use a gradient).
#' These look very pretty with 324 x 799.
#' 
#' I tried for rounded rectangles:
#' 
#' * https://stackoverflow.com/a/52822082/573332
#' * https://github.com/hrbrmstr/statebins/blob/master/R/geom-rrect.r
#' 
#' But they didn't work right with a reversed y-axis.
#' 
#' Also tried a fade on the right,
#' but it just looked blurry, accidental, and dirty.
#' 
#' Finally, here's the StackOverflow link
#' to [flip the y-axis](https://stackoverflow.com/a/43626186/573332).
#' 
#' This is what I've got for now.
#' Colors and proportions and such might change.
#' Heck, I'm really open to changing any of it; this was a POC.
#' But let's get everything else down, and then decide how to proceed.
#' 
#' Also under consideration...
#' When I started working with what languages, and what projects I worked on?
#' Awards, etc.?
#' For the management section, include a concise summary, perhaps?
#' 2 teams, 6 hires, 5 promoted?
#+ fig.width = 472 / 96, fig.height = 799 / 96, dpi = 96
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
gradations <- 250
decay      <- .25
timeline_x         <- 0.23
timeline_to_labels <- 0.03
timeline_to_team   <- 0.08
team_spacing       <- 0.05
locations_x               <- .7
locations_to_employer_box <- 0.05
employer_box_to_team_box  <- 0.05
box_to_name               <- 0.02

# default text parameters on timeline itself
update_geom_defaults("text", list(
  size       = 3,
  lineheight = 1,
  hjust      = 1,
  vjust      = 0.75
))

ggplot() +
  theme_nothing() +
  theme(
    axis.text.y = element_text(
      face          = "italic",
      color         = "gray30",
      size          = 9,
      vjust         = 0.5,
      hjust         = 1,
      angle         = 0,
      lineheight    = 0.9,
      margin        = margin(0, 10, 0, -7),
      inherit.blank = FALSE
    )
  ) +
  scale_y_continuous(
    trans    = rev_date,
    breaks   = ymd("2005-01-01") + years(0:14),
    labels   = function(x) format(x, "'%y"),
    position = "right"
  ) +
  scale_alpha_identity() +
  scale_fill_manual(
    values = saturated
  ) +
  scale_shape_manual(
    values = c(
      start             = 21,
      end               = 16,
      hired             = 21,
      promoted          = 16,
      `transitioned in` = 16
    )
  ) +
  geom_rect( # locations as colored bars
    aes(
      xmin  = locations_x - (alpha - 1) / gradations,
      xmax  = locations_x -  alpha      / gradations,
      ymin  = start_date,
      ymax  = end_date,
      fill  = region,
      alpha = exp(-decay * alpha)
    ),
    data = crossing(work_locations, tibble(alpha = 1:gradations - 1))
  ) +
  geom_rect( # employers as rectangles
    aes(
      xmin = 0,
      xmax = locations_x - locations_to_employer_box,
      ymin = start_date,
      ymax = end_date
    ),
    alpha = 0.2,
    data  = employers
  ) +
  geom_text( # employer names
    aes(
      x     = locations_x - locations_to_employer_box - box_to_name,
      y     = start_date + weeks(11),
      label = paste(employer, cities, sep = "\n")
    ),
    data = employers
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
      x     = timeline_x - timeline_to_labels,
      y     = padded_start,
      label = position
    ),
    data = pad_ends(positions)
  ) +
  geom_rect( # teams as rectangles
    aes(
      xmin = timeline_x + timeline_to_team,
      xmax = locations_x - locations_to_employer_box - employer_box_to_team_box,
      ymin = start_date,
      ymax = end_date
    ),
    alpha = 0.15,
    data  = pad_teams(teams_managed)
  ) +
  geom_text( # team labels
    aes(
      x     = locations_x - 
                locations_to_employer_box -
                employer_box_to_team_box -
                box_to_name,
      y     = start_date + weeks(12),
      label = label
    ),
    color = "gray20",
    hjust = 0,
    vjust = 1,
    angle = -90,
    data  = teams_managed
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
    color = "gray20",
    fill  = "gray",
    data  = people_managed %>% 
      filter(event %in% c("hired", "promoted", "transitioned in")) %>% 
      mutate(
        date = date +
          if_else(date %in% teams_managed$start_date, weeks(6), weeks(0)))
  )
