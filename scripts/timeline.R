#' ---
#' title: "Resume"
#' author: "Benjamin Wolfe"
#' date: "September 20, 2019"
#' output: html_document
#' ---
#' 
#+ message=FALSE
library(tidyverse)
library(lubridate)
library(scales)
library(here)
library(png)
library(gridGraphics)

#' I'm working on a way to encapsulate the management _portion_
#' of my work history at Esurance.
#' 
#' Teams I've managed:
teams_managed <- 
  tribble(
    ~id,          ~type,                 ~details,  ~start_date,    ~end_date,
      1, "Data Science",     "built, handed over", "2013-03-11", "2015-09-23",
      2, "CX Analytics", "promoted to, flattened", "2015-09-23", "2019-04-18"
  ) %>% 
  mutate(end_date = ymd(end_date))

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
)

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
        2,   6,    4, "2015-09-23",  "transitioned in",      3,             NA,
        2,   6,    4, "2015-12-21",           "exited",     NA,     "external",
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
  mutate(date = ymd(date))

#' This `plot_date` just lets me add a little padding inside team boundaries:
people_managed <- 
  people_managed %>% 
  mutate(
    plot_date = date +
      case_when(
        id == 1 & event == "hired"  ~ weeks(4),
        event == "transitioned in"  ~ weeks(2),
        event == "transitioned out" ~ weeks(-2),
        TRUE ~ weeks(0)
      )
  )

#' Here's an arbitrary transform function for flipping the y-axis.
#' There are other ways to do this, but some are more brittle.
#' For example, custom annotations don't work with reverse transforms.
arbitrary_transform <- function(date) {
  ymd("2030-01-01") - as.period(ymd("1990-01-01") %--% date)
}

#' There are likely better ways to do these icons (below).
#' Since they've become vanishingly small, I may end up using colored dots.
#' If I use icons, I may use https://thenounproject.com/.
icons <- c(
  hired    = "green-check.png", # http://bit.ly/green-check-png
  promoted = "star.png"         # http://bit.ly/star-png
) %>% 
  map(
    ~ here("assets", .) %>% 
      readPNG() %>% 
      rasterGrob(interpolate = FALSE)
  )

#' Here's a function to draw the timeline.
draw_timeline <- function(plot_margin    =  1.00,
                          team_l_margin  =  1.25,
                          team_r_margin  =  2.75,
                          team_alpha     =  0.20,
                          line_size      =  1.50,
                          joint_size     =  1.30,
                          line_color     = "grey",
                          hired_width    =  0.36,
                          hired_nudge    = -0.02,
                          promoted_width =  0.45,
                          exited_nudge   =  0.30,
                          text_size      =  2.50,
                          arrow_x_length =  0.30,
                          arrow_y_length = months(2)) {
  teams <- 
    geom_rect(
      aes(
        xmin = min(people_managed$col) - team_l_margin,
        xmax = max(people_managed$col) + team_r_margin,
        ymin = arbitrary_transform(start_date),
        ymax = arbitrary_transform(end_date),
        fill = type,
        color = type
      ),
      alpha = team_alpha,
      data  = teams_managed
    )
  
  trajectories <- 
    geom_line(
      aes(
        y     = arbitrary_transform(plot_date),
        x     = col,
        group = id
      ),
      color = line_color,
      size  = line_size,
      data  = people_managed
    )
  
  hires <- 
    people_managed %>% 
    filter(event == "hired") %>% 
    transmute(
      annotation = map2(
        col,
        plot_date,
        ~ annotation_custom(
          icons[["hired"]],
          xmin = .x + hired_nudge - hired_width / 2,
          xmax = .x + hired_nudge + hired_width / 2,
          ymax = arbitrary_transform(.y) + months(1),
          ymin = arbitrary_transform(.y) - months(1)
        ))
    ) %>% 
    pull()
  
  promotions <- 
    people_managed %>% 
    filter(event == "promoted") %>% 
    transmute(
      annotation = map2(
        col,
        plot_date,
        ~ annotation_custom(
          icons[["promoted"]],
          xmin = .x - promoted_width / 2,
          xmax = .x + promoted_width / 2,
          ymax = arbitrary_transform(.y) + months(1),
          ymin = arbitrary_transform(.y) - months(1)
        ))
    ) %>% 
    pull()
  
  departures <- 
    geom_segment(
      aes(
        y    = arbitrary_transform(plot_date),
        x    = col,
        yend = arbitrary_transform(plot_date + arrow_y_length),
        xend = col + arrow_x_length
      ),
      arrow = arrow(length = unit(0.1, "inches")),
      color = line_color,
      size  = line_size,
      data  = filter(people_managed, event == "exited")
    )
  
  departure_joints <- 
    geom_point(
      aes(
        y = arbitrary_transform(plot_date),
        x = col
      ),
      color = line_color,
      size  = joint_size,
      data  = filter(people_managed, event == "exited")
    )
  
  departure_notes <- 
    geom_text(
      aes(
        y     = (arrow_y_length + plot_date) %>% arbitrary_transform(),
        x     = (arrow_x_length + col),
        label = memo
      ),
      hjust   = "left",
      nudge_x = exited_nudge,
      color   = line_color,
      size    = text_size,
      data    = filter(people_managed, event == "exited")
    )
  
  team_labels <- list(
    annotate(
      "text",
      x = 4.5,
      y = arbitrary_transform(ymd("2017-02-15")),
      label = "CX\nAnalytics",
      lineheight = 1
    ),
    annotate(
      "text",
      x = 4.5,
      y = arbitrary_transform(ymd("2014-02-15")),
      label = "Data\nScience",
      lineheight = 1
    )
  )

  ggplot() +
    trajectories +
    hires +
    departures +
    departure_joints +
    departure_notes +
    promotions +
    teams +
    team_labels +
    scale_y_date() +
    scale_x_continuous(
      limits = c(
        min(people_managed$col) - team_l_margin - plot_margin,
        max(people_managed$col) + team_r_margin + plot_margin
      )
    ) +
    theme(
      panel.background = element_rect(fill = NA),
      panel.grid       = element_blank(),
      axis.title       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      legend.position  = "none"
    )
  }

#' Let's draw it. This is what I've got for now.
#' Colors and proportions and such might change.
#' Heck, I'm really open to changing any of it; this was a POC.
#' But let's get everything else down, and then decide how to proceed.
#' 
#' Also under consideration...
#' Integrate this with the rest of a timeline?
#' A larger narrative?
#' When I started working with what languages, and what projects I worked on?
#' Awards, etc.?
#' For the management section, include a concise summary, perhaps?
#' 2 teams, 6 hires, 5 promoted?
#+ fig.width=2, fig.height=6.5, dpi=96
draw_timeline()
