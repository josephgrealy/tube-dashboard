
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(janitor)
library(lubridate)
library(bslib)

# Load your data
journeys <- read_csv("data/victoria-line-data.csv") %>%
  clean_names() |> 
  mutate(
    journey_time = dmy_hm(what_time_did_you_get_on_the_tube),
    date = as_date(journey_time),
    time = format(journey_time, "%H:%M")
  ) |> 
  mutate(
    who_are_you = factor(who_are_you),
    direction = factor(direction, levels = c("Northbound", "Southbound")),
    how_busy = factor(how_busy, 
                      levels = c("Empty", "Some seats taken", "All seats taken", "Busy", "Crazy busy", "Can't get on!"))
  ) |> 
  select(journey_time, date, time, who_are_you, boarding_station,
          destination_station, direction, carriage_number, 
          where_on_the_train, how_busy, straight_from_the_depot)

# ---- Colours ----
tfl_colours <- c(
  victoria = "#039BE5",  # main hero
  circle   = "#FFC80A",
  central  = "#DC241F",
  jubilee  = "#838B93"
)

# helper for styled narrative text
styled_text <- function(label, value, colour="#039BE5") {
  HTML(paste0(label, " <b style='color:", colour, ";'>", value, "</b>"))
}

# ---- UI ----
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", primary = tfl_colours["victoria"]),
  
  titlePanel(div("Victoria Line Wrapped 🚇", 
                 style = paste0("color:", tfl_colours["victoria"], 
                                "; font-weight:bold; font-size:2em;"))),
  
  selectInput("who", "Select traveller:", choices = unique(journeys$who_are_you)),
  br(),
  
  # Core stats
  uiOutput("total_journeys_text"),
  plotlyOutput("journeys_over_time"),
  br(),
  
  uiOutput("popular_day_text"),
  plotlyOutput("weekday_pattern"),
  br(),
  
  uiOutput("popular_hour_text"),
  plotlyOutput("time_of_day"),
  br(),
  
  uiOutput("station_text"),
  plotlyOutput("stations_board"),
  br(),
  
  # Fun extras
  h3("Fun Extras 🎉", style=paste0("color:", tfl_colours["victoria"], ";")),
  br(),
  
  uiOutput("seat_text"),
  plotlyOutput("seat_position"),
  br(),
  
  uiOutput("busy_text"),
  plotlyOutput("how_busy"),
  br(), 
  
  uiOutput("streak_text"),
  br(), br(),
  
  uiOutput("carriage_numbers"),
  br(),
  
  uiOutput("depot_count"),
  
  br(), br(),
  div("See you down the line 👋", 
      style=paste0("font-size:1.5em; font-weight:bold; color:", 
                   tfl_colours["victoria"], "; text-align:center; margin:40px;"))
)

# ---- Server ----
server <- function(input, output, session) {
  
  filtered <- reactive({
    journeys %>% filter(who_are_you == input$who)
  })
  
  # ---- Core stats ----
  output$total_journeys_text <- renderUI({
    total <- nrow(filtered())
    styled_text("You've been on", paste(total, "journeys this year."), tfl_colours["victoria"])
  })
  
  output$journeys_over_time <- renderPlotly({
    filtered() %>%
      count(date) %>%
      plot_ly(x=~date, y=~n, type="bar", marker=list(color=tfl_colours["victoria"])) %>%
      layout(title="Journeys per day")
  })
  
  output$popular_day_text <- renderUI({
    topday <- filtered() %>%
      mutate(weekday = wday(date, label=TRUE)) %>%
      count(weekday) %>% arrange(desc(n)) %>% slice(1)
    styled_text("Your most popular day to travel the Viccy line is", as.character(topday$weekday), tfl_colours["circle"])
  })
  
  output$weekday_pattern <- renderPlotly({
    filtered() %>%
      mutate(weekday = wday(date, label=TRUE)) %>%
      count(weekday) %>%
      plot_ly(x=~weekday, y=~n, type="bar", marker=list(color=tfl_colours["circle"])) %>%
      layout(title="Journeys by weekday")
  })
  
  output$popular_hour_text <- renderUI({
    hourtab <- filtered() %>%
      mutate(hour = hour(journey_time)) %>%
      count(hour) %>% arrange(desc(n)) %>% slice(1)
    styled_text("Your busiest travel hour is", paste0(hourtab$hour, ":00"), tfl_colours["central"])
  })
  
  output$time_of_day <- renderPlotly({
    filtered() %>%
      mutate(hour = hour(journey_time)) %>%
      count(hour) %>%
      plot_ly(x=~hour, y=~n, type="bar", marker=list(color=tfl_colours["central"])) %>%
      layout(title="Journeys by time of day", xaxis=list(dtick=1))
  })
  
  output$station_text <- renderUI({
    topstation <- filtered() %>%
      filter(!is.na(boarding_station)) %>%
      count(boarding_station, sort=TRUE) %>% slice(1)
    styled_text("Your most common boarding station was", topstation$boarding_station, tfl_colours["jubilee"])
  })
  
  output$stations_board <- renderPlotly({
    filtered() %>%
      filter(!is.na(boarding_station)) %>%
      count(boarding_station, sort=TRUE) %>%
      top_n(10) %>%
      plot_ly(x=~n, y=~reorder(boarding_station, n), type="bar",
              orientation="h", marker=list(color=tfl_colours["jubilee"])) %>%
      layout(title="Most common boarding stations", xaxis=list(title="Journeys"), yaxis=list(title=""))
  })
  
  # ---- Fun extras ----
  output$seat_text <- renderUI({
    styled_text("Here’s where you like to sit/stand on the train:", "", tfl_colours["victoria"])
  })
  
  output$seat_position <- renderPlotly({
    filtered() %>%
      count(where_on_the_train) %>%
      plot_ly(x=~where_on_the_train, y=~n, type="bar", marker=list(color=tfl_colours["victoria"])) %>%
      layout(title="Seat position (carriage sections)")
  })
  
  output$busy_text <- renderUI({
    styled_text("How crowded were your journeys?", "", tfl_colours["central"])
  })
  
  output$how_busy <- renderPlotly({
    filtered() %>%
      count(how_busy) %>%
      plot_ly(x=~how_busy, y=~n, type="bar", marker=list(color=tfl_colours["central"])) %>%
      layout(title="Crowding experience")
  })
  
  output$streak_text <- renderUI({
    d <- filtered() %>% distinct(date) %>% arrange(date)
    if (nrow(d) == 0) return("No journeys recorded.")
    streaks <- rle(c(1, diff(d$date)) == 1)
    longest <- max(streaks$lengths[streaks$values], na.rm=TRUE)
    styled_text("Your longest streak of consecutive travel days was", paste(longest, "days"), tfl_colours["circle"])
  })
  
  output$carriage_numbers <- renderUI({
    d <- filtered() %>% count(carriage_number, sort=TRUE)
    
    grouped <- d %>%
      group_by(n) %>%
      summarise(numbers = paste(carriage_number, collapse=", "), .groups="drop") %>%
      arrange(desc(n)) %>%
      head(3)   # 👈 only keep top 3 groups
    
    txt <- paste0(
      "<div style='border:2px solid ", tfl_colours["jubilee"],
      "; border-radius:10px; padding:15px; background:#f9f9f9;'>",
      "<b>Your top 3 carriage ride counts:</b><br>"
    )
    for (i in seq_len(nrow(grouped))) {
      txt <- paste0(txt, grouped$n[i], " times: ", grouped$numbers[i], "<br>")
    }
    txt <- paste0(txt, "</div>")
    HTML(txt)
  })
  
  output$depot_count <- renderUI({
    depot <- filtered() %>% filter(!is.na(straight_from_the_depot) & straight_from_the_depot == "Yes")
    styled_text("You got on a fresh-from-the-depot train", paste(nrow(depot), "times!"), tfl_colours["victoria"])
  })
}

shinyApp(ui, server)