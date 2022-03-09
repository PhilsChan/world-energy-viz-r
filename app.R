library(tidyverse)
library(dash)
library(dashBootstrapComponents)
library(plotly)

data <- read_csv("data/Primary-energy-consumption-from-fossilfuels-nuclear-renewables.csv")
all_country <- data %>%
  filter(!is.na(Code) & Entity != "World") %>%
  pull(Entity) %>%
  unique()

all_continent <- data %>%
  filter(is.na(Code)) %>%
  pull(Entity) %>%
  unique()

all_years <- data$Year %>%
  unique()

app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$title("World Energy VIsualization")

sidebarStyle = list(
  "position"= "fixed",
  "top"= 0,
  "left"= 0,
  "bottom"= 0,
  "padding"= "2rem 1rem",
   #"background-image"= "url(/assets/wind-energy.jpg)",
  "background-color" = "rgba(64, 192, 64, 0.6)",
  "background-blend-mode" = "overlay"
)

tabStyle = list(
  "position"="fixed",
  "top"= 0,
  "right"= 20,
  "bottom"= 0,
  "padding"= "3vh 3vw",
  "overflow-y"= "scroll"
)
sideBar <- dbcCol(list(
    html$h3("World Energy Consumption"),
    html$h4("Historical Trends", style = list("color" = "#686868")),
    html$br(),
    html$h5("Country", style = list("width" = "50%", "display" = "inline-block")),
    html$p("Select a country to visualize its trend:", style=list("color" = "#686868", "margin" = 0, "font-size" = "14px")),
    dccDropdown(id = "tab2-country-dropdown", options = all_country, value = "Canada"),
    html$br(),
    html$h5("Region", style = list("width" = "50%", "display" = "inline-block")),
    html$p("Select regions to compare with the country:", style=list("color" = "#686868", "margin" = 0, "font-size" = "14px")),
    dccDropdown(id = "tab2-region-dropdown", options = all_continent, value = "North America", multi = TRUE),
    html$br(),
    dbcRow(list(
      html$h5("Show World Trend", style = list("width" = "80%", "display" = "inline-block")),
      dbcChecklist(options = list(list("label" = "", "value" = 1)), value = list(1), id = "tab2-world-toggle", switch = TRUE)
    ))
  ), md = 2, style = sidebarStyle
)

slider_marks <- list()
show_years <- all_years[c(seq(1, length(all_years), 5), length(all_years))]
for (y in show_years){
  slider_marks[as.character(y)] = as.character(y)
}

tab2_layout <- dbcCol(list(
  html$div(list(
    html$p("Select the year range for the below plots:", style = list("color" = "#888888")),
    dccRangeSlider(min = min(all_years), max = max(all_years), step = 1,
                   value = c(min(all_years), max(all_years)),
                   tooltip = list("placement" = "top", "always_visible" = FALSE),
                   marks = slider_marks,
                   id = "tab2-year-slider"),
    dccGraph(id = "tab2-lineplot-fossil"),
    dccGraph(id = "tab2-lineplot-nuclear"),
    dccGraph(id = "tab2-lineplot-renewable")
  ), style = list("padding-top" = "30px"))
), style = tabStyle, md = 10)

app$layout(
  dbcContainer(list(
    dbcRow(list(
      sideBar,
      tab2_layout
      ))
    ),
    style = list("width" = "80%"),
    fluid = TRUE
  )
)

app$callback(
  output("tab2-lineplot-fossil", "figure"),
  list(
    input("tab2-country-dropdown", "value"),
    input("tab2-region-dropdown", "value"),
    input("tab2-world-toggle", "value"),
    input("tab2-year-slider", "value")
  ),
  function(country, region, toggle, years){
    entity_vec <- c(country, region %>% as.character())
    if (length(toggle) > 0) {
      entity_vec <- c(entity_vec, "World")
    }

    data_use <- data %>%
      filter(Entity %in% entity_vec & Year >= years[1] & Year <= years[2])

    graph <- ggplot(data_use, aes(x = Year, y = Fossil, color = Entity)) +
      geom_line() +
      labs(title = paste("Fossil fuels usage from", years[1], "to", years[2]),
           y = "Fossil fuel Usage (%)")

    ggplotly(graph)
  }
)

app$callback(
  output("tab2-lineplot-nuclear", "figure"),
  list(
    input("tab2-country-dropdown", "value"),
    input("tab2-region-dropdown", "value"),
    input("tab2-world-toggle", "value"),
    input("tab2-year-slider", "value")
  ),
  function(country, region, toggle, years){
    entity_vec <- c(country, region %>% as.character())
    if (length(toggle) > 0) {
      entity_vec <- c(entity_vec, "World")
    }

    data_use <- data %>%
      filter(Entity %in% entity_vec & Year >= years[1] & Year <= years[2])

    graph <- ggplot(data_use, aes(x = Year, y = Nuclear, color = Entity)) +
      geom_line() +
      labs(title = paste("Nuclear energy usage from", years[1], "to", years[2]),
           y = "Nuclear energy Usage (%)")

    ggplotly(graph)
  }
)

app$callback(
  output("tab2-lineplot-renewable", "figure"),
  list(
    input("tab2-country-dropdown", "value"),
    input("tab2-region-dropdown", "value"),
    input("tab2-world-toggle", "value"),
    input("tab2-year-slider", "value")
  ),
  function(country, region, toggle, years){
    entity_vec <- c(country, region %>% as.character())
    if (length(toggle) > 0) {
      entity_vec <- c(entity_vec, "World")
    }

    data_use <- data %>%
      filter(Entity %in% entity_vec & Year >= years[1] & Year <= years[2])

    graph <- ggplot(data_use, aes(x = Year, y = Renewables, color = Entity)) +
      geom_line() +
      labs(title = paste("Renewable energy usage from", years[1], "to", years[2]),
           y = "Renewable energy Usage (%)")

    ggplotly(graph)
  }
)
app$run_server(host = "0.0.0.0")

