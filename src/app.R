cat('\014')
rm(list=ls())

library(dplyr)
library(ggvis)
library(reshape2)
library(shiny)


load_data <- function() {
  fert_df <- read_data("../API_SP/API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", 4)
  le_df <- read_data("../API_SP-2/API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", 4)
  pop_df <- read_data("../API_SP.POP.TOTL_DS2_en_csv_v2.csv")
  cont_df <- read.csv("../continents.csv", stringsAsFactors = FALSE)
  
  fert_df <- reshape_df(fert_df, "Fertility.Rate")
  le_df <- reshape_df(le_df, "Life.Expectancy")
  pop_df <- reshape_df(pop_df, "Population")
  
  df_temp <- merge(cont_df, fert_df, by = c("Country.Name"), all.x = TRUE, all.y = FALSE)
  df_temp <- merge(df_temp, le_df, by = c("Country.Name", "Year"))
  return (merge(df_temp, pop_df, by = c("Country.Name", "Year")))
}


plot_data <- function(df) {
  
  year_slider <- input_slider(1961, 
                              2014, 
                              value = 1961, 
                              animate = animationOptions(interval = 200),
                              ticks = FALSE, 
                              sep = ''
                              )
  
  continent_select <- input_select(unique(df$Continent), 
                                   multiple = TRUE, 
                                   selected = NA)
  
  # key is set to Country.Name simply to pass info down the pipe
  df %>% 
    ggvis(~Life.Expectancy, ~Fertility.Rate, fill = ~Continent, size = ~Population, key := ~Country.Name) %>% 
    set_options(height = 650, width = 1000) %>%
    add_tooltip(point_hover, "hover") %>%
    add_legend("fill") %>%
    hide_legend("size") %>%
    add_axis('x', title = 'Life expectancy') %>%
    add_axis('y', title = 'Fertility rate') %>%
    filter(Year == eval(year_slider)) %>%
    layer_points(opacity := 0.3) %>%
    filter(if (length(eval(continent_select)) >= 1 && any(Continent %in% eval(continent_select))) {
              Continent %in% eval(continent_select)
           } else {
             TRUE
           }) %>%
    layer_points(opacity := 1.0) %>%
    scale_numeric("x", domain = c(10, 85), nice = FALSE) %>%
    scale_numeric("y", domain = c(0.5, 9), nice = FALSE) %>%
    bind_shiny("p", "p_ui")
}


point_hover <- function(x) {
  paste(x$Country.Name)
}


read_data <- function(file_name, skip=0) {
  df <- read.csv(file_name, skip=skip, stringsAsFactors = FALSE)
  df <- df[, -c(2,3,4,60, 61, 62)]
  return (df)
}


reshape_df <- function(df, value) {
  df <- melt(df, measure.vars = names(df)[2:length(names(df))])
  df <- plyr::rename(df, c("variable" = "Year", "value" = value))
  df$Year <- lapply(df$Year, function(s) substr(s, 2, 5))
  return (df)
}


ui <- bootstrapPage(
  ggvisOutput("p"),
  uiOutput("p_ui")
)

server <- function(..., session) {
  plot_data(load_data())
}

shinyApp(ui = ui, server = server)








