library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)

# data retrieved from https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/data:
# Herunterladen > Tabelle
covid19_de <- read_csv("data/RKI_COVID19.csv",
                       col_types = cols(
                           IdBundesland = col_integer(),
                           Bundesland = col_character(),
                           Landkreis = col_character(),
                           Altersgruppe = col_character(),
                           Geschlecht = col_character(),
                           AnzahlFall = col_integer(),
                           AnzahlTodesfall = col_integer(),
                           ObjectId = col_double(),
                           Meldedatum = col_datetime(),
                           IdLandkreis = col_character(),
                           Datenstand = col_datetime(format = "%d.%m.%Y %H:%M"),
                           NeuerFall = col_integer(),
                           NeuerTodesfall = col_integer()
                       )) %>% 
    mutate(Meldedatum = as_date(Meldedatum),
           Datenstand = as_date(Datenstand)) %>% 
    print()

covid19_bl <- covid19_de %>%
    group_by(IdBundesland, Bundesland, Meldedatum) %>% 
    summarise(AnzahlFall = sum(AnzahlFall, na.rm = TRUE),
              AnzahlTodesfall = sum(AnzahlTodesfall)) %>% 
    ungroup() %>%
    complete(nesting(IdBundesland, Bundesland), 
             Meldedatum = seq(min(Meldedatum), max(Meldedatum), by = 1), 
             fill = list(AnzahlFall = 0, AnzahlTodesfall = 0)) %>%
    arrange(IdBundesland, Meldedatum) %>% 
    group_by(IdBundesland, Bundesland) %>% 
    mutate(GesamtFall = cumsum(AnzahlFall),
           GesamtTodesfall = cumsum(AnzahlTodesfall)) %>% 
    group_by(IdBundesland, Bundesland) %>% 
    mutate(FaelleLetzteWoche = rollsum(AnzahlFall, k = 7, fill = NA, align = "right")) %>% 
    mutate(FaelleLetzteWoche = ifelse(is.na(FaelleLetzteWoche), cumsum(AnzahlFall), FaelleLetzteWoche)) %>%  # need to fill rollsum at start of the windows
    ungroup() %>% 
    # arrange(IdBundesland, Meldedatum) %>% 
    filter(GesamtFall > 0) %>%
    print(n = 50)

bundeslaender <- unique(covid19_bl$Bundesland)

log1p_breaks <- function (n = 5, base = 10) {
    # 
    scales:::force_all(n, base)
    function(x) {
        rng <- log(range(x + 1, na.rm = TRUE), base = base)      # + 1
        min <- floor(rng[1])
        max <- ceiling(rng[2])
        if (max == min) 
            return(base^min)
        by <- floor((max - min)/n) + 1
        breaks <- c(0, base^seq(min, max, by = by))
        relevant_breaks <- floor(base^rng[1] - 1) <= breaks & 
            breaks <= ceiling(base^rng[2] - 1)
        if (sum(relevant_breaks) >= (n - 2)) 
            return(breaks)
        while (by > 1) {
            by <- by - 1
            breaks <- base^seq(min, max, by = by)
            if (min == 0) {
                breaks <- c(0, breaks)
            }
            relevant_breaks <- floor(base^rng[1] - 1) <= breaks & 
                breaks <= ceiling(base^rng[2] - 1)
            if (sum(relevant_breaks) >= (n - 2)) 
                return(breaks)
        }
        breaks
    }
}
log_limits_x <- c(0, 1e5)
log_breaks_x <- log1p_breaks(n = 7)(log_limits_x)
log_minor_breaks_x <- as.numeric(0:10 %o% log_breaks_x)
log_limits_y <- c(0, 1e5)
log_breaks_y <- log1p_breaks(n = 7)(log_limits_y)
log_minor_breaks_y <- as.numeric(0:10 %o% log_breaks_y)

ui <- fluidPage(
    
    titlePanel(
        sprintf("Trajektorien von bestätigten COVID-19-Fällen in den deutschen Bundesländern (Stand: %s)", 
                strftime(max(covid19_bl$Meldedatum), "%d.%m.%Y"))
    ),
    
    sidebarLayout(position = "right",
                  
                  sidebarPanel(width = 3,
                               selectizeInput("bundeslaender",
                                              label = "Auswahl Bundesländer",
                                              choices = bundeslaender,
                                              selected = bundeslaender,
                                              multiple = TRUE),
                               includeMarkdown("doc/description.md")
                  ),
                  
                  mainPanel(width = 9,
                            plotOutput("trajectory_plot",
                                       height = "600px"),
                            
                            fluidRow(
                                column(11, offset = 1,
                                       sliderInput("datum",
                                                   label = "Meldedatum",
                                                   min = min(covid19_bl$Meldedatum),
                                                   max = max(covid19_bl$Meldedatum),
                                                   value = max(covid19_bl$Meldedatum),
                                                   timeFormat = "%d.%m.%Y",
                                                   animate = TRUE,
                                                   width = "100%"),
                                       includeMarkdown("doc/credits.md")
                                )
                            )
                  )
    )
)

server <- function(input, output) {
    
    filtered <- reactive({
        covid19_bl %>% 
            filter(Bundesland %in% input$bundeslaender)
    })
    
    filtered_to_date <- reactive({
        filtered() %>% 
            filter(Meldedatum <= input$datum)
    })
    
    bundesland_labels <- reactive({
        # wir setzen den roten Punkt wegen des Meldeverzuges auf den vorletzten Tag, wenn alle Daten geplottet werden
        k <- ifelse(input$datum == max(filtered()$Meldedatum), 1, 0)
        filtered_to_date() %>% 
            filter(Meldedatum == (input$datum - k)) %>%    
            mutate(label = paste0(Bundesland, " "))
    })
    
    output$trajectory_plot <- renderPlot({
        filtered_to_date() %>% 
            ggplot(aes(x = GesamtFall, y = FaelleLetzteWoche, group = Bundesland)) +
            geom_abline(slope = 1, linetype = "dashed", color = "darkgray") +
            geom_line(size = 0.4, color = "darkgray", alpha = 1) +
            # geom_point(size = 1, color = "darkgray") +
            geom_point(size = 1.1, color = "red",
                       data = bundesland_labels()) +
            geom_text(aes(label = label), 
                      size = 5,
                      hjust = 1.1, vjust = 0,
                      data = bundesland_labels()) +
            # scale_x_continuous("Bestätigte Fälle (Gesamt)") +
            # scale_y_continuous("Neue bestätigte Fälle (in der vergangenen Woche)") +
            # coord_cartesian(xlim = linear_limits, ylim = linear_limits) +
            scale_x_continuous("Bestätigte Fälle (Gesamt)", trans = "log1p",
                               breaks = log_breaks_x, minor_breaks = log_minor_breaks_x,
                               labels = scales::number_format(accuracy = 1, big.mark = " ")) +
            scale_y_continuous("Neue bestätigte Fälle\n(in der vergangenen Woche)", trans = "log1p",
                               breaks = log_breaks_y, minor_breaks = log_minor_breaks_y,
                               labels = scales::number_format(accuracy = 1, big.mark = " ")) +
            coord_cartesian(xlim = log_limits_x, ylim = log_limits_y) +
            theme_bw(base_size = 22) +
            theme(axis.title.x = element_text(margin = margin(1, 0, 0, 0, "lines")),
                  axis.title.y = element_text(margin = margin(0, 1, 0, 0, "lines")))
    })
}

shinyApp(ui = ui, server = server)
