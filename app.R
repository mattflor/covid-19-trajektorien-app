library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(viridis)

# suppress scientific notation:
options(scipen = 999)

einwohner_bl <- read_delim("data/Einwohnerzahlen_12411-0010.csv", 
                           delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"),
                           skip = 5, n_max = 16,
                           trim_ws = TRUE) %>%
    rename(Bundesland = X1) %>% 
    pivot_longer(cols = -Bundesland, names_to = "Stichtag", values_to = "Einwohner") %>% 
    mutate(Stichtag = dmy(Stichtag)) %>%
    filter(Stichtag == max(Stichtag)) %>% 
    select(-Stichtag)
# einwohner_bl %>% print()

#' get current data if necessary:
current_date <- Sys.Date()
download_date <- file.mtime("data/RKI_COVID19.csv") %>% as_date()
if (current_date > download_date) {
    download.file(url = "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv",
                  destfile = "data/RKI_COVID19.csv",
                  method = "libcurl")
}

#' read in data
covid19_de <- read_csv("data/RKI_COVID19.csv") %>% 
    mutate(Meldedatum = as_date(Meldedatum)) %>% 
    select(IdBundesland, Bundesland, AnzahlFall, AnzahlTodesfall, Meldedatum)
# TO DO: better check for complete data file
if (length(unique(covid19_de$Bundesland)) == 16) {             # current data may be incomplete, so we save a backup file ...
    write_csv(covid19_de, "data/RKI_COVID19_bak.csv")
} else {
    covid19_de <- read_csv("data/RKI_COVID19_bak.csv") %>%     # ... that we can fall back to
        mutate(Meldedatum = as_date(Meldedatum)) %>% 
        select(IdBundesland, Bundesland, AnzahlFall, AnzahlTodesfall, Meldedatum)
}

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
    filter(GesamtFall > 0) %>%
    left_join(einwohner_bl, by = "Bundesland") %>% 
    mutate(GesamtFallPro1e5 = 1e5 * GesamtFall / Einwohner)
# covid19_bl %>% print(n = 50)

bundeslaender <- unique(covid19_bl$Bundesland) %>% sort()

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

size_breaks <- 10^seq(-3, 5, by = 1)

ui <- fluidPage(
    
    # avoid graying out of plots during recalculation:
    tags$style(type = "text/css",
               ".recalculating {opacity: 1.0;}
                h2 {font-weight: bold;}"
    ),
    
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
                                       height = "700px"),
                            
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

server <- function(input, output, session) {
    
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
            mutate(label = paste0(Bundesland, "  "))
    })
    
    observeEvent(input$bundeslaender, {
        updateSliderInput(session, "datum", min = min(filtered()$Meldedatum)) 
    })
    
    output$trajectory_plot <- renderPlot({
        filtered_to_date() %>% 
            ggplot(aes(x = GesamtFall, y = FaelleLetzteWoche, group = Bundesland)) +
            geom_abline(slope = 1, linetype = "dashed", color = "darkgray") +
            geom_line(size = 0.4, color = "darkgray", alpha = 1) +
            # geom_point(size = 1.1, color = "red",
            #            data = bundesland_labels()) +
            geom_point(aes(size = GesamtFallPro1e5,
                           color = GesamtFallPro1e5),
                       alpha = 0.7,
                       data = bundesland_labels()) +
            geom_text(aes(label = label), 
                      size = 5,
                      hjust = 1.1, vjust = 0,
                      data = bundesland_labels()) +
            scale_x_continuous("Gesamtfälle", trans = "log1p",
                               breaks = log_breaks_x, minor_breaks = log_minor_breaks_x,
                               labels = scales::number_format(accuracy = 1, big.mark = " ")) +
            scale_y_continuous("Neue Fälle\nin der vergangenen Woche", trans = "log1p",
                               breaks = log_breaks_y, minor_breaks = log_minor_breaks_y,
                               labels = scales::number_format(accuracy = 1, big.mark = " ")) +
            scale_size_continuous("Gesamtfälle pro 100 000 Einwohner:", trans = "log",
                                  breaks = size_breaks, limits = range(size_breaks),
                                  range = c(1, 10),
                                  labels = prettyNum(signif(size_breaks, 1), 
                                                     decimal.mark = ",", big.mark = " "),
                                  guide = guide_legend(nrow = 1)) +
            scale_color_viridis("Gesamtfälle pro 100 000 Einwohner:", trans = "log", 
                                option = "plasma", begin = 0.2,
                                breaks = size_breaks, limits = range(size_breaks),
                                labels = prettyNum(signif(size_breaks, 1), 
                                                   decimal.mark = ",", big.mark = " "),
                                guide = guide_legend(nrow = 1)) +
            coord_cartesian(xlim = log_limits_x, ylim = log_limits_y) +
            theme_bw(base_size = 22) +
            theme(axis.title.x = element_text(margin = margin(1, 0, 0, 0, "lines")),
                  axis.title.y = element_text(margin = margin(0, 1, 0, 0, "lines")),
                  legend.position = "top", legend.justification = "right", 
                  legend.text = element_text(margin = margin(0, 1, 0, -0.25, "lines")))
    })
}

shinyApp(ui = ui, server = server)
