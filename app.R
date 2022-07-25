#######################################
## Stop and search in England and Wales
## 'Your Map' - StopWatch
## Thiago R. Oliveira
## 2022


# load necessary packages
library(shiny)
library(leaflet)
library(readxl)
library(sf)
library(RColorBrewer)
library(tidyverse)
library(sf)
library(ggplot2)
library(kableExtra)

## PREAMBLE

# Define colour palette
pal <- colorBin(palette = "Reds", 
                domain = c(0:2000))

# Read and clean  stop and search data by police force from 2011
data <- 
  read_excel('data/stop-search-open-data-tables-ppp-mar2020 (2006-07 to 2019-20).xlsx') %>%
  dplyr::select("Financial Year", "Force Name", "Legislation", "Ethnic Group (self-defined - new style)", "Ethnicity (self-defined)", 'Searches') %>%
  dplyr::rename('financial_year' = "Financial Year",
                'police_force_area' = "Force Name", 
                'legislation' = "Legislation", 
                'self_defined_ethnicity_group' = "Ethnic Group (self-defined - new style)",
                'self_defined_ethnicity' = "Ethnicity (self-defined)",
                'number_of_searches' =  'Searches') %>%
  mutate(number_of_searches = as.numeric(number_of_searches)) %>%
  bind_rows(read_excel('data/stop-search-open-data-tables-ppp-mar2021.xlsx') %>%
              dplyr::select(financial_year, police_force_area, legislation, self_defined_ethnicity_group, self_defined_ethnicity, number_of_searches)) %>%
  filter(police_force_area != 'British Transport Police') %>%
  mutate(police_force_area = case_when(
    police_force_area == 'Avon & Somerset' ~ 'Avon and Somerset',
    police_force_area == 'Devon & Cornwall' ~ 'Devon and Cornwall',
    police_force_area == 'London, City of' ~ 'City of London',
    TRUE ~ police_force_area
  )) %>% 
  mutate(police_force_area = factor(police_force_area),
         legislation = factor(legislation),
         year = substr(financial_year, 1, 4) %>% factor) %>%
  mutate(year = format(year, format = "%Y"),
         ethnicity = case_when(
           self_defined_ethnicity_group %in% c("N/A - vehicle search",
                                               "Not stated",
                                               "Not Stated",
                                               "Vehicle only") ~ 'Not Stated / Unknown',
           TRUE ~ self_defined_ethnicity_group
         )) %>%
  filter(year > 2010)

# Read 2011 census data: ethnic profile by police force areas
ethn <- read_excel('data/PoliceForceAreas2009PopnCensus2011_new.xlsx', sheet = 8, skip = 1) %>%
  rename(police_force_area = police_force) %>%
  mutate(police_force_area = case_when(
    police_force_area == 'Devon & Cornwall' ~ 'Devon and Cornwall',
    TRUE ~ police_force_area
  )) %>%
  mutate(police_force_area = factor(police_force_area)) %>%
  dplyr::select(-All)

# Read shapefile - police force areas
forces.sf <- st_read('data/Police_Force_Areas_December_2016_EW_BFC_v2-shp/Police_Force_Areas_December_2016_EW_BFC_v2.shp') %>%
  st_transform(crs = 4326) %>%
  rename("police_force_area" = "PFA16NM") %>%
  mutate(police_force_area = factor(police_force_area))

### APP
## Define UI

ui <- bootstrapPage(
  fluidPage(
    
    # Define title
    h1("Stop and search in England and Wales"),
    
    # First output: map
    leafletOutput("mymap"),
    
    # Second output: 
    sidebarLayout(
      # Left hand side: Panel to select legislation, date, and police force area
      sidebarPanel(
        selectInput("select.legislation", "Which legislation would like to visualise?",
                    choices = levels(data$legislation),
                    selected = "Section 1 (PACE)"),
        sliderInput("select.date", "Select date range", #start = min(data$year), end = max(data$year),
                       min = 2011, max = 2020, value = c(2011, 2020), sep = ""),
                       #start = "2006-01-01", end = "2020-12-31", min = "2006-01-01", max = "2020-12-31", format = "yyyy"),
        selectInput("select.force", "Which region would you like to visualise?",
                    choices = levels(forces.sf$police_force_area)),
        textOutput("my.force")
      ),
      # Right hand side: plot time series of stop and search count
      mainPanel(
        plotOutput("myplotF",
                   hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
        uiOutput("hover_info")
        )
    ),
    
    # Third output:
    sidebarLayout(
      # Left hand side: text about ethnic disparities
      sidebarPanel(
        textOutput("text.disparities")
      ),
      # Right hand side: table comparing S&S and ethnic profile  
      mainPanel(
        tableOutput("table.disparities")
      )
    ),
    
    # Fourth output
    sidebarLayout(
      # Left hand side: ethnic disparities in England and Wales
      sidebarPanel(
        plotOutput("barplot.ew")
      ),
      # Right hand side: ethnic disparities in region
      mainPanel(
        plotOutput("barplot.force", width = "85%")
      )
    ),
    
    # Fifth output
    sidebarLayout(
      # Left hand side: odds ratios in England & Wales
      sidebarPanel(
        plotOutput("oddsratios.ew")
      ),
      # Right hand side: odds ratios in police force area
      mainPanel(
        plotOutput("oddsratios.force", width = "85%")
      )
    )
    
  )
)

## DEFINE SERVER

server <- function(input, output, session) {
  
  # define new data: filter based on user input
  new.data <- reactive({
    req(input$select.legislation, input$select.date)
    data %>%
      filter(legislation == input$select.legislation) %>%
      filter(year >= input$select.date[1] & year <= input$select.date[2]) %>%
      group_by(police_force_area) %>%
      summarise(stops = sum(number_of_searches), .groups = "drop")
  })
  
  #  define map
  output$mymap <- renderLeaflet({
    
    forces.sf <-
      forces.sf %>%
      left_join(new.data(), by = "police_force_area")
    
    pal <- colorBin(palette = "Reds", 
                    domain = c(new.data() %>% dplyr::select(stops) %>% min(na.rm = T), 
                               new.data() %>% filter(police_force_area != "Metropolitan Police") %>% dplyr::select(stops) %>% max(na.rm = T))
    )
    
    leaflet(forces.sf) %>% 
      addProviderTiles("Stamen.TonerBackground",
                       options = providerTileOptions(minZoom = 5.5)) %>% 
      addPolygons(color = "red",
                  label = ~police_force_area,
                  fillColor = ~pal(forces.sf$stops),
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 1,
                  layerId = ~police_force_area) %>%
      addLegend("topright",
                pal = pal,
                values = ~stops,
                title = paste(input$select.legislation),
                opacity = 1) #%>%
  })
  
  # info provided by user: select police force area through clicking on map
  click <- observe({
    my.click <- input$mymap_shape_click
    
    updateSelectInput(session,
                      inputId = "select.force",
                      label = "Which region would you like to visualise?",
                      choices = levels(forces.sf$police_force_area),
                      selected = my.click$id)
  })
  
  # prepare data for time series plot
  dataplot <- reactive({
    req(input$select.legislation, input$select.force)
    data %>%
      filter(legislation == input$select.legislation) %>%
      group_by(police_force_area, year) %>%
      summarise(stops = sum(number_of_searches), .groups = "drop")  
  })
  
  data.plot.ew <- reactive({
    req(input$select.legislation)
    data %>%
      filter(legislation == input$select.legislation) %>%
      group_by(police_force_area, year) %>%
      summarise(stops = sum(number_of_searches), .groups = "drop")
  })
  
  # define time series plot
  output$myplotF <- renderPlot({
    event <- input$mymap_shape_click$id
    
    this.force <- 
      dataplot() %>% 
      filter(police_force_area == input$select.force)
    
    data.firstplot <- 
      data.plot.ew() %>% 
      group_by(year) %>%
      summarise(EnglandWales = mean(stops, na.rm = T) %>% round(2), .groups = "drop")
    
    myplotF <- ggplot(this.force, aes(y = value, x = year, group = 1)) + 
      geom_point(aes(y = stops, colour = input$select.force)) + geom_line(aes(y = stops, colour = input$select.force)) +
      geom_point(data = data.firstplot, aes(y = EnglandWales, colour = "England and Wales (average)")) + geom_line(data = data.firstplot, aes(y = EnglandWales, colour = "England and Wales (average)")) +
      ylim(0,pmax(this.force$stops, data.firstplot$EnglandWales, na.rm = T) %>% max()) +
      labs(y = paste(input$select.legislation), x = "", title = paste(input$select.legislation, "searches in England and Wales (average) and in", input$select.force)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 14)) + 
      scale_color_manual(values = c("black", "red"),
                         breaks = c("England and Wales (average)", input$select.force),
                         labels = c("England and Wales (average)", input$select.force)) + 
      guides(color = guide_legend(title = ""))
    myplotF
  })
  
  output$hover_info <- renderUI({ 
    hover <- input[["plot_hover"]]
    if(is.null(hover)) return(NULL)
    
    point <- nearPoints(dataplot(), hover, threshold = 5, maxpoints = 1)
    if(nrow(point) == 0) return(NULL)
    
    left_px <- hover$coords_css$x
    top_px  <- hover$coords_css$y
    
    style <- paste0(
      "position:absolute; z-index:100; pointer-events:none; ", 
      "background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px, 
      "px; top:", top_px, "px;"
    )
    
    # tooltip created as wellPanel
    tooltip <- paste0(
      "<b> Number of searches: </b>",     point[["stops"]],     "<br/>"
    )
    wellPanel(
      style = style, p(HTML(tooltip))
    )
  }) 
  
  # prepare data for texts
  data.n <- reactive({
    req(input$select.legislation, input$select.date, input$select.force)
    data %>%
      filter(legislation == input$select.legislation) %>%
      filter(year >= input$select.date[1] & year <= input$select.date[2]) %>%
      group_by(police_force_area, year, ethnicity) %>%
      summarise(stops = sum(number_of_searches), .groups = "drop")
    
  })
  
  # output: text about ethnic disparities
  output$my.force <- renderText({
    paste(data.n() %>% filter(police_force_area == input$select.force) %>% dplyr::select(stops) %>% sum(), "stops based on the", input$select.legislation, "legislation were recorded in the region of", 
          input$select.force, "between", input$select.date[1], "and", input$select.date[2], ". This is", 
          if_else(data.n() %>% filter(police_force_area == input$select.force) %>% dplyr::select(stops) %>% sum(na.rm = T) > (data.n() %>% dplyr::select(stops) %>% sum(na.rm = T) / 43), paste("above"), paste("below")),
          "the average number of",
          #round(((data.n() %>% filter(police_force_area == input$select.force) %>% dplyr::select(stops) %>% sum(na.rm = T)) / (data.n() %>% dplyr::select(stops) %>% sum(na.rm = T))) * 100, 2), "% of the average number of",
          input$select.legislation, "searches in England and Wales in this period, which is", round((data.n() %>% dplyr::select(stops) %>% sum(na.rm = T) / 43), 2), ".")
  })
  
  output$text.disparities <- renderText({
    paste("Considering all ", input$select.legislation, " searches in ", input$select.force, " between ", input$select.date[1], " and ", input$select.date[2], 
          ", ", 
          round(
            data.n() %>% 
              filter(police_force_area  == input$select.force) %>%
              mutate(ethnicity = factor(ethnicity, levels = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group', 'Not Stated / Unknown'))) %>%
              group_by(ethnicity) %>%
              summarise(n = sum(stops, na.rm = T)) %>%
              mutate(prop = (n / sum(n)) * 100) %>%
              filter(ethnicity == "Black or Black British") %>%
              dplyr::select(prop)
          , 2),
          "% were against people self-defined as Black (or Black British), while ", 
          round(
            data.n() %>% 
              filter(police_force_area  == input$select.force) %>%
              mutate(ethnicity = factor(ethnicity, levels = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group', 'Not Stated / Unknown'))) %>%
              group_by(ethnicity) %>%
              summarise(n = sum(stops, na.rm = T)) %>%
              mutate(prop = (n / sum(n)) * 100) %>%
              filter(ethnicity == "White") %>%
              dplyr::select(prop)
            , 2),
          "% were against people self-defined as White. According to Census 2011, ",
          round(
            (
              (
                ethn %>% 
                  filter(police_force_area  == input$select.force) %>% 
                  dplyr::select('Black')
                ) / 
              (
                ethn %>% 
                filter(police_force_area  == input$select.force) %>% 
                mutate(pop = White + Asian + Black + Other + Mixed) %>% 
                dplyr::select(pop)
              ) * 100
              ), 2),
          "% of the population living in ", input$select.force, " are Black or Black British, whereas ",
          round(
            (
              (
                ethn %>% 
                  filter(police_force_area  == input$select.force) %>% 
                  dplyr::select('White')
              ) / 
                (
                  ethn %>% 
                    filter(police_force_area  == input$select.force) %>% 
                    mutate(pop = White + Asian + Black + Other + Mixed) %>% 
                    dplyr::select(pop)
                ) * 100
            ), 2),
          "% are White.",
          sep = "")
  })
  
  # table: ethnic disparities
  output$table.disparities <- function(){
    this.region <- 
      data.n() %>% 
      filter(police_force_area  == input$select.force) %>%
      mutate(ethnicity = factor(ethnicity, levels = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group', 'Not Stated / Unknown'))) %>%
      group_by(ethnicity) %>%
      summarise(n = sum(stops, na.rm = T)) %>%
      mutate(prop = (n / sum(n)) * 100) %>%
      data.frame() %>%
      left_join(ethn %>% 
                  filter(police_force_area == input$select.force) %>%
                  pivot_longer(cols = White:Mixed,
                               names_to = "ethnicity",
                               values_to = "pop") %>%
                  mutate(prop_pop = (pop / sum(pop)) * 100,
                         ethnicity = case_when(
                    ethnicity == "Black" ~ "Black or Black British",
                    ethnicity == "Asian" ~ "Asian or Asian British",
                    ethnicity == "Other" ~ "Other Ethnic Group",
                    TRUE ~ ethnicity
                  )) %>%
                  dplyr::select(ethnicity, prop_pop)
                , by = "ethnicity") %>%
      kbl("html", digits = 2, col.names = c('Self-defined ethnic group', 
                                            paste('Number of searches in', input$select.force), 
                                            paste('Percentage of searches in', input$select.force, '(%)'),
                                            paste('Ethnic composition in', input$select.force, '(%)')
      )) %>%
      kable_styling(bootstrap_options = c("striped", "hover"))
  }
  
  # barplot: England & Wales
  output$barplot.ew <- renderPlot({
    ethn.ew <- 
      data.n() %>%
      mutate(ethnicity = factor(ethnicity, levels = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group', 'Not Stated / Unknown'))) %>%
      group_by(ethnicity) %>%
      summarise(n = sum(stops, na.rm = T)) %>%
      mutate(prop = n / sum(n)) %>%
      data.frame() %>%
      mutate(type = 'stops') %>%
      mutate(ethnicity = factor(ethnicity, levels = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group', 'Not Stated / Unknown'))) %>%
      dplyr::select(ethnicity, prop, type) %>%
      bind_rows(
        tibble(
          ethnicity = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group', 'Not Stated / Unknown'),
          prop = c(sum(ethn$White) / ethn %>% mutate(total = rowSums(across(White:Mixed))) %>% dplyr::select(total) %>% sum(),
                   sum(ethn$Black) / ethn %>% mutate(total = rowSums(across(White:Mixed))) %>% dplyr::select(total) %>% sum(),
                   sum(ethn$Mixed) / ethn %>% mutate(total = rowSums(across(White:Mixed))) %>% dplyr::select(total) %>% sum(),
                   sum(ethn$Asian) / ethn %>% mutate(total = rowSums(across(White:Mixed))) %>% dplyr::select(total) %>% sum(),
                   sum(ethn$Other) / ethn %>% mutate(total = rowSums(across(White:Mixed))) %>% dplyr::select(total) %>% sum(),
                   0),
          type = 'population'
        )
      ) %>%
      mutate(ethnicity = factor(ethnicity, levels = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group', 'Not Stated / Unknown')))
    
    barplot.ew <- ggplot(ethn.ew, aes(y = prop, fill = type, x = ethnicity)) + 
      geom_bar(position="dodge", stat="identity") +
      labs(y = "", x = "", title = paste('Ethnic disparities in England and Wales', '(', input$select.legislation, ')')) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
            legend.text = element_text(size = 10),
            plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) + 
      scale_x_discrete(breaks = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group', 'Not Stated / Unknown')) +
      scale_fill_manual(breaks = c("stops", "population"),
                        values = c("red", "black"),
                        labels = c(paste(input$select.legislation, "\n searches in England and Wales"), "Ethnic composition in England and Wales")) + 
      guides(fill = guide_legend(title = ""))
    barplot.ew
  })
  
  # barplot: police force area
  output$barplot.force <- renderPlot({
    ethn.force <- 
      data.n() %>%
      filter(police_force_area  == input$select.force) %>%
      mutate(ethnicity = factor(ethnicity, levels = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group', 'Not Stated / Unknown'))) %>%
      group_by(ethnicity) %>%
      summarise(n = sum(stops, na.rm = T)) %>%
      mutate(prop = n / sum(n)) %>%
      data.frame() %>%
      mutate(type = 'stops') %>%
      mutate(ethnicity = factor(ethnicity, levels = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group', 'Not Stated / Unknown'))) %>%
      dplyr::select(ethnicity, prop, type) %>%
      bind_rows(
        ethn %>% 
          filter(police_force_area == input$select.force) %>%
          pivot_longer(cols = White:Mixed,
                       names_to = "ethnicity",
                       values_to = "pop") %>%
          mutate(prop = (pop / sum(pop)),
                 ethnicity = case_when(
                   ethnicity == "Black" ~ "Black or Black British",
                   ethnicity == "Asian" ~ "Asian or Asian British",
                   ethnicity == "Other" ~ "Other Ethnic Group",
                   TRUE ~ ethnicity
                 )) %>%
          dplyr::select(ethnicity, prop) %>%
          add_row(ethnicity = 'Not Stated / Unknown', 
                  prop = 0) %>%
          mutate(type = 'population')
      ) %>%
      mutate(ethnicity = factor(ethnicity, levels = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group', 'Not Stated / Unknown')))
    
    
    barplot.force <- ggplot(ethn.force, aes(y = prop, fill = type, x = ethnicity)) + 
      geom_bar(position="dodge", stat="identity") +
      labs(y = "", x = "", title = paste('Ethnic disparities in', input$select.force, '(', input$select.legislation, ')')) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
            legend.text = element_text(size=15),
            plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 14)) + 
      scale_fill_manual(breaks = c("stops", "population"),
                        values = c("red", "black"),
                        labels = c(paste(input$select.legislation, "\n searches in", input$select.force),
                                   paste("Ethnic composition in", input$select.force))) + 
      guides(fill = guide_legend(title = ""))
    barplot.force
  })
  
  # odds ratios: london
  output$oddsratios.ew <- renderPlot({
    
    odds.ew <-
      tibble(
        ethnicity = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group') %>% 
          factor(levels = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group')),
        odds = c(
          1,
          
          round(
            (data.n() %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "Black or Black British") %>% pull(n) /
               sum(ethn$Black)) /
              (data.n() %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "White") %>% pull(n) /
                 sum(ethn$White)),
            2),
          
          round(
            (data.n() %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "Mixed") %>% pull(n) /
               sum(ethn$Mixed)) /
              (data.n() %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "White") %>% pull(n) /
                 sum(ethn$White)),
            2),
          
          round(
            (data.n() %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "Asian or Asian British") %>% pull(n) /
               sum(ethn$Asian)) /
              (data.n() %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "White") %>% pull(n) /
                 sum(ethn$White)),
            2),
          
          round(
            (data.n() %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "Other Ethnic Group") %>% pull(n) /
               sum(ethn$Other)) /
              (data.n() %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "White") %>% pull(n) /
                 sum(ethn$White)),
            2)
        ),
        white = c(T, rep(F, 4))
      )
    
    barplot.odds.ew <- ggplot(odds.ew, aes(y = odds, x = ethnicity, fill = white)) + 
      geom_bar(position = "dodge", stat = "identity", width = .25) +
      geom_text(aes(label = odds), vjust = -.75, colour = "red", size = 5) +
      ylim(0,8) +
      labs(y = "", x = "", title = paste('ODDS RATIOS: Ethnic disparities in England & Wales (', input$select.legislation, ')', sep = "")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
            legend.text = element_text(size = 10), legend.position = "None",
            plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
      scale_x_discrete(breaks = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group')) + 
      scale_fill_manual(values = c("black", "red"),
                        labels = c("", ""))
    
    barplot.odds.ew
    
  })
  
  # odds ratios: borough
  output$oddsratios.force <- renderPlot({
    
    odds.force <-
      tibble(
        ethnicity = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group') %>% 
          factor(levels = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group')),
        odds = c(
          1,
          
          round(
            (data.n() %>% filter(police_force_area == input$select.force) %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "Black or Black British") %>% pull(n) /
               ethn %>% filter(police_force_area == input$select.force) %>% pull(Black)) /
              (data.n() %>% filter(police_force_area == input$select.force) %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "White") %>% pull(n) /
                 ethn %>% filter(police_force_area == input$select.force) %>% pull(White)),
            2),
          
          round(
            (data.n() %>% filter(police_force_area == input$select.force) %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "Mixed") %>% pull(n) /
               ethn %>% filter(police_force_area == input$select.force) %>% pull(Mixed)) /
              (data.n() %>% filter(police_force_area == input$select.force) %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "White") %>% pull(n) /
                 ethn %>% filter(police_force_area == input$select.force) %>% pull(White)),
            2),
          
          round(
            (data.n() %>% filter(police_force_area == input$select.force) %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "Asian or Asian British") %>% pull(n) /
               ethn %>% filter(police_force_area == input$select.force) %>% pull(Asian)) /
              (data.n() %>% filter(police_force_area == input$select.force) %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "White") %>% pull(n) /
                 ethn %>% filter(police_force_area == input$select.force) %>% pull(White)),
            2),
          
          round(
            (data.n() %>% filter(police_force_area == input$select.force) %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "Other Ethnic Group") %>% pull(n) /
               ethn %>% filter(police_force_area == input$select.force) %>% pull(Other)) /
              (data.n() %>% filter(police_force_area == input$select.force) %>% group_by(ethnicity) %>% summarise(n = sum(stops, na.rm = T)) %>% filter(ethnicity == "White") %>% pull(n) /
                 ethn %>% filter(police_force_area == input$select.force) %>% pull(White)),
            2)
        ),
        white = c(T, rep(F, 4))
      )
    
    barplot.odds.force <- ggplot(odds.force, aes(y = odds, x = ethnicity, fill = white)) + 
      geom_bar(position = "dodge", stat = "identity", width = .45) +
      geom_text(aes(label = odds), vjust = -.75, colour = "red", size = 5) +
      ylim(0,8) +
      labs(y = "", x = "", title = paste('ODDS RATIOS: Ethnic disparities in ', input$select.force, ' (', input$select.legislation, ')', sep = "")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
            legend.text = element_text(size = 10), legend.position = "None",
            plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 14)) +
      scale_x_discrete(breaks = c('White', 'Black or Black British', 'Mixed', 'Asian or Asian British', 'Other Ethnic Group')) + 
      scale_fill_manual(values = c("black", "red"),
                        labels = c("", ""))
    
    barplot.odds.force
    
  })
  
}


shinyApp(ui, server)
