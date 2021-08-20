##########################################################################
# Create New OLI (11) Dashboard
# Fabian Stephany | fabian.stephany@oii.ox.ac.uk
# iLabour Group | ilabour.ox.ac.uk
# Created: 14.05.2020
# Example layout: https://shiny.rstudio.com/gallery/navbar-example.html
# Plotly Toolbar: https://plotly-r.com/control-modebar.html
##########################################################################

#%#%#%#%#%#%#%#%#%#%#%#%
# Load Packages
#%#%#%#%#%#%#%#%#%#%#%#%
library(shiny)
library(dplyr)
library(tidyverse)
library(plotly)
library(readr)
library(data.table)
library(scales)
library(RColorBrewer)
library(zoo)
library(lubridate)
library(htmlwidgets)
library(leaflet)
library(raster)
library(rsconnect)
library("rfigshare")
library("viridis")  
'%!in%' <- function(x,y)!('%in%'(x,y))

library(profvis)

# The palette with black
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#%#%#%#%#%#%#%#%#%#%#%#%
# Generate Dashboard user interface
#%#%#%#%#%#%#%#%#%#%#%#%

country_list <- c("United States", "United Kingdom", "India", "Canada", "Australia", "Germany", 
                  "United Arab Emirates", "France", "Singapore", "Israel", "Netherlands", "Pakistan", 
                  "Spain", "Saudi Arabia", "Switzerland", "Hong Kong", "Italy", "New Zealand", "Russia", 
                  "China", "Ukraine", "Philippines", "Ireland", "Malaysia", "Belgium", "Nigeria", "Sweden",
                  "Denmark", "Egypt", "South Africa", "Turkey", "Romania", "Poland", "Norway", "Japan", 
                  "Austria", "Thailand", "Brazil", "Mexico", "Bangladesh", "Bulgaria", "Estonia", "Kenya", 
                  "Lithuania", "Portugal", "Greece", "Hungary", "Kuwait", "Qatar", "Cyprus", "Indonesia", 
                  "Jordan", "Morocco", "South Korea", "Finland", "Czech Republic", "Lebanon", "Vietnam", 
                  "Argentina", "Serbia", "Colombia", "Malta", "Sri Lanka", "Latvia", "Slovenia", "Oman", 
                  "Belarus", "Jamaica", "Bahrain", "Puerto Rico", "Croatia", "Panama", "Chile", "Armenia", 
                  "Dominican Republic", "Costa Rica", "Luxembourg", "Moldova", "Ghana", "Slovakia", 
                  "Ecuador", "Taiwan", "Bahamas", "Albania", "Georgia", "Mauritius", "Uruguay", "Algeria", 
                  "Peru", "Azerbaijan", "Nepal", "Tunisia", "Trinidad and Tobago", "Guatemala", 
                  "Palestinian Territories", "Bosnia and Herzegovina", "Macedonia", "Venezuela", "El Salvador", 
                  "Tanzania", "Iceland", "Uganda", "Bolivia", "Cambodia", "Honduras", "Mongolia", "Cameroon", 
                  "Namibia", "Zambia", "Ethiopia", "Myanmar", "Angola", "Benin", "Botswana", "Cape Verde", 
                  "Somalia", "Nicaragua", "Rwanda", "Aruba", "Curacao", "Monaco", "Dominica", "Madagascar", 
                  "Maldives", "Jersey", "Suriname", "Grenada", "Laos", "Papua New Guinea", "Paraguay", "Afghanistan", 
                  "Saint Kitts and Nevis", "United States Minor Outlying Islands", "Antigua and Barbuda", 
                  "Burundi", "French Polynesia", "Sierra Leone", "St. Lucia", "Burkina Faso")

# Define domain list
domain_list <- c("English", "Spanish", "Russian")

# Define User Interface
ui = fluidPage(
  navbarPage("Online Labour Index 2020",
    tabPanel("OLI 2020", fluid = TRUE,
             sidebarLayout(
               mainPanel(br(),
                         br(),
                         br(),
                         plotlyOutput("oli_plotly"),
                         "Source: OLI 2020 | onlinelabourobservatory.org"),
               sidebarPanel(radioButtons("version", "Version",
                                         list("OLI 2020" = 1, "OLI 5" = 2), selected = 1),
                 dateRangeInput("dates", "Customise date range", start = "2016-06-01", end = Sys.Date()),
                            downloadButton("download_oli_data", "Download Data"))
               )
             ),
    tabPanel("By platform language", Fluid = TRUE,
             #tabsetPanel(type = "tabs",
                       #  tabPanel("By platform language", fluid = TRUE,
                                 # sidebarLayout(
                                  #  mainPanel(plotlyOutput("domain_bar"),
                                   # "Source: OLI 2020 | onlinelabourobservatory.org"),
                                  #  sidebarPanel(
                                    #radioButtons("norm", "Normalise time series values",
                                                              #list("Global market share" = 1, "Indexed values" = 2), selected = 2),
                                                 #dateRangeInput("dates2", "Customise date range", start = "2020-09-01", end = Sys.Date()),
                                                 #sliderInput("dates2", label = "", min = 2017, max = 2021, value = c(2017,2021), sep = ""),
                                                # selectizeInput('domain', 'Select domain', 
                                                #                choices = domain_list, 
                                                #                selected = c("English", "Spanish", "Russian"), multiple = TRUE),
                                                # downloadButton("download_domain_data", "Download Data"))
                                  #)
                         #),
                         #tabPanel("By platform language and occupation", fluid = TRUE,
                                  sidebarLayout(
                                    mainPanel(br(),
                                              br(),
                                              br(),
                                              plotlyOutput("domain_occ_bar"),
                                              "Source: OLI 2020 | onlinelabourobservatory.org"),
                                    sidebarPanel(radioButtons("slice", "Define market",
                                                                          list("Within language share" = 1, "Global market share" = 2), selected = 1),
                                                 selectizeInput('domain2', 'Select platform language', 
                                                                choices = domain_list, 
                                                                selected = c("English", "Spanish", "Russian"), multiple = TRUE),
                                                 selectizeInput('occupation2', 'Select occupation', 
                                                                choices = c("Clerical and data entry","Creative and multimedia","Professional services",
                                                                            "Sales and marketing support","Software dev. and tech.",
                                                                            "Writing and translation"), 
                                                                selected = c("Clerical and data entry","Creative and multimedia","Professional services",
                                                                             "Sales and marketing support","Software dev. and tech.",
                                                                             "Writing and translation"), multiple = TRUE),
                                                 downloadButton("download_lng_occ_data", "Download Data"))
                                  )
                         #)
                       #)
             ),
    tabPanel("By occupation", fluid = TRUE,
             sidebarLayout(
               mainPanel(tabsetPanel(type = "tabs",
                                     tabPanel("Bar chart", 
                                              br(),
                                              br(),
                                              br(),
                                              plotlyOutput("occupation_bar")),
                                     tabPanel("Time series",
                                              fluidRow(column(3,
                                                radioButtons("norm1", "",
                                                               list("Global market share" = 1, "Indexed values" = 2), selected = 1)),
                                                column(3,
                                                       sliderInput("dates3", label = "", min = 2017, max = 2021, value = c(2017,2021), sep = ""))
                                              ),
                                              plotlyOutput("occupation_ts"))),
                         "Source: OLI 2020 | onlinelabourobservatory.org"),
               sidebarPanel(#radioButtons("slice", "Define market",
                             #            list("Within language share" = 1, "Global market share" = 2), selected = 1),
                            #dateRangeInput("dates3", "Customise date range", start = "2016-08-16", end = Sys.Date()),
                            selectizeInput('occupation', 'Select occupation', 
                                           choices = c("Clerical and data entry","Creative and multimedia","Professional services",
                                                       "Sales and marketing support","Software dev. and tech.",
                                                       "Writing and translation"), 
                                           selected = c("Clerical and data entry","Creative and multimedia","Professional services",
                                                        "Sales and marketing support","Software dev. and tech.",
                                                        "Writing and translation"), multiple = TRUE),
                            downloadButton("download_occupation_data", "Download Data"))
             )
    ),
    tabPanel("By country", fluid = TRUE,
             sidebarLayout(
               mainPanel(tabsetPanel(type = "tabs",
                                     tabPanel("Bar chart", 
                                              br(),
                                              br(),
                                              br(),
                                              plotlyOutput("country_bar")),
                                     tabPanel("Time series",
                                              fluidRow(column(3,
                                                radioButtons("norm2", "",
                                                                    list("Global market share" = 1, "Indexed values" = 2), selected = 1)),
                                                column(3,
                                                       sliderInput("dates4", label = "", min = 2017, max = 2021, value = c(2017,2021), sep = ""))
                                              ),
                                              plotlyOutput("country_ts"))
                                     ),
                         "Source: OLI 2020 | onlinelabourobservatory.org"),
               sidebarPanel(selectizeInput('country', 'Select country', 
                                           choices = country_list, 
                                           selected = c("United States", "United Kingdom", "India", "Canada", "Australia", "Germany"), multiple = TRUE),
                            downloadButton("download_country_data", "Download Data"),
                            downloadButton("download_world_data", "Download OLI World"))
             )
    ),
    tabPanel("By countries and occupations", fluid = TRUE,
             sidebarLayout(
               mainPanel(br(),
                         br(),
                         br(),
                         plotlyOutput("cty_occ_plotly"),
                         "Source: OLI 2020 | onlinelabourobservatory.org"),
               sidebarPanel(radioButtons("slice2", "Define market",
                                         list("Within country share" = 1, "Global market share" = 2), selected = 1),
                            selectizeInput('country2', 'Select country', 
                                           choices = country_list, 
                                           selected = c("United States", "United Kingdom", "India", "Canada", "Australia", "Germany"), multiple = TRUE),
                            selectizeInput('occupation3', 'Select occupation', 
                                           choices = c("Clerical and data entry","Creative and multimedia","Professional services",
                                                       "Sales and marketing support","Software dev. and tech.",
                                                       "Writing and translation"), 
                                           selected = c("Clerical and data entry","Creative and multimedia","Professional services",
                                                        "Sales and marketing support","Software dev. and tech.",
                                                        "Writing and translation"), multiple = TRUE),
                            downloadButton("download_cty_occ_data", "Download Data"))
             )
    ),
    tabPanel(
      HTML(
        '<span class="glyphicon glyphicon-info-sign" aria-hidden="true""></span>'
      ),
      fluidPage(wellPanel(includeMarkdown(
        knitr::knit("app_description.Rmd")
      )))
    )
    )
  )

#%#%#%#%#%#%#%#%#%#%#%#%
# Load Dashboard data
#%#%#%#%#%#%#%#%#%#%#%#%
## Access data from FigShare repository
### Request access to repository
fs_deposit_id <- 3761562
deposit_details <- fs_details(fs_deposit_id)
deposit_details <- unlist(deposit_details$files)
deposit_details <- data.frame(split(deposit_details, names(deposit_details)), stringsAsFactors = F)

## Access OLI data
### Traditional five ENGLISH platforms
oli_import <- read_csv(deposit_details[grepl("OLIdata_",deposit_details$name),"download_url"][1])
#oli_import <- read_csv("OLIdata_2021-08-16.txt.zip")

oli_import <- oli_import %>% filter(status == "new") %>% group_by(date) %>% dplyr::summarise(count = sum(count))
oli_import$domain <- "OLI-5"
### New six ES/RU/PH platforms
oli11_import <- read_csv(deposit_details[grepl("OLI11data_",deposit_details$name),"download_url"][1])
#oli11_import <- read_csv("OLI11data_2021-08-16.txt.zip")
names(oli11_import)[1] <- "date"
oli11_import$date <- as.Date(oli11_import$date)
oli11_import <- oli11_import %>% filter(date >= "2020-08-01", status == "new") %>% group_by(date) %>% dplyr::summarise(count = sum(count)) 
oli11_import$domain <- "OLI-6"
### Merge both DFs
oli2020 <- rbind(oli_import,oli11_import)
### Format OLI2020 DF
oli2020$date <- as.Date(as.character(oli2020$date), format = "%Y-%m-%d")
oli2020$count <- as.numeric(oli2020$count)


## Access Buyer Country Data
### Of traditional five platforms
bcountry <- read_csv(deposit_details[grepl("bcountrydata_",deposit_details$name),"download_url"][1])
#bcountry <- read_csv("bcountrydata_2021-08-04.txt.zip")
bcountry <- bcountry %>% dplyr::select(timestamp,country,count,occupation)
bcountry$domain <- "en"
### Of new six platforms
bcountry11 <- read_csv(deposit_details[grepl("OLI11_buyer_",deposit_details$name),"download_url"][1])
#bcountry11 <- read_csv("OLI11_buyer_countrydata_2021-08-04.txt.zip")
bcountry11 <- bcountry11 %>% filter(timestamp >= "2020-08-01")
names(bcountry11)[3] <- "country"
### Merge both DFs
bcountry2020 <- rbind(bcountry,bcountry11)
### Format Bcountry DF
names(bcountry2020)[1] <- "date"
bcountry2020$date <- as.Date(as.character(bcountry2020$date), format = "%Y-%m-%d")
bcountry2020$count <- as.numeric(bcountry2020$count)
## Rename Occupation
bcountry2020$occupation <- as.character(bcountry2020$occupation)
bcountry2020$occupation[bcountry2020$occupation == "Software development and technology"] <- "Software dev. and tech."
## Relabel domains
bcountry2020$domain <- ifelse(bcountry2020$domain == "en", "English",
                              ifelse(bcountry2020$domain == "ru", "Russian",
                                     ifelse(bcountry2020$domain == "es", "Spanish",
                                            ifelse(bcountry2020$domain == "ph", "Philippine", NA))))


#%#%#%#%#%#%#%#%#%#%#%#%%#%#%#%
# Calculate static data
#%#%#%#%#%#%#%#%#%#%#%#%%#%#%#%
# For OLI 2020
## Use chain linking to avoid level swift when adding six new platforms to the OLI 5 in January 2021
### Calculate Moving Average
df_oli_old <- oli2020 %>% filter(domain=="OLI-5") %>% group_by(date) %>% dplyr::summarise(count_old = mean(count)) %>% 
  mutate(moving.average.old = rollmean(count_old, 28, fill = list(NA, NULL, NA), align = "right")) %>%
  mutate(moving.average.old = round(moving.average.old/moving.average.old[date=="2016-06-01"]*100,2)) 

### Define a break point for the date of adding new platforms
break_point <- df_oli_old$moving.average.old[df_oli_old$date=="2021-01-01"]

### Adjust new ts after the break point
df_oli_new <- oli2020 %>% group_by(date) %>% dplyr::summarise(count_new = mean(count)) %>% 
  mutate(moving.average.new = rollmean(count_new, 28, fill = list(NA, NULL, NA), align = "right")) %>%
  mutate(moving.average.new = round(moving.average.new/moving.average.new[date=="2021-01-01"]*break_point,2))

df_oli <- left_join(df_oli_old,df_oli_new)

# Domain and Occupations
df_domain_occ <- bcountry2020 %>% group_by(domain, occupation) %>% dplyr::summarise(count = sum(count)) %>% 
  group_by(domain) %>% mutate(share_group = count/sum(count)) 
df_domain_occ <- df_domain_occ %>% mutate(share_market = count/sum(df_domain_occ$count)) 
# Regroup domains and occupations again
df_domain_occ <-  df_domain_occ %>% ungroup(domain, occupation) %>% mutate(domain = factor(domain), occupation = factor(occupation))

# Occupations
df_occupation <- bcountry2020 %>% group_by(date, occupation) %>% dplyr::summarise(count = sum(count)) %>% 
  group_by(date) %>% mutate(total = sum(count)) %>% group_by(date, occupation) %>% mutate(share = count/total) %>%
  group_by(occupation) %>% mutate(moving.average=rollmean(share, 28, fill = list(NA, NULL, NA), align = "right"))

df_occupation <-  df_occupation %>% ungroup(occupation) %>% mutate(occupation = factor(occupation))

# Countries
## Renormalise ts in August 2020 when new platforms are added
df_country <- bcountry2020
df_country <- df_country %>% group_by(date, country) %>% dplyr::summarise(count = sum(count)) 
df_country <- df_country %>% group_by(country) %>% mutate(count_weight = count/count[date=="2020-08-01"][1L]*count[date=="2020-07-31"][1L])
df_country$count <- ifelse(df_country$date >= "2020-08-01",(df_country$count_weight),df_country$count)

df_country <- df_country %>% 
  group_by(date) %>% mutate(total = sum(count, na.rm=TRUE)) %>% group_by(date, country) %>% mutate(share = count/total) %>%
  group_by(country) %>% mutate(moving.average=rollmean(share, 28, na.pad=TRUE, align="right")) 

df_country <-  df_country %>% ungroup(country) %>% mutate(country = factor(country))

# Countries and Occupations
df_cty_occ <- bcountry2020 %>% group_by(country, occupation) %>% dplyr::summarise(count = sum(count)) %>% group_by(country) %>% 
  mutate(share_group = count/sum(count)) 

df_cty_occ <- df_cty_occ %>% mutate(share_market = count/sum(df_cty_occ$count)) 

# Regroup occupations and countries again
df_cty_occ <-  df_cty_occ %>% ungroup(country, occupation) %>% mutate(country = factor(country), occupation = factor(occupation))


#%#%#%#%#%#%#%#%#%#%#%#%%#%#%#%
# Calculate reactive data
#%#%#%#%#%#%#%#%#%#%#%#%%#%#%#%
# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Make Online Labour Index dataframe reactive
  oli_select <- reactive({
    
    if (input$version == "1") {
      
      df_oli$moving.average <- ifelse(df_oli$date>="2021-01-01", df_oli$moving.average.new, df_oli$moving.average.old)
    
      df_oli <- df_oli %>% mutate(moving.average = df_oli$moving.average/df_oli$moving.average[df_oli$date == input$dates[1]]*100) 
      
    }
    
    if (input$version == "2") {
      
      df_oli <- df_oli %>% 
        mutate(moving.average = df_oli$moving.average.old/df_oli$moving.average.old[df_oli$date == input$dates[1]]*100)
      
    }
    
    df_oli <- df_oli %>% filter(as.character(date) >= input$dates[1] & as.character(date) <= input$dates[2])
    
  })
  
  # Make Country Occupation dataframe reactive
  domain_occ_select <- reactive({
    
    # Allow definition of market vs group share
    df_domain_occ$share <- df_domain_occ$share_group
    
    if (input$slice == "1") {
      df_domain_occ$share <- df_domain_occ$share_group
    }
    if (input$slice == "2") {
      df_domain_occ$share <- df_domain_occ$share_market
    }
    
    # Filter by country and occupatiom
    df_domain_occ <- df_domain_occ %>% filter(domain %in% input$domain2 & occupation %in% input$occupation2) 
    
  })
  
  # Make Occupation dataframe reactive
  occupation_select <- reactive({
    
    if (input$norm1 == "1") {
      df_occupation <- df_occupation
    }
    if (input$norm1 == "2") {
      
      df_occupation <- df_occupation %>%
        group_by(occupation) %>% mutate(moving.average = moving.average/moving.average[as.character(date) == paste(input$dates3[1],"-01-01",sep="")][1L])
      df_occupation <-  df_occupation %>% ungroup(occupation) %>% mutate(occupation = factor(occupation))
    }
    
    df_occupation <- df_occupation %>% filter(as.character(date) >= input$dates3[1] & as.character(date) <= input$dates3[2]+1 & occupation %in% input$occupation) 
    
  })
  
  
  world_select <- reactive({
  
    df_country <- df_country %>% filter(as.character(date) >= input$dates4[1] & as.character(date) <= input$dates4[2]+1) 
    
  })
  
  # Make Country dataframe reactive
  country_select <- reactive({
    
    if (input$norm2 == "1") {
      df_country <- df_country
    }
    if (input$norm2 == "2") {
      
      df_country <- df_country %>% group_by(country) %>% 
        mutate(moving.average = moving.average/moving.average[as.character(date) == paste(input$dates4[1],"-01-01",sep="")][1L])
      df_country <-  df_country %>% ungroup(country) %>% mutate(country = factor(country))
    }
    
    df_country <- df_country %>% filter(as.character(date) >= input$dates4[1] & as.character(date) <= input$dates4[2]+1 & country %in% input$country) 
    
  })
  
  # Make Country Occupation dataframe reactive
  cty_occ_select <- reactive({
    
    # Allow definition of market vs group share
    df_cty_occ$share <- df_cty_occ$share_group
    
    if (input$slice2 == "1") {
      df_cty_occ$share <- df_cty_occ$share_group
    }
    if (input$slice2 == "2") {
      df_cty_occ$share <- df_cty_occ$share_market
    }
    
    # Filter by country and occupatiom
    df_cty_occ <- df_cty_occ %>% filter(country %in% input$country2 & occupation %in% input$occupation3) 
    
  })
  
  
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%  
# Create Plots
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
  # Generate a online labour index plot of the data
  output$oli_plotly <- renderPlotly({

    oli_plot <- oli_select() %>% 
      #df_oli %>%
      ggplot(aes(x = date, y = moving.average)) +
      geom_point(aes(text=sprintf("OLI 2020: %s<br>Date: %s", round(moving.average,1), date)), size = 0.1, color = "#0072B2") +
      geom_line(lty = 1, lwd = 1, color = "#0072B2", show.legend = F) +
      labs(x = "", y = paste("OLI 2020 | 100 = ", input$dates[1], sep=""), colour = "", fill = "") +
      theme_bw() + theme(text = element_text(size = 12), legend.position = "bottom", panel.grid.minor = element_blank(),
                         panel.grid.major.x = element_blank())
  # Turn it into plotly
  ggplotly(oli_plot, tooltip="text") %>% 
    layout(showlegend = F)
  })
  
  # Generate bar chart
  output$domain_occ_bar <- renderPlotly({
    
    dmn_occ_plot <- domain_occ_select() %>% group_by(domain) %>% mutate(size = sum(count)) %>%
      ggplot(aes(fill=occupation, y=share*100, x=reorder(domain, size), 
                 text=sprintf("Language: %s<br>Occupation: %s<br>%s percent of demand", domain, occupation, round(share*100,1)))) +
      geom_bar(position="stack", stat="identity") +
      labs(x = "", y = "", colour = "", fill = "") + coord_flip() +
      theme_bw() + theme(text = element_text(size = 12), legend.position = "bottom", panel.grid.minor = element_blank(),
                         panel.grid.major.x = element_blank()) + scale_fill_manual(values=cbPalette)
    # Turn it into plotly
    ggplotly(dmn_occ_plot, tooltip="text") %>% 
      layout(showlegend = T,
             legend=list(orientation="h", yanchor="bottom", y=-.4, xanchor="right", x=0.7, font=list(size=10)))
    
  })
  
  # Generate occupation time series
  output$occupation_ts <- renderPlotly({
    
    occupation_ts <- occupation_select() %>% 
      ggplot(aes(x = date, y = moving.average*100, group = occupation)) + 
      geom_line(aes(color=occupation, text=sprintf("%s<br>%s<br>Date: %s", round(moving.average*100,1), occupation, date))) +
      labs(x = "", y = paste("Online Labour Index | Start: ",input$dates3[1],"-01", sep=""), colour = "", fill = "")+
      theme_bw() + theme(text = element_text(size = 12), legend.position = "bottom", panel.grid.minor = element_blank(),
                         panel.grid.major.x = element_blank()) + scale_color_manual(values=cbPalette)
    # Turn it into plotly
    ggplotly(occupation_ts, tooltip="text") %>% 
      layout(showlegend = T, 
             legend=list(orientation="h", yanchor="bottom", y=-.3, xanchor="right", x=0.7, font=list(size=10))) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))
      
  })
  
  # Generate occupation bar chart
  output$occupation_bar <- renderPlotly({
    
    occupation_bar <- occupation_select() %>% group_by(occupation) %>% dplyr::summarise(share = mean(share)) %>%
      ggplot(aes(fill=occupation, y=share*100, x=reorder(occupation, share), 
                 text=sprintf("Occupation: %s<br>%s percent of demand", occupation, round(share*100,1)))) +
      geom_bar(position="stack", stat="identity") +
      labs(x = "", y = "", colour = "", fill = "") + coord_flip() +
      theme_bw() + theme(text = element_text(size = 12), legend.position = "bottom", panel.grid.minor = element_blank(),
                         panel.grid.major.x = element_blank()) + scale_fill_manual(values=cbPalette)
    # Turn it into plotly
    ggplotly(occupation_bar, tooltip="text") %>% 
      layout(showlegend = F)
    
  })
  
  # Generate country time series
  output$country_ts <- renderPlotly({
    
    
    if (length(unique(country_select()$country)) <= 7) {
      
      country_ts <- country_select() %>%
        #df_country %>% filter(country %in% c("United States", "Russia", "Spain")) %>%
        ggplot(aes(x = date, y = moving.average*100, group = country)) + 
        geom_line(aes(color=country, text=sprintf("%s<br>Country: %s<br>Date: %s",round(moving.average*100,1), country, date))) +
        labs(x = "", y = paste("Online Labour Index | Start: ",input$dates4[1],"-01", sep=""), colour = "", fill = "")+
        theme_bw() + theme(text = element_text(size = 12), legend.position = "bottom", panel.grid.minor = element_blank(),
                           panel.grid.major.x = element_blank()) + scale_color_manual(values = cbPalette)
    }
    
    if (length(unique(country_select()$country)) > 7) {
      
    country_ts <- country_select() %>%
      #df_country %>% filter(country %in% c("United States", "Russia", "Spain")) %>%
      ggplot(aes(x = date, y = moving.average*100, group = country)) + 
      geom_line(aes(color=country, text=sprintf("%s<br>Country: %s<br>Date: %s",round(moving.average*100,1), country, date))) +
      labs(x = "", y = paste("Online Labour Index | Start: ",input$dates4[1],"-01", sep=""), colour = "", fill = "")+
      theme_bw() + theme(text = element_text(size = 12), legend.position = "bottom", panel.grid.minor = element_blank(),
                         panel.grid.major.x = element_blank()) 
    
    }
    
    # Turn it into plotly
    ggplotly(country_ts, tooltip="text") %>% 
      layout(showlegend = T, 
             legend=list(orientation="h", yanchor="bottom", y=-.2, xanchor="right", x=0.7, font=list(size=10))) %>%
      config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))
    
  })
  
  # Generate country bar chart
  output$country_bar <- renderPlotly({
    
    if (length(unique(country_select()$country)) <= 7) {
      
      country_bar <- country_select() %>% 
        group_by(country) %>% dplyr::summarise(share = mean(share)) %>%
        ggplot(aes(fill=country, y=share*100, x=reorder(country, share), 
                   text=sprintf("Country: %s<br>%s percent of demand", country, round(share*100,1)))) +
        geom_bar(position="stack", stat="identity") +
        labs(x = "", y = "", colour = "", fill = "") + coord_flip() +
        theme_bw() + theme(text = element_text(size = 12), legend.position = "bottom", panel.grid.minor = element_blank(),
                           panel.grid.major.x = element_blank()) + scale_fill_manual(values = cbPalette)
      
    }
    
    
    if (length(unique(country_select()$country)) > 7) {
      
      country_bar <- country_select() %>% 
        group_by(country) %>% dplyr::summarise(share = mean(share)) %>%
        ggplot(aes(fill=country, y=share*100, x=reorder(country, share), 
                 text=sprintf("Country: %s<br>%s percent of demand", country, round(share*100,1)))) +
        geom_bar(position="stack", stat="identity") +
        labs(x = "", y = "", colour = "", fill = "") + coord_flip() +
        theme_bw() + theme(text = element_text(size = 12), legend.position = "bottom", panel.grid.minor = element_blank(),
                         panel.grid.major.x = element_blank()) 
      
    }
    
    # Turn it into plotly
    ggplotly(country_bar, tooltip="text") %>% 
      layout(showlegend = F)
    
  })
 
  # Generate bar chart
  output$cty_occ_plotly <- renderPlotly({
    
    bar_plot <- cty_occ_select() %>% group_by(country) %>% mutate(size = sum(count)) %>%
      ggplot(aes(fill=occupation, y=share*100, x=reorder(country, size), 
                 text=sprintf("Country: %s<br>Occupation: %s<br>%s percent of demand", country, occupation, round(share*100,1)))) +
      geom_bar(position="stack", stat="identity") +
      labs(x = "", y = "", colour = "", fill = "") + coord_flip() +
      theme_bw() + theme(text = element_text(size = 15), legend.position = "bottom", panel.grid.minor = element_blank(),
                         panel.grid.major.x = element_blank()) + scale_fill_manual(values=cbPalette)
    # Turn it into plotly
    ggplotly(bar_plot, tooltip="text") %>% 
      layout(showlegend = T, 
             legend=list(orientation="h", yanchor="bottom", y=-.3, xanchor="right", x=0.7, font=list(size=10)))
    
  })
  
  #%#%#%#%#%#%#%#%#%#%#%#%
  # Download functions
  #%#%#%#%#%#%#%#%#%#%#%#%
  
  # Downloadable csv of selected dataset
  output$download_oli_data <- downloadHandler(
    
    filename = function() {
      paste("oli_",input$dates[1],"_to_",input$dates[2],".csv", sep = "")
    },
    content = function(file) {
      write.csv(oli_select(), file, row.names = FALSE, quote = FALSE)
    }
  )
  
  # Downloadable csv of selected dataset
  output$download_domain_data <- downloadHandler(
    
    filename = function() {
      paste("oli_domains_",input$dates2[1],"_to_",input$dates2[2],".csv", sep = "")
    },
    content = function(file) {
      write.csv(domain_select()[,c("date","domain","share","moving.average")], file, row.names = FALSE, quote = FALSE)
    }
  )
  
  # Downloadable csv of selected dataset
  output$download_bar_data <- downloadHandler(
    
    filename = function() {
      paste("oli_bars_selected.csv", sep = "")
    },
    content = function(file) {
      write.csv(cty_occ_select()[,c("domain","occupation","share")], file, row.names = FALSE, quote = FALSE)
    }
  )
  
  # Downloadable csv of selected dataset
  output$download_occupation_data <- downloadHandler(
    
    filename = function() {
      paste("oli_occupations_",input$dates3[1],"_to_",input$dates3[2],".csv", sep = "")
    },
    content = function(file) {
      write.csv(occupation_select()[,c("date","occupation","share","moving.average")], file, row.names = FALSE, quote = FALSE)
    }
  )
  
  # Downloadable csv of selected dataset
  output$download_country_data <- downloadHandler(
    
    filename = function() {
      paste("oli_countries_",input$dates4[1],"_to_",input$dates4[2],".csv", sep = "")
    },
    content = function(file) {
      write.csv(country_select()[,c("date","country","share","moving.average")], file, row.names = FALSE, quote = FALSE)
    }
  )
  
  # Downloadable csv of selected dataset
  output$download_world_data <- downloadHandler(
    
    filename = function() {
      paste("oli_world_",input$dates4[1],"_to_",input$dates4[2],".csv", sep = "")
    },
    content = function(file) {
      write.csv(world_select()[,c("date","country","share","moving.average")], file, row.names = FALSE, quote = FALSE)
    }
  )
  
  # Downloadable csv of selected dataset
  output$download_cty_occ_data <- downloadHandler(
    
    filename = function() {
      paste("oli_cty_occ_selected.csv", sep = "")
    },
    content = function(file) {
      write.csv(cty_occ_select()[,c("country","occupation","share")], file, row.names = FALSE, quote = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)
