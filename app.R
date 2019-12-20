    library(shiny)
    library(dplyr)
    library(ggplot2)
    library(httr)
    library(lubridate)
    library(jsonlite)
    library(leaflet)
    library(leaflet.extras)
    library(DT)
    
    # Load data.
    kcCrime5 <- read.csv('kcCrimeClean.csv')
    zipCodes <- readLines('kcZips.geojson', warn = FALSE) %>%
        paste(collapse = '\n') %>%
        fromJSON(simplifyVector = FALSE)
    
    # Set cityLimits style.
    zipCodes$style = list(
        weight = 1,
        color = '#000000',
        opacity = 1,
        fillOpacity = 0.4
    )
    
    # Format the Date.Time variable.
    kcCrime5$Date.Time <- as.POSIXct(kcCrime5$Date.Time)
    
    # Make an ordered factor out of the Month.Year variable.
    levels = c('Jan 2016',
               'Feb 2016',
               'Mar 2016',
               'Apr 2016',
               'May 2016',
               'Jun 2016',
               'Jul 2016',
               'Aug 2016',
               'Sep 2016',
               'Oct 2016',
               'Nov 2016',
               'Dec 2016',
               'Jan 2017',
               'Feb 2017',
               'Mar 2017',
               'Apr 2017',
               'May 2017',
               'Jun 2017',
               'Jul 2017',
               'Aug 2017',
               'Sep 2017',
               'Oct 2017',
               'Nov 2017',
               'Dec 2017',
               'Jan 2018',
               'Feb 2018',
               'Mar 2018',
               'Apr 2018',
               'May 2018',
               'Jun 2018',
               'Jul 2018',
               'Aug 2018',
               'Sep 2018',
               'Oct 2018',
               'Nov 2018',
               'Dec 2018')
    
    kcCrime5$monthYear <- factor(kcCrime5$monthYear, levels = levels, ordered = TRUE)
    
    
    # Factorize the categorical variables.
    kcCrime5$Involvement <- factor(kcCrime5$Involvement)
    kcCrime5$Race <- factor(kcCrime5$Race)
    kcCrime5$Sex <- factor(kcCrime5$Sex)
    kcCrime5$Description <- factor(kcCrime5$Description)
    kcCrime5$DVFlag <- factor(kcCrime5$DVFlag)
    kcCrime5$Firearm.Used.Flag <- factor(kcCrime5$Firearm.Used.Flag)
    kcCrime5$Offense <- factor(kcCrime5$Offense)
    kcCrime5$Offense.Description <- factor(kcCrime5$Offense.Description)
    kcCrime5$Crime.Against <- factor(kcCrime5$Crime.Against)
    kcCrime5$Month <- factor(kcCrime5$Month)
    kcCrime5$Is.Juvenile <- factor(kcCrime5$Is.Juvenile)
    
    kcCrime5 <- na.omit(kcCrime5)
    
    violents <- c("Murder & Non-negligent Manslaughter",
                  "Negligent Manslaughter",
                  "Rape",
                  "Sodomy",
                  "Sexual Assault With An Object",
                  "Fondling",
                  "Aggravated Assault",
                  "Robbery")
    
    offenseTable <- kcCrime5 %>% 
        group_by(Offense = Offense.Description) %>% 
        summarize(`2016` = sum(Year == 2016),
                  `2017` = sum(Year == 2017),
                  `2018` = sum(Year == 2018))
    
    offenseTable2 <- kcCrime5 %>% 
        group_by(Offense = Offense.Description, Year) %>% 
        summarize(Offenses = n())
    
    offenseTable3 <- kcCrime5 %>% 
        group_by(Offense = Offense.Description, Year) %>% 
        summarize(Offenses = n(),
                  Suspects = sum(Involvement == 'SUS'),
                  Arrests = sum(Involvement == 'ARR'),
                  Victims = sum(Involvement == 'VIC'),
                  Involved_Firearm = sum(Firearm.Used.Flag == "Y"))
    
    # Define UI for application that draws a histogram
    ui <- fluidPage(
        
        navbarPage(
            
            title = 'Kansas City Crime Analysis',
            
            theme = 'bootstrap5.css',
            
            tabPanel('Overview',
                     tags$section(
                         id = 'greetings',
                         tags$div(
                             class = 'container',
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-12 text-center',
                                     img(src = 'kcpd_patch.png'),
                                     tags$h2(
                                         class = 'section-heading text-uppercase',
                                         "Crime in Kansas City, MO"
                                     ),
                                     tags$h3(
                                         class = 'section-subheading text-muted',
                                         "A brief analysis of KCPD crime data from 2016 thru 2018."
                                     )
                                 )
                             ),
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-2'
                                 ),
                                 tags$div(
                                     class = 'col-lg-8',
                                     tags$p("In an effort to provide transparency to the public, the City of Kansas City, Missouri has made all of its data available online. Its data pportal ", tags$a(href = 'https://data.kcmo.org/', "Open Data KC"), ", is part of Socrata's ", tags$a(href = 'https://en.wikipedia.org/wiki/Socrata', "Open Data Network"), ", an initiative developed in July, 2014 designed to foster collaberation between governments and the private-sector.  Up to now, numerous government entities have subscribed to the Open Data Network at multiple levels making public access to government data more accessible than ever before."),
                                     tags$p("On this webpage we will take a look at data from the Kansas City Police Department from 2016 through 2018. We will compare the years visually and see if we can determine which years were different. I'll also produde an interactive heatmap of Kansas City to show where more crime activity happened in the city at different times of the day."),
                                     tags$p("Here is a map of Kansas City, MO. Outlines are the Zip Codes."),
                                     tags$div(leafletOutput('kcCityLimits'))
                                 ),
                                 tags$div(
                                     class = 'col-lg-2'
                                 )
                             ),
                             tags$div(
                                 id = 'row',
                                 tags$div(
                                     class = 'col-lg-2'
                                 ),
                                 tags$div(
                                     class = 'col-lg-8',
                                     tags$h3(
                                         class = 'text-muted text-center',
                                         "A word about the KCPD Dataset"
                                     ),
                                     tags$p("This was a bit of a tough dataset to tackle for this project. First of all, for each year there was a separate dataset that needed to be downloaded. The three datasets were then trimmed to have uniform columns and then were concatenated together to form one dataset. Next, a separate list of Kansas City zip codes were downloaded and rows which were outside of these area codes were trimmed off the dataset. For the purpose of this analysis I chose to focus on offenses which only happened within the Kansas City, MO city limits."),
                                     tags$p("While cleaning up the description fields for use in categorizing the entries by crime, I noticed that each entry had an IBRS number. After applying some research, I found out that the IBRS numbers coincide with a master list of offenses at the federal level. So, I downloaded a list of IBRS numbers from the FBI's website and merged the offenses into the dataframe by IBRS number. This produced a clean and uniform categorization system that didn't rely much on manual cleaning of the description field, where officers tend to spell the same thing a multitude of different ways."),
                                     tags$p("For the maps I downloaded a geojson file of Kansas City Zipcodes, and superimposed them over the map. I was unable to obtain one for the KCPD beat numbers, though I would have liked to include those in the heatmap."),
                                     tags$p("This is just a general overview. More detailed analysis and visualization should result from more in-depth study of this subject. For now, enjoy the interactive visuals.")
                                 ),
                                 tags$div(
                                     class = 'col-lg-2'
                                 )
                             )
                         )
                     )
                ),
            
            tabPanel('Crime Analysis',
                         tags$div(
                             class = 'container',
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-12 text-center',
                                     tags$h2(
                                         "Kansas City, MO Crime At A Glance"
                                     )
                                 )
                             ),
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-4',
                                     selectInput('offenses',
                                                  'Choose an Offense',
                                                  choices = unique(kcCrime5$Offense.Description))
                                 ),
                                 tags$div(
                                     class = 'col-lg-8',
                                     plotOutput('offenseByYear')
                                 )
                             ),
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-4',
                                     radioButtons('year',
                                                  'Year',
                                                  choices = unique(kcCrime5$Year),
                                                  selected = 2018)
                                 ),
                                 tags$div(
                                     class = 'col-lg-8',
                                     plotOutput('offensePlot')
                                 )
                             )
                        )
                     ),
            
            tabPanel('Heatmap',
                         tags$div(
                             class = 'container',
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-12 text-center',
                                     tags$h2(
                                         "Heatmap of Crime in Kansas City, MO"
                                     ),
                                     tags$h5(
                                         class = 'section-subheading text-muted',
                                         "To change the heatmap, you can change all of the boxes to the left by clicking on the box and adding items from the drop-down menu. Multiple items may be selected at a time. To remove an item, simply click on that item and hit the Delete key on your keyboard."
                                     ),
                                     tags$br()
                                 )
                             ),
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-4',
                                     sliderInput('year2',
                                                 'Year',
                                                 min = 2016,
                                                 max = 2018,
                                                 round = TRUE,
                                                 value = 2018,
                                                 ticks = FALSE),
                                     selectizeInput('offense',
                                                  'Offense',
                                                  choices = sort(unique(kcCrime5$Offense.Description)),
                                                  multiple = TRUE,
                                                  selected = "Aggravated Assault"),
                                     selectizeInput('involvement',
                                                 'Involvement',
                                                 choices = unique(kcCrime5$Involvement),
                                                 multiple = TRUE,
                                                 selected = unique(kcCrime5$Involvement)),
                                     selectizeInput('crimeAgainst',
                                                    'Crime Against',
                                                    choices = unique(kcCrime5$Crime.Against),
                                                    multiple = TRUE,
                                                    selected = unique(kcCrime5$Crime.Against)),
                                     selectizeInput('firearmUsed',
                                                 'Firearm Used',
                                                 choices = unique(kcCrime5$Firearm.Used.Flag),
                                                 multiple = TRUE,
                                                 selected = unique(kcCrime5$Firearm.Used.Flag))
                                 ),
                                 tags$div(
                                     class = 'col-lg-8',
                                     leafletOutput('heatmap')
                                 )
                             )
                         )
                     ),
            
            tabPanel('Basic Crime Stats',
                     
                     tags$div(
                         class = 'container',
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-12 text-center',
                                 tags$h2(
                                     "Basic Crime Stats"
                                 )
                             )
                         ),
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-2'
                             ),
                             tags$div(
                                 class = 'col-lg-8',
                                 dataTableOutput("data2")
                             ),
                             tags$div(
                                 class = 'col-lg-2'
                             )
                         )
                     )
                ),
            
            tabPanel('Crime Summary',
                     tags$div(
                         class = 'container',
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-12 text-center',
                                 tags$h2(
                                     "Kansas City, MO Offenses Between 2016 and 2018"
                                 )
                             )
                         ),
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-2'
                             ),
                             tags$div(
                                 class = 'col-lg-8',
                                 dataTableOutput("data")
                             ),
                             tags$div(
                                 class = 'col-lg-2'
                             )
                         )
                     )
                )
            
        )
        
    )
    
    # Define server logic required to draw a histogram
    server <- function(input, output, session) {
        
            output$kcCityLimits <- renderLeaflet({
                
                map <- leaflet(width = 600, height = 800) %>%
                    addTiles() %>% 
                    setView(lng = mean(kcCrime5$Lon), lat = mean(kcCrime5$Lat), zoom = 9)  %>%
                    addGeoJSON(zipCodes, color = NA)
                
                print(map)
                
            })
            
            output$offensePlot <- renderPlot({
                
                summary <- offenseTable2 %>%
                    subset(data = offenseTable2, Year == input$year) %>%
                    filter(Offense != "All Other Offenses") %>%
                    arrange(desc(Offenses)) %>%
                    ungroup() %>%
                    top_n(10)
                
                summary$Offense <- factor(summary$Offense, levels = summary$Offense[order(summary$Offenses)])
                
                p <- summary %>%
                     ggplot(aes_string('Offense', 'Offenses')) +
                        geom_bar(stat = 'identity',
                                 fill = '#165CAA',
                                 color = 'yellow') +
                        geom_text(aes_string(label = 'Offenses'),
                                  color = 'gray',
                                  hjust = 1.25) +
                        coord_flip() +
                        labs(title = 'Ten Most Frequent Crimes in Kansas City',
                             x ='')
                
                p
                
            })
            
            output$offenseByYear <- renderPlot({
                
                summary <- offenseTable3 %>%
                    subset(data = offenseTable3, Offense == input$offenses)
                
                p <- summary %>%
                     ggplot(aes_string('Year', 'Offenses')) +
                        geom_point(color = '#165CAA', fill = NA) +
                        geom_smooth(fill = '#165CAA') +
                        labs(title = "Annual change in Crimes Committed per Year",
                             x = '')
                
                p
                
            })
            
            output$heatmap <- renderLeaflet({
                
                map <- leaflet(width = 600, height = 800) %>%
                    addTiles() %>% 
                    setView(lng = mean(kcCrime5$Lon), lat = mean(kcCrime5$Lat), zoom = 9)  %>%
                    addGeoJSON(zipCodes, color = NA)
                
                map <- map %>%
                    addHeatmap(lat = ~Lat,
                               lng = ~Lon, 
                               data = subset(kcCrime5, Year == input$year2 &
                                                        Offense.Description == input$offense &
                                                        Involvement == input$involvement &
                                                        Crime.Against == input$crimeAgainst &
                                                        Firearm.Used.Flag == input$firearmUsed),
                               radius = 8) %>%
                    addLabelOnlyMarkers(lat = ~Lat,
                                        lng = ~Lon, 
                                        data = subset(kcCrime5, Year == input$year2 &
                                                          Offense.Description == input$offense &
                                                          Involvement == input$involvement &
                                                          Crime.Against == input$crimeAgainst &
                                                          Firearm.Used.Flag == input$firearmUsed))
                
                print(map)
                
            })
            
            output$data <- renderDataTable(offenseTable)
            
            output$data2 <- renderDataTable(offenseTable3)
        
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)