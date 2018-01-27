########################### GROUP 10 - TEAM MEMBERS ######################################################################
# Florence SAVARYDEBEAUREGARD
# Yongsheng LOH
# Chenpei TAN

########################### LIBRARIES ####################################################################################

install.packages("shiny")
install.packages("ggmap")
install.packages("mapproj")
install.packages("scatterD3")
install.packages("shinythemes")

library(shiny)
library(ggmap)
library(mapproj)
library(scatterD3)
library(shinythemes)

########################### DATA MANIPULATION ############################################################################

###### 1st tab: Lofts' location
#extracts all loft_id to plot of the loft's location on belgium map
removedupid <- data_aggregate %>% filter(!is.na(loft_latitude)) # Remove those loft_id that have no latitude and longitude
loftid <- sort(removedupid$loft_id) # Sort loft_id by ascending order


###### 2nd tab: Lofts'performance
# Keep only relevant columns that can be used to compare with other loft_id or overall average performance
loft_avg <- data_aggregate[c(1,4,8,9,10,11,12,13,14,15,16,17,18,19,21,22,25,26,27)]

# Calculate average performance of all variables used for comparison
overallavg <- loft_avg %>% 
              summarise(v1_avg_velocity = mean(v1_avg_velocity, na.rm = TRUE),
                        v2_max_velocity = mean(v2_max_velocity, na.rm = TRUE),
                        v3_number_pigeon = mean(v3_number_pigeon, na.rm = TRUE),
                        v4_max_race = mean(v4_max_race, na.rm = TRUE),
                        v5_best_position = mean(v5_best_position, na.rm = TRUE),
                        v6_avg_position = mean(v6_avg_position, na.rm = TRUE),
                        v7_avg_basket = mean(v7_avg_basket, na.rm = TRUE),
                        v8_sum_basket = mean(v8_sum_basket, na.rm = TRUE),
                        v9_race_count = mean(v9_race_count, na.rm = TRUE),
                        v10_avg_distance = mean(v10_avg_distance, na.rm = TRUE),
                        v11_accuracy_percent = mean(v11_accuracy_percent, na.rm = TRUE),
                        v12_number_top_25 = mean(v12_number_top_25, na.rm = TRUE),
                        v14_avg_best_pigeon_position = mean(v14_avg_best_pigeon_position, na.rm = TRUE),
                        v15_std_error_pred = mean(v15_std_error_pred, na.rm = TRUE),
                        v18_sd_velocity = mean(v18_sd_velocity, na.rm = TRUE),
                        v19_daysbetweenraces = mean(v19_daysbetweenraces, na.rm = TRUE),
                        v20_avg_raceinterval_pigeon = mean(v20_avg_raceinterval_pigeon, na.rm = TRUE)) %>%
              mutate(loft_id = "overall average", loft = "NA") %>%
              select(loft_id, loft, everything()) %>%
              arrange(loft_id)

# bind overall average data to the information for each loft  
avg_data = rbind(overallavg,loft_avg)

# give the correct rounding for the values in the variables
avg_data = avg_data %>% 
           mutate(v1_avg_velocity = round(v1_avg_velocity, 2),
                  v2_max_velocity = round(v2_max_velocity, 2),
                  v3_number_pigeon = round(v3_number_pigeon),
                  v4_max_race = round(v4_max_race),
                  v5_best_position = round(v5_best_position),
                  v6_avg_position = round(v6_avg_position),
                  v7_avg_basket = round(v7_avg_basket),
                  v8_sum_basket = round(v8_sum_basket),
                  v9_race_count = round(v9_race_count),
                  v10_avg_distance = round(v10_avg_distance, 2),
                  v11_accuracy_percent = round(v11_accuracy_percent, 2),
                  v12_number_top_25 = round(v12_number_top_25, 2),
                  v14_avg_best_pigeon_position = round(v14_avg_best_pigeon_position),
                  v15_std_error_pred = round(v15_std_error_pred, 2),
                  v18_sd_velocity = round(v18_sd_velocity, 2),
                  v19_daysbetweenraces = round(v19_daysbetweenraces),
                  v20_avg_raceinterval_pigeon = round(v20_avg_raceinterval_pigeon))

# extracts all unique loft_id and average
avg_dataid <- sort(unique(avg_data$loft_id))


###### 3rd tab: Graphs
#extracts all performance variables (variables starting with v followed by a digit)
performance_var  = colnames(data_aggregate[grep("^v[0-9]", colnames(data_aggregate))])

#variables for histogram
hist_var = performance_var[c(3,4,7,8,9)]


###### 4th tab: Loft information by race
#extracts all 24 races
themes <- sort(unique(data_all$UKey_Race))

#create function to extract information for the below data.table created to look at all/specific race information as selected by user
groupByTheme <- function(dt, themes) {
  dt <- data_all %>% 
        select(loft, position, UKey_Race) %>% filter(UKey_Race %in% themes) %>% arrange(position) 
  return(dt)
}


###### 5th tab: Race Performance
# Calculates race specific variables
race = data_all %>% 
       group_by(UKey_Race) %>% 
       summarise(avg_velocity = mean(velocity, na.rm = TRUE), 
                 max_velocity = max(velocity), 
                 min_velocity = min(velocity),
                 sd_velocity = sd(velocity, na.rm = TRUE),
                 avg_distance = mean(distance, na.rm = TRUE))
# Extracts the names of the calculated race variables
race_var = colnames(race)[c(2:length(colnames(race)))]


########################### UI ###########################################################################################

ui <- fluidPage(theme = shinytheme("sandstone"),
  
  titlePanel("The Pigeon Racing Dashboard"),

    tabsetPanel(
      
###### 1st tab: Lofts' location
      tabPanel(
        p(icon("map-marker"),"Lofts' location"), 
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            #Magnification
            sliderInput(inputId = "magnify",
                        label = "Select magnification",
                        min = 7, max = 9,
                        value = 8,
                        step = 1),
            # Select all loft_id option
            actionLink("selectall","Select All"),
            # Unselect all loft_id option
            actionLink("unselectall","Unselect All"),
            #submitButton("Update View", icon("refresh")),
            checkboxGroupInput(inputId = "map",
                               label = "Select loft(s):",
                               choices = loftid,
                               selected = loftid)
          ),
          mainPanel(
            h5('Note: Please wait... It takes a while for the map to load', align = "center"),
            plotOutput("map", width = "100%", height = "1000px")
          )
        )
      ),
      
###### 2nd tab: Lofts'performance
      tabPanel(
        p(icon("info-circle"),"Lofts' performance"), 
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(
            # Create a multi-select box
            # SELECTIZE
            selectizeInput(
              'e2', 'Select multiple loft_id for comparison', choices = avg_dataid, selected = c("overall average","100042-35","100168-64"), multiple = TRUE
            )
          ),
          mainPanel(
            tableOutput(outputId="dTablebyid")
          )
        )
      ),

###### 3rd tab: Graphs
      tabPanel(
        p(icon("line-chart"),"Graphs"),
        fluid = TRUE,
        sidebarLayout(
          sidebarPanel(p(icon("filter"),"Filters for Histogram"),
            selectInput(
                inputId = "variableselect",
                label = "Select variables:",
                choices = hist_var,
                selected = "v9_race_count"),
            sliderInput(inputId = "numberselect",
                        label = "Select number of breaks:",
                        min = 1, max = 100,
                        value = 20,
                        step = 1),
            #Add radio buttons to choose colour of histogram
            radioButtons(inputId="colselect",
                         "Select bar colour:",c("blue" = "skyblue", "green"="lightseagreen", "pink"= "lightpink2"))

            ),
          mainPanel(
            # Histogram
            h4('Histogram of selected variable', align = "center"),
            plotOutput("vgraphs", click = "plot_click",hover = "plot_hover"),
            h6('Values from Histogram', align = "left"),
            verbatimTextOutput("info"),
            
            # Scatterplot
            h4('Scatterplot - Average Velocity vs Average Distance', align = "center"),
            scatterD3Output("sgraphs", height = "700px") 
          )
      )
    ),
    
###### 4th tab: Loft information by race
#               You can view information about the loft for all/specific races it has participated 
    tabPanel(
      p(icon("info-circle"),"Loft information by race"), 
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          
          #create selection checkbox for race
          actionButton(inputId = "clearAllTop", 
                       label = "Clear selection", 
                       icon = icon("square-o")),
          actionButton(inputId = "selectAllTop",
                       label = "Select all",
                       icon = icon("check-square-o")),
          uiOutput("themesControl"), # the id
          actionButton(inputId = "clearAllBottom",
                       label = "Clear selection",
                       icon = icon("square-o")),
          actionButton(inputId = "selectAllBottom",
                       label = "Select all",
                       icon = icon("check-square-o"))
        ),
        mainPanel(
          dataTableOutput(outputId="dTable")
        )
      )
    ),
 

###### 5th tab: Race Performance
    tabPanel(
      p(icon("bar-chart"),"Race Performance"), 
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          # select the race metric
          selectInput(
            inputId = "racedata",
            label = "Compare race by variable:",
            choices = race_var,
            selected = "avg_velocity")
        ),
        mainPanel(
          h4('Race performance by selected variable', align = "center"),
          plotOutput("race_compare", click = "plot_click",hover = "plot_hover"),
          h6('Values from Barplot', align = "left"),
          verbatimTextOutput("info2")
        )
      )
    ),

###### 6th tab: information about races within selected date range
    tabPanel(
      p(icon("calendar"),"Race Information"),  
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          #Add daterange to allow user to specify their desired date range
          #however, we have limit the date selection between first and last race available in the dataset
          dateRangeInput("daterange", "Date range:",
                         label = 'Select Date Range to view information about races',
                         start  = "2017-05-27",
                         end    = "2017-09-10",
                         min    = "2017-05-27",
                         max    = "2017-09-10",
                         format = "yyyy-mm-dd",
                         separator = " - ")
        ),
        mainPanel(
          tableOutput("raceinfotable")
        )
      )
    )
  )
)


########################### SERVER #######################################################################################

server <- function(input, output, session) {
  
###### 1st tab: Lofts' location 
  
  #Create map plot
  output$map <- renderPlot({
    
    #filters the loft based on user selection 
    selected_loft = data_aggregate %>%
      filter(loft_id %in% input$map)
    
    #generate map of Belgium
    map <- get_map(location = 'Belgium', zoom = input$magnify)
    
    # Adds coordinates of the selected loft to the map 
    mapPoints <- ggmap(map) +
      geom_point(data = selected_loft, aes(x = loft_longitude, y = loft_latitude, color = "red", alpha = 1))
    mapPoints
    
    
  })
  
  # select all tickbox
  observe({
    if(input$selectall == 0) return(NULL) # do nothing if it is not clicked
    
    # select all if clicked
    else
    {
      updateCheckboxGroupInput(session,"map","Select loft(s):",choices=loftid,selected=loftid)
    }
  })
  
  # unselect all tickbox
  observe({
    if(input$unselectall == 0) return(NULL) # do nothing if it is not clicked
    
    # unselect all if clicked
    else
    {
      updateCheckboxGroupInput(session,"map","Select loft(s):",choices=loftid,selected=NULL)
    }
  })

  
###### 2nd tab: Lofts'performance
  # Allow comparison of performance for the various variables with "overall average" and other loft_id as compared to own loft's performance
  output$dTablebyid <- renderTable({
    avg_data <- avg_data %>% filter(loft_id %in% input$e2) 
  })
  
###### 3rd tab: Graphs
  
  # Create Histogram for variable performance (selected variables)
  output$vgraphs <- renderPlot({
    hist(as.numeric(unlist(data_aggregate[input$variableselect])),xlab = input$variableselect, main = " ", breaks= input$numberselect, col=input$colselect)
  })
  
  # Create info when the mouse hover or click on the histogram
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x-axis=", round(e$x), ", number of lofts=", round(e$y), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin), " xmax=", round(e$xmax), 
             " ymin=", round(e$ymin), " ymax=", round(e$ymax))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "hover: ", xy_str(input$plot_hover)
    )
  })
  
  # Create Scatterplot to compare relationship between v10_avg_distance and v1_avg_velocity
  output$sgraphs <- renderScatterD3({
    tooltips <- paste("Loft_id: ", data_aggregate$loft_id,"<br /> has an average velocity of ", 
                      data_aggregate$v1_avg_velocity, "m/min & average distance of ", data_aggregate$v10_avg_distance,"m")

    scatterD3(x = data_aggregate$v10_avg_distance, y = data_aggregate$v1_avg_velocity, point_opacity = 0.4,
              hover_size = 4, hover_opacity = 1, colors = "powderblue", tooltip_text = tooltips)
    
  }) 
  
###### 4th tab: Loft information by race
  
  # Initialize reactive values
  values <- reactiveValues()
  values$themes <- themes
  
  # Add observer on select-all button
  observe({
    if(input$selectAllTop > 0) {
      updateCheckboxGroupInput(session=session, inputId="themes", 
                               choices=themes, selected=themes)
      values$themes <- themes
    }
  })
  observe({
    if(input$selectAllBottom > 0) {
      updateCheckboxGroupInput(session=session, inputId="themes", 
                               choices=themes, selected=themes)
      values$themes <- themes
    }
  })
  
  # Add observer on clear-all button
  observe({
    if(input$clearAllTop > 0) {
      updateCheckboxGroupInput(session=session, inputId="themes", 
                               choices=themes, selected=NULL)
      values$themes <- c()
    }
  })
  observe({
    if(input$clearAllBottom > 0) {
      updateCheckboxGroupInput(session=session, inputId="themes", 
                               choices=themes, selected=NULL)
      values$themes <- c()
    }
  })
  
  # Create checkbox for all 24 races
  output$themesControl <- renderUI({
    checkboxGroupInput('themes', 'Select Race:', 
                       themes, selected = values$themes)
  })
  
  # Prepare dataset 
  dataTable <- reactive({
    groupByTheme(data_all, input$themes)
  })
  
  # Render data table
  output$dTable <- renderDataTable({
    dataTable()
  } , options = list(bFilter = TRUE, iDisplayLength = 10)
  )
  
###### 5th tab: Race Performance
  
  # Create table for comparison of race performance
  output$race_compare <- renderPlot({
    par(mar = c(10, 8, 0.5, 0.5), mgp = c(6,0,-1))
    barplot(height = as.numeric(unlist(race[input$racedata])), names.arg = as.vector(race$UKey_Race), 
            xlab = "Race", ylab = input$racedata, space = 0.5, las = 2, width = 3, col=rgb(0.2,0.4,0.6,0.6))
  })
  
  
  # Create info when the mouse hover or click on the barplot
  output$info2 <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("value = ", round(e$y,2), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0(" ymin=", round(e$ymin,2), " ymax=", round(e$ymax))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "hover: ", xy_str(input$plot_hover)
    )
  })
  

###### 6th tab: information about races within selected date range
  
  # Create table to show the races and number of loft participated within a selected date range
  output$raceinfotable <- renderTable({
    
    min <- input$daterange[1] 
    max <- input$daterange[2]
    
    race_info_date <- i_file %>% 
                      filter(date >= min & date <= max) %>% # Show only data based on selected date range
                      select(date, UKey_Race, nbr_lofts, nbr_birds) %>% 
                      arrange(date)
    
    format(race_info_date, nsmall=5) # format date to be readable
  })
 
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

