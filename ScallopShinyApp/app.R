################################################################################
################################### SETUP ######################################
################################################################################

# Read in required packages
if (!require("shiny"))
  install.packages("shiny")
if (!require("bslib"))
  install.packages("bslib")
if (!require("shinyjs"))
  install.packages("shinyjs")
if (!require("sf"))
  install.packages("sf")
if (!require("ggplot2"))
  install.packages("ggplot2")
if (!require("dplyr"))
  install.packages("dplyr")
if (!require("here"))
  install.packages("here")
if (!require("markdown"))
  install.packages("markdown")
if (!require("tidyverse"))
  install.packages("tidyverse")
if (!require("ggridges"))
  install.packages("ggridges")
if (!require("cowplot"))
  install.packages("cowplot")

# General setup
`%ni%` <- Negate(`%in%`) # creates "not in" function
options(scipen = 999) # removes scientific notation
# defines drawing order of the size categories
order <- c("≥ 10cm", "8.9cm - 10cm", "7.5cm - 8.9cm", "3.5cm - 7.5cm", "≤ 3.5cm") 


# Read in shapefiles and make sure they have consistent projections 
## coastline of the U.S.
coastline <- st_read(here("ScallopShinyApp/Shapefiles/coastline.shp"))
## Georges Bank SAMS 
GB_strata <- st_read(here("ScallopShinyApp/Shapefiles/GB_Estimation_Areas_2024_UTM19_PDT.shp"))
## Mid-Atlantic SAMS
MAB_strata <- st_read(here("ScallopShinyApp/Shapefiles/MAB_Estimation_Areas_2024_UTM18_PDT.shp"))
## previously used NEFSC shellfish strata
nefsc_shellfish <- st_read(here("ScallopShinyApp/Shapefiles/Shellfish_Strata_UTM19.shp"))
# make sure all layers are in the same projection
GB_strata <- GB_strata %>% st_transform(crs = 4326)
MAB_strata <- MAB_strata %>% st_transform(crs = 4326)
nefsc_shellfish <- nefsc_shellfish %>% st_transform(crs = 4326)
coastline <- coastline %>% st_transform(crs = 4326)
# make a single SAMS shapefile that includes the Mid-Atlantic and Georges Bank
SAMS <- rbind(GB_strata, MAB_strata)

# Read in data files

data_dir <- if (dir.exists("ScallopShinyApp/Data")) {
  "ScallopShinyApp/Data"
} else if (dir.exists("Data")) {
  "Data"
} else {
  here::here("Data")
}

files <- list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE)

file_groups <- split(files, sub("_.*", "", basename(files)))

iwalk(file_groups, function(file_paths, obj_name) {
  combined_df <- map_dfr(file_paths, read_csv, show_col_types = FALSE)
  assign(obj_name, combined_df, envir = .GlobalEnv)
})

# Filter out records from 2023 because the survey was cancelled
catch <- catch %>% filter(EST_YEAR %ni% 2023)
length <- length %>% filter(EST_YEAR %ni% 2023)

# Make spatial versions of length and catch tables for maps
# also joins the data to the SAMS shapefiles for future filtering
catch_sf <- catch %>%
  st_as_sf(coords = c("DECDEG_BEGLON", "DECDEG_BEGLAT"),
           crs = 4326) %>% 
  ## arrange so that larger catches draw on top
  arrange(desc(EXPCATCHNUM)) %>%
  ## joins the shapefiles and uses nearest feature to ensure each point is only linked to a single SAMS/strata
  st_join(SAMS, join = st_nearest_feature) %>%
  st_join(nefsc_shellfish, join = st_nearest_feature) %>% 
  ## for any point outside of the SAMS, flag that it is beyond the boundary
  mutate(NewSAMS = if_else(is.na(NewSAMS), "Beyond SAMS Boundary", NewSAMS)) %>% 
  ## remove any points that intersect with the coastline
  filter(lengths(st_intersects(., coastline)) == 0) %>% 
  distinct()


length_sf <- length %>%
  st_as_sf(coords = c("DECDEG_BEGLON", "DECDEG_BEGLAT"),
           crs = 4326) %>%
  ## joins the shapefiles and uses nearest feature to ensure each point is only linked to a single SAMS/strata
  st_join(SAMS, join = st_nearest_feature) %>%
  st_join(nefsc_shellfish, join = st_nearest_feature) %>% 
  ## for any point outside of the SAMS, flag that it is beyond the boundary
  mutate(NewSAMS = if_else(is.na(NewSAMS), "Beyond SAMS Boundary", NewSAMS)) %>% 
  ## remove any points that intersect with the coastline
  filter(lengths(st_intersects(., coastline)) == 0) %>% 
  distinct()

station_sf <- station %>%
  st_as_sf(coords = c("DECDEG_BEGLON", "DECDEG_BEGLAT"),
           crs = 4326) %>% 
  ## remove any points that intersect with the coastline
  filter(lengths(st_intersects(., coastline)) == 0) %>% 
  distinct()

# Sets a color palette for vessel names to make sure the color stays consistent and that each vessel has a unique color. 
# This should also support the addition of multiple new vessels
get_vessel_colors <- function(dataset, vessel_col = "VESSEL_NAME") {
  all_vessels <- sort(unique(dataset[[vessel_col]]))
  
  colors <- c(
    "o33white" = "#54ADDB",
    "lightpurple" = "#7171FF",
    "warmred" = "#FF4438",
    "S33white" = "#A2D269",
    "U50black" = "#28225C",
    "CO50black" = "#681617",
    "CR33white" = "#FFAC54",
    "w33white" = "#6DDBE1",
    "w30black" = "#005761",
    "w50black" = "#003A40",
    "S66white" = "#CEE8B1",
    "S33white" = "#A2D269",
    "S30black" = "#234515",
    "U33white" = "#A2A2FF",
    "U30black" = "#3D348A",
    "CR30black" = "#9F4700",
    "CO33white" = "#FF827A",
    "CO30black" = "#9C2122",
    "lightteal" = "#1ECAD3",
    "lightpurple" = "#7171FF",
    "warmred" = "#FF4438"
  )
  
  setNames(colors, all_vessels)
}

# App text is pulled from an R markdown file. This function calls the markdown in
# and isolates the text based on the appropriate header.
app_text <- function(rmd_path) {
  lines <- readLines(rmd_path, warn = FALSE)
  
  text_list <- list()
  current_header <- NULL
  current_block <- c()
  
  for (line in lines) {
    ## collects each line starting with "##" in the markdown as the section header
    if (startsWith(line, "##")) { 
      if (!is.null(current_header)) {
        text_list[[current_header]] <- HTML(markdownToHTML(
          text = paste(current_block, collapse = "\n"),
          fragment.only = TRUE
        ))
      }
      current_header <- trimws(sub("## ", "", line))
      current_block <- c()
    } else if (!is.null(current_header)) {
      if (!startsWith(line, "---") &&
          !startsWith(line, "title:") && !startsWith(line, "output:")) {
        current_block <- c(current_block, line)
      }
    }
  }
  if (!is.null(current_header)) {
    text_list[[current_header]] <- HTML(markdownToHTML(
      text = paste(current_block, collapse = "\n"),
      fragment.only = TRUE
    ))
  }
  return(text_list)
}

text <- app_text(here("ScallopShinyApp/AppText.Rmd"))


# a function to build a caption that will render displaying the plot selections
cap_input <- function(val) {
  if (is.null(val) || length(val) == 0 || all(val == "")) return("All")
  paste(val, collapse = ", ")
}

build_download_caption <- function(inputs_list) {
  details <- sapply(names(inputs_list), function(name) {
    paste0(name, ": ", cap_input(inputs_list[[name]]))
  })
  paste0("Northeast Fisheries Science Center, 2026: Sea Scallop Survey, https://www.fisheries.noaa.gov/inport/item/22564.", 
         "\nFilters Applied - ", paste(details, collapse = ", "), "\nExported on: ", Sys.Date(),
         "\nUse Constraints:",
         "\nUser must read and fully comprehend the metadata prior to use. Applications or inferences derived from the data should be carefully considered for accuracy. 
Acknowledgement of NOAA/NMFS/NEFSC, as the source from which these data were obtained in any publications and/or other representations of these, data is suggested.")
}

caption_theme <- theme(
  plot.caption = element_text(
    family = "sans",
    size = 10,
    face = "italic",
    hjust = 0,
    color = "black",
    margin = margin(t = 10)
  )
)

################################################################################
#################################### UI ########################################
################################################################################
# UI defines the visuals, buttons, text, etc. ALl changes to the app structure will happen in this section
ui <- fluidPage(
  includeCSS(here("ScallopShinyApp/www/style.css")), # calls in the css file for styling
  div( # defines the header using code in the CSS file, the logo is called here.
    class = "app-header",
    tags$img(src = "NMFS_logo.png", class = "app-logo"),
    div(class = "app-title-group",
        h2("Northeast Fisheries Science Center", class = "app-subtitle"),
        h1("Atlantic Sea Scallop Survey Data Dashboard", class = "app-title"),
    )),
  page_navbar(
    ## Intro and Methods Tab ##
    nav_panel(title = "Introduction", card(
      card_header("Welcome to the NEFSC Scallop Survey Data Dashboard!"),
      text$Welcome_Intro # calls the text from the text markdown file
    )),
    nav_spacer(), # adds a space between the panel headers
    
    ## Catch/Landings Data ##
    nav_panel(
      title = "Catch Data",
      #Plot 1
      card(
        card_header("Average Number and Weight of Scallops Caught per Tow"),
        card_body(layout_sidebar(
          sidebar = sidebar(
            # creates a dropdown selector for the regions
            selectizeInput( 
              "plot_1_region_selectbox", ## name of the dropdown which is used for filtering in the server section
              "Select Sampling Region:", ## label of the dropdown that will appear in the app
              choices = c("Mid-Atlantic", "New England"), ## dropdown options
              selected = "New England" ## default selection
            ),
            # creates a dropdown selector for the station types
            selectizeInput(
              "plot_1_selectbox",
              "Select Station Type:",
              choices = c("All Stations", unique(catch$Station_Type)), ## uses all the station types in the data and an option for all stations
              selected = "Random Stations"
            ),
            # creates a slider for the year range 
            sliderInput(
              "plot_1_date_slider",
              "Select Year Range:",
              min = min(catch$EST_YEAR), ## automatically pulls the earliest year in the data
              max = max(catch$EST_YEAR), ## automatically pulls the most recent year of data
              value = c(min(catch$EST_YEAR), max(catch$EST_YEAR)), ## sets the start and end pf the slider to match years in the data
              sep = "",
              step = 1 ## 1 year increments
            ),
            downloadButton("download_plot1", "Download Figure", class = "btn-primary btn-sm")
          ),
          plotOutput("plot_1", height = "500px") ## defines the plot to render and the desired size
        )),
        # secondary body beneath the plot to call on the plot caption in the text markdown file
        card_body(fill = FALSE, gap = 0, text$plot_1_caption),
        # allows the user to make the card full screen
        full_screen = TRUE
      ),
      #Plots 2a and 2b were made dynamic so that users can toggle between the two
      #this also increases the amount of space available to each plot
      card(
        card_header(
          # the conditional panels change the plot, caption, etc. based on the select plot (count vs. weight)
          conditionalPanel(
            condition = "input.plot_2_ab_toggle == 'plot_2a'",
            "Average Number of Scallops Caught per Tow in SAMS Areas"
          ),
          conditionalPanel(
            condition = "input.plot_2_ab_toggle == 'plot_2b'",
            "Average Weight of Scallops Caught per Tow in SAMS Areas"
          ),
        ),
        card_body(
          # allows the user to choose which plot they are interested in.
          # the app will automatically swap between headers, plots, etc. when the selection is changed
          radioButtons(
            inputId = "plot_2_ab_toggle",
            label = "Select Plot:",
            choices = c("Scallop Count" = "plot_2a", "Total Scallop Weight" = "plot_2b"),
            inline = TRUE
          )
        ),
        #Plot 2
        card_body(
          layout_sidebar(
            # both plots use the same sidebar/selection options so they will automatically show the same data
            sidebar = sidebar(
              # creates a dropdown selector for the SAMS areas
              selectizeInput(
                "plot_2_selectbox",
                "Select Management Area:",
                choices = unique(catch_sf$NewSAMS),
                selected = "GSC"
              ),
              # creates a slider bar for the year range
              sliderInput(
                "plot_2_date_slider",
                "Select Year Range:",
                min = min(catch$EST_YEAR),
                max = max(catch$EST_YEAR),
                value = c(min(catch$EST_YEAR), max(catch$EST_YEAR)),
                sep = "",
                step = 1
              ),
              # ensures that the download button is associated with the active plot
              conditionalPanel(
                condition = "input.plot_2_ab_toggle == 'plot_2a'",
                downloadButton("download_plot2a", "Download Figure", class = "btn-primary btn-sm w-100")
              ),
              conditionalPanel(
                condition = "input.plot_2_ab_toggle == 'plot_2b'",
                downloadButton("download_plot2b", "Download Figure", class = "btn-primary btn-sm w-100")
              )
            ),
            # picks which plot to render based on above selections
            conditionalPanel(condition = "input.plot_2_ab_toggle == 'plot_2a'", plotOutput("plot_2a")),
            conditionalPanel(condition = "input.plot_2_ab_toggle == 'plot_2b'", plotOutput("plot_2b"))
          )
        ),
        
        #Plot 2a and 2b captions
        card_body(
          fill = FALSE,
          gap = 0,
          conditionalPanel(condition = "input.plot_2_ab_toggle == 'plot_2a'", text$plot_2a_caption),
          conditionalPanel(condition = "input.plot_2_ab_toggle == 'plot_2b'", text$plot_2b_caption)
        ),
        full_screen = TRUE
      ),
      
      #Plot 4
      card(
        card_header("Abundance of Scallops per Station"),
        card_body(layout_sidebar(
          sidebar = sidebar(
            # creates a checkbox to select regions of interest
            checkboxGroupInput(
              "plot_4_checkbox",
              "Select Sampling Region:",
              choices = sort(unique(catch_sf$REGION), decreasing = TRUE),
              selected = unique(catch_sf$REGION)
            ),
            # creates a dropdown selector for the station types
            selectizeInput(
              "plot_4_station_selectbox",
              "Select Station Type:",
              choices = c("All Stations", unique(catch$Station_Type)),
              selected = "Random Stations"
            ),
            # creates a dropdown selector for year, this only allows one year of data to be shown at a time
            selectizeInput(
              "plot_4_date_selectbox",
              "Select Year:",
              ## makes sure that every year between the min and max is in the dropdown, and is in the correct order
              choices = sort(seq(min(catch_sf$EST_YEAR), max(catch_sf$EST_YEAR)), decreasing = TRUE),
              ## defaults to the most recent year of data
              selected = max(catch_sf$EST_YEAR)
            ),
            # creates a dropdown selector to change between the strata maps
            selectizeInput(
              "plot_4_strata_selectbox",
              "Select Strata Map:",
              choices = c("SAMS" = "SAMS", "NEFSC Shellfish" = "nefsc_shellfish"),
              ## defaults to the SAMS areas
              selected = "SAMS"
            ),
            downloadButton("download_plot4", "Download Figure", class = "btn-primary btn-sm")
          ),
          plotOutput("plot_4", height = "800px", width = "100%")
        )),
        card_body(fill = FALSE, gap = 0, text$plot_4_caption),
        full_screen = TRUE
      )
    ),
    nav_spacer(),
    
    ## Size Data ##
    nav_panel(
      title = "Size Data",
      #Plot 6
      card(
        card_header("Abundance of Different Size Ranges of Scallops per Station"),
        card_body(layout_sidebar(
          sidebar = sidebar(
            # creates a checkbox selector for the sampling regions
            checkboxGroupInput(
              "plot_6_checkbox",
              "Select Sampling Region:",
              choices = sort(unique(length_sf$REGION), decreasing = TRUE),
              selected = unique(length_sf$REGION)
            ),
            # creates a dropdown selector for the station types
            selectizeInput(
              "plot_6_station_selectbox",
              "Select Station Type:",
              choices = c("All Stations", unique(catch$Station_Type)),
              selected = "Random Stations"
            ),
            # creates a dropdown selector for the different size categories 
            selectizeInput(
              "plot_6_size_selectbox",
              "Select Size Range (cm):",
              choices = intersect(order, unique(length_sf$Size_Cat)),
              selected = "< 3.5cm"
            ),
            # creates a dropdown selector for the year
            selectizeInput(
              "plot_6_date_selectbox",
              "Select Year:",
              choices = sort(seq(min(length_sf$EST_YEAR), max(length_sf$EST_YEAR)), decreasing = TRUE),
              selected = max(length_sf$EST_YEAR)
            ),
            # creates a dropdown selector for the strata
            selectizeInput(
              "plot_6_strata_selectbox",
              "Select Strata Map:",
              choices = c("SAMS" = "SAMS", "NEFSC Shellfish" = "nefsc_shellfish"),
              selected = "SAMS"
            ),
            downloadButton("download_plot6", "Download Figure", class = "btn-primary btn-sm")
          ),
          plotOutput("plot_6", width = "100%", height = "800px")
        )),
        card_body(fill = FALSE, gap = 0, text$plot_6_caption),
        full_screen = TRUE
      ),
      
      #Plot 7
      card(
        card_header("Median Number of Scallops Caught per Size Range, per Tow"),
        card_body(layout_sidebar(
          sidebar = sidebar(
            # creates a dropdown selector for the sampling regions
            selectizeInput(
              "plot_7_region_selectbox",
              "Select Sampling Region:",
              choices = c("Mid-Atlantic", "New England"),
              selected = "New England"
            ),
            # creates a dropdown selector for the station types
            selectizeInput(
              "plot_7_station_selectbox",
              "Select Station Type:",
              choices = c("All Stations", unique(catch$Station_Type)),
              selected = "Random Stations"
            ),
            # creates a slider for selecting a year range
            sliderInput(
              "plot_7_date_slider",
              "Select Year Range:",
              min = min(length$EST_YEAR),
              max = max(length$EST_YEAR),
              value = c(min(length$EST_YEAR), max(length$EST_YEAR)),
              sep = "",
              step = 1
            ),
            downloadButton("download_plot7", "Download Figure", class = "btn-primary btn-sm")
          ),
          plotOutput("plot_7")
        )),
        card_body(fill = FALSE, gap = 0, text$plot_7_caption),
        full_screen = TRUE
      ),
      
      #Plot 8
      card(
        card_header("Size of Scallops Caught in the Dredge Survey per Year"),
        layout_sidebar(
          sidebar = sidebar(
            # creates a dropdown selector for sampling region
            selectizeInput(
              "plot_8_region_selectbox",
              "Select Sampling Region:",
              choices = c("Mid-Atlantic", "New England"),
              selected = "New England"
            ),
            # creates a dropdown selector for SAMS area, this automatically updates depending on the selected sampling region
            selectizeInput(
              "plot_8_SAMS_selectbox",
              "Select Management Area:",
              choices = NULL
            ),
            # creates a dropdown selector for station type
            selectizeInput(
              "plot_8_station_selectbox",
              "Select Station Type:",
              choices = c("All Stations", unique(catch$Station_Type)),
              selected = "Random Stations"
            ),
            # creates a slider to select date ranges
            sliderInput(
              "plot_8_date_slider",
              "Select Year Range:",
              min = min(length$EST_YEAR),
              max = max(length$EST_YEAR),
              value = c(min(length$EST_YEAR), max(length$EST_YEAR)),
              sep = "",
              step = 1
            ),
            downloadButton("download_plot8", "Download Figure", class = "btn-primary btn-sm")
          ),
          plotOutput("plot_8", height = "800px")
        ),
        card_body(fill = FALSE, gap = 0, text$plot_8_caption),
        full_screen = TRUE
      )
    ),
    nav_spacer(),
    ## Biological Data ##
    nav_panel(
      title = "Biological Data",
      card(
        card_header("Coming Soon!"),
        layout_sidebar(sidebar = sidebar(), text$plot_9_caption),
        full_screen = TRUE
      ),
      card(card_header("Coming Soon!"))
    ),
    nav_spacer(),
    nav_panel(
      title = "Methods",
      card(
        card_header("Survey Background and Methods"),
        card_body(text$Methods_Background)
      ),
      #Plot 5
      card(
        card_header("Success Metrics"),
        card_body(
          plotOutput("plot_5"),
          card_body(fill = FALSE, gap = 0, text$plot_5_caption)
        )
      )
    )
  )
)


################################################################################
################################### SERVER #####################################
################################################################################

# Define server logic required to draw plots - essentially controls the visuals
server <- function(input, output, session) {
  plot_1 <- reactive({
    # make all years available in the slider
    all_years <- seq(input$plot_1_date_slider[1], input$plot_1_date_slider[2])
    # filter data to match user selections
    plot_1_data <- catch %>%
      filter(
        ## these input names match to the ID listed in the UI definition 
        EST_YEAR >= input$plot_1_date_slider[1] & 
          EST_YEAR <= input$plot_1_date_slider[2],
        REGION %in% input$plot_1_region_selectbox,
        (input$plot_1_selectbox == "All Stations" |
            Station_Type == input$plot_1_selectbox)
      ) %>%
      # summarize the data to find a mean and standard error catch/weight for each year
      group_by(EST_YEAR) %>%
      summarize(
        mean_catch = mean(EXPCATCHNUM),
        se_catch   = sd(EXPCATCHNUM) / sqrt(n()),
        mean_weight = mean(EXPCATCHWT, na.rm = TRUE),
        se_weight   = sd(EXPCATCHWT, na.rm = TRUE) / sqrt(n())
      ) %>%
      ungroup() %>%
      ## set all weight values collected before 2001 to NA since weight was not consistently recorded pre-FSCS 
      mutate(
        mean_weight = if_else(EST_YEAR >= 2001, mean_weight, NA),
        se_weight   = if_else(EST_YEAR >= 2001, se_weight, NA)
      ) %>% 
      ## ensures all years exist in a given year range, even if there is no data
      ## this preserves gaps in the plot for years that don't exist in the data (2020 and 2023)
      complete(EST_YEAR = all_years)
    
    shiny::validate(
      need(
        nrow(plot_1_data) > 0, 
        "No data available for the current selections.")) ## adds a popup if no data matches the selections
    
    # set a standard theme because this uses two stacked plots
    theme_shared <- theme_minimal() +
      theme(
        text = element_text(family = "sans", size = 16),
        axis.text.x = element_text(family = "sans", angle = 45, hjust = 1),
        axis.text.y = element_text(family = "sans", size = 16),
        axis.title = element_text(family = "sans"),
        plot.title = element_text(family = "sans", size = 16, hjust = 0.5)
      )
    
    p_catch <- ggplot( ## creates a plot for the catch data
      plot_1_data, 
      aes(x = factor(EST_YEAR, levels = all_years), y = mean_catch)) +
      geom_text(
        aes(y = 0, label = if_else(is.na(mean_catch), "ND", "")),
        vjust = -0.5, color = "grey40", fontface = "bold", size = 4) +
      geom_col(fill = "#FF8400", alpha = 0.9) +
      geom_errorbar(
        aes(ymin = mean_catch - se_catch, ymax = mean_catch + se_catch),
        width = 0.3, color = "black", linewidth = 0.6) +
      scale_x_discrete(drop = FALSE) +
      labs(
        title = "Mean Catch per Tow",
        x = NULL,                           
        y = "Mean Number per Tow ± SE") +
      theme_shared
    
    
    p_weight <- ggplot( ## creates a plot for weight data
      plot_1_data, 
      aes(x = factor(EST_YEAR, levels = all_years), y = mean_weight)) +
      geom_text(
        aes(y = 0, label = if_else(is.na(mean_weight), "ND", "")),
        vjust = -0.5, color = "grey40", fontface = "bold", size = 4) +
      geom_col(fill = "#737BE6", alpha = 0.9) +
      geom_errorbar(
        aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight),
        width = 0.3, color = "black", linewidth = 0.6) +
      scale_x_discrete(drop = FALSE) +
      labs(
        title = "Mean Basket Weight per Tow",
        x = "Year",                        
        y = "Mean Basket Weight (kg) per Tow ± SE") +
      theme_shared
    
    # stack the plots
    cowplot::plot_grid(p_catch, p_weight, ncol = 1, align = "v")
    
  })
  
  output$plot_1 <- renderPlot({
    plot_1() # renders the plot so it will appear when called upon by the UI
  })
  
  output$download_plot1 <- downloadHandler(
    filename = function() {
      paste0("Mean_Number_Scallops_Caught_per_Tow_", Sys.Date(), ".png")
    },
    content = function(file) {

      cap <- build_download_caption(list(
        "Region"       = input$plot_1_region_selectbox,
        "Station Type" = input$plot_1_selectbox,
        "Years"        = paste(input$plot_1_date_slider, collapse = "-")
      ))
      
      plot_export <- patchwork::wrap_elements(plot_1()) + 
        patchwork::plot_annotation(
          caption = cap,
          theme = theme(
            plot.caption = element_text(
              family = "sans",
              size = 11,
              face = "italic",
              color = "black",
              hjust = 0,
              margin = margin(t = 10)
            )
          )
        )
      
      ggsave(file, plot = plot_export, width = 18, height = 12, dpi = 300)
    }
  )
  
  plot_2a <- reactive({
    # make all years available in the slider
    all_years <- seq(input$plot_2_date_slider[1], input$plot_2_date_slider[2])
    
    plot_2a_data <- catch_sf %>%
      filter(EST_YEAR >= input$plot_2_date_slider[1] &
               EST_YEAR <= input$plot_2_date_slider[2])
    
    if (!is.null(input$plot_2_selectbox) &&
        length(input$plot_2_selectbox) > 0 &&
        any(input$plot_2_selectbox != "")) {
      plot_2a_data <- plot_2a_data %>%
        filter(NewSAMS == input$plot_2_selectbox)
    }
    
    shiny::validate(
      need(
        nrow(plot_2a_data) > 0, 
        "No data available for the current selections."
      )
    )
    
    plot_2a_data <- plot_2a_data %>%
      group_by(EST_YEAR) %>%
      summarize(
        mean_catch = mean(EXPCATCHNUM, na.rm = TRUE),
        se = sd(EXPCATCHNUM, na.rm = TRUE) / sqrt(n())
      ) %>%
      ungroup() %>% 
      complete(EST_YEAR = all_years)
    
    ggplot(data = plot_2a_data, aes(x = factor(EST_YEAR), y = mean_catch)) +
      geom_text(
        data = plot_2a_data, 
        aes(
          x = factor(EST_YEAR, levels = all_years), 
          y = 0,            
          label = if_else(is.na(mean_catch), "ND", "")
        ),
        vjust = -0.5, 
        color = "grey40",   
        fontface = "bold", 
        size = 4, 
        inherit.aes = FALSE 
      ) +
      geom_col(fill = "#FF8400") +
      geom_errorbar(aes(
        ymin = (mean_catch - se),
        ymax = (mean_catch + se)
      )) +
      labs(x = "Year", y = "Mean Number of Individuals") +
      theme_minimal() +
      theme(
        text = element_text(family = "sans", size = 16),
        axis.text.x = element_text(
          family = "sans",
          angle = 45,
          hjust = 1
        ),
        axis.text.y = element_text(family = "sans"),
        axis.title = element_text(family = "sans")
      )
  })
  
  output$plot_2a <- renderPlot({
    plot_2a()
  })
  
  output$download_plot2a <- downloadHandler(
    filename = function() {
      paste0("Mean_Number_Scallops_Caught_per_Tow_by_SAMS_", Sys.Date(), ".png")
    },
    content = function(file) {
      cap <- build_download_caption(list(
        "Management Area" = input$plot_2_selectbox,
        "Years" = paste(input$plot_2_date_slider, collapse = "-")
      ))
      
      plot_export <- plot_2a() + labs(caption = cap) + caption_theme
      
      ggsave(file, plot = plot_export, width = 14, height = 8, dpi = 300)
    }
  )
  
  plot_2b <- reactive({
    
    all_years <- seq(input$plot_2_date_slider[1], input$plot_2_date_slider[2])
    
    plot_2b_data <- catch_sf %>%
      filter(EST_YEAR >= input$plot_2_date_slider[1] &
               EST_YEAR <= input$plot_2_date_slider[2])
    
    if (!is.null(input$plot_2_selectbox) &&
        length(input$plot_2_selectbox) > 0 &&
        any(input$plot_2_selectbox != "")) {
      plot_2b_data <- plot_2b_data %>%
        filter(NewSAMS == input$plot_2_selectbox)
    }
    
    shiny::validate(
      need(
        nrow(plot_2b_data) > 0, 
        "No data available for the current selections."
      )
    )
    
    plot_2b_data <- plot_2b_data %>%
      group_by(EST_YEAR) %>%
      summarize(
        mean_weight = mean(EXPCATCHWT, na.rm = TRUE),
        se = sd(EXPCATCHWT, na.rm = TRUE) / sqrt(n())
      ) %>%
      ungroup() %>%
      # omit data from pre-2001 since it wasn't standard collection
      mutate(
        mean_weight = if_else(EST_YEAR >= 2001, mean_weight, NA),
        se = if_else(EST_YEAR >= 2001, se, NA)
      ) %>% 
      complete(EST_YEAR = all_years) 
    
    
    ggplot(data = plot_2b_data, aes(x = factor(EST_YEAR), y = mean_weight)) +
      geom_text(
        data = plot_2b_data, 
        aes(
          x = factor(EST_YEAR, levels = all_years), 
          y = 0,            
          label = if_else(is.na(mean_weight), "ND", "")
        ),
        vjust = -0.5, 
        color = "grey40",   
        fontface = "bold", 
        size = 4, 
        inherit.aes = FALSE 
      )+
      geom_col(fill = "#737BE6") +
      geom_errorbar(aes(
        ymin = (mean_weight - se),
        ymax = (mean_weight + se)
      )) +
      labs(x = "Year", y = "Mean Catch Weight (kg)") +
      theme_minimal() +
      theme(
        text = element_text(family = "sans", size = 16),
        axis.text.x = element_text(
          family = "sans",
          angle = 45,
          hjust = 1
        ),
        axis.text.y = element_text(family = "sans"),
        axis.title = element_text(family = "sans")
      )
  })
  
  output$plot_2b <- renderPlot({
    plot_2b()
  })
  
  output$download_plot2b <- downloadHandler(
    filename = function() {
      paste0("Mean_Weight_Scallops_Caught_per_Tow_by_SAMS_", Sys.Date(), ".png")
    },
    content = function(file) {
      cap <- build_download_caption(list(
        "Management Area" = input$plot_2_selectbox,
        "Years" = paste(input$plot_2_date_slider, collapse = "-")
      ))
      
      plot_export <- plot_2b() + labs(caption = cap) + caption_theme
      
      ggsave(file, plot = plot_export, width = 14, height = 8, dpi = 300)
    }
  )
  
  plot_4 <- reactive({
    shiny::validate(
      need( # ensure that a region is checked in order to view the plot
        input$plot_4_checkbox != "",
        "No region selected. Please choose a region to view the map."
      )
    )
    
    vessel_colors <- get_vessel_colors(catch_sf, "VESSEL_NAME") # calls on the vessel color function to define colors for each vessel
    
    plot_4_data <- catch_sf %>%
      filter(
        # filter data to match button selections
        EST_YEAR == input$plot_4_date_selectbox,
        REGION %in% input$plot_4_checkbox,
        (
          input$plot_4_station_selectbox == "All Stations" |
            Station_Type == input$plot_4_station_selectbox
        )
      )
    
    if (!is.null(input$plot_4_checkbox) &&
        length(input$plot_4_checkbox) > 0 &&
        any(input$plot_4_checkbox != "")) {
      plot_4_data <- plot_4_data %>%
        filter(REGION %in% input$plot_4_checkbox)
    }
    
    # creates a layer of stations that had no recorded scallop catch
    zero_catch <- station_sf %>%
      filter(EST_YEAR == input$plot_4_date_selectbox,
             ID %ni% catch_sf$ID) %>% 
      mutate(catch_number = "No Scallops Caught")
    
    shiny::validate(
      need(
        nrow(plot_4_data) > 0, 
        "No data available for the current selections."
      )
    )
    
    # sets bounding boxes for the three different region views
    if (all(c("Mid-Atlantic", "New England") %in% input$plot_4_checkbox)) {
      x_lims <- c(-76.5, -65.8)
      y_lims <- c(35.4, 42.25)
    } else if ("Mid-Atlantic" %in% input$plot_4_checkbox) {
      x_lims <- c(-76.5, -71.2)
      y_lims <- c(35.4, 41.2)
    } else if ("New England" %in% input$plot_4_checkbox) {
      x_lims <-  c(-70.5, -65.8)
      y_lims <- c(40.1, 42.25)
    }
    
    # creates the basemap
    plot4 <- ggplot() +
      geom_sf(
        data = coastline,
        fill = "#D0D0D0",
        color = "#9A9A9A",
        linewidth = 0.5
      )
    
    # draws SAMS depending on the strata selection
    if ("SAMS" %in% input$plot_4_strata_selectbox) {
      plot4 <- plot4 +
        geom_sf(
          data = SAMS,
          fill = NA,
          color = "gray20",
          linewidth = 0.5
        )
      
    }
    # draws strata map depending on strata selection
    if ("nefsc_shellfish" %in% input$plot_4_strata_selectbox) {
      plot4 <- plot4 + geom_sf(
        data = nefsc_shellfish,
        fill = NA,
        color = "gray20",
        linewidth = 0.5
      ) 
    }
    
    plot4 <- plot4 +
      geom_sf(
        data = plot_4_data,
        aes(size = EXPCATCHNUM, 
            fill = VESSEL_NAME),
        shape = 21,
        alpha = 0.6
      ) +
      scale_fill_manual(values =  vessel_colors) +
      scale_size_continuous( ## sets the scale for the bubbles
        range = c(3, 15),
        breaks = c(0, 10, 100, 1000, 10000, 20000),
        limits = c(min(catch_sf$EXPCATCHNUM), max(catch_sf$EXPCATCHNUM))
      ) +
      geom_sf(data = zero_catch, aes(shape = factor(catch_number)), size = 2) +
      scale_shape_manual(name = NULL, values = c("No Scallops Caught" = 4)) +
      guides(
        size = guide_legend(order = 1), # requires the bubbles to sit on top of the legend
        shape = guide_legend(order = 2) 
      ) +
      labs(x = "Longitude", y = "Latitude", size = "Number of Scallops", fill = "Vessel Name") +
      coord_sf(xlim = x_lims,
               ylim = y_lims,
               expand = FALSE) +
      theme_minimal() +
      theme(
        text = element_text(family = "sans", size = 16),
        axis.text.x = element_text(
          family = "sans",
          angle = 45,
          hjust = 1
        ),
        axis.text.y = element_text(family = "sans"),
        axis.title = element_text(family = "sans"),
        panel.grid.major = element_blank(),
        legend.box = "vertical",          
        legend.box.just = "left",        
        legend.key = element_blank()
      )
    
    plot4 # IMPORTANT - this is needed to make sure the plot actually draws
    
  })
  
  output$plot_4 <- renderPlot({
    plot_4()
  })
  
  output$download_plot4 <- downloadHandler(
    filename = function() {
      paste0("Number_Scallops_Caught_per_Station_", Sys.Date(), ".png")
    },
    content = function(file) {
      cap <- build_download_caption(list(
        "Region" = input$plot_4_checkbox,
        "Station Type" = input$plot_4_station_selectbox,
        "Year" = input$plot_4_date_selectbox,
        "Strata Map" = input$plot_4_strata_selectbox
      ))
      
      plot_export <- plot_4() + labs(caption = cap) + caption_theme
      
      ggsave(file, plot = plot_export, width = 14, height = 8, dpi = 300)
    }
  )
  
  output$plot_5 <- renderPlot({
    ggplot(data = station, aes(x = EST_YEAR, fill = REGION)) +
      geom_bar(stat = "count", position = "stack") +
      labs(x = "Year", y = "Number of Stations") +
      scale_fill_discrete(palette = c("#0085CA", "#001743")) +
      labs(fill = "Region") +
      theme_minimal() +
      theme(
        text = element_text(family = "sans", size = 16),
        axis.text.x = element_text(
          family = "sans",
          angle = 45,
          hjust = 1
        ),
        axis.text.y = element_text(family = "sans"),
        axis.title = element_text(family = "sans")
      )
  })
  
  plot_6 <- reactive({
    shiny::validate(need( # ensure that a region is checked in order to view the plot
      !is.null(input$plot_6_checkbox) && any(input$plot_6_checkbox != ""),
      "No region selected. Please choose a region to view the map."
    ))
    
    plot_6_data <- length_sf %>%
      filter(
        # filter to data based on the button selections
        REGION %in% input$plot_6_checkbox,
        EST_YEAR == input$plot_6_date_selectbox,
        Size_Cat == input$plot_6_size_selectbox,
        (
          input$plot_6_station_selectbox == "All Stations" |
            Station_Type == input$plot_6_station_selectbox
        )
      ) %>%
      arrange(desc(EXPNUMLEN)) %>% # this helps make the larger values draw on top
      group_by(CRUISE6, STATION, Size_Cat) %>%
      summarize(sum_expnumlen = sum(EXPNUMLEN)) %>%
      ungroup()
    
    shiny::validate(
      need(
        nrow(plot_6_data) > 0, 
        "No data available for the current selections."
      )
    )
    
    # set the bounding boxes for the regional views
    if (all(c("Mid-Atlantic", "New England") %in% input$plot_6_checkbox)) {
      x_lims <- c(-76.5, -65.8)
      y_lims <- c(35.4, 42.25)
    } else if ("Mid-Atlantic" %in% input$plot_6_checkbox) {
      x_lims <- c(-76.5, -71.2)
      y_lims <- c(35.4, 41.2)
    } else if ("New England" %in% input$plot_6_checkbox) {
      x_lims <-  c(-70.5, -65.8)
      y_lims <- c(40.1, 42.25)
    }
    
    # create the basemap
    plot6 <- ggplot() +
      geom_sf(
        data = coastline,
        fill = "#D0D0D0",
        color = "#9A9A9A",
        linewidth = 0.5
      )
    
    # draw SAMS strata based on strata selection
    if ("SAMS" %in% input$plot_6_strata_selectbox) {
      plot6 <- plot6 + geom_sf(
        data = SAMS,
        fill = NA,
        color = "gray20",
        linewidth = 0.5
      )
    }
    
    # draw strata based on strata selection
    if ("nefsc_shellfish" %in% input$plot_6_strata_selectbox) {
      plot6 <- plot6 + geom_sf(
        data = nefsc_shellfish,
        fill = NA,
        color = "gray20",
        linewidth = 0.5
      )
    }
    
    plot6 <- plot6 +
      geom_sf(
        data = plot_6_data,
        aes(size = sum_expnumlen),
        fill = "#4B8320",
        shape = 21,
        alpha = 0.6
      ) +
      scale_size_continuous(
        range = c(3, 15),
        breaks = c(0, 10, 100, 1000, 10000),
        limits = c(0, 10000)
      ) +
      labs(x = "Longitude", y = "Latitude", size = "Number of Scallops") +
      coord_sf(xlim = x_lims,
               ylim = y_lims,
               expand = FALSE) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "sans", size = 16),
        axis.text.x = element_text(
          family = "sans",
          angle = 45,
          hjust = 1
        ),
        axis.text.y = element_text(family = "sans"),
        axis.title = element_text(family = "sans")
      )
    
    plot6
  })
  
  output$plot_6 <- renderPlot({
    plot_6()
  })
  
  output$download_plot6 <- downloadHandler(
    filename = function() {
      paste0("Number_Scallops_by_Size_per_Station_", Sys.Date(), ".png")
    },
    content = function(file) {
      cap <- build_download_caption(list(
        "Region" = input$plot_6_checkbox,
        "Station Type" = input$plot_6_station_selectbox,
        "Size Range" = input$plot_6_size_selectbox,
        "Year" = input$plot_6_date_selectbox,
        "Strata Map" = input$plot_6_strata_selectbox
      ))
      
      plot_export <- plot_6() + labs(caption = cap) + caption_theme
      
      ggsave(file, plot = plot_export, width = 14, height = 8, dpi = 300)
    }
  )
  
  plot_7 <- reactive({
   #filters data based on button selections
    plot_7_data <- length %>%
      filter(
        EST_YEAR >= input$plot_7_date_slider[1] &
          EST_YEAR <= input$plot_7_date_slider[2],
        REGION %in% input$plot_7_region_selectbox,
        (
          input$plot_7_station_selectbox == "All Stations" |
            Station_Type == input$plot_7_station_selectbox
        )
      )
    
    shiny::validate(
      need(
        nrow(plot_7_data) > 0, 
        "No data available for the current selections."
      )
    )
    
    ggplot(data = plot_7_data,
           aes(
             x = EST_YEAR,
             y = EXPNUMLEN,
             fill = factor(Size_Cat, levels = order), 
             group = factor(Size_Cat, levels = order) # calls on the pre-set order to make sure categories are stacked correctly
           )) +
      geom_area(position = "stack",
                stat = "summary",
                fun = "median") +
      labs(x = "Year", y = "Median of Scallops", fill = "Length (cm)", ) +
      scale_fill_discrete(palette = c("#003A40", "#005e5e", "#00797f", "#1EBEC7", "#90DFE3")) +
      theme_minimal() +
      theme(
        text = element_text(family = "sans", size = 16),
        axis.text.x = element_text(
          family = "sans",
          angle = 45,
          hjust = 1
        ),
        axis.text.y = element_text(family = "sans"),
        axis.title = element_text(family = "sans")
      )
  })
  
  output$plot_7 <- renderPlot({
    plot_7()
  })
  
  output$download_plot7 <- downloadHandler(
    filename = function() {
      paste0("Scallop_Size_Category_", Sys.Date(), ".png")
    },
    content = function(file) {
      cap <- build_download_caption(list(
        "Region" = input$plot_7_region_selectbox,
        "Station Type" = input$plot_7_station_selectbox,
        "Years" = paste(input$plot_7_date_slider, collapse = "-")
      ))
      
      plot_export <- plot_7() + labs(caption = cap) + caption_theme
      
      ggsave(file, plot = plot_export, width = 14, height = 8, dpi = 300)
    }
  )
  
  # defines which SAMS are associated with each region (New England vs. Mid-Atlantic)
  NE_SAMS  <- c("All Areas", unique(GB_strata$NewSAMS),"Beyond SAMS Boundary")
  MAB_SAMS <- c("All Areas", unique(MAB_strata$NewSAMS),"Beyond SAMS Boundary")
  
  # allows the SAMS selectbox to update based on which region is selected
  observeEvent(input$plot_8_region_selectbox, {
    req(input$plot_8_region_selectbox)
    
    if (input$plot_8_region_selectbox == "New England") {
      updateSelectizeInput(
        session,
        inputId = "plot_8_SAMS_selectbox",
        label = "Select Management Area:",
        choices = NE_SAMS,
        selected = "All Areas" 
      )
    } else if (input$plot_8_region_selectbox == "Mid-Atlantic") {
      updateSelectizeInput(
        session,
        inputId = "plot_8_SAMS_selectbox",
        label = "Select Management Area:",
        choices = MAB_SAMS,
        selected = "All Areas"  
      )
    }
  }, ignoreNULL = FALSE)
  
  plot_8 <- reactive({
    # requires the region and SAMS selectboxes to be rendered
    req(input$plot_8_region_selectbox, input$plot_8_SAMS_selectbox)
    
    plot_8_data <- length_sf %>%
      filter(
        EST_YEAR >= input$plot_8_date_slider[1] &
          EST_YEAR <= input$plot_8_date_slider[2],
        REGION %in% input$plot_8_region_selectbox,
        
        # if "All Areas" is chose, it will not filter by SAMS
        if (input$plot_8_SAMS_selectbox == "All Areas") TRUE else NewSAMS %in% input$plot_8_SAMS_selectbox,
        
        (
          input$plot_8_station_selectbox == "All Stations" |
            Station_Type == input$plot_8_station_selectbox
        )
      )
    
    shiny::validate(
      need(
        nrow(plot_8_data) > 0, 
        "No data available for the current selections."
      )
    )
    
    # finds the years, the midpoint, and splits the years selected in order to split the plot into two columns
    unique_years <- sort(unique(plot_8_data$EST_YEAR))
    mid_index <- ceiling(length(unique_years) / 2)
    split_year <- unique_years[mid_index]
    
    plot_8_data <- plot_8_data %>%
      mutate(Era = if_else(
        EST_YEAR <= split_year,
        paste0(min(unique_years), " - ", split_year),
        paste0(split_year + 1, " - ", max(unique_years))
      ))
    # defines the two columns and labels them according to the years available
    col1 <- paste0(split_year + 1, " - ", max(unique_years))
    col2 <- paste0(min(unique_years), " - ", split_year)
    # defines an "era" based on the split between years
    plot_8_data$Era <- factor(plot_8_data$Era, levels = c(col1, col2))
    
    ggplot(data = plot_8_data, aes(y = factor(EST_YEAR), x = LENGTH)) +
      ggridges::geom_density_ridges_gradient(
        scale = 0.8,
        rel_min_height = 0.01,
        color = "white",
        linewidth = 0.3,
        fill = "#002364"
      ) +
      facet_wrap( ~ Era, ncol = 2, scales = "free_y") + ## facet wrap based on the "Era"
      labs(x = "Length (cm)", y = "Year") +
      theme_minimal() +
      theme(
        text = element_text(family = "sans", size = 16),
        axis.text.x = element_text(
          family = "sans",
          angle = 45,
          hjust = 1
        ),
        axis.text.y = element_text(family = "sans"),
        axis.title = element_text(family = "sans")
      ) +
      coord_cartesian(xlim = c(0, 20))
  })
  
  output$plot_8 <- renderPlot({
    plot_8()
  })
  
  output$download_plot8 <- downloadHandler(
    filename = function() {
      paste0("Distribution_Scallop_Size_", Sys.Date(), ".png")
    },
    content = function(file) {
      cap <- build_download_caption(list(
        "Region" = input$plot_8_region_selectbox,
        "Management Area" = input$plot_8_SAMS_selectbox,
        "Station Type" = input$plot_8_station_selectbox,
        "Years" = paste(input$plot_8_date_slider, collapse = "-")
      ))
      
      plot_export <- plot_8() + labs(caption = cap) + caption_theme
      
      ggsave(file, plot = plot_export, width = 14, height = 8, dpi = 300)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
