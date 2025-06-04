if(!require(colourpicker)){
  install.packages("colourpicker")
}
if(!require(bslib)){
  install.packages("bslib")
}

library(colourpicker)
library(bslib)

# User Interface
ui <- page_navbar(
  # App title ----
  title = "Kinetic Lab for Y-axis (KLY)",
  id = "tabs",
  theme = bs_theme(bootswatch = "minty"),
  
  # panel for data and stats ----
  nav_panel(
    title = "Data & Statistics",
    layout_sidebar(
      sidebar = sidebar(
        # Input: upload the csv file
        fileInput("file1", "Choose CSV File", accept = ".csv"),
        # Input: display rows
        numericInput("nRow", "Display rows", value = 10),
        
        # Input: check normality
        checkboxInput("norm","Check data normality",FALSE),
        conditionalPanel(
          condition = "input.norm == true",
          selectInput('norm_dat',tags$span(style = "color:red;","select numeric data for testing*"), choices = NULL)
        ),
        
        # Input: perform statistics
        checkboxInput("stats_tbl","Perform statistics",FALSE),
        
        conditionalPanel(
          condition = "input.stats_tbl == true",
          # Input: select column for grouping 
          selectInput("stats_group",
                      label = tags$span(style = "color:red;","Group(categorical)*"),
                      choices = NULL),
          # Input: select column for testing
          selectInput("stats_value",
                      label = tags$span(style = "color:red;","y_axis(numeric)*"),
                      choices = NULL),
          selectInput("stats_subgroup",
                      label = tags$span(style = "color:black;","subgroup(categorical)"),
                      choices = NULL),
          tags$div("Select NULL if there is no subgroup", style = "font-size: 12px; color: black;"),
          
          # Input: statistic test methods
          selectInput("test_method", 
                      label = tags$span(style = "color:black;","Statistics test method"),
                      choices = c("","t test",
                                  "Mann-Whitney U test",
                                  "one-way-ANNOVA (Tukey)",
                                  "pairwise t test", 
                                  "pairwise Mann-Whitney U test",
                                  "two-way-ANNOVA (Tukey)"))
          
        ),
    
        # Input: adjusted method
        conditionalPanel(
          condition = "input.test_method == 'pairwise t test' || input.test_method == 'pairwise Mann-Whitney U test'",
          # Input: adjusted methods
          selectInput("adj_method",
                      label = tags$span(style = "color:red;","*Select adjusted method"),
                      choices = c("","none","holm","hochberg","hommel",
                                  "BH","BY","bonferroni","fdr"))
        ),
        
        # Input: adjusted method
        conditionalPanel(
          condition = "input.stats_subgroup != 'NULL' && input.test_method == 'two-way-ANNOVA (Tukey)'",
          selectInput("test_for", label = "test between", choices = c("group","subgroup"), selected = "subgroup"),
          checkboxInput("comb_var", label = "combine group and subgroup for testing",FALSE)
        ),
        # check box for selecting plot type
        radioButtons("plt_type",
                     "Select plot types", 
                     choices = c("Bar plot","Box-Violin plot",
                                 "Density-Histogram plot","Survival plot",
                                 "Scatter plot", "Heatmap plot"),
                     selected = character(0)),
        # conditional panel based on plot type
        # bar plot or box plot
        conditionalPanel(
          condition = "input.plt_type == 'Bar plot'||input.plt_type =='Box-Violin plot'",
          # Input: select column for grouping 
          selectInput("group",
                      label = tags$span(style = "color:red;","Group(categorical)"),
                      choices = NULL),
          # Input: select column for testing
          selectInput("value",
                      label = tags$span(style = "color:red;","y_axis(numeric)*"),
                      choices = NULL),
          checkboxInput("grouped","subgrouped", FALSE)),
        
        # Density plot
        conditionalPanel(
          condition = "input.plt_type == 'Density-Histogram plot'",
          selectInput("den_x_var",tags$span(style = "color:red;","x variable*"),choices = NULL),
          selectInput("den_group",tags$span(style = "color:red;","Group*"),choices = NULL),
          tags$div("Select NULL if there is only one group", style = "font-size: 12px; color: black;"),
        ),
        
        # Survival plot
        conditionalPanel(
          condition = "input.plt_type == 'Survival plot'",
          selectInput("sur_time",tags$span(style = "color:red;","Time points*"),choices = NULL),
          selectInput("sur_status",tags$span(style = "color:red;","Status*"),choices = NULL),
          selectInput("sur_group",tags$span(style = "color:red;","Group*"),choices = NULL),
          tags$div("Select NULL if there is only one group", style = "font-size: 12px; color: black;"),
          selectInput("alive",tags$span(style = "color:red;","Status_alive*"),choices = NULL),
          selectInput("dead",tags$span(style = "color:red;","Status_dead*"),choices = NULL),
        ),
        
        # Scatter plot
        conditionalPanel(
          condition = "input.plt_type == 'Scatter plot'",
          selectInput("scat_x",tags$span(style = "color:red;","x_var(numeric)*"),choices = NULL),
          selectInput("scat_y",tags$span(style = "color:red;","y_var(numeric)*"),choices = NULL),
          selectInput("scat_group",tags$span(style = "color:red;","Group(categorical)*"),choices = NULL),
          tags$div("Select NULL if there is only one group", style = "font-size: 12px; color: black;"),
        ),
        
        # Heatmap plot
        conditionalPanel(
          condition = "input.plt_type == 'Heatmap plot'",
          tags$div("No variables have to be selected", style = "font-size: 12px; color: black;"),
        ),
        
        tags$div("Navigate to plotting panel for plots", style = "font-size: 12px; color: red;"),
        # conditional panel if grouped data
        conditionalPanel(
          condition = "input.grouped == true",
          selectInput("subgroup",
                      label = tags$span(style = "color:red;","subgroup(categorical)*"),
                      choices = NULL),
          tags$div("Select NULL if there is no subgroup", style = "font-size: 12px; color: black;")
        ),

        actionButton("reset_btn","Reset Inputs"),
        tags$div("Click reset button when switch datasets", style = "font-size: 12px; color: red;")
        ),
      open = TRUE,
      card(
        full_screen = TRUE,
        card_header("Upload Table"),
        tableOutput("output1")
      ),
      layout_column_wrap(
        card(
          full_screen = TRUE,
          card_header("Histogram"),
          plotOutput("output2")
        ),
        card(
          full_screen = TRUE,
          card_header("Q-Q plot"),
          plotOutput("output3")
        ),
        card(
          full_screen = TRUE,
          card_header("Shapiro-Wilk normality test"),
          tableOutput("norm_test")
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Statistics test"),
        tableOutput("output4"),
        downloadButton("download_stats","Download")
        )
      )
    ),
    
  # panel for bar plot ----
  nav_panel(
    title = "Barplot",
    layout_sidebar(
      sidebar = sidebar(
        uiOutput("barplot_sidebar")
      ),
      open = TRUE,
      # Output for bar plot
      card(
        height = 800,
        full_screen = TRUE,
        card_header("Bar Plot"),
        plotOutput("output5"),
        downloadButton("download","Download"))
      )
    ),
  
  # panel for box plot ----
  nav_panel(
    title = "Box-Violin plot",
    layout_sidebar(
        sidebar = sidebar(
          uiOutput("boxplot_sidebar")
      ),
      open = TRUE,
      # Output for bar plot
      card(
        height = 800,
        full_screen = TRUE,
        card_header("Box-Violin Plot"),
        plotOutput("output6"),
        downloadButton("download2","Download"))
      )
    ),
  
  # panel for density plot ----
  nav_panel(
    title = "Density-Histogram plot",
    layout_sidebar(
      sidebar = sidebar(
        uiOutput("denplot_sidebar")
      ),
      open = TRUE,
      # Output for bar plot
      card(
        height = 800,
        full_screen = TRUE,
        card_header("Density-Histogram Plot"),
        plotOutput("output7"),
        downloadButton("download3","Download"))
      )
    ),
  # panel for survival plot ----
  nav_panel(
    title = "Survival plot",
    layout_sidebar(
      sidebar = sidebar(
        uiOutput("surplot_sidebar")
      ),
      open = T,
      card(height = 400,
           full_screen = TRUE,
           card_header("Survival Plot"),
           plotOutput("output8"),
           downloadButton("download4","Download"))
      )
    ),
  
  # panel for scatter plot ----
  nav_panel(
    title = "Scatter plot",
    layout_sidebar(
      sidebar = sidebar(
        uiOutput("scatplot_sidebar"),
        conditionalPanel(
          condition = "input.fit_line == true",
          sliderInput("eq_pos_x", "equation position (x)",
                      min = 0, max = 20, value = 10, step = 1),
          sliderInput("eq_pos_y", "equation position (y)",
                      min = 0, max = 20, value = 10, step = 1),
          sliderInput("eq_size", "size of equations",
                      min = 0, max = 20, value = 5, step = 0.5)
        ),
        # Input for download
        numericInput("scat_width","Download width (px)",value = 1000),
        numericInput("scat_height","Download height (px)", value = 1000),
        numericInput("scat_res","Download resolution (dpi)", value = 300)
      ),
    open = T,
    card(height = 800,
         full_screen = TRUE,
         card_header("Scatter Plot"),
         plotOutput("output9"),
         downloadButton("download5","Download"))
    )
  ),
  
  # panel for heatmap ----
  nav_panel(
    title = "Heatmap plot",
    layout_sidebar(
      sidebar = sidebar(
        uiOutput("htmap_sidebar")
      ),
      open = T,
      card(height = 800,
           full_screen = T,
           card_header("Heatmap Plot"),
           plotOutput("output10"),
           downloadButton("download6","Download"))
    )
  ),
  # panel for example data ----
  nav_panel(
    title = "Prepare dataset",
    layout_sidebar(
      sidebar = sidebar(
        title = "Data template",
        radioButtons("data_type",
                     "Select plot types", 
                     choices = c("Bar plot","Box-Violin plot",
                                 "Density-Histogram plot","Survival plot","
                                 Scatter plot","Heatmap plot"),
                     selected = character(0))
      ),
      card(
        full_screen = TRUE,
        card_header("Data templeate"),
        tableOutput("data_temp"),
        downloadButton("download_temp","Download template data")
      )
    )
  )
)
  



