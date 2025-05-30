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
  title = "EasyPlot Dashboard",
  id = "tabs",
  theme = bs_theme(bootswatch = "minty"),
  
  # panel for showing the data ----
  nav_panel(
    title = "Data & Statistics",
    # Side bar panel ----
    layout_sidebar(
      sidebar = sidebar(
        # Input: upload the csv file
        fileInput("file1", "Choose CSV File", accept = ".csv"),
        
        # Input: display rows
        numericInput("nRow", "Display rows", value = 10),
        
        h6(tags$span(style = "color:red;","*Barplot and Boxplot Only")),
        
        # Input: select column for grouping 
        selectInput("group",
                    label = tags$span(style = "color:red;","*Select for group"),
                    choices = NULL),
        
        # Input: select column for testing
        selectInput("value",
                    label = tags$span(style = "color:red;","*Select for value"),
                    choices = NULL),
        
        # Input: statistic test methods
        selectInput("test_method", 
                    label = tags$span(style = "color:red;","*Select test method"),
                    choices = c("","t test",
                                "Mann-Whitney U test",
                                "one-way-ANNOVA (Tukey)",
                                "t test (Multisamples)", 
                                "Mann-Whitney U test (Multisamples)")),
        
        # Input: adjusted method
        conditionalPanel(
          condition = "input.test_method == 't test (Multisamples)' || input.test_method == 'Mann-Whitney U test (Multisamples)'",
          # Input: adjusted methods
          selectInput("adj_method",
                      label = tags$span(style = "color:red;","*Select adjusted method"),
                      choices = c("","none","holm","hochberg","hommel",
                                  "BH","BY","bonferroni","fdr"))
          )
        ),
      open = TRUE,
      card(
        full_screen = TRUE,
        card_header("Upload Table"),
        tableOutput("output1")
      ),
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("Table for plotting"),
          tableOutput("output2")
        ),
        card(
          full_screen = TRUE,
          card_header("Q-Q plot"),
          plotOutput("output3")
          )
        ),
      card(
        full_screen = TRUE,
        card_header("Statistics test"),
        tableOutput("output4")
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
        h6(tags$span(style = "color:red;","Required*")),
        selectInput("den_x_var",tags$span(style = "color:red;","x variable*"),choices = NULL),
        selectInput("den_group",tags$span(style = "color:red;","group_by*"),choices = NULL),
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
  # panel for survival plot
  nav_panel(
    title = "Survival plot",
    layout_sidebar(
      sidebar = sidebar(
        h6(tags$span(style = "color:red;","Required*")),
        selectInput("sur_time",tags$span(style = "color:red;","time points*"),choices = NULL),
        selectInput("sur_status",tags$span(style = "color:red;","status*"),choices = NULL),
        selectInput("sur_group",tags$span(style = "color:red;","group*"),choices = NULL),
        selectInput("alive",tags$span(style = "color:red;","status_alive*"),choices = NULL),
        selectInput("dead",tags$span(style = "color:red;","status_dead*"),choices = NULL),
        uiOutput("surplot_sidebar")
      ),
      open = T,
      card(height = 800,
           full_screen = TRUE,
           card_header("Survival Plot"),
           plotOutput("output8"),
           downloadButton("download4","Download"))
      )
    )
  )

