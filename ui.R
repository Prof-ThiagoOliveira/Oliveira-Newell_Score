# Ui
#=======================================================================
# Body
#=======================================================================
body <- dashboardBody(
  #---------------------------------------------------------------------
  setShadow("box"),
  setShadow("info-box"),
  setShadow("progress"),
  withMathJax(),
  #---------------------------------------------------------------------
  tabItems(
    #-------------------------------------------------------------------
    tabItem(tabName =  "descriptive",
            #-----------------------------------------------------------
            fluidRow(
              #---------------------------------------------------------
              column(width =  5,
                     #--------------------------------------------------
                     shinydashboardPlus::box(width = NULL,
                             title = "Linear Regression",
                             status = "success",
                             closable = TRUE,
                             collapsible = TRUE,
                             plotOutput("pairs")),
                     ),
              #---------------------------------------------------------
              column(width = 2,
                     selectInput(inputId = "Variable1",
                                 label = "Y-axis:",
                                 choices = colnames(data.shiny),
                                 selected = "Height"),
                     selectInput(inputId = "Variable2",
                                 label = "X-axis:",
                                 choices = colnames(data.shiny),
                                 selected = "Weight"),
                     hr(),
                     shinydashboardPlus::box(width = NULL,
                             title = "Linear Correlation",
                             status = "success",
                             closable = TRUE,
                             collapsible = TRUE,
                             verbatimTextOutput("cor"))
                     ),
              #---------------------------------------------------------
              column(width = 5,
                     shinydashboardPlus::box(width = NULL,
                             title = "Regression Summary",
                             status = "success",
                             closable = TRUE,
                             collapsible = TRUE,
                             verbatimTextOutput("fit"))
                     ),
              hr(),
              hr(),
              #---------------------------------------------------------
              ),
            fluidRow(
              shinydashboardPlus::box(width = NULL,
                      Title =  "Pairs Plot",
                      closable = TRUE,
                      collapsible = TRUE,
                      status = "warning",
                      HTML(paste(h4(HTML('&nbsp;'),
                                    "Here, we can produce a matrix of
                                     scatter plots. This is useful to
                                     visualize correlation among
                                     variables in the dataset.
                                     Just take care with the number of
                                     selected variables.")
                                 )
                           ),
                      selectInput(inputId = "variablesPairs",
                                  label = "Variables:",
                                  choices = colnames(data.shiny),
                                  multiple = TRUE,
                                  selected = c("Height", "Weight")),
                      plotOutput("pairsPlot")
                      )
              #---------------------------------------------------------
            )
            #-----------------------------------------------------------
            ),
    #-------------------------------------------------------------------
    tabItem(tabName = "heatmap",
            fluidRow(
              selectInput("palette", "Palette", c("YlOrRd", "RdYlBu",
                                                  "Greens", "Blues")),
              checkboxInput("cluster", "Apply clustering"),
              d3heatmapOutput("heatplot", width = "97%",
                              height = "1300px")
            )),
    #-------------------------------------------------------------------
    tabItem(tabName = "pca",
            #-----------------------------------------------------------
            fluidRow(
              column(width = 6,
              #---------------------------------------------------------
                     shinydashboardPlus::box(width = NULL,
                             Title = "PCA Results",
                             status = "success",
                             closable = TRUE,
                             collapsible = TRUE,
                             HTML(paste(h4(HTML('&nbsp;'),
                                           "PCA analysis"))),
                             hr(),
                             verbatimTextOutput("pca_res")
                             )
                     #--------------------------------------------------
                     ),
              column(width = 6,
                     #--------------------------------------------------
                     shinydashboardPlus::box(width = NULL,
                             Title = "PCA Results",
                             status = "success",
                             closable = TRUE,
                             collapsible = TRUE,
                             HTML(paste(h4(HTML('&nbsp;'),
                                           "The Oliveira-Newell Score based on
                                            PCA analysis"))),
                             hr(),
                             selectInput(inputId = "variablesPCA",
                                  label = "Variables:",
                                  choices =
                                    colnames(score_data)[-c(1:17, 21,
                                                             23, 26:29,
                                                             54, 55, 61,
                                                             62, 64:66)]
                                 ),
                                  selectInput(inputId = "year",
                                              label = "Season",
                                              choices = levels(score_data$Year),
                                  multiple = FALSE,
                                  selected = c("2018-2019")
                                  ),
                             plotlyOutput("on_pca")
                             )
                     #--------------------------------------------------
                     )
              #---------------------------------------------------------
            )
            #-----------------------------------------------------------
            ),
    #-------------------------------------------------------------------
    tabItem(tabName = "model",
            fluidRow(
              #---------------------------------------------------------
              column(width = 6,
                     #--------------------------------------------------
                     shinydashboardPlus::box(width = NULL,
                             status = "success",
                             closable = TRUE,
                             collapsible = TRUE,
                             HTML(paste(h4(HTML('&nbsp;'),
                                           "Analysis of variance table"))),
                             hr(),
                             tableOutput("model_res")
                             ),
                     #--------------------------------------------------
                     shinydashboardPlus::box(width = NULL,
                             status = "success",
                             closable = TRUE,
                             collapsible = TRUE,
                             HTML(paste(h4(HTML('&nbsp;'),
                                           "Conditional Residual
                                            versus Fitted Values"))),
                             hr(),
                             plotOutput("plot_res")
                             ),
                     shinydashboardPlus::box(width = NULL,
                             status = "success",
                             closable = TRUE,
                             collapsible = TRUE,
                             HTML(paste(h4(HTML('&nbsp;'),
                                           "Random effect - Team:Player"))),
                             hr(),
                             plotOutput("ranef_player")
                             )
                     #--------------------------------------------------
                     ),
              #---------------------------------------------------------
              column(width = 6,
                     #--------------------------------------------------
                     shinydashboardPlus::box(width = NULL,
                             status = "success",
                             closable = TRUE,
                             collapsible = TRUE,
                             HTML(paste(h4(HTML('&nbsp;'),
                                           "Fitted versus Observed Values"))),
                             hr(),
                             selectInput
                             (inputId = "year_pred",
                               label = "Season",
                               choices = levels(model.final@frame$Year),
                               multiple = FALSE,
                               selected = c("2018-2019")
                             ),
                             plotlyOutput("pred_model")
                             ),
                     #--------------------------------------------------
                     shinydashboardPlus::box(width = NULL,
                             status = "success",
                             closable = TRUE,
                             collapsible = TRUE,
                             HTML(paste(h4(HTML('&nbsp;'),
                                           "Conditional Residual - Normal Plot"))),
                             hr(),
                             plotOutput("plot_norm")
                             ),
                     #--------------------------------------------------
                     shinydashboardPlus::box(width = NULL,
                             status = "success",
                             closable = TRUE,
                             collapsible = TRUE,
                             HTML(paste(h4(HTML('&nbsp;'),
                                           "Random effect - Year"))),
                             hr(),
                             plotOutput("ranef_year")
                             )
                     #--------------------------------------------------
                     )
              #---------------------------------------------------------
            )
            #-----------------------------------------------------------
            ),
    #-------------------------------------------------------------------
    tabItem(tabName = "prediction",
            #-----------------------------------------------------------
            fluidRow(
              #---------------------------------------------------------
              column(width = 8,
                     #--------------------------------------------------
                     shinydashboardPlus::box(
                       width = NULL,
                       status = "success",
                       closable = TRUE,
                       collapsible = TRUE,
                       HTML(paste(h4(HTML('&nbsp;'),
                                     "Test data - Season 2018-2019",
                                     hr(),
                                     " Training data - Seasons from 2015-2016 to 2017-2018"))),
                       hr(),
                       HTML(paste(h4(HTML('&nbsp;'),
                          "Concordance Correlation Coefficient (CCC),
                          Pearson Correlation Coefficient (r),
                          Accuracy Coefficient (Cb)  and
                          Root Mean Square Error (RMSE)
                          between true and predict values:"))),
                       tableOutput("ccc_pred"),
                       hr(),
                       HTML(paste(h4(HTML('&nbsp;'),
                                     "Best Players:"))),
                       hr(),
                       numericInput("best.pred", "Best Players based on
                                     model prediction: ", 10),
                       checkboxInput("last.players", "Last Players",
                                     FALSE),
                       tableOutput("best_pred")
                     )
                     #--------------------------------------------------
                     ),
              #---------------------------------------------------------
              column(width = 4,
                     #--------------------------------------------------
                     shinydashboardPlus::box(
                       width = NULL,
                       status = "success",
                       closable = TRUE,
                       collapsible = TRUE,
                       HTML(paste(h4(HTML('&nbsp;'),
                                     "Fitted versus Observed Values"))),
                       hr(),
                       plotlyOutput("pred_test")
                     )
                     #--------------------------------------------------
                     )
              #---------------------------------------------------------
            )
            #-----------------------------------------------------------
            ),
    tabItem(tabName = "onscore",
      #-----------------------------------------------------------------
      fluidRow(
        #---------------------------------------------------------------
        column(width = 6,
               #--------------------------------------------------------
               shinydashboardPlus::box(
                 width = NULL,
                 status = "success",
                 closable = TRUE,
                 collapsible = TRUE,
                 HTML(paste(h4(HTML('&nbsp;'),
                               ""))),
                 hr(),
                 HTML(paste(h4(HTML('&nbsp;'),
                               "Ranking of the top N players by season
                                who had a better performance compared
                                with their expected performance
                                within team and season:"))),
                 #------------------------------------------------------
                 fluidRow(
                   #----------------------------------------------------
                   column(width = 3,
                          selectInput(inputId = "season",
                                      label = "Season",
                                      choices = levels(bp.team$Season),
                                      multiple = FALSE,
                                      selected = c("2018-2019")
                                      )
                          ),
                   #----------------------------------------------------
                   column(width = 3,
                          selectInput(inputId = "order",
                                      label = "Sort",
                                      choices = c("Decreasing", "Increasing"),
                                      multiple = FALSE,
                                      selected = c("Decreasing")
                                      )
                          ),
                   #----------------------------------------------------
                   column(width = 4,
                        pickerInput(inputId = "position",
                                    label = "Position",
                                    choices= levels(bp.team$Position),
                                    options = list(`actions-box` = TRUE),
                                    multiple = TRUE,
                                    selected = NULL)
                        ),
                   #----------------------------------------------------
                   column(width = 2,
                          selectInput(inputId = "rookie",
                                      label = "Rookie",
                                      choices= levels(bp.team$Rookie),
                                      multiple = TRUE,
                                      selected = NULL)
                          )
                   #----------------------------------------------------
                   ),
                 fluidRow(
                   #----------------------------------------------------
                   column(width = 9,
                          pickerInput(inputId = "team",
                                      label = "Team",
                                      choices = levels(bp.team$Team),
                                      options = list(`actions-box` = TRUE),
                                      multiple = TRUE,
                                      selected = NULL
                                      )
                          ),
                   column(width = 3,
                          checkboxInput("isRookie", "Grouped by Rookie",
                                     FALSE),
                          )
                   #----------------------------------------------------
                   ),
                 dataTableOutput("bp_pred")
               ),
               #--------------------------------------------------------
               shinydashboardPlus::box(
                 width = NULL,
                 status = "success",
                 closable = TRUE,
                 collapsible = TRUE,
                 HTML(paste(h4(HTML('&nbsp;'),
                               ""))),
                 hr(),
                 HTML(paste(h4(HTML('&nbsp;'),
                               "Density plot based on Consistence Score by Team in a Season:"))),
                 hr(),
                 plotOutput("ridges_plot", height = "1100px")
                 #------------------------------------------------------

                 #------------------------------------------------------
               )
               #--------------------------------------------------------
               ),
        column(width = 6,
               #--------------------------------------------------------
               shinydashboardPlus::box(
                 width = NULL,
                 status = "success",
                 closable = TRUE,
                 collapsible = TRUE,
                 HTML(paste(h4(HTML('&nbsp;'),
                               ""))),
                 hr(),
                 HTML(paste(h4(HTML('&nbsp;'),
                               "Ranking of the N best players over
                                the season:"))),
                 br(),
                 br(),
                 dataTableOutput("bp_pred_best")
               ),
               #--------------------------------------------------------
               shinydashboardPlus::box(
                 width = NULL,
                 status = "success",
                 closable = TRUE,
                 collapsible = TRUE,
                 HTML(paste(h4(HTML('&nbsp;'),
                               ""))),
                 hr(),
                 HTML(paste(h4(HTML('&nbsp;'),
                               "Density plot based on Relevance Score
                                by season:"))),
                 hr(),
                 plotOutput("ridges_plot2", height = "1100px")
                 #------------------------------------------------------

                 #------------------------------------------------------
               )
               #--------------------------------------------------------
               )
        #---------------------------------------------------------------
      )
      #-----------------------------------------------------------------
    )
    #-------------------------------------------------------------------
  )
  #---------------------------------------------------------------------
)
#=======================================================================
# Sidebar
#=======================================================================
sidebar  <- dashboardSidebar(
  sidebarMenu(
    #-------------------------------------------------------------------
    # Create three menuIten lccData and lcc,  lccplot
    menuItem(text =  "Regression Summary",  tabName = "descriptive"),
    menuItem(text = "Heatmap",  tabName = "heatmap"),
    menuItem(text = "PCA Score",  tabName = "pca"),
    menuItem(text = "Mixed-Effects Model",  tabName = "model"),
    menuItem(text = "Prediction (Test data)",  tabName = "prediction"),
    menuItem(text = "Adjusted Oliveira-Newell Score",  tabName = "onscore")
  )
)
#=======================================================================
# Header
#=======================================================================
header  <- shinydashboardPlus::dashboardHeader(
  title = "Oliveira-Newell Score",
  titleWidth = 350,
  controlbarIcon = "gears",
  dropdownMenu(
    type = "messages"
  )
)
#=======================================================================
ui <- shinydashboardPlus::dashboardPage(header, sidebar, body)
#=======================================================================
