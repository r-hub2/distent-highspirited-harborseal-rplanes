library(shiny)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(rplanes)
library(lubridate)

## helper for title casing
tcase <- function(x) {
  first_char <- substr(x,1,1)
  chartr(first_char,toupper(first_char),x)
}

# list module files and iterate sourcing them to use within the app.
module_sources <- list.files(path = system.file("app/modules/", package = "rplanes"), full.names = TRUE)
sapply(module_sources, source)


# UI SIDE ####

ui <- navbarPage(title = "rplanes Explorer",
                 inverse = TRUE, # invert color of navigation top bar to black
                 useShinyjs(),  # Set up shinyjs
                 tabPanel(title = "Analysis",
                          fluidPage(
                            sidebarLayout(position = "left",
                                          sidebarPanel(width = 3,
                                                       prettyRadioButtons("choice", "Choose Dataset", choices = c("Custom", "Example"), selected = "Custom",  status = "warning", inline = TRUE, icon = icon("check"), bigger = TRUE),
                                                       awesomeRadio("status", "Type of signal to be evaluated", choices = c("Forecast", "Observed"), selected= "Forecast", inline = TRUE, status = "warning"),
                                                       shinyjs::hidden(div(id = "example_info",
                                                                           HTML("<strong>The example data includes 4 week-ahead forecasts for flu hospitalizations in select United States locations.</strong>" ),
                                                                           tags$hr())),
                                                       shinyjs::hidden(div(id = "choice_obs_upload",
                                                                           fileInput("upload_1", label = "Upload Observed Data", multiple = FALSE, accept = ".csv"))),
                                                       shinyjs::hidden(div(id = "choice_forc_upload",
                                                                           fileInput("upload_2", label = "Upload Forecast Data", multiple = FALSE, accept = ".csv"))),
                                                       shinyjs::hidden(div(id = "choice_forc_format",
                                                                           awesomeRadio("forecast_format", "Forecast file format", choices = c("Hubverse" = "hubverse", "Legacy" = "legacy"), selected = "hubverse", inline = TRUE, status = "warning"))),

                                                       shinyjs::hidden(div(id = "choice_nobs",
                                                                           numericInput("n_obs_eval", "Number of Observed Values to Evaluate", value = 1, min = 1, max = Inf, step = 1))),

                                                       awesomeRadio("rez", "Resolution", choices = c("Weekly" = "weeks", "Daily" = "days", "Monthly" = "months"), inline = TRUE, status = "warning"),
                                                       textInput("outcome", label = "Outcome", value = ""),
                                                       shinyjs::hidden(div(id = "forc_opt",
                                                                           numericInput("horizon", "Forecast Horizon", value = 4, min = 1, max = 100, step = 1))),
                                                       materialSwitch("opts", label = "Modify Defaults", value = FALSE, status = "success"),
                                                       shinyjs::hidden(div(id = "add_options",
                                                                           textInput("width", label = "Prediction Interval", value = "95"),
                                                                           pickerInput(inputId = "components", label = "PLANES Component(s)", choices = "", options = list(`actions-box` = TRUE), multiple = TRUE),
                                                                           awesomeRadio("custom_weights", "Weights", choices = c("Equal Weights", "Custom"), selected= "Equal Weights", inline = TRUE, status = "warning"),

                                                                           shinyjs::hidden(div(id = "weight_choices",
                                                                                               uiOutput("weights"),
                                                                           )),
                                                                           shinyjs::hidden(div(id = "args_trend",
                                                                                               numericInput("sig", "Significance (Trend)", value = 0.1, min = 0, max = 1, step = 0.01))),
                                                                           shinyjs::hidden(div(id = "args_repeat",
                                                                                               numericInput("tol", label = "Tolerance (Repeat)", value = 0, min = 0, max = 50, step = 1),
                                                                                               numericInput("pre", label = "Prepend Values (Repeat)",  value = 0, min = 0, max = 365, step = 1))),
                                                                           shinyjs::hidden(div(id = "args_shape",
                                                                                               radioButtons("method", label = "Method (Shape)", choices = c("sdiff (Default)" = "sdiff", "Dynamic Time Warping" = "dtw"), selected = "sdiff")))
                                                       )),
                                                       actionBttn("run", "Analyze", style = "unite", color = "danger"),
                                                       actionBttn("reset", "Reset", style = "stretch", color = "warning")
                                          ),
                                          mainPanel(
                                            tabsetPanel(id = "tabs1",
                                                        tabPanel("Scoring",
                                                                 shinyjs::hidden(div(id = "instructions1",
                                                                                     includeMarkdown((system.file("app/instructions.md", package = "rplanes"))))),

                                                                 plotUI("tab2")),
                                                        tabPanel("Raw Data",
                                                                 shinyjs::hidden(div(id = "instructions2",
                                                                                     includeMarkdown((system.file("app/instructions.md", package = "rplanes"))))),
                                                                 shinyjs::hidden(div(id="raw_data",
                                                                                     dataUI("tab1")))))

                                          )
                                          ), # sidebarLayout


                          )), # plots tab
                 tabPanel(title = "Help",
                          includeMarkdown(system.file("app/help.md", package = "rplanes")))
) # UI end


# SERVER SIDE ####

server <- function(input, output, session){

  ## initiate reactive value for logic in toggling instructions
  rv <- reactiveValues(value = 0)

  observeEvent(input$run, {
    ## increment the value after the analyze button has been clicked
    inc_value <- rv$value + 1
    rv$value <- inc_value
  })

  observe({
    # unhide the upload custom dataset when choosing "Custom" radiobutton
    shinyjs::toggle(id = "choice_obs_upload", condition = {input$choice == "Custom"})
    shinyjs::toggle(id = "choice_forc_upload", condition = {input$status == "Forecast" & input$choice == "Custom"})
    shinyjs::toggle(id = "choice_forc_format", condition = {input$status == "Forecast" & input$choice == "Custom"})
    ## show the example description text when the choice is example
    shinyjs::toggle(id = "example_info", condition = {input$choice == "Example"})
    # unhide additional options upon switch
    shinyjs::toggle(id = "add_options", condition = {input$opts == TRUE})
    shinyjs::toggle(id = "forc_opt", condition = {input$status == "Forecast"})
    ## certain modifications for data evaluation should not be available for example ...
    ## ... these are fixed by the example and if user changed them the processing would break
    ## toggle on only if example is not selected
    shinyjs::toggle(id = "rez", condition = {input$choice != "Example"})
    shinyjs::toggle(id = "horizon", condition = {input$choice != "Example"})
    shinyjs::toggle(id = "status", condition = {input$choice != "Example"})
    shinyjs::toggle(id = "outcome", condition = {input$choice != "Example"})
    ## toggle contents of scoring / data tabs
    ## if the analysis hasn't been run yet then show basic instructions
    shinyjs::toggle(id = "instructions1", condition = {rv$value == 0})
    shinyjs::toggle(id = "instructions2", condition = {rv$value == 0})
    shinyjs::toggle(id = "raw_data", condition = {rv$value > 0 })
    ## toggle the number of obs picker if the signal type is 'Observed'
    shinyjs::toggle(id = "choice_nobs", condition = {input$status == "Observed"})
    shinyjs::toggle(id = "weight_choices", condition = {input$custom_weights == "Custom"})
    shinyjs::toggle(id = "args_trend", condition = {"trend" %in% input$components})
    shinyjs::toggle(id = "args_repeat", condition = {"repeat" %in% input$components})
    shinyjs::toggle(id = "args_shape", condition = {"shape" %in% input$components})
    })

  # update scoring options based on user input of observed or forecast comparison
  observe({
    if(input$status == "Observed"){
      score_opt <- c("Difference" = "diff", "Repeat" = "repeat", "Zero" = "zero")
      updatePickerInput(session = session, inputId = "components", choices = score_opt, selected = c("diff", "repeat", "zero"))
    } else {
      score_opt <- c("Coverage" = "cover", "Difference" = "diff", "Repeat" = "repeat", "Taper" = "taper", "Trend" = "trend", "Shape" = "shape", "Zero" = "zero")
      updatePickerInput(session = session, inputId = "components", choices = score_opt, selected = c("Coverage" = "cover", "Difference" = "diff", "Repeat" = "repeat", "Taper" = "taper", "Trend" = "trend",  "Shape" = "shape", "Zero" = "zero"))
    }
  })

  output$weights <- renderUI({
    ## create a list of numeric inputs
    ## each will have a label with selected component in title case
    ## value will be set at 1 by default
    ## id will be weight_ component to make it easy to find / parse as input for plane_score
    components() %>%
      purrr::map(., function(x) numericInput(inputId = paste0("weight_",x), label = paste0("Weight: ", tcase(x)), value = 1, min = 1))
  })

    # pass in actionBttn to module plots
    btn1 <- reactive({ input$run })
    btn2 <- reactive({ input$reset })

    # pass input$status, input$outcome, input$score to module plots
    status <- reactive({ input$status })
    outcome <- reactive({ input$outcome })
    components <- reactive({ input$components })


    data_1 <- reactive({
        if (input$choice == "Example") {
            # example observed data
            df <- read.csv(system.file("extdata/observed", "hdgov_hosp_weekly.csv", package = "rplanes"))  %>%
              ## truncate the example data for performance
              dplyr::filter(location %in% c("US","02","06","09","12","15","22","25","28","32","37","45","48","51","54"))
        } else {
            # Uploading observed data
            req(input$upload_1)
            ext <- tools::file_ext(input$upload_1$name)
            df <- switch(ext,
                         csv = read.csv(input$upload_1$datapath),
                         validate("Invalid file; Please upload a .csv file"))
        }
        df$date <-  as.Date(df$date)
        df <-
          df %>%
          arrange(location,date)
        df
    })

    # function to check if date can be converted to the format yyyy-mm-dd
    is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), format = '%Y-%m-%d'))


    data_2 <- reactive({
        if(input$choice == "Example") {
            # example forecast data
            df <- read.csv(system.file("extdata/forecast", "2022-10-31-SigSci-TSENS.csv", package = "rplanes"))  %>%
              ## truncate the example data for performance
              dplyr::filter(location %in% c("US","02","06","09","12","15","22","25","28","32","37","45","48","51","54"))
        } else {
            # Uploading forecast data
            req(input$upload_2)
            ext <- tools::file_ext(input$upload_2$datapath)
            df <- switch(ext,
                         csv = read.csv(input$upload_2$datapath),
                         validate("Invalid file; Please upload a .csv file"))

            if(input$status == "Forecast"){

              if(input$forecast_format == "legacy") {
                df$forecast_date <- as.Date(df$forecast_date, format = "%Y-%m-%d")
                df$target_end_date <- as.Date(df$target_end_date, format = "%Y-%m-%d")
                width <- round(rplanes:::q_boundary(as.numeric(input$width)), 2)
                quant_list <- round(c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), 2)
                validate(need(all(width %in% quant_list), message = "Quantiles unavailable for width specified."))
                df <- df %>%
                  dplyr::mutate(quantile = ifelse(is.na(df$quantile), 0.5, df$quantile)) %>%
                  filter(quantile %in% width)

              } else if (input$forecast_format == "hubverse") {
                df$forecast_date <- as.Date(df$reference_date, format = "%Y-%m-%d")
                df$target_end_date <- as.Date(df$target_end_date, format = "%Y-%m-%d")
                width <- round(rplanes:::q_boundary(as.numeric(input$width)), 2)
                quant_list <- round(c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), 2)
                validate(need(all(width %in% quant_list), message = "Quantiles unavailable for width specified."))
                df <- df %>%
                  dplyr::mutate(quantile = ifelse(is.na(df$output_type_id), 0.5, df$output_type_id)) %>%
                  filter(quantile %in% width)

              }
            } else {
              validate(need(is.convertible.to.date(df$date[1]), message = "Columns containing dates need to be formatted like: 2022-10-31"))
              df$date <- as.Date(df$date, format = "%Y-%m-%d")
            }
        }

        df
    }) %>% bindEvent(input$width, input$choice, input$upload_2)


    prepped_seed <- reactive({
        if(input$status == "Forecast"){
            df <- data_1() %>% dplyr::filter(location %in% unique(data_2()$location))
            date <- unique(df$date)[unique(df$date) < min(data_2()$target_end_date)]
            date <- tail(date, 1)

            ## handle outcome name for example data set if selected
            if(input$choice == "Example") {
              signal <- to_signal(df, outcome = "flu.admits", type = "observed", resolution = input$rez)
            } else {
              signal <- to_signal(df, outcome = input$outcome, type = "observed", resolution = input$rez)
            }

        } else if (input$status == "Observed"){
            signal <- to_signal(data_1(), outcome = input$outcome, type = "observed", resolution = input$rez)
            date <- min(tail(data_1()$date,input$n_obs_eval)-1)
        }
      ## prep the seed with the cut date
      prepped_seed  <- plane_seed(signal, cut_date = date)
      prepped_seed
    })

    ## conditionally prepare signal for data to be evaluated
    prepped_signal <- reactive({
        if (input$choice == "Example"){
            prepped <- read_forecast(system.file("extdata/forecast", "2022-10-31-SigSci-TSENS.csv", package = "rplanes"), pi_width = as.numeric(input$width)) %>%
              ## truncate the example data for performance
              dplyr::filter(location %in% c("US","02","06","09","12","15","22","25","28","32","37","45","48","51","54")) %>%
                to_signal(., outcome = "flu.admits", type = "forecast", horizon = 4, resolution = "weekly")
        } else if (input$status == "Forecast"){
            prepped <- read_forecast(input$upload_2$datapath, pi_width = as.numeric(input$width), format = input$forecast_format) %>%
                filter(location %in% unique(data_1()$location)) %>%
                to_signal(., outcome = input$outcome, type = "forecast", horizon = input$horizon, resolution = input$rez)
        } else if (input$status == "Observed"){
          ## if the data is an observed signal use the data uploaded
          ## the prepped seed will have a cut date defined by the number of points to evaluate
          ## therefore we can just use the uploaded data as-is
            prepped <- to_signal(data_1(), outcome = input$outcome, type = "observed", horizon = input$horizon, resolution = input$rez)
        }
        prepped
    })

    # get all intersecting locations between the datasets to use as input$loc in plots module
    locations <- reactive({
      ## conditionally get locations either as intersection of observed (i.e., seed) and forecast locations
      ## or if the signal is observed then just use observed locations
      if(input$choice == "Example" | input$status == "Forecast") {
        generics::intersect(data_1()$location, data_2()$location)
      } else if (input$status == "Observed") {
        unique(data_1()$location)
      }
    })

    # run the scoring using logic to modify the args parameter in plane_score for the repeats function
    # This applies to the repeats option, was not taking my direct inputs unless I specified it out into a list like below.
    scoring <- eventReactive(input$run,{

      if (input$tol == 0 & input$pre == 0){
        comp_args <- list(trend = list(sig_lvl = input$sig), `repeat` = list(prepend = NULL, tolerance = NULL), shape = list(method = input$method))
      } else if (input$tol == 0){
        comp_args <- list(trend = list(sig_lvl = input$sig), `repeat` = list(prepend = input$pre, tolerance = NULL), shape = list(method = input$method))
      } else if (input$pre == 0){
        comp_args <- list(trend = list(sig_lvl = input$sig), `repeat` = list(prepend = NULL, tolerance = input$tol), shape = list(method = input$method))
      } else {
        comp_args <- list(trend = list(sig_lvl = input$sig), `repeat` = list(prepend = input$pre, tolerance = input$tol), shape = list(method = input$method))
      }

      ## handle weights
      if(input$custom_weights != "Custom" | input$opts == FALSE) {
        weight_vals <- NULL
      } else {
        weight_inputs <- paste0("weight_", input$components)

        weight_vals <-
          weight_inputs %>%
          purrr::map_dbl(., function(x) input[[x]]) %>%
          purrr::set_names(input$components)
      }
      scores <- plane_score(prepped_signal(), prepped_seed(), components = input$components, args = comp_args, weights = weight_vals)
      scores
    })

    dataServer("tab1", data_1 = data_1, data_2 = data_2, signal_type = input$status, n_obs_eval = input$n_obs_eval)

    plotServer("tab2", scoring = scoring, components = components, data_1 = data_1, locations = locations, seed = prepped_seed, signal_to_eval = prepped_signal, btn1 = btn1, status = status, outcome = outcome, btn2 = btn2)

    # reset all inputs including the ones in the modules
    observeEvent(input$reset,{
      session$reload()
    })

} # server end



# Run the application
shinyApp(ui = ui, server = server)

