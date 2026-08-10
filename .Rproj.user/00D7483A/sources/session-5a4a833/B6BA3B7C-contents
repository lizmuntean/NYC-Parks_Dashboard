## check to see if other kinds of marsh datasets also can run in this, build functionality for that 
## read me file with how to edit and AI prompt examples
## github
## print figures
## annual or else code? or might we add other buttons under analysis

library(shiny)
library(tidyverse)
library(car)
library(lmerTest)
library(emmeans)
library(janitor)
library(lubridate)
library(FSA)

# Definitions 

source("app_functions.R")

continuous_variables <- c(
  "total_cover",
  "unveg",
  "avg_spal_h_cm",
  "avg_spal_d_mm"
)

count_variables <- c(
  "crab_bur",
  "mussel_count",
  "snail_count",
  "spal_stem_count",
  "sppa_stem_count",
  "disp_stem_count",
  "phau_stem_count"
)

response_variables <-c(
  continuous_variables,
  count_variables
)


# User Interface

ui <- fluidPage(
  
  titlePanel("Wetland Environmental Compliance Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      
      fileInput(
        "datafile",
        "Upload Wetland Data",
        accept = c(".csv", ".xlsx")
      ),
      
      uiOutput("response_ui"),
      
      uiOutput("treatment_ui"),
      
      uiOutput("marsh_ui"),
      
      radioButtons(
        "analysis_type",
        "Analysis Type",
        choices = c(
          "Single Year" = "annual",
          "Across Years" = "time"
        ),
        selected = "annual"
      ),
      
      uiOutput("year_ui"),
      
      actionButton("run", "Run Analysis")
      
    ),
    
    mainPanel(
      
      style = "height: 900px; overflow-y: auto;",
      
      # Summary cards at the top
      fluidRow(
        
        column(
          4,
          wellPanel(
            style = "height:120px;",
            h4("Marsh Type"),
            textOutput("summary_marsh")
          )
        ),
        
        column(
          4,
          wellPanel(
            style = "height:120px;",
            h4("Year"),
            textOutput("summary_year")
          )
        ),
        
        column(
          4,
          wellPanel(
            style = "height:120px;",
            h4("Sample Size"),
            textOutput("summary_n")
          )
        ),
      
      # Tabs below the cards
      tabsetPanel(
        tabPanel("Data Distribution",
                 plotOutput("histograms", height = "800px")),
        tabPanel("Boxplot",
                 plotOutput("boxplot")
        ),
        
        tabPanel("Diagnostics and Assumptions",
                 plotOutput("qqplot"),
                 plotOutput("residplot"),
                 verbatimTextOutput("shapiro"),
                 verbatimTextOutput("levene")
                 
      ),
      
       tabPanel("ANOVA",
               tableOutput("anova"),
               
               h4("How to interpret these results"),
               
               tags$ul(
                 tags$li(strong("sumsq:"), "Total variation attributed to that source. Larger values indicate more variation explained."),
                 tags$li(strong("meansq:"), "SumSq divided by Df. This is the average variation per degree of freedom."),
                 tags$li(strong("statistic:"), "The likelihood ratio statistic (often denoted χ² or LR χ²). It measures how much better the model fits when that term is included. 
                         Larger values indicate a greater improvement in fit. Or, the F statistic, which compares the variation explained by the factor to the unexplained (residual) variation. 
                         Larger values provide stronger evidence against the null hypothesis."),
                 tags$li(strong("p.value:"), "The probability of obtaining a test statistic at least this large if there were truly no treatment effect."),
                 tags$li(strong("test:"), "Likelihood ratio is used to fit a negative binomial GLM, as is used for count data. This methods 
                         compares the full model with a reduced model to determine if the term of interest significantly increases the model's 
                         likelihood and is ideed explaining the variation better than random chance. The F test determines if the variance explained by the fixed effect
                         is large relative to the residual values and follows the assumptions of a linear or linear mixed effects model. It is the ratio of between-group variation over within-group variation.")
      )),
      
        tabPanel("Estimated Marginal Means",
              verbatimTextOutput("emmeans"),
              h4("How to interpret these results"),
              
              tags$ul(
                tags$li(strong("top table, emmean:"), "The estimated marginal mean (also called the least-squares mean). This is the model-predicted mean for that group after accounting for the other variables in the model."),
                tags$li(strong("lower.Cl, upper.CL:"), "Lower and upper bounds of the 95% confidence interval."),
                tags$li(strong("bottom table, estimate:"), "The estimated difference between the two group means. A negative value means the first group has a lower mean than the second; a positive value means the first group has a higher mean.."),
                tags$li(strong("t.ratio:"), "The test statistic. It is the estimated difference divided by its standard error: estimate/SE. Larger absolute values indicate stronger evidence that the groups differ."),
                tags$li(strong("p.value:"), "The probability of observing a difference at least this extreme if the true difference between the groups were zero. This is often adjusted for multiple comparisons (e.g., Tukey adjustment).")
                )),
      
        tabPanel("Interpretation",
                 uiOutput("interpretation"),
                 uiOutput("emmeans_interpretation")
        ),
      
        tabPanel("Non-parametric tests",
                 actionButton("run_nonparam", "Run Non-Parametric Analysis"),
                 
                 br(),
                 
                 h3("Overall Test"),
                 tableOutput("non_param"),
                 
                 uiOutput("non_param_interp"),
                 
                 h3("Post Hoc Comparisons"),
                 tableOutput("non_param_posthoc"),
                 
                 h4("About Non-Parametric Tests"),
                 
                 tags$p(
                   "Non-parametric tests are rank-based alternatives to ANOVA that are used when assumptions of normality or equal variance are not met. 
                   The Wilcoxon rank-sum test compares two groups, while the Kruskal-Wallis test compares three or more independent groups. 
                   Significant results indicate that at least one group differs, 
                   but post-hoc comparisons are required to identify which groups are different."
                 ),
                 
                 tags$ul(
                   
                   tags$li(
                     strong("Wilcoxon rank-sum test: "),
                     "Used when comparing two independent groups. This test evaluates whether observations from one group tend to have higher or lower values than the other group. "
                   ),
                   
                   tags$li(
                     strong("Kruskal-Wallis test: "),
                     "Used when comparing three or more independent groups. This is the non-parametric equivalent of a one-way ANOVA and tests whether at least one group differs from the others."
                   ),
      
                   )
                 ),

      tabPanel("Elevation",
               plotOutput("elevation_plot", height = "700px"),
               
               h4("Enter planting ranges and tide datum:"),
               
               textInput(
                 "line1_label",
                 "Reference line 1 label",
                 value = "MHHW"
               ),
               
               numericInput(
                 "line1_value",
                 "Reference line 1 elevation (ft)",
                 value = 3.03
               ),
               
               textInput(
                 "line2_label",
                 "Reference line 2 label",
                 value = "MHW"
               ),
               
               numericInput(
                 "line2_value",
                 "Reference line 2 elevation (ft)",
                 value = 2.43
               ),
               
               textInput(
                 "line3_label",
                 "Reference line 3 label",
                 value = "MTL"
               ),
               
               numericInput(
                 "line3_value",
                 "Reference line 3 elevation (ft)",
                 value = -0.2
               ),
               
               textInput(
                 "line4_label",
                 "Reference line 4 label",
                 value = "MLW"
               ),
               
               numericInput(
                 "line4_value",
                 "Reference line 4 elevation (ft)",
                 value = -2.83
               ),
      
                br(),
      
                h4("Figure specs:"),
      
                numericInput("elev_fig_width", "Figure Width (in)", 8),
      
                numericInput("elev_fig_height", "Figure Height (in)", 6),
      
                numericInput("elev_fig_dpi", "Resolution (dpi)", 300),
      
                downloadButton("download_elevation",
                                      "Download Figure"),
      
      ),
      
      tabPanel(
        "Bar Graphs for year comparisons",
        
                    fluidRow(
                      column(
                        4,
                        uiOutput("plot_years_ui")
                      ),
                      column(
                        4,
                        checkboxInput("show_means", "Show mean values", TRUE)
                      ),
                      column(
                        4,
                        br(),
                        actionButton("update_plots", "Update Plots")
                      )
                    ),
                    
                    hr(),
                    
                    plotOutput("average_plot", height = "800px"),
                  
                  br(),
                  
                  h4("Figure specs:"),
                  
                  numericInput("bar_fig_width", "Figure Width (in)", 8),
                  
                  numericInput("bar_fig_height", "Figure Height (in)", 6),
                  
                  numericInput("bar_fig_dpi", "Resolution (dpi)", 300),
                  
                  downloadButton(
                    "download_average",
                    "Download Figure"
                  ),
      ),
                  
      tabPanel(
        "Count Comparisons",
        
                fluidRow(
                  column(
                    4,
                    uiOutput("comparison_year_ui")
                  ),
                  
                  column(
                    4,
                    uiOutput("comparison_response_ui")
                  ),
                  
                  column(
                    4,
                    checkboxInput("show_comparison_means", "Show mean values", TRUE)
                  ),
                  
                  column(
                    4,
                    actionButton(
                      "update_comparison",
                      "Run Comparison"
                    )
                  )
                ),
                
                hr(),
                
                plotOutput(
                  "response_comparison_plot",
                  height = "700px"
                ),
                
                br(),
                
                h4("Figure specs:"),
                
                numericInput("count_fig_width", "Figure Width (in)", 8),
                
                numericInput("count_fig_height", "Figure Height (in)", 6),
                
                numericInput("count_fig_dpi", "Resolution (dpi)", 300),
                
                downloadButton(
                  "download_comparison",
                  "Download Figure"
                ),
      )
      )
    )
  )
))

# Server

server <- function(input, output, session) {
  
  dataset <- reactive({
    
    req(input$datafile)
    
    print("1. File uploaded")
    
    ext <- tools::file_ext(input$datafile$name)
    print(ext)
    
    if (ext == "csv") {
      raw <- read.csv(input$datafile$datapath)
    } else if (ext == "xlsx") {
      raw <- readxl::read_excel(input$datafile$datapath)
    } else {
      stop("Unsupported file type")
    }
    
    print("2. Raw file loaded")
    
    processed <- preprocess_data(raw)
    
    # -----------------------------
    # Input validation
    # -----------------------------
    
    required_columns <- c(
      "marsh",
      "treatment",
      "year",
      "plotID"
    )
    
    missing_columns <- setdiff(
      required_columns,
      names(processed)
    )
    
    
    if(length(missing_columns) > 0){
      
      validate(
        need(
          FALSE,
          paste(
            "Missing required columns:",
            paste(missing_columns, collapse=", ")
          )
        )
      )
      
    }
    
    print("3. Preprocessing complete")
    
    return(processed)
    
  })
  
  observeEvent(input$datafile, {
    
    print("Dataset loaded:")
    print(head(dataset()))
    
  })
  
  output$response_ui <- renderUI({
    
    req(dataset())
    
    available_responses <- response_variables[response_variables %in% names(dataset())]
    
    selectInput(
      "response",
      "Response Variable",
      choices = available_responses,
      multiple = FALSE
    )
    
  })
  
  output$marsh_ui <- renderUI({
    
    req(dataset())
    
    selectInput(
      "marsh",
      "Marsh Type",
      choices = unique(dataset()$marsh),
      selected = unique(dataset()$marsh),
      multiple = TRUE
    )
    
  })
  
  output$year_ui <- renderUI({
    
    req(dataset())
    
    if(input$analysis_type == "annual"){
      
      selectInput(
        "year",
        "Year",
        choices = sort(unique(dataset()$year))
      )
      
    }
    
  })
  
  output$treatment_ui <- renderUI({
    
    req(dataset())
    
    checkboxGroupInput(
      "groups",
      "Treatments",
      choices = unique(dataset()$treatment),
      selected = unique(dataset()$treatment)
    )
    
  })
  
  elevation_plot_obj <- reactive({
    
    req(dataset())
    req(input$year)
    
    df <- dataset() %>%
      filter(
        year == input$year,
        marsh %in% input$marsh,
        treatment %in% input$groups
      )
    
    plot_elevation(
      data = df,
      season_year = input$year,
      line_labels = c(
        input$line1_label,
        input$line2_label,
        input$line3_label,
        input$line4_label
      ),
      line_values = c(
        input$line1_value,
        input$line2_value,
        input$line3_value,
        input$line4_value
      )
    )
    
  })
  
  output$elevation_plot <- renderPlot({
    elevation_plot_obj()
  })
  
  output$download_elevation <- downloadHandler(
    
    filename = function() {
      paste0(
        "Elevation_",
        input$year,
        ".pdf"
      )
    },
    
    content = function(file) {
      
      ggsave(
        filename = file,
        plot = elevation_plot_obj(),
        width = input$elev_fig_width,
        height = input$elev_fig_height,
        device = "pdf"
      )
      
    }
  )
  
  output$summary_n <- renderText({
    
    req(dataset(), input$year, input$marsh)
    
    n <- dataset() %>%
      filter(
        year == input$year,
        marsh %in% input$marsh
      ) %>%
      nrow()
    
    paste("n =", n)
    
  })
  
  output$histograms <- renderPlot({
    
    req(dataset())
    req(input$response)
    req(input$year)
    req(input$marsh)
    
    df_plot <- dataset() %>%
      filter(
        year == input$year,
        marsh %in% input$marsh,
        treatment %in% input$groups
      )
    
      r <- input$response
      
      p <- ggplot(
        df_plot,
        aes(x = .data[[r]])
      ) +
        geom_histogram(
          bins = 15,
          color = "black"
        ) +
        facet_wrap(~treatment) +
        labs(
          title = r,
          x = "",
          y = "Count"
        ) +
        theme_minimal()
      
      
      if (r %in% count_variables) {
        
        p <- p +
          scale_x_continuous(
            trans = "log1p"
          )
        
      }
      
      p
      
    })
  
  output$boxplot <- renderPlot({
    
    req(results())
    
    if(!is.null(results()$error)){
      return(NULL)
    }
    
    results()$boxplot
    
  })
  
  output$plot_years_ui <- renderUI({
    
    req(dataset())
    
    years <- sort(unique(dataset()$year))
    
    selectizeInput(
      "plot_years",
      "Years to display",
      choices = years,
      selected = years,
      multiple = TRUE
    )
    
  })
  
  output$anova <- renderTable({
    
    req(results())
    
    if(!is.null(results()$error)){
      
      return(
        data.frame(
          Message = results()$error
        )
      )
      
    }
    
    results()$anova
    
  })
  
  output$emmeans <- renderPrint({
    
    req(results())
    
    if(!is.null(results()$error)){
      cat(results()$error)
      return()
    }
    
    results()$emmeans
    
  })
  
  output$shapiro <- renderPrint({
    results()$shapiro
  })
  
  output$levene <- renderPrint({
    results()$levene
  })
  
  output$qqplot <- renderPlot({
    results()$qqplot
  })
  
  output$residplot <- renderPlot({
    results()$residplot
  })
  
  average_plot_obj <- reactive({
    
    req(plot_data())
    
    plot_average_counts(
      data = plot_data(),
      response = input$response,
      show_means = input$show_means
    )
    
  })
  
  output$average_plot <- renderPlot({
    average_plot_obj()
  })
  
  output$download_average <- downloadHandler(
    
    filename = function() {
      
      paste0(
        input$response,
        "_average_by_year.png"
      )
      
    },
    
    content = function(file){
      
      ggsave(
        file,
        comparison_plot_obj(),
        width = input$bar_fig_width,
        height = input$bar_fig_height,
        device = "pdf"
      )
      
    }
    
  )
  
  output$summary_marsh <- renderText({
    
    if(is.null(input$marsh)){
      return("No marsh selected")
    }
    
    paste(input$marsh, collapse = ", ")
    
  })
  
  output$comparison_year_ui <- renderUI({
    
    req(dataset())
    
    years <- sort(unique(dataset()$year))
    
    selectInput(
      "comparison_year",
      "Year",
      choices = sort(unique(dataset()$year)),
      selected = tail(years, 1)
    )
    
  })
  
  output$comparison_response_ui <- renderUI({
    
    req(dataset())
    
    available_counts <- count_variables[
      count_variables %in% names(dataset())
    ]
    
    selectizeInput(
      "comparison_responses",
      "Count Variables to Compare",
      choices = available_counts,
      selected = available_counts[1],
      multiple = TRUE
    )
    
  })
  
  comparison_plot_obj <- reactive({
    
    req(input$update_comparison)
    
    df <- dataset() %>%
      filter(
        year == input$comparison_year,
        marsh %in% input$marsh,
        treatment %in% input$groups
      )
    
    plot_response_comparison(
      data = df,
      responses = input$comparison_responses,
      show_means = input$show_comparison_means
    )
    
  })
  
  output$response_comparison_plot <- renderPlot({
    comparison_plot_obj()
  })
  
  output$download_comparison <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "Count_Comparison_",
        input$comparison_year,
        ".pdf"
      )
      
    },
    
    content = function(file){
      
      ggsave(
        file,
        comparison_plot_obj(),
        width = input$count_fig_width,
        height = input$count_fig_height,
        device = "pdf"
      )
      
    }
    
  )

  
  output$summary_year <- renderText({
    
    req(input$analysis_type)
    
    if (input$analysis_type == "annual") {
      
      req(input$year)
      paste(input$year)
      
    } else {
      
      yrs <- dataset() %>%
        filter(
          marsh %in% input$marsh,
          !grepl("Pre", age_yr)
        ) %>%
        pull(year) %>%
        as.character() %>%
        unique() %>%
        sort()
      
      paste(yrs, collapse = ", ")
    }
  })
  
  
  output$summary_response <- renderText({
    
    req(input$response)
    
    input$response
    
  })
  
  output$non_param <- renderTable({
    
    req(non_param_results)
    
    non_param_results()
    
  })
  
  output$non_param_posthoc <- renderTable({
    
    req(non_param_posthoc())
    
    non_param_posthoc()
    
  })
  
  output$non_param_interp <- renderUI({
    req(non_param_results)
    
    res <- non_param_results()
    
    paste0(
      "There was a ",
      ifelse(res$p.value < 0.05, "significant", "non-significant"),
      " effect of treatment on ",
      input$response,
      " (",
      res$test,
      ", p = ",
      signif(res$p.value,3),
      ")."
      )
  })
  
  output$interpretation <- renderUI({
    
    req(results())
    
    # Handle failed analysis
    if(!is.null(results()$error)){
      
      return(
        HTML(
          paste0(
            "<h3>Analysis not available</h3>",
            "<p>",
            results()$error,
            "</p>"
          )
        )
      )
      
    }
    
    anova_results <- results()$anova
    
    req(nrow(anova_results) > 0)
    
    # Determine year text
    if (input$analysis_type == "annual") {
      
      year_text <- input$year
      
    } else {
      
      year_text <- dataset() %>%
        filter(marsh %in% input$marsh) %>%
        pull(year) %>%
        unique() %>%
        sort() %>%
        paste(collapse = ", ")
      
    }
    
    interpretations <- lapply(seq_len(nrow(anova_results)), function(i){
      
      effect <- anova_results$term[i]
      p_value <- anova_results$p.value[i]
      
      if (is.na(p_value))
        return(NULL)
      
      # Convert p-value to numeric
      if (grepl("<", p_value)) {
        p_num <- 0.001
      } else {
        p_num <- as.numeric(p_value)
      }
      
      significant <- p_num < 0.05
      
      # Create interpretation
      if (grepl(":", effect)) {
        
        vars <- strsplit(effect, ":")[[1]]
        
        if (significant) {
          
          interpretation <- paste0(
            "There was a significant interaction between <b>",
            vars[1], "</b> and <b>", vars[2],
            "</b> affecting <b>", input$response,
            "</b> (<i>p = ", p_value, "</i>)."
          )
          
        } else {
          
          interpretation <- paste0(
            "There was no significant interaction between <b>",
            vars[1], "</b> and <b>", vars[2],
            "</b> affecting <b>", input$response,
            "</b> (<i>p = ", p_value, "</i>)."
          )
          
        }
        
      } else {
        
        if (significant) {
          
          interpretation <- paste0(
            "There was a significant effect of <b>",
            effect,
            "</b> on <b>",
            input$response,
            "</b> (<i>p = ", p_value, "</i>)."
          )
          
        } else {
          
          interpretation <- paste0(
            "There was no significant effect of <b>",
            effect,
            "</b> on <b>",
            input$response,
            "</b> (<i>p = ", p_value, "</i>)."
          )
          
        }
        
      }
      
      HTML(
        paste0(
          "<p><b>",
          effect,
          ":</b> ",
          interpretation,
          "</p>"
        )
      )
      
    })
    
    tagList(
      
      h3("Key Findings"),
      
      HTML(
        paste0(
          "<p>Results for <b>",
          input$response,
          "</b> (",
          year_text,
          ").</p>"
        )
      ),
      
      interpretations
      
    )
    
  })
  output$emmeans_interpretation <- renderUI({
    
    req(results())
    
    # If model failed, don't try to interpret emmeans
    if(!is.null(results()$error)){
      
      return(
        HTML(
          paste0(
            "<h3>Pairwise comparisons unavailable</h3>",
            "<p>",
            results()$error,
            "</p>"
          )
        )
      )
      
    }
    
    # If no emmeans were generated
    if(is.null(results()$emmeans)){
      
      return(
        HTML(
          "<h3>Pairwise comparisons unavailable</h3>"
        )
      )
      
    }
    
    pairs <- as.data.frame(results()$emmeans$contrasts)
    
    if(!"p.value" %in% names(pairs)){
      
      return(
        HTML(
          "<h3>No pairwise comparisons available</h3>"
        )
      )
      
    }
    
    sig <- pairs %>%
      filter(p.value < 0.05)
    
    if(input$analysis_type == "annual"){
      year_text <- input$year
    } else {
      year_text <- NULL
    }
    
    # singular fit warning
    if(isTRUE(results()$singular)){
      
      warning_text <- paste0(
        "<div class='alert alert-warning'>",
        "<b>Model warning:</b> The statistical model had a singular fit. ",
        "This means that one or more random effects contributed little variation, ",
        "so results should be interpreted cautiously. It means after accounting for 
        the other variables (treatment and year, plots did not differ substantially.",
        "</div>"
      )
      
    } else {
      
      warning_text <- ""
      
    }
    
    
    # no significant results
    if(nrow(sig) == 0){
      
      year_text <- if(input$analysis_type == "annual"){
        input$year
      } else {
        "the selected years"
      }
      
      return(
        HTML(
          paste0(
            warning_text,
            "<p>No pairwise treatment differences were statistically significant in <b>",
            year_text,
            "</b>.</p>"
          )
        )
      )
    }
    
    
    bullets <- apply(sig, 1, function(x){
      
      groups <- strsplit(x["contrast"], " - ")[[1]]
      
      if(input$analysis_type == "annual"){
        
        text <- paste0(
          "<li><b>",
          groups[1],
          "</b> differed significantly from <b>",
          groups[2],
          "</b> (p = ",
          signif(as.numeric(x["p.value"]), 3),
          ").</li>"
        )
        
      } else {
        
        text<- paste0(
          "<li>In <b>", x["year"], "</b>, <b>",
          groups[1],
          "</b> differed significantly from <b>",
          groups[2],
          "</b> (p = ",
          signif(as.numeric(x["p.value"]), 3),
          ").</li>"
        )
        
      }
      
      text
      
    })
      
      HTML(
        paste0(
          warning_text,
          "<h3>Pairwise Comparisons</h3>",
          "<ul>",
          paste(bullets, collapse = ""),
          "</ul>"
        )
      )
  })
  
    
  results <- eventReactive(input$run, {
    
    req(input$response)
    req(input$groups)
    req(input$marsh)
    
    df <- dataset() %>%
      filter(
        marsh %in% input$marsh,
        treatment %in% input$groups
      )
    
    if(input$analysis_type =="annual"){
      
      req(input$year)
    
      if (input$response %in% count_variables) {
        
        out <- run_annual_glm(
          data = dataset(),
          response = input$response,
          groups = input$groups,
          season_year = input$year,
          marsh = input$marsh
        )
        
      } else {
        
        out <- run_annual_analysis(
          data = dataset(),
          response = input$response,
          groups = input$groups,
          season_year = input$year,
          marsh = input$marsh
        )
        
      }
      
    } else {
      
      if (input$response %in% count_variables) {
        
        out <- run_time_glm(
          data = dataset(),
          response = input$response,
          groups = input$groups,
          marsh = input$marsh
        )
        
      } else {
        
        out <- run_time_analysis(
          data = dataset(),
          response = input$response,
          groups = input$groups,
          marsh = input$marsh
        )
        
      }
      
    }
    
    return(out)
 })
  
  non_param_results <- eventReactive(input$run_nonparam, {
    
    req(input$response)
    req(input$groups)
    req(input$marsh)
    
    data <- dataset() %>%
      filter(
        marsh %in% input$marsh,
        treatment %in% input$groups
      )
    
    
    response <- input$response
    
    
    # Across years = compare years as independent groups
    if(input$analysis_type == "time"){
      
      formula <- as.formula(
        paste(response, "~ year")
      )
      
      result <- kruskal.test(
        formula,
        data = data
      )
      
      return(
        data.frame(
          term = "year",
          statistic = unname(result$statistic),
          df = unname(result$parameter),
          p.value = format.pval(
            result$p.value,
            digits = 3,
            eps = 0.001
          ),
          test = "Kruskal-Wallis by year"
        )
      )
      
    }
    
    
    # Single year = compare treatments
    
    formula <- as.formula(
      paste(response, "~ treatment")
    )
    
    
    n_groups <- data %>%
      pull(treatment) %>%
      unique() %>%
      length()
    
    
    if(n_groups == 2){
      
      result <- wilcox.test(
        formula,
        data = data
      )
      
      data.frame(
        term = "treatment",
        statistic = unname(result$statistic),
        p.value = format.pval(
          result$p.value,
          digits = 3,
          eps = 0.001
        ),
        test = "Wilcoxon rank-sum"
      )
      
      
    } else {
      
      result <- kruskal.test(
        formula,
        data = data
      )
      
      data.frame(
        term = "treatment",
        statistic = unname(result$statistic),
        df = unname(result$parameter),
        p.value = format.pval(
          result$p.value,
          digits = 3,
          eps = 0.001
        ),
        test = "Kruskal-Wallis"
      )
      
    }
    
  })
  
  non_param_posthoc <- eventReactive(input$run_nonparam, {
    
    req(input$response)
    req(input$groups)
    req(input$marsh)
    
    data <- dataset() %>%
      filter(
        marsh %in% input$marsh,
        treatment %in% input$groups
      )
    
    response <- input$response
    
    formula <- as.formula(
      paste(response, "~ treatment")
    )
    
    
    n_groups <- data %>%
      pull(treatment) %>%
      unique() %>%
      length()
    
    
    # No posthoc for two groups
    if(n_groups == 2){
      
      return(NULL)
      
    }
    
    
    # Dunn post hoc after Kruskal-Wallis
    
    dunn <- FSA::dunnTest(
      formula,
      data = data,
      method = "holm"
    )
    
    
    dunn$res <- dunn$res %>% mutate(
      p.value = format.pval(P.adj, digits = 3, eps = 0.001)
    )
    
  })
  
  
  plot_data <- eventReactive(input$update_plots, {
    
    req(input$plot_years)
    
    dataset() %>%
      filter(
        year %in% input$plot_years,
        marsh %in% input$marsh,
        treatment %in% input$groups
      )
    
  })
  
}
# Run App 

shinyApp(ui = ui, server = server)
