# make a similar app for SSIM?
## do we want to get rid of interp tab and edit the ANOVA tab

# Required libraries loading into the project:
library(shiny)
library(tidyverse)
library(car)
library(lmerTest)
library(emmeans)
library(janitor)
library(lubridate)
library(FSA)

# Point to files containing the analysis and data cleaning functions.  
source("app_functions.R")


# Naming of R-cleaned variables from the data sheet. 
#Continuous variables should include things like percentages and measurements.
continuous_variables <- c(
  "total_cover",
  "unveg",
  "avg_spal_h_cm",
  "avg_spal_d_mm"  
                    # <- add new variables here
)

# Naming of R-cleaned variables from the data sheet. 
#Count variables should include counts and presence/absence.
count_variables <- c(
  "crab_bur",
  "mussel_count",
  "snail_count",
  "spal_stem_count",
  "sppa_stem_count",
  "disp_stem_count",
  "phau_stem_count"
                    # <- add new variables here
)

# An umbrella category when all response variables above are included
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
                 
              h3("Model-Adjusted Means"),
              
              uiOutput("emmeans_summary"),
              
              uiOutput("emmeans_table"),
              
              uiOutput("emmeans_contrasts"),
              
              h4("How to interpret these results"),
              
              tags$ul(
                tags$li(strong("$emmeans, emmean:"), "The estimated marginal mean (also called the least-squares mean). This is the model-predicted mean for that group after accounting for the other variables in the model."),
                tags$li(strong("$emmeans, lower.Cl, upper.CL:"), "Lower and upper bounds of the 95% confidence interval."),
                tags$li(strong("$contrasts, estimate:"), "The estimated difference between the two group means. A negative value means the first group has a lower mean than the second; a positive value means the first group has a higher mean.."),
                tags$li(strong("$contrasts, t.ratio:"), "The test statistic. It is the estimated difference divided by its standard error: estimate/SE. Larger absolute values indicate stronger evidence that the groups differ."),
                tags$li(strong("$contrasts, p.value:"), "The probability of observing a difference at least this extreme if the true difference between the groups were zero. This is often adjusted for multiple comparisons (e.g., Tukey adjustment).")
                )),
      
        tabPanel("Interpretation",
                 uiOutput("interpretation"),
                 uiOutput("emmeans_interpretation")
        ),
      
        tabPanel("Non-parametric tests",
                 actionButton("run_nonparam", "Run Non-Parametric Analysis"),
                 
                 br(),
                 
                 h3("Overall Test"),
                 
                 uiOutput("non_param_interp"),
                 
                 h3("Pairwise Comparisons"),
                 
                 uiOutput("non_param_posthoc"),
                 
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
    
    req(input$comparison_year)
    
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
    
    req(non_param_results())
    
    res <- non_param_results()
    overall <- res$overall
    
    p <- overall$p.value
    
    # Determine whether result is significant
    significant <- p < 0.05
    
    # Label the effect
    effect_label <- if (overall$term == "year") {
      "Year effect"
    } else {
      "Treatment effect"
    }
    
    # Test statistic formatting
    if (overall$test == "Kruskal-Wallis by year" ||
        overall$test == "Kruskal-Wallis") {
      
      statistic_text <- paste0(
        "Kruskal-Wallis χ² = ",
        round(overall$statistic, 2),
        ", df = ",
        overall$df
      )
      
    } else {
      
      statistic_text <- paste0(
        overall$test,
        ", statistic = ",
        round(overall$statistic, 2)
      )
    }
    
    HTML(
      paste0(
        
        "<div style='padding:15px;
                  border-radius:8px;
                  background:#f5f5f5;
                  margin-bottom:15px;'>",
        
        "<h4 style='margin-top:0;'>",
        effect_label,
        ": ",
        if (significant) {
          "<span style='color:#2e7d32;'>Significant</span>"
        } else {
          "<span style='color:#666;'>Not significant</span>"
        },
        "</h4>",
        
        "<p style='margin-bottom:0;'>",
        statistic_text,
        ", <strong>p = ",
        format_p(p),
        "</strong>",
        "</p>",
        
        "</div>"
      )
    )
  })
  
  output$dunn_table <- renderTable({
    req(non_param_results())
    
    res <- non_param_results()
    
    if(is.null(res$dunn)){
      return(NULL)
    }
    
    dunn <- res$dunn
    
    dunn %>% transmute(
      Comparison = Comparison,
      `Adjusted p` = P.adj
    )
    
})
  
  output$non_param_posthoc <- renderUI({
    
    req(non_param_results())
    
    res <- non_param_results()
    
    # No Dunn test was performed
    if (is.null(res$dunn)) {
      
      if (res$overall$p.value >= 0.05) {
        
        return(
          HTML(
            "<p style='color:#666;'>
          No post-hoc test was performed because the overall test was not significant.
          </p>"
          )
        )
        
      } else {
        
        return(
          HTML(
            "<p style='color:#666;'>
          No pairwise comparisons are available for this analysis.
          </p>"
          )
        )
      }
    }
    
    dunn <- res$dunn
    
    # Keep only significant comparisons
    sig <- dunn %>%
      filter(P.adj < 0.05) %>%
      transmute(
        Comparison = Comparison,
        Z = round(Z, 2),
        `Adjusted p` = sapply(P.adj, format_p)
      )
    
    # No significant pairwise differences
    if (nrow(sig) == 0) {
      
      return(
        HTML(
          paste0(
            "<p style='color:#666;'>",
            "The overall test was significant, but no individual ",
            "pairwise differences remained significant after Holm correction.",
            "</p>"
          )
        )
      )
    }
    
    # Create compact HTML table
    table_html <- paste0(
      
      "<table class='table table-sm table-striped'>",
      
      "<thead>",
      "<tr>",
      "<th>Comparison</th>",
      "<th>Z</th>",
      "<th>Adjusted p</th>",
      "</tr>",
      "</thead>",
      
      "<tbody>",
      
      paste0(
        apply(sig, 1, function(x) {
          
          p_text <- x[["Adjusted p"]]
          
          p_style <- if (
            p_text == "<0.001" ||
            suppressWarnings(as.numeric(p_text)) < 0.05
          ) {
            "font-weight:bold;"
          } else {
            ""
          }
          
          paste0(
            "<tr>",
            "<td>", x[["Comparison"]], "</td>",
            "<td>", x[["Z"]], "</td>",
            "<td style='", p_style, "'>",
            p_text,
            "</td>",
            "</tr>"
          )
          
        }),
        collapse = ""
      ),
      
      "</tbody>",
      "</table>"
    )
    
    HTML(
      paste0(
        
        "<p style='color:#666;'>",
        "Significant pairwise differences after Holm correction:",
        "</p>",
        
        table_html
      )
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
  
  
  output$emmeans_summary <- renderUI({
    
    req(results())
    
    # Handle failed analysis
    if (!is.null(results()$error)) {
      
      return(
        HTML(
          paste0(
            "<div style='padding:15px;
                      border-radius:8px;
                      background:#f8d7da;
                      border:1px solid #f5c2c7;
                      margin-bottom:15px;'>",
            "<strong>Estimated marginal means unavailable</strong><br>",
            results()$error,
            "</div>"
          )
        )
      )
    }
    
    if (is.null(results()$emmeans)) {
      
      return(
        HTML(
          "<div style='padding:15px;
                    border-radius:8px;
                    background:#f5f5f5;
                    margin-bottom:15px;'>
          Estimated marginal means are not available for this analysis.
        </div>"
        )
      )
    }
    
    emm <- as.data.frame(results()$emmeans$emmeans)
    
    # Identify confidence interval columns
    lower_col <- intersect(
      c("lower.CL", "asymp.LCL"),
      names(emm)
    )[1]
    
    upper_col <- intersect(
      c("upper.CL", "asymp.UCL"),
      names(emm)
    )[1]
    
    mean_col <- if ("emmean" %in% names(emm)) {
      "emmean"
    } else {
      "response"
    }
    
    # Build a compact summary
    HTML(
      paste0(
        
        "<div style='padding:15px;
                  border-radius:8px;
                  background:#f5f5f5;
                  margin-bottom:15px;'>",
        
        "<h4 style='margin-top:0;'>",
        "Model-adjusted means for <strong>",
        input$response,
        "</strong>",
        "</h4>",
        
        "<p style='margin-bottom:0;color:#666;'>",
        "Estimated marginal means account for the other variables included in the statistical model.",
        "</p>",
        
        "</div>"
      )
    )
  })
  
  
  output$emmeans_table <- renderUI({
    
    req(results())
    
    if (!is.null(results()$error) ||
        is.null(results()$emmeans)) {
      return(NULL)
    }
    
    emm <- as.data.frame(results()$emmeans$emmeans)
    
    # Identify columns
    mean_col <- if ("emmean" %in% names(emm)) {
      "emmean"
    } else if ("response" %in% names(emm)) {
      "response"
    } else {
      NULL
    }
    
    lower_col <- intersect(
      c("lower.CL", "asymp.LCL"),
      names(emm)
    )[1]
    
    upper_col <- intersect(
      c("upper.CL", "asymp.UCL"),
      names(emm)
    )[1]
    
    # Find grouping columns
    grouping_cols <- setdiff(
      names(emm),
      c(
        mean_col,
        "SE",
        "df",
        lower_col,
        upper_col
      )
    )
    
    # Construct display table
    display <- emm
    
    if (!is.null(mean_col)) {
      
      display$Estimate <- round(
        as.numeric(display[[mean_col]]),
        2
      )
    }
    
    if (!is.null(lower_col) &&
        !is.null(upper_col)) {
      
      display$`95% CI` <- paste0(
        round(
          as.numeric(display[[lower_col]]),
          2
        ),
        " – ",
        round(
          as.numeric(display[[upper_col]]),
          2
        )
      )
    }
    
    # Keep grouping variables + estimate + CI
    keep_cols <- c(
      grouping_cols,
      "Estimate",
      if ("95% CI" %in% names(display)) "95% CI"
    )
    
    keep_cols <- keep_cols[
      keep_cols %in% names(display)
    ]
    
    display <- display[, keep_cols, drop = FALSE]
    
    # Make grouping column names readable
    names(display) <- gsub(
      "_",
      " ",
      names(display)
    )
    
    names(display) <- tools::toTitleCase(
      names(display)
    )
    
    table_html <- paste0(
      
      "<table class='table table-sm table-striped'>",
      
      "<thead><tr>",
      
      paste0(
        "<th>",
        names(display),
        "</th>",
        collapse = ""
      ),
      
      "</tr></thead>",
      
      "<tbody>",
      
      paste0(
        apply(display, 1, function(x) {
          
          paste0(
            "<tr>",
            paste0(
              "<td>",
              x,
              "</td>",
              collapse = ""
            ),
            "</tr>"
          )
          
        }),
        collapse = ""
      ),
      
      "</tbody></table>"
    )
    
    HTML(table_html)
  })

  
  output$emmeans_contrasts <- renderUI({
    
    req(results())
    
    # ============================================================
    # Check for model errors
    # ============================================================
    
    if (!is.null(results()$error)) {
      
      return(
        HTML(
          paste0(
            "<div style='padding:15px;
                      background:#f8d7da;
                      border:1px solid #f5c2c7;
                      border-radius:8px;
                      margin-bottom:20px;'>",
            
            "<strong>Pairwise comparisons unavailable</strong><br>",
            results()$error,
            
            "</div>"
          )
        )
      )
    }
    
    
    # ============================================================
    # Check that EMMs and contrasts exist
    # ============================================================
    
    if (
      is.null(results()$emmeans) ||
      is.null(results()$emmeans$contrasts)
    ) {
      
      return(
        HTML(
          "<div style='padding:15px;
                    background:#f5f5f5;
                    border:1px solid #ddd;
                    border-radius:8px;
                    margin-bottom:20px;'>
          Pairwise comparisons are not available for this analysis.
        </div>"
        )
      )
    }
    
    
    # ============================================================
    # Extract contrasts
    # ============================================================
    
    pairs <- as.data.frame(
      results()$emmeans$contrasts
    )
    
    
    # ============================================================
    # Check for empty contrasts
    # ============================================================
    
    if (nrow(pairs) == 0) {
      
      return(
        HTML(
          "<div style='padding:15px;
                    background:#f5f5f5;
                    border:1px solid #ddd;
                    border-radius:8px;
                    margin-bottom:20px;'>
          No pairwise comparisons are available.
        </div>"
        )
      )
    }
    
    
    # ============================================================
    # Check p-value column
    # ============================================================
    
    if (!"p.value" %in% names(pairs)) {
      
      return(
        HTML(
          "<div style='padding:15px;
                    background:#f5f5f5;
                    border:1px solid #ddd;
                    border-radius:8px;
                    margin-bottom:20px;'>
          Pairwise comparisons were generated, but no p-values were returned.
        </div>"
        )
      )
    }
    
    
    # ============================================================
    # Determine whether year is present
    # ============================================================
    
    has_year <- "year" %in% names(pairs)
    
    
    # ============================================================
    # Format p-value
    # ============================================================
    
    format_p <- function(p) {
      
      if (is.na(p)) {
        return("")
      }
      
      if (p < 0.001) {
        return("&lt;0.001")
      }
      
      formatC(
        p,
        format = "f",
        digits = 3
      )
    }
    
    
    # ============================================================
    # Create table rows directly from pairs
    # ============================================================
    
    rows <- lapply(
      seq_len(nrow(pairs)),
      function(i) {
        
        p <- pairs$p.value[i]
        
        significant <- !is.na(p) && p < 0.05
        
        
        # ------------------------------------------
        # Row background
        # ------------------------------------------
        
        row_style <- if (significant) {
          
          "background-color:#f0f8f2;"
          
        } else {
          
          ""
        }
        
        
        # ------------------------------------------
        # Year
        # ------------------------------------------
        
        year_cell <- ""
        
        if (has_year) {
          
          year_cell <- paste0(
            "<td><strong>",
            pairs$year[i],
            "</strong></td>"
          )
          
        } else if (
          input$analysis_type == "annual" &&
          !is.null(input$year)
        ) {
          
          year_cell <- paste0(
            "<td><strong>",
            input$year,
            "</strong></td>"
          )
        }
        
        
        # ------------------------------------------
        # Comparison
        # ------------------------------------------
        
        comparison_cell <- ""
        
        if ("contrast" %in% names(pairs)) {
          
          comparison_cell <- paste0(
            "<td>",
            pairs$contrast[i],
            "</td>"
          )
        }
        
        
        # ------------------------------------------
        # Estimate
        # ------------------------------------------
        
        estimate_cell <- ""
        
        if ("estimate" %in% names(pairs)) {
          
          estimate_cell <- paste0(
            "<td>",
            round(
              as.numeric(pairs$estimate[i]),
              2
            ),
            "</td>"
          )
        }
        
        
        # ------------------------------------------
        # SE
        # ------------------------------------------
        
        se_cell <- ""
        
        if ("SE" %in% names(pairs)) {
          
          se_cell <- paste0(
            "<td>",
            round(
              as.numeric(pairs$SE[i]),
              2
            ),
            "</td>"
          )
        }
        
        
        # ------------------------------------------
        # df
        # ------------------------------------------
        
        df_cell <- ""
        
        if ("df" %in% names(pairs)) {
          
          df_cell <- paste0(
            "<td>",
            round(
              as.numeric(pairs$df[i]),
              1
            ),
            "</td>"
          )
        }
        
        
        # ------------------------------------------
        # p-value
        # ------------------------------------------
        
        p_display <- format_p(p)
        
        
        p_cell <- if (significant) {
          
          paste0(
            "<td style='
              font-weight:bold;
              color:#2e7d32;
          '>",
            
            p_display,
            
            "</td>"
          )
          
        } else {
          
          paste0(
            "<td style='color:#666;'>",
            p_display,
            "</td>"
          )
        }
        
        
        # ------------------------------------------
        # Complete row
        # ------------------------------------------
        
        paste0(
          
          "<tr style='",
          row_style,
          "'>",
          
          year_cell,
          
          comparison_cell,
          
          estimate_cell,
          
          se_cell,
          
          df_cell,
          
          p_cell,
          
          "</tr>"
        )
      }
    )
    
    
    # ============================================================
    # Determine table columns
    # ============================================================
    
    header <- ""
    
    if (has_year ||
        input$analysis_type == "annual") {
      
      header <- paste0(
        header,
        "<th>Year</th>"
      )
    }
    
    
    if ("contrast" %in% names(pairs)) {
      
      header <- paste0(
        header,
        "<th>Comparison</th>"
      )
    }
    
    
    if ("estimate" %in% names(pairs)) {
      
      header <- paste0(
        header,
        "<th>Estimate</th>"
      )
    }
    
    
    if ("SE" %in% names(pairs)) {
      
      header <- paste0(
        header,
        "<th>SE</th>"
      )
    }
    
    
    if ("df" %in% names(pairs)) {
      
      header <- paste0(
        header,
        "<th>df</th>"
      )
    }
    
    
    header <- paste0(
      header,
      "<th>Adjusted p</th>"
    )
    
    
    # ============================================================
    # Count significant comparisons
    # ============================================================
    
    n_sig <- sum(
      pairs$p.value < 0.05,
      na.rm = TRUE
    )
    
    n_total <- sum(
      !is.na(pairs$p.value)
    )
    
    
    # ============================================================
    # Summary
    # ============================================================
    
    if (n_sig > 0) {
      
      summary_text <- paste0(
        
        "<p style='margin-bottom:8px;'>",
        
        "<strong style='color:#2e7d32;font-size:16px;'>",
        n_sig,
        " significant",
        "</strong>",
        
        " of ",
        n_total,
        " pairwise comparisons.",
        
        "</p>"
      )
      
    } else {
      
      summary_text <- paste0(
        
        "<p style='margin-bottom:8px;color:#666;'>",
        
        "No significant pairwise comparisons.",
        
        "</p>"
      )
    }
    
    
    # ============================================================
    # Final UI
    # ============================================================
    
    HTML(
      paste0(
        
        "<div style='
        padding:18px;
        background:#f8f9fa;
        border:1px solid #dee2e6;
        border-radius:10px;
        margin-bottom:20px;
      '>",
        
        "<h4 style='
        margin-top:0;
        margin-bottom:10px;
      '>
        Pairwise Comparisons
      </h4>",
        
        summary_text,
        
        "<p style='
        color:#666;
        font-size:13px;
        margin-bottom:15px;
      '>
        All pairwise comparisons are shown. 
        Significant comparisons are highlighted in green.
      </p>",
        
        "<div style='overflow-x:auto;'>",
        
        "<table class='table table-sm table-striped'
             style='margin-bottom:0;'>",
        
        "<thead>",
        "<tr>",
        header,
        "</tr>",
        "</thead>",
        
        "<tbody>",
        paste(
          rows,
          collapse = ""
        ),
        "</tbody>",
        
        "</table>",
        
        "</div>",
        
        "</div>"
      )
    )
  })
  
  
  
  
  output$emmeans_interpretation <- renderUI({
    
    req(results())
    
    # ------------------------------------------------------------
    # Handle model failure
    # ------------------------------------------------------------
    
    if (!is.null(results()$error)) {
      
      return(
        HTML(
          paste0(
            "<div style='padding:15px;
                      border-radius:8px;
                      background:#f8d7da;
                      border:1px solid #f5c2c7;
                      margin-bottom:15px;'>",
            
            "<strong>Pairwise comparisons unavailable</strong><br>",
            results()$error,
            
            "</div>"
          )
        )
      )
    }
    
    
    # ------------------------------------------------------------
    # Check that pairwise comparisons exist
    # ------------------------------------------------------------
    
    if (is.null(results()$emmeans)) {
      
      return(
        HTML(
          "<div style='padding:15px;
                    border-radius:8px;
                    background:#f5f5f5;
                    margin-bottom:15px;'>
          Pairwise comparisons are not available for this analysis.
        </div>"
        )
      )
    }
    
    
    pairs <- as.data.frame(
      results()$emmeans$contrasts
    )
    
    if (!"p.value" %in% names(pairs)) {
      
      return(
        HTML(
          "<div style='padding:15px;
                    border-radius:8px;
                    background:#f5f5f5;
                    margin-bottom:15px;'>
          No pairwise comparisons are available.
        </div>"
        )
      )
    }
    
    
    # ------------------------------------------------------------
    # Determine significant comparisons
    # ------------------------------------------------------------
    
    sig <- pairs %>%
      filter(p.value < 0.05)
    
    
    # ------------------------------------------------------------
    # Model warning
    # ------------------------------------------------------------
    
    warning_text <- ""
    
    if (isTRUE(results()$singular)) {
      
      warning_text <- paste0(
        
        "<div style='padding:12px;
                  border-radius:8px;
                  background:#fff3cd;
                  border:1px solid #ffecb5;
                  margin-bottom:15px;'>",
        
        "<strong>Model warning:</strong> ",
        "The statistical model had a singular fit. ",
        "One or more random effects contributed little variation, ",
        "so pairwise comparisons should be interpreted cautiously.",
        
        "</div>"
      )
    }
    
    
    # ------------------------------------------------------------
    # No significant comparisons
    # ------------------------------------------------------------
    
    if (nrow(sig) == 0) {
      
      return(
        HTML(
          paste0(
            
            warning_text,
            
            "<div style='padding:15px;
                      border-radius:8px;
                      background:#f5f5f5;
                      margin-bottom:15px;'>",
            
            "<h4 style='margin-top:0;'>",
            "<span style='color:#666;'>No significant differences</span>",
            "</h4>",
            
            "<p style='margin-bottom:0;color:#666;'>",
            "No pairwise treatment differences were statistically significant ",
            "after adjustment for multiple comparisons.",
            "</p>",
            
            "</div>"
          )
        )
      )
    }
    
    
    # ------------------------------------------------------------
    # Create table
    # ------------------------------------------------------------
    
    table_html <- paste0(
      
      "<table class='table table-sm table-striped'>",
      
      "<thead>",
      "<tr>",
      
      # Add year column only for across-year analysis
      if (input$analysis_type == "time") {
        "<th>Year</th>"
      } else {
        ""
      },
      
      "<th>Comparison</th>",
      "<th>Estimate</th>",
      "<th>Adjusted p</th>",
      
      "</tr>",
      "</thead>",
      
      "<tbody>",
      
      paste0(
        apply(sig, 1, function(x) {
          
          p <- as.numeric(x["p.value"])
          
          p_display <- if (p < 0.001) {
            "<0.001"
          } else {
            formatC(
              p,
              format = "f",
              digits = 3
            )
          }
          
          
          # Determine year
          if (input$analysis_type == "annual") {
            
            year_value <- input$year
            
          } else if ("year" %in% names(sig)) {
            
            year_value <- x["year"]
            
          } else {
            
            year_value <- ""
          }
          
          
          # Estimate
          estimate_value <- ""
          
          if ("estimate" %in% names(sig)) {
            
            estimate_value <- round(
              as.numeric(x["estimate"]),
              2
            )
          }
          
          
          paste0(
            
            "<tr>",
            
            if (input$analysis_type == "time") {
              paste0(
                "<td><strong>",
                year_value,
                "</strong></td>"
              )
            } else {
              ""
            },
            
            "<td>",
            x["contrast"],
            "</td>",
            
            "<td>",
            estimate_value,
            "</td>",
            
            "<td style='font-weight:bold;color:#2e7d32;'>",
            p_display,
            "</td>",
            
            "</tr>"
          )
          
        }),
        collapse = ""
      ),
      
      "</tbody>",
      "</table>"
    )
    
    
    # ------------------------------------------------------------
    # Interpretation text
    # ------------------------------------------------------------
    
    interpretation_text <- paste0(
      
      "<ul>",
      
      paste0(
        apply(sig, 1, function(x) {
          
          p <- as.numeric(x["p.value"])
          
          p_display <- if (p < 0.001) {
            "<0.001"
          } else {
            formatC(
              p,
              format = "f",
              digits = 3
            )
          }
          
          
          # Determine year
          if (input$analysis_type == "annual") {
            
            year_value <- input$year
            
          } else if ("year" %in% names(sig)) {
            
            year_value <- x["year"]
            
          } else {
            
            year_value <- "the selected year"
          }
          
          
          groups <- strsplit(
            x["contrast"],
            " - "
          )[[1]]
          
          
          paste0(
            
            "<li>",
            
            "<strong>",
            year_value,
            ":</strong> ",
            
            "<strong>",
            groups[1],
            "</strong> differed significantly from ",
            
            "<strong>",
            groups[2],
            "</strong> ",
            
            "(adjusted p = ",
            p_display,
            ").",
            
            "</li>"
          )
          
        }),
        collapse = ""
      ),
      
      "</ul>"
    )
    
    
    # ------------------------------------------------------------
    # Final output
    # ------------------------------------------------------------
    
    HTML(
      paste0(
        
        warning_text,
        
        "<div style='padding:15px;
                  border-radius:8px;
                  background:#f0f8f2;
                  border:1px solid #c8e6c9;
                  margin-bottom:15px;'>",
        
        "<h4 style='margin-top:0;color:#2e7d32;'>",
        "Significant Pairwise Differences",
        "</h4>",
        
        "<p style='color:#666;'>",
        "The following treatment comparisons remained significant ",
        "after multiple-comparison adjustment.",
        "</p>",
        
        interpretation_text,
        
        table_html,
        
        "</div>"
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
          marsh_type = input$marsh
        )
        
      } else {
        
        out <- run_annual_analysis(
          data = dataset(),
          response = input$response,
          groups = input$groups,
          season_year = input$year,
          marsh_type = input$marsh
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
    
    
    # ============================================================
    # Across years = compare years as independent groups
    # ============================================================
    
    if (input$analysis_type == "time") {
      
      formula <- as.formula(
        paste(response, "~ year")
      )
      
      # Kruskal-Wallis test
      result <- kruskal.test(
        formula,
        data = data
      )
      
      # Dunn's post-hoc test
      dunn_result <- NULL
      
      if (result$p.value < 0.05) {
        
        dunn_result <- FSA::dunnTest(
          x = data[[response]],
          g = as.factor(data$year),
          method = "holm"
        )$res
      }
      
      return(
        list(
          overall = data.frame(
            term = "year",
            statistic = unname(result$statistic),
            df = unname(result$parameter),
            p.value = result$p.value,
            test = "Kruskal-Wallis by year"
          ),
          dunn = dunn_result
        )
      )
    }
    
    
    # ============================================================
    # Single year = compare treatments
    # ============================================================
    
    formula <- as.formula(
      paste(response, "~ treatment")
    )
    
    n_groups <- data %>%
      pull(treatment) %>%
      unique() %>%
      length()
    
    
    # ------------------------------------------------------------
    # Two treatment groups = Wilcoxon rank-sum
    # ------------------------------------------------------------
    
    if (n_groups == 2) {
      
      result <- wilcox.test(
        formula,
        data = data
      )
      
      return(
        list(
          overall = data.frame(
            term = "treatment",
            statistic = unname(result$statistic),
            p.value = result$p.value,
            test = "Wilcoxon rank-sum"
          ),
          dunn = NULL
        )
      )
    }
    
    
    # ------------------------------------------------------------
    # More than two treatment groups = Kruskal-Wallis + Dunn
    # ------------------------------------------------------------
    
    result <- kruskal.test(
      formula,
      data = data
    )
    
    dunn_result <- NULL
    
    if (result$p.value < 0.05) {
      
      dunn_result <- FSA::dunnTest(
        x = data[[response]],
        g = as.factor(data$treatment),
        method = "holm"
      )$res
    }
    
    return(
      list(
        overall = data.frame(
          term = "treatment",
          statistic = unname(result$statistic),
          df = unname(result$parameter),
          p.value = result$p.value,
          test = "Kruskal-Wallis"
        ),
        dunn = dunn_result
      )
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
