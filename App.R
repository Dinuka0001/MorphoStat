# =========================================================
# MorphoStat - Modern Morphological Analysis Shiny App
# Version 1.0 | © 2025 Dinuka Adasooriya
# =========================================================

library(shiny)
library(shinydashboard)
library(geomorph)
library(ggplot2)
library(RRPP)
library(colourpicker)
library(rgl)
library(DT)
library(shinyjs)
library(bslib)
library(vegan)
library(Morpho)
library(rstatix)
library(scatterplot3d)

# Increase maximum file upload size to 100MB (for large project files)
options(shiny.maxRequestSize = 100 * 1024^2)

# =========================================================
# Custom CSS for Modern Professional Styling
# =========================================================
custom_css <- "
  /* Card and panel enhancements */
  .card {
    border: none;
    border-radius: 0.75rem;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08);
    transition: box-shadow 0.2s ease;
  }
  .card:hover {
    box-shadow: 0 6px 20px rgba(0, 0, 0, 0.12);
  }
  .card-header {
    background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
    font-weight: 500;
    padding: 1rem 1.25rem;
  }
  
  /* Well panel modern styling */
  .well {
    background: linear-gradient(180deg, #ffffff 0%, #f8f9fa 100%);
    border: 1px solid #e9ecef;
    border-radius: 0.75rem;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.05);
    padding: 1.25rem;
  }
  
  /* Sidebar styling */
  .sidebar-section {
    background: white;
    border-radius: 0.5rem;
    padding: 1rem;
    margin-bottom: 1rem;
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
  }
  
  /* Button enhancements */
  .btn-primary {
    background: linear-gradient(135deg, #2980b9 0%, #3498db 100%);
    border: none;
    transition: all 0.2s ease;
  }
  .btn-primary:hover {
    background: linear-gradient(135deg, #1f6591 0%, #2980b9 100%);
    transform: translateY(-1px);
    box-shadow: 0 4px 12px rgba(41, 128, 185, 0.4);
  }
  .btn-success {
    background: linear-gradient(135deg, #27ae60 0%, #2ecc71 100%);
    border: none;
  }
  .btn-success:hover {
    background: linear-gradient(135deg, #1e8449 0%, #27ae60 100%);
    transform: translateY(-1px);
  }
  
  /* Tab styling */
  .nav-tabs .nav-link {
    border-radius: 0.5rem 0.5rem 0 0;
    font-weight: 500;
    padding: 0.75rem 1.25rem;
  }
  .nav-tabs .nav-link.active {
    background: linear-gradient(180deg, #ffffff 0%, #f8f9fa 100%);
    border-bottom-color: transparent;
  }
  
  /* Input styling */
  .form-control, .form-select {
    border-radius: 0.5rem;
    border: 1px solid #dee2e6;
    transition: border-color 0.2s ease, box-shadow 0.2s ease;
  }
  .form-control:focus, .form-select:focus {
    border-color: #3498db;
    box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.15);
  }
  
  /* Slider styling */
  .irs--shiny .irs-bar {
    background: linear-gradient(90deg, #2980b9 0%, #3498db 100%);
  }
  .irs--shiny .irs-handle {
    border-color: #2980b9;
  }
  
  /* Section headers */
  .section-header {
    font-size: 0.875rem;
    font-weight: 600;
    color: #6c757d;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    margin-bottom: 0.75rem;
  }
  
  /* Plot output styling */
  .shiny-plot-output {
    border-radius: 0.5rem;
    overflow: hidden;
  }
  
  /* DataTable styling */
  .dataTables_wrapper {
    padding: 0.5rem 0;
  }
  
  /* Verbatim text output */
  pre {
    background: #f8f9fa;
    border: 1px solid #e9ecef;
    border-radius: 0.5rem;
    padding: 1rem;
    font-size: 0.875rem;
  }
  
  /* Download button group */
  .download-btn-group .btn {
    margin-bottom: 0.5rem;
  }
  
  /* Responsive adjustments */
  @media (max-width: 768px) {
    .well {
      padding: 1rem;
    }
  }
"

# =========================================================
# UI Definition
# =========================================================
ui <- page_sidebar(
  tags$head(tags$style(HTML(custom_css))),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2980b9",
    secondary = "#34495e",
    success = "#27ae60",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins"),
    code_font = font_google("Fira Code"),
    "navbar-bg" = "#2c3e50",
    "card-border-radius" = "0.5rem",
    "card-box-shadow" = "0 4px 6px rgba(0, 0, 0, 0.1)"
  ),
  title = tags$span(
    style = "font-weight: 600; letter-spacing: 0.5px;",
    "MorphoStat v1.0",
    tags$small(class = "ms-2", style = "font-size: 0.7em; color: #95a5a6; opacity: 0.9;", "Statistical Morphological Analysis")
  ),

  # Sidebar for inputs
  sidebar = sidebar(
    width = 350,
    bg = "#f8f9fa",
    
    # Project Name Section
    tags$div(
      class = "sidebar-section mb-4",
      tags$div(
        class = "d-flex align-items-center mb-2",
        icon("project-diagram", class = "text-primary me-2"),
        tags$h5("Project", class = "mb-0 text-primary", style = "font-weight: 600;")
      ),
      tags$hr(class = "mt-1 mb-3"),
      textInput("project_name", "Project Name:",
        value = "Morphostat_Project_1",
        placeholder = "Enter project name"
      )
    ),
    
    # Data Input Section
    tags$div(
      class = "sidebar-section mb-4",
      tags$div(
        class = "d-flex align-items-center mb-2",
        icon("folder-open", class = "text-primary me-2"),
        tags$h5("Data Input", class = "mb-0 text-primary", style = "font-weight: 600;")
      ),
      tags$hr(class = "mt-1 mb-3"),
      radioButtons("input_type", "Input Type:",
        choices = c(
          "Morphologika File" = "morphologika",
          "TPS File" = "tps",
          "NTS File" = "nts",
          "FCSV File (3D Slicer)" = "fcsv",
          "MorphoStat Project" = "morphostat_project"
        ),
        selected = "morphologika"
      ),
      conditionalPanel(
        condition = "input.input_type == 'morphostat_project'",
        fileInput("project_file", "Load MorphoStat Project",
          accept = c(".rds")
        ),
        helpText(class = "text-muted small", "Load a previously saved MorphoStat project (.rds file) to restore all settings and data.")
      ),
      conditionalPanel(
        condition = "input.input_type != 'morphostat_project'",
        fileInput("data_file", "Upload Data File",
          accept = c(".txt", ".tps", ".nts", ".fcsv")
        )
      ),
      conditionalPanel(
        condition = "input.input_type == 'tps' || input.input_type == 'fcsv'",
        fileInput("classifiers_file", "Upload Classifiers (Optional)",
          accept = c(".txt", ".csv")
        )
      ),
      conditionalPanel(
        condition = "input.input_type == 'fcsv'",
        helpText(class = "text-muted small", "For FCSV files from 3D Slicer. Upload multiple .fcsv files by selecting them together, or upload a single file with multiple specimens.")
      )
    ),

    # Dimension Selection
    tags$div(
      class = "sidebar-section mb-4",
      tags$div(
        class = "d-flex align-items-center mb-2",
        icon("cube", class = "text-primary me-2"),
        tags$h5("Analysis Dimensions", class = "mb-0 text-primary", style = "font-weight: 600;")
      ),
      tags$hr(class = "mt-1 mb-3"),
      radioButtons("dimension", "Dimension Type:",
        choices = c(
          "3D Analysis" = "3d",
          "2D Analysis" = "2d"
        ),
        selected = "3d"
      )
    ),

    # Wireframe Settings
    tags$div(
      class = "sidebar-section mb-4",
      tags$div(
        class = "d-flex align-items-center mb-2",
        icon("bezier-curve", class = "text-primary me-2"),
        tags$h5("Wireframe Settings", class = "mb-0 text-primary", style = "font-weight: 600;")
      ),
      tags$hr(class = "mt-1 mb-3"),
      fileInput("links_file", "Upload Links File (Optional)",
        accept = c(".txt", ".csv")
      ),
      helpText(class = "text-muted small", "File should contain comma-separated landmark pairs (e.g., 1,2,2,3,3,4)"),
      textAreaInput("links_input", "Landmark Links (pairs, comma-separated):",
        value = "",
        rows = 4,
        placeholder = "e.g., 1,2, 2,3, 3,4, 4,5"
      ),
      fluidRow(
        column(6, actionButton("clear_links", "Clear", icon = icon("eraser"), class = "btn-outline-secondary btn-sm w-100")),
        column(6, downloadButton("save_links", "Save Links", class = "btn-outline-primary btn-sm w-100"))
      )
    ),

    # Analysis Settings
    tags$div(
      class = "sidebar-section mb-4",
      tags$div(
        class = "d-flex align-items-center mb-2",
        icon("sliders", class = "text-primary me-2"),
        tags$h5("Analysis Settings", class = "mb-0 text-primary", style = "font-weight: 600;")
      ),
      tags$hr(class = "mt-1 mb-3"),
      sliderInput("n_perms", "PERMANOVA Permutations:",
        min = 99, max = 9999, value = 999, step = 100
      ),
      sliderInput("n_perms_pc95", "PC-based PERMANOVA Permutations:",
        min = 1000, max = 20000, value = 10000, step = 1000
      ),
      sliderInput("allometry_perms", "Allometry Test Permutations:",
        min = 1000, max = 20000, value = 10000, step = 1000
      ),
      sliderInput("confidence", "Confidence Level:",
        min = 0.90, max = 0.99, value = 0.95, step = 0.01
      ),
      sliderInput("pca_variance_threshold", "PCA Variance Threshold (%):",
        min = 85, max = 99, value = 95, step = 1
      )
    ),
    
    # Action Buttons
    tags$div(
      class = "sidebar-section",
      tags$hr(class = "mb-3"),
      actionButton("run_analysis", 
        tags$span(icon("play-circle", class = "me-2"), "Run Analysis"),
        class = "btn-primary btn-lg w-100 mb-3",
        style = "font-weight: 500; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
      ),
      downloadButton("download_results", 
        tags$span(icon("download", class = "me-2"), "Download Results"),
        class = "btn-success w-100 mb-2",
        style = "font-weight: 500;"
      ),
      downloadButton("save_project", 
        tags$span(icon("save", class = "me-2"), "Save Project"),
        class = "btn w-100",
        style = "font-weight: 500; background: linear-gradient(135deg, #1a5276 0%, #2471a3 100%); border: none; color: white;"
      )
    )
  ),

  # Main content area
  navset_card_tab(
    id = "main_tabs",
    nav_panel(
      title = tags$span(icon("project-diagram", class = "me-1"), "Shape Analysis"),
      card(
        card_header(
          class = "bg-light border-bottom",
          tags$div(
            class = "d-flex align-items-center",
            icon("vector-square", class = "me-2 text-primary"),
            tags$span("Procrustes Analysis & Outlier Detection", style = "font-weight: 500;")
          )
        ),
        card_body(
          fluidRow(
            column(
              8,
              tabsetPanel(
                id = "procrustes_tabs",
                tabPanel("Procrustes Plot",
                  plotOutput("procrustes_plot", height = "600px")
                ),
                tabPanel("Outlier Detection",
                  h5("Outlier Detection (plotOutliers)"),
                  helpText("This analysis identifies potential outliers in the Procrustes-aligned coordinates using Procrustes distance from the mean shape."),
                  plotOutput("outlier_plot", height = "500px"),
                  hr(),
                  h6("Outlier Summary:"),
                  verbatimTextOutput("outlier_summary")
                )
              )
            ),
            column(
              4,
              wellPanel(
                class = "settings-panel",
                tags$div(
                  class = "d-flex align-items-center mb-3",
                  icon("cog", class = "text-primary me-2"),
                  tags$h5("Settings", class = "mb-0 text-primary", style = "font-weight: 600;")
                ),
                hr(class = "mt-0"),
                conditionalPanel(
                  condition = "input.procrustes_tabs == 'Procrustes Plot'",
                  tags$p(class = "section-header", icon("vector-square", class = "me-1"), "Procrustes Plot Settings"),
                  selectInput("procrustes_view", "View Mode:",
                    choices = c("All Samples Together" = "all", "Individual Sample" = "individual"),
                    selected = "all"
                  ),
                  conditionalPanel(
                    condition = "input.procrustes_view == 'individual'",
                    selectInput("procrustes_specimen", "Select Specimen:",
                      choices = c()
                    )
                  ),
                  hr(),
                  tags$p(class = "section-header", icon("expand-arrows-alt", class = "me-1"), "PC Deformation"),
                  selectInput("procrustes_pc", "PC Axis:",
                    choices = paste0("PC", 1:10),
                    selected = "PC1"
                  ),
                  sliderInput("procrustes_pc_value", "PC Value:",
                    min = -0.1, max = 0.1, value = 0, step = 0.01
                  ),
                  hr(),
                  tags$p(class = "section-header", icon("arrows-alt-h", class = "me-1"), "PC Axis Direction"),
                  checkboxInput("procrustes_flip_pc_axis", "Flip PC Axis (+/-)", value = FALSE),
                  helpText(class = "text-muted small", "Or use 'Sync with PCA' below to match PCA plot settings."),
                  checkboxInput("procrustes_sync_pca_axis", "Sync with PCA Plot Axis Settings", value = TRUE),
                  hr(),
                  h6("Diagram Transformations:"),
                  selectInput("procrustes_flip", "Flip Diagram:",
                    choices = c("None" = "none", "Horizontal" = "horizontal", "Vertical" = "vertical"),
                    selected = "none"
                  ),
                  selectInput("procrustes_rotate", "Rotate Diagram:",
                    choices = c("None" = "0", "90° Clockwise" = "90", "180°" = "180", "270° Clockwise" = "270"),
                    selected = "0"
                  ),
                  conditionalPanel(
                    condition = "input.dimension == '3d'",
                    selectInput("procrustes_axes", "Display Axes:",
                      choices = c("Dorsal (XY)" = "xy", "Sagittal (XZ)" = "xz", "Coronal (YZ)" = "yz"),
                      selected = "xy"
                    )
                  ),
                  hr(),
                  h6("Display Options:"),
                  checkboxInput("procrustes_show_wireframe", "Show Wireframe", value = TRUE),
                  checkboxInput("procrustes_show_mean", "Show Mean Shape", value = TRUE),
                  checkboxInput("procrustes_show_landmarks", "Show Landmark Numbers", value = FALSE),
                  conditionalPanel(
                    condition = "input.procrustes_show_landmarks == true",
                    sliderInput("procrustes_landmark_size", "Landmark Number Size:",
                      min = 0.5, max = 3, value = 1, step = 0.1
                    ),
                    colourpicker::colourInput("procrustes_landmark_color", "Landmark Number Color:",
                      value = "#000000"
                    )
                  ),
                  sliderInput("procrustes_point_size", "Point Size:",
                    min = 0.5, max = 5, value = 2, step = 0.1
                  ),
                  sliderInput("procrustes_line_width", "Line Width:",
                    min = 0.5, max = 5, value = 1.5, step = 0.1
                  ),
                  hr(),
                  h6("Colors:"),
                  colourpicker::colourInput("procrustes_point_color", "Point Color:",
                    value = "#3498db"
                  ),
                  colourpicker::colourInput("procrustes_wire_color", "Wireframe Color:",
                    value = "#2c3e50"
                  ),
                  colourpicker::colourInput("procrustes_mean_color", "Mean Shape Color:",
                    value = "#e74c3c"
                  ),
                  hr(),
                  tags$p(class = "section-header", icon("download", class = "me-1"), "Download"),
                  fluidRow(
                    column(6, numericInput("procrustes_download_width", "Width (px):", value = 800, min = 400, max = 2000, step = 50)),
                    column(6, numericInput("procrustes_download_height", "Height (px):", value = 600, min = 300, max = 1500, step = 50))
                  ),
                  downloadButton("download_procrustes_png", "PNG",
                    class = "btn-sm btn-primary w-100", style = "margin-bottom: 5px;"
                  ),
                  downloadButton("download_procrustes_svg", "SVG",
                    class = "btn-sm btn-primary w-100"
                  )
                ),
                conditionalPanel(
                  condition = "input.procrustes_tabs == 'Outlier Detection'",
                  tags$p(class = "section-header", icon("exclamation-triangle", class = "me-1"), "Outlier Detection Settings"),
                  helpText("Outliers are specimens whose Procrustes distance from the mean shape exceeds a threshold based on standard deviations."),
                  sliderInput("outlier_threshold", "Outlier Threshold (SD):",
                    min = 1.5, max = 4, value = 2.5, step = 0.1
                  ),
                  helpText(class = "text-muted small", "Specimens beyond this many standard deviations from the mean are flagged as potential outliers."),
                  checkboxInput("outlier_show_labels", "Show Specimen Labels", value = TRUE),
                  checkboxInput("outlier_highlight", "Highlight Outliers", value = TRUE),
                  hr(),
                  h6("Colors:"),
                  colourpicker::colourInput("outlier_point_color", "Normal Points:",
                    value = "#3498db"
                  ),
                  colourpicker::colourInput("outlier_highlight_color", "Outlier Points:",
                    value = "#e74c3c"
                  ),
                  hr(),
                  tags$p(class = "section-header", icon("download", class = "me-1"), "Download"),
                  fluidRow(
                    column(6, numericInput("outlier_download_width", "Width (px):", value = 800, min = 400, max = 2000, step = 50)),
                    column(6, numericInput("outlier_download_height", "Height (px):", value = 600, min = 300, max = 1500, step = 50))
                  ),
                  downloadButton("download_outlier_png", "PNG",
                    class = "btn-sm btn-primary w-100", style = "margin-bottom: 5px;"
                  ),
                  downloadButton("download_outlier_svg", "SVG",
                    class = "btn-sm btn-primary w-100", style = "margin-bottom: 5px;"
                  ),
                  downloadButton("download_outlier_summary", "Download Outlier Summary (.txt)",
                    class = "btn-sm btn-success w-100"
                  )
                )
              )
            )
          )
        )
      )
    ),
    nav_panel(
      title = tags$span(icon("chart-scatter-3d", class = "me-1"), "PCA Plot"),
      card(
        card_header(
          class = "bg-light border-bottom",
          tags$div(
            class = "d-flex align-items-center",
            icon("chart-area", class = "me-2 text-primary"),
            tags$span("Principal Component Analysis", style = "font-weight: 500;")
          )
        ),
        card_body(
          fluidRow(
            column(
              8,
              div(class = "plot-container p-2",
                plotOutput("pca_plot_main", height = "600px")
              )
            ),
            column(
              4,
              wellPanel(
                class = "settings-panel",
                tags$div(
                  class = "d-flex align-items-center mb-3",
                  icon("cog", class = "text-primary me-2"),
                  tags$h5("Plot Settings", class = "mb-0 text-primary", style = "font-weight: 600;")
                ),
                hr(class = "mt-0"),
                selectInput("pca_plot_type", "Plot Type:",
                  choices = c("Colored" = "color", "Black & White" = "bw", "3D Plot" = "3d"),
                  selected = "color"
                ),
                textInput("pca_title", "Plot Title:",
                  value = "Principal Component Analysis"
                ),
                tags$p(class = "section-header mt-3", icon("eye", class = "me-1"), "Display Options"),
                checkboxInput("pca_show_labels", "Show Data Point Labels", value = FALSE),
                checkboxInput("pca_flip_x", "Flip X-axis", value = FALSE),
                checkboxInput("pca_flip_y", "Flip Y-axis", value = FALSE),
                hr(),
                tags$p(class = "section-header", icon("chart-area", class = "me-1"), "Data Spread Visualization"),
                conditionalPanel(
                  condition = "input.pca_plot_type == '3d'",
                  tags$div(
                    class = "alert alert-info",
                    style = "margin-top: 10px; padding: 10px;",
                    icon("info-circle"),
                    " Data Spread Visualization is not supported for 3D PCA plots."
                  )
                ),
                conditionalPanel(
                  condition = "input.pca_plot_type != '3d'",
                  selectInput("pca_spread_type", "Visualization Type:",
                    choices = c(
                      "None" = "none",
                      "Confidence Ellipse" = "ellipse",
                      "Convex Hulls" = "hull",
                      "Density Contour Lines" = "density"
                    ),
                    selected = "ellipse"
                  ),
                  conditionalPanel(
                    condition = "input.pca_spread_type != 'none'",
                    checkboxInput("pca_spread_customize", "Customize Data Spread Appearance", value = FALSE),
                    conditionalPanel(
                      condition = "input.pca_spread_customize == true",
                      conditionalPanel(
                        condition = "input.pca_spread_type == 'ellipse'",
                        sliderInput("pca_ellipse_level", "Confidence Level:",
                          min = 0.80, max = 0.99, value = 0.95, step = 0.01
                        )
                      ),
                      conditionalPanel(
                        condition = "input.pca_spread_type == 'density'",
                        sliderInput("pca_density_bins", "Number of Contour Levels:",
                          min = 2, max = 10, value = 5, step = 1
                        )
                      ),
                      checkboxInput("pca_spread_fill", "Show Fill", value = TRUE),
                      sliderInput("pca_spread_fill_alpha", "Fill Transparency:",
                        min = 0.05, max = 0.5, value = 0.2, step = 0.05
                      ),
                      checkboxInput("pca_spread_outline", "Show Outline", value = TRUE),
                      sliderInput("pca_spread_line_width", "Outline Thickness:",
                        min = 0.5, max = 3, value = 1.5, step = 0.1
                      ),
                      checkboxInput("pca_spread_custom_colors", "Use Custom Colors", value = FALSE),
                      conditionalPanel(
                        condition = "input.pca_spread_custom_colors == true",
                        uiOutput("pca_spread_color_ui")
                      )
                    )
                  )
                ),
                hr(),
                tags$p(class = "section-header", icon("arrows-alt", class = "me-1"), "PC Selection"),
                fluidRow(
                  column(
                    6,
                    selectInput("pc_x", "X-axis:",
                      choices = paste0("PC", 1:10),
                      selected = "PC1"
                    )
                  ),
                  column(
                    6,
                    selectInput("pc_y", "Y-axis:",
                      choices = paste0("PC", 1:10),
                      selected = "PC2"
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.pca_plot_type == '3d'",
                  selectInput("pc_z", "Z-axis (3D):",
                    choices = paste0("PC", 1:10),
                    selected = "PC3"
                  ),
                  sliderInput("pca_3d_point_size", "Point Size (3D):",
                    min = 0.5, max = 3, value = 1.5, step = 0.1
                  )
                ),
                hr(),
                h6("B&W Plot Customization:"),
                conditionalPanel(
                  condition = "input.pca_plot_type == 'bw'",
                  checkboxInput("pca_bw_auto_shapes", "Auto Assign Shapes", value = TRUE),
                  conditionalPanel(
                    condition = "!input.pca_bw_auto_shapes",
                    uiOutput("pca_bw_shape_ui")
                  ),
                  sliderInput("pca_bw_point_size", "Point Size:",
                    min = 2, max = 8, value = 5, step = 0.5
                  )
                ),
                hr(),
                h6("Group Renaming:"),
                uiOutput("group_rename_ui"),
                hr(),
                h6("Color Settings:"),
                colourpicker::colourInput("plot_bg", "Background:", value = "#FFFFFF"),
                checkboxInput("use_custom_colors", "Custom Group Colors", FALSE),
                conditionalPanel(
                  condition = "input.use_custom_colors == true",
                  uiOutput("group_color_ui")
                ),
                hr(),
                tags$p(class = "section-header", icon("download", class = "me-1"), "Download"),
                fluidRow(
                  column(6, numericInput("pca_download_width", "Width (px):", value = 800, min = 400, max = 2000, step = 50)),
                  column(6, numericInput("pca_download_height", "Height (px):", value = 600, min = 300, max = 1500, step = 50))
                ),
                downloadButton("download_pca_png", "PNG",
                  class = "btn-sm btn-primary w-100", style = "margin-bottom: 5px;"
                ),
                downloadButton("download_pca_svg", "SVG",
                  class = "btn-sm btn-primary w-100"
                )
              )
            )
          )
        )
      )
    ),
    nav_panel(
      title = tags$span(icon("table", class = "me-1"), "Statistics"),
      card(
        card_header(
          class = "bg-light border-bottom d-flex justify-content-between align-items-center",
          tags$div(
            class = "d-flex align-items-center",
            icon("calculator", class = "me-2 text-primary"),
            tags$span("Statistical Analysis Results", style = "font-weight: 500;")
          ),
          downloadButton("download_all_statistics", "Download Statistics", class = "btn-success btn-sm")
        ),
        card_body(
          class = "p-4",
          # Data Summary Section
          tags$div(
            class = "stats-section mb-4",
            tags$div(
              class = "d-flex align-items-center mb-2",
              icon("database", class = "text-primary me-2"),
              tags$h5("Data Summary", class = "mb-0", style = "font-weight: 600;")
            ),
            verbatimTextOutput("data_summary")
          ),
          hr(class = "my-4"),
          # PC Variance Section
          tags$div(
            class = "stats-section mb-4",
            tags$div(
              class = "d-flex align-items-center mb-2",
              icon("chart-pie", class = "text-info me-2"),
              tags$h5("PC Variance Explained", class = "mb-0", style = "font-weight: 600;")
            ),
            verbatimTextOutput("pc_variance_output")
          ),
          hr(class = "my-4"),
          # Centroid Size Section
          tags$div(
            class = "stats-section mb-4",
            tags$div(
              class = "d-flex align-items-center mb-2",
              icon("ruler-combined", class = "text-warning me-2"),
              tags$h5("Centroid Size Analysis", class = "mb-0", style = "font-weight: 600;")
            ),
            verbatimTextOutput("cs_analysis")
          ),
          hr(class = "my-4"),
          # Allometry Section
          tags$div(
            class = "stats-section mb-4",
            tags$div(
              class = "d-flex align-items-center mb-2",
              icon("arrows-alt-h", class = "text-secondary me-2"),
              tags$h5("Allometry Regression", class = "mb-0", style = "font-weight: 600;")
            ),
            verbatimTextOutput("allometry_results")
          ),
          hr(class = "my-4"),
          # PERMANOVA Shape Section
          tags$div(
            class = "stats-section mb-4",
            tags$div(
              class = "d-flex align-items-center mb-2",
              icon("vials", class = "text-success me-2"),
              tags$h5("PERMANOVA Results (Shape-based)", class = "mb-0", style = "font-weight: 600;")
            ),
            verbatimTextOutput("permanova_results"),
            uiOutput("permanova_interpretation")
          ),
          hr(class = "my-4"),
          # PERMANOVA PC Section
          tags$div(
            class = "stats-section mb-4",
            tags$div(
              class = "d-flex align-items-center mb-2",
              icon("chart-line", class = "text-primary me-2"),
              tags$h5("PERMANOVA on 95% PCs (Euclidean Distance)", class = "mb-0", style = "font-weight: 600;")
            ),
            verbatimTextOutput("permanova_pc95_results")
          ),
          hr(class = "my-4"),
          # PERMDISP Section
          tags$div(
            class = "stats-section mb-4",
            tags$div(
              class = "d-flex align-items-center mb-2",
              icon("balance-scale", class = "text-danger me-2"),
              tags$h5("PERMDISP Test (Homogeneity of Dispersions)", class = "mb-0", style = "font-weight: 600;")
            ),
            verbatimTextOutput("permdisp_results"),
            uiOutput("permdisp_interpretation")
          ),
          hr(class = "my-4"),
          # Pairwise Section
          tags$div(
            class = "stats-section mb-4",
            tags$div(
              class = "d-flex align-items-center mb-2",
              icon("exchange-alt", class = "text-info me-2"),
              tags$h5("Pairwise Comparisons", class = "mb-0", style = "font-weight: 600;")
            ),
            verbatimTextOutput("pairwise_results")
          ),
          hr(class = "my-4"),
          # Centroids Section
          tags$div(
            class = "stats-section",
            tags$div(
              class = "d-flex align-items-center mb-2",
              icon("crosshairs", class = "text-warning me-2"),
              tags$h5("Group Centroids", class = "mb-0", style = "font-weight: 600;")
            ),
            uiOutput("centroids_ui")
          )
        )
      )
    ),
    nav_panel(
      title = tags$span(icon("chart-bar", class = "me-1"), "Statistical Plots"),
      card(
        card_header(
          class = "bg-light border-bottom",
          tags$div(
            class = "d-flex align-items-center",
            icon("chart-line", class = "me-2 text-primary"),
            tags$span("Statistical Result Visualizations", style = "font-weight: 500;")
          )
        ),
        card_body(
          fluidRow(
            column(
              8,
              tabsetPanel(
                id = "stat_plot_tabs",
                tabPanel("Centroid Size", 
                  plotOutput("centroid_size_plot", height = "500px")
                ),
                tabPanel("Allometry Regression", 
                  plotOutput("allometry_plot", height = "500px")
                ),
                tabPanel("Shape Difference Vectors", 
                  plotOutput("shape_vectors_plot", height = "500px")
                ),
                tabPanel("Dispersion Plot", 
                  plotOutput("dispersion_plot", height = "500px")
                ),
                tabPanel("Eigenvalues", 
                  plotOutput("eigenvalues_plot", height = "500px")
                )
              )
            ),
            column(
              4,
              wellPanel(
                class = "settings-panel",
                tags$div(
                  class = "d-flex align-items-center mb-3",
                  icon("cog", class = "text-primary me-2"),
                  tags$h5("Plot Settings", class = "mb-0 text-primary", style = "font-weight: 600;")
                ),
                hr(class = "mt-0"),
                conditionalPanel(
                  condition = "input.stat_plot_tabs == 'Centroid Size'",
                  tags$p(class = "section-header", icon("chart-bar", class = "me-1"), "Centroid Size Box Plot"),
                  selectInput("cs_groups_select", "Select Groups to Display:",
                    choices = c(), multiple = TRUE
                  ),
                  helpText(class = "text-muted small", "Groups will appear left to right in the order selected above."),
                  checkboxInput("cs_show_points", "Show Individual Points", value = TRUE),
                  checkboxInput("cs_show_mean", "Show Group Means", value = FALSE),
                  hr(),
                  tags$p(class = "section-header", icon("palette", class = "me-1"), "Group Colors"),
                  uiOutput("cs_group_colors_ui"),
                  colourpicker::colourInput("cs_outline_color", "Box Outline:", value = "#2c3e50"),
                  hr(),
                  tags$p(class = "section-header", icon("font", class = "me-1"), "Font Sizes"),
                  sliderInput("cs_title_size", "Title Size:", min = 10, max = 24, value = 14, step = 1),
                  sliderInput("cs_axis_title_size", "Axis Title Size:", min = 8, max = 18, value = 12, step = 1),
                  sliderInput("cs_axis_text_size", "Axis Text Size:", min = 6, max = 16, value = 10, step = 1)
                ),
                conditionalPanel(
                  condition = "input.stat_plot_tabs == 'Allometry Regression'",
                  tags$p(class = "section-header", icon("chart-line", class = "me-1"), "Allometry Plot"),
                  checkboxInput("allo_flip_x", "Flip X-axis (Size)", value = FALSE),
                  checkboxInput("allo_flip_y", "Flip Y-axis (PC1)", value = FALSE),
                  helpText(class = "text-muted small", "Or use 'Sync with PCA' below to match PCA plot PC1 setting."),
                  checkboxInput("allo_sync_pca_axis", "Sync PC1 with PCA Plot Axis Settings", value = TRUE),
                  checkboxInput("allo_show_ci", "Show Confidence Interval", value = TRUE),
                  checkboxInput("allo_show_groups", "Color by Groups", value = TRUE),
                  sliderInput("allo_point_size", "Point Size:", min = 1, max = 5, value = 2, step = 0.5),
                  sliderInput("allo_line_width", "Line Width:", min = 0.5, max = 3, value = 1, step = 0.1),
                  hr(),
                  tags$p(class = "section-header", icon("palette", class = "me-1"), "Colors"),
                  conditionalPanel(
                    condition = "input.allo_show_groups == true",
                    uiOutput("allo_group_colors_ui")
                  ),
                  conditionalPanel(
                    condition = "input.allo_show_groups == false",
                    colourpicker::colourInput("allo_single_color", "Point Color:", value = "#3498db")
                  ),
                  colourpicker::colourInput("allo_line_color", "Regression Line:", value = "#2c3e50"),
                  hr(),
                  tags$p(class = "section-header", icon("font", class = "me-1"), "Font Sizes"),
                  sliderInput("allo_title_size", "Title Size:", min = 10, max = 24, value = 14, step = 1),
                  sliderInput("allo_axis_title_size", "Axis Title Size:", min = 8, max = 18, value = 12, step = 1),
                  sliderInput("allo_axis_text_size", "Axis Text Size:", min = 6, max = 16, value = 10, step = 1)
                ),
                conditionalPanel(
                  condition = "input.stat_plot_tabs == 'Shape Difference Vectors'",
                  h6("Shape Vectors:"),
                  selectInput("vector_comparison", "Group Comparison:",
                    choices = c("Group 1 vs Group 2" = "1vs2"),
                    selected = "1vs2"
                  ),
                  sliderInput("vector_scale", "Vector Scale:", min = 1, max = 20, value = 10, step = 1),
                  colourpicker::colourInput("vector_color", "Vector Color:", value = "#e74c3c"),
                  sliderInput("vector_width", "Vector Width:", min = 0.5, max = 3, value = 1, step = 0.1),
                  hr(),
                  h6("Font Sizes:"),
                  sliderInput("vector_title_size", "Title Size:", min = 10, max = 24, value = 14, step = 1),
                  sliderInput("vector_axis_title_size", "Axis Title Size:", min = 8, max = 18, value = 12, step = 1),
                  sliderInput("vector_axis_text_size", "Axis Text Size:", min = 6, max = 16, value = 10, step = 1)
                ),
                conditionalPanel(
                  condition = "input.stat_plot_tabs == 'Dispersion Plot'",
                  h6("Dispersion Plot:"),
                  selectInput("disp_groups_select", "Select Groups to Display:",
                    choices = c(), multiple = TRUE
                  ),
                  helpText("Groups will appear left to right in the order selected above."),
                  checkboxInput("disp_show_points", "Show Individual Points", value = TRUE),
                  checkboxInput("disp_show_mean", "Show Group Means", value = TRUE),
                  checkboxInput("disp_show_ellipse", "Show Confidence Ellipses", value = TRUE),
                  sliderInput("disp_ellipse_level", "Ellipse Confidence:", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
                  checkboxInput("disp_show_centroids", "Show Group Centroids", value = TRUE),
                  sliderInput("disp_point_size", "Point Size:", min = 1, max = 5, value = 2, step = 0.5),
                  hr(),
                  h6("Group Colors:"),
                  uiOutput("disp_group_colors_ui"),
                  hr(),
                  h6("Box Styling:"),
                  colourpicker::colourInput("disp_outline_color", "Box Outline:", value = "#2c3e50"),
                  hr(),
                  h6("Font Sizes:"),
                  sliderInput("disp_title_size", "Title Size:", min = 10, max = 24, value = 14, step = 1),
                  sliderInput("disp_axis_title_size", "Axis Title Size:", min = 8, max = 18, value = 12, step = 1),
                  sliderInput("disp_axis_text_size", "Axis Text Size:", min = 6, max = 16, value = 10, step = 1)
                ),
                conditionalPanel(
                  condition = "input.stat_plot_tabs == 'Eigenvalues'",
                  h6("Eigenvalues Plot:"),
                  sliderInput("eigen_n_components", "Number of PCs to Display:", min = 5, max = 20, value = 10, step = 1),
                  checkboxInput("eigen_show_cumulative", "Show Cumulative Variance Line", value = TRUE),
                  colourpicker::colourInput("eigen_bar_color", "Bar Color:", value = "#3498db"),
                  colourpicker::colourInput("eigen_line_color", "Cumulative Line Color:", value = "#e74c3c"),
                  hr(),
                  h6("Font Sizes:"),
                  sliderInput("eigen_title_size", "Title Size:", min = 10, max = 24, value = 14, step = 1),
                  sliderInput("eigen_axis_title_size", "Axis Title Size:", min = 8, max = 18, value = 12, step = 1),
                  sliderInput("eigen_axis_text_size", "Axis Text Size:", min = 6, max = 16, value = 10, step = 1)
                ),
                hr(),
                tags$p(class = "section-header", icon("download", class = "me-1"), "Download Current Plot"),
                fluidRow(
                  column(6, numericInput("stat_download_width", "Width (px):", value = 800, min = 400, max = 2000, step = 50)),
                  column(6, numericInput("stat_download_height", "Height (px):", value = 600, min = 300, max = 1500, step = 50))
                ),
                downloadButton("download_stat_plot_png", "PNG",
                  class = "btn-sm btn-primary w-100", style = "margin-bottom: 5px;"
                ),
                downloadButton("download_stat_plot_svg", "SVG",
                  class = "btn-sm btn-primary w-100", style = "margin-bottom: 5px;"
                ),
                hr(),
                downloadButton("download_all_stat_plots", "Download All Plots",
                  class = "btn-sm btn-success w-100"
                )
              )
            )
          )
        )
      )
    ),
    nav_panel(
      title = tags$span(icon("shapes", class = "me-1"), "PC Deformations"),
      card(
        card_header(
          class = "bg-light border-bottom",
          tags$div(
            class = "d-flex align-items-center",
            icon("cube", class = "me-2 text-primary"),
            tags$span("PC Shape Deformations and Shape Visualizations", style = "font-weight: 500;")
          )
        ),
        card_body(
          fluidRow(
            column(
              8,
              tabsetPanel(
                id = "deform_tabs",
                tabPanel("Combined View",
                  plotOutput("pc_deformation_plot", height = "600px")
                ),
                tabPanel("Shape Visualizations",
                  conditionalPanel(
                    condition = "input.dimension == '3d' && input.wire_view == 'dorsal'",
                    h5("Dorsal View (Top View - XY Plane)"),
                    fluidRow(
                      column(6, plotOutput("dorsal_minus", height = "400px")),
                      column(6, plotOutput("dorsal_plus", height = "400px"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.dimension == '3d' && input.wire_view == 'sagittal'",
                    h5("Sagittal View (Side View - XZ Plane)"),
                    fluidRow(
                      column(6, plotOutput("sagittal_minus", height = "400px")),
                      column(6, plotOutput("sagittal_plus", height = "400px"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.dimension == '3d' && input.wire_view == 'coronal'",
                    h5("Coronal View (Frontal View - YZ Plane)"),
                    fluidRow(
                      column(6, plotOutput("coronal_minus", height = "400px")),
                      column(6, plotOutput("coronal_plus", height = "400px"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.dimension == '2d'",
                    h5("2D Shape Visualization"),
                    fluidRow(
                      column(6, plotOutput("wireframe_2d_minus", height = "400px")),
                      column(6, plotOutput("wireframe_2d_plus", height = "400px"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.dimension == '2d' && input.wire_viz_type == 'tps_grid'",
                    h5("TPS Transformation Grid"),
                    fluidRow(
                      column(6, plotOutput("tps_grid_minus", height = "400px")),
                      column(6, plotOutput("tps_grid_plus", height = "400px"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.dimension == '2d' && input.wire_viz_type == 'outline'",
                    h5("Wrapped Outline Drawing"),
                    fluidRow(
                      column(6, plotOutput("outline_minus", height = "400px")),
                      column(6, plotOutput("outline_plus", height = "400px"))
                    )
                  )
                ),
                tabPanel("Landmark Displacement",
                  h5("Landmark Displacement Graph"),
                  helpText("This graph shows the displacement magnitude of each landmark from the mean shape to the minimum and maximum PC scores."),
                  plotOutput("lollipop_plot", height = "500px")
                )
              )
            ),
            column(
              4,
              wellPanel(
                class = "settings-panel",
                tags$div(
                  class = "d-flex align-items-center mb-3",
                  icon("cog", class = "text-primary me-2"),
                  tags$h5("Settings", class = "mb-0 text-primary", style = "font-weight: 600;")
                ),
                hr(class = "mt-0"),
                conditionalPanel(
                  condition = "input.deform_tabs == 'Combined View'",
                  tags$p(class = "section-header", icon("shapes", class = "me-1"), "PC Deformation Settings"),
                  selectInput("deform_pc", "Select PC:",
                    choices = paste0("PC", 1:10),
                    selected = "PC1"
                  ),
                  sliderInput("deform_magnitude", "Deformation Magnitude:",
                    min = 0.5, max = 5, value = 1, step = 0.1
                  ),
                  numericInput("deform_pc_min", "Min PC Score:", value = -0.05, step = 0.01),
                  numericInput("deform_pc_max", "Max PC Score:", value = 0.05, step = 0.01),
                  hr(),
                  tags$p(class = "section-header", icon("arrows-alt-h", class = "me-1"), "PC Axis Direction"),
                  checkboxInput("deform_flip_pc_axis", "Flip PC Axis (+/-)", value = FALSE),
                  helpText(class = "text-muted small", "Or use 'Sync with PCA' below to match PCA plot settings."),
                  checkboxInput("deform_sync_pca_axis", "Sync with PCA Plot Axis Settings", value = TRUE),
                  hr(),
                  tags$p(class = "section-header", icon("sync-alt", class = "me-1"), "Diagram Transformations"),
                  selectInput("deform_flip", "Flip Diagram:",
                    choices = c("None" = "none", "Horizontal" = "horizontal", "Vertical" = "vertical"),
                    selected = "none"
                  ),
                  selectInput("deform_rotate", "Rotate Diagram:",
                    choices = c("None" = "0", "90° Clockwise" = "90", "180°" = "180", "270° Clockwise" = "270"),
                    selected = "0"
                  ),
                  conditionalPanel(
                    condition = "input.dimension == '3d'",
                    selectInput("deform_axes", "Display Axes:",
                      choices = c("Dorsal (XY)" = "xy", "Sagittal (XZ)" = "xz", "Coronal (YZ)" = "yz"),
                      selected = "xy"
                    )
                  ),
                  sliderInput("deform_point_size", "Data Point Size:",
                    min = 0.5, max = 5, value = 1.5, step = 0.1
                  ),
                  hr(),
                  tags$p(class = "section-header", icon("eye", class = "me-1"), "Display Options"),
                  checkboxInput("deform_show_wireframe", "Show Wireframe", value = TRUE),
                  checkboxInput("deform_show_mean", "Show Mean Shape", value = TRUE),
                  checkboxInput("deform_show_landmarks", "Show Landmark Numbers", value = FALSE),
                  conditionalPanel(
                    condition = "input.deform_show_landmarks == true",
                    sliderInput("deform_landmark_size", "Landmark Number Size:",
                      min = 0.5, max = 3, value = 1, step = 0.1
                    ),
                    colourpicker::colourInput("deform_landmark_color", "Landmark Number Color:",
                      value = "#000000"
                    )
                  ),
                  hr(),
                  h6("Shape Colors:"),
                  colourpicker::colourInput("deform_mean_color", "Mean Shape:",
                    value = "#95a5a6"
                  ),
                  colourpicker::colourInput("deform_minus_color", "Min PC Shape:",
                    value = "#e74c3c"
                  ),
                  colourpicker::colourInput("deform_plus_color", "Max PC Shape:",
                    value = "#3498db"
                  ),
                  colourpicker::colourInput("deform_point_color", "Point Color:",
                    value = "#2c3e50"
                  ),
                  sliderInput("deform_line_width", "Line Width:",
                    min = 0.5, max = 5, value = 2, step = 0.5
                  ),
                  hr(),
                  tags$p(class = "section-header", icon("download", class = "me-1"), "Download"),
                  fluidRow(
                    column(6, numericInput("deform_download_width", "Width (px):", value = 800, min = 400, max = 2000, step = 50)),
                    column(6, numericInput("deform_download_height", "Height (px):", value = 600, min = 300, max = 1500, step = 50))
                  ),
                  downloadButton("download_deformation_png", "PNG",
                    class = "btn-sm btn-primary w-100", style = "margin-bottom: 5px;"
                  ),
                  downloadButton("download_deformation_svg", "SVG",
                    class = "btn-sm btn-primary w-100"
                  )
                ),
                conditionalPanel(
                  condition = "input.deform_tabs == 'Landmark Displacement'",
                  tags$p(class = "section-header", icon("chart-bar", class = "me-1"), "Landmark Displacement Settings"),
                  selectInput("disp_pc", "Select PC:",
                    choices = paste0("PC", 1:10),
                    selected = "PC1"
                  ),
                  numericInput("disp_pc_value", "PC Score:", value = 0.05, step = 0.01),
                  helpText(class = "text-muted small", "Displacement is calculated from mean shape to this PC score."),
                  sliderInput("disp_mag", "Displacement Magnification:",
                    min = 1, max = 10, value = 1, step = 0.5
                  ),
                  hr(),
                  tags$p(class = "section-header", icon("arrows-alt-h", class = "me-1"), "PC Axis Direction"),
                  checkboxInput("disp_flip_pc_axis", "Flip PC Axis (+/-)", value = FALSE),
                  checkboxInput("disp_sync_pca_axis", "Sync with PCA Plot Axis Settings", value = TRUE),
                  hr(),
                  h6("Bar Color:"),
                  colourpicker::colourInput("disp_color", "Displacement Color:",
                    value = "#3498db"
                  ),
                  hr(),
                  h6("Download:"),
                  fluidRow(
                    column(6, numericInput("disp_download_width", "Width (px):", value = 1000, min = 400, max = 2000, step = 50)),
                    column(6, numericInput("disp_download_height", "Height (px):", value = 500, min = 300, max = 1500, step = 50))
                  ),
                  downloadButton("download_displacement_png", "PNG",
                    class = "btn-sm btn-primary w-100", style = "margin-bottom: 5px;"
                  ),
                  downloadButton("download_displacement_svg", "SVG",
                    class = "btn-sm btn-primary w-100"
                  )
                ),
                conditionalPanel(
                  condition = "input.deform_tabs == 'Shape Visualizations'",
                  h6("Visualization Type:"),
                  selectInput("wire_viz_type", "Select Visualization:",
                    choices = c(
                      "Wireframe" = "wireframe",
                      "TPS Grid (2D only)" = "tps_grid",
                      "Wrapped Outline (2D only)" = "outline"
                    ),
                    selected = "wireframe"
                  ),
                  hr(),
                  conditionalPanel(
                    condition = "input.wire_viz_type == 'wireframe'",
                    h6("Wireframe Settings:"),
                    conditionalPanel(
                      condition = "input.dimension == '3d'",
                      selectInput("wire_view", "Select View:",
                        choices = c(
                          "Dorsal (XY)" = "dorsal",
                          "Sagittal (XZ)" = "sagittal",
                          "Coronal (YZ)" = "coronal"
                        ),
                        selected = "dorsal"
                      )
                    )
                  ),
                  selectInput("wire_pc", "Select PC:",
                    choices = paste0("PC", 1:10),
                    selected = "PC1"
                  ),
                  h6("PC Score Range:"),
                  fluidRow(
                    column(6, numericInput("wire_pc_min", "Min:", value = -0.05, step = 0.01)),
                    column(6, numericInput("wire_pc_max", "Max:", value = 0.05, step = 0.01))
                  ),
                  hr(),
                  tags$p(class = "section-header", icon("arrows-alt-h", class = "me-1"), "PC Axis Direction"),
                  checkboxInput("wire_flip_pc_axis", "Flip PC Axis (+/-)", value = FALSE),
                  helpText(class = "text-muted small", "Or use 'Sync with PCA' below to match PCA plot settings."),
                  checkboxInput("wire_sync_pca_axis", "Sync with PCA Plot Axis Settings", value = TRUE),
                  hr(),
                  h6("Diagram Transformations:"),
                  selectInput("wire_flip", "Flip Diagram:",
                    choices = c("None" = "none", "Horizontal" = "horizontal", "Vertical" = "vertical"),
                    selected = "none"
                  ),
                  selectInput("wire_rotate", "Rotate Diagram:",
                    choices = c("None" = "0", "90° Clockwise" = "90", "180°" = "180", "270° Clockwise" = "270"),
                    selected = "0"
                  ),
                  sliderInput("wire_thickness", "Line Thickness:",
                    min = 0.5, max = 5, value = 2, step = 0.5
                  ),
                  sliderInput("wire_mag", "Deformation Magnification:",
                    min = 0.5, max = 5, value = 1, step = 0.1
                  ),
                  hr(),
                  tags$p(class = "section-header", icon("eye", class = "me-1"), "Display Options"),
                  checkboxInput("wire_show_wireframe", "Show Wireframe", value = TRUE),
                  checkboxInput("show_average", "Show Mean Shape", value = FALSE),
                  checkboxInput("wire_show_landmarks", "Show Landmark Numbers", value = FALSE),
                  conditionalPanel(
                    condition = "input.wire_show_landmarks == true",
                    sliderInput("wire_landmark_size", "Landmark Number Size:",
                      min = 0.5, max = 3, value = 1, step = 0.1
                    ),
                    colourpicker::colourInput("wire_landmark_color", "Landmark Number Color:",
                      value = "#000000"
                    )
                  ),
                  hr(),
                  h6("Colors:"),
                  colourpicker::colourInput("wire_color_minus", "Min PC Color:",
                    value = "#e74c3c"
                  ),
                  colourpicker::colourInput("wire_color_plus", "Max PC Color:",
                    value = "#3498db"
                  ),
                  conditionalPanel(
                    condition = "input.show_average == true",
                    colourpicker::colourInput("wire_color_avg", "Average Color:",
                      value = "#95a5a6"
                    )
                  ),
                  colourpicker::colourInput("point_color", "Point Color:",
                    value = "#2c3e50"
                  ),
                  hr(),
                  h6("Download:"),
                  downloadButton("download_wireframe_png", "PNG",
                    class = "btn-sm btn-primary w-100", style = "margin-bottom: 5px;"
                  ),
                  downloadButton("download_wireframe_svg", "SVG",
                    class = "btn-sm btn-primary w-100"
                  )
                )
              )
            )
          )
        )
      )
    ),
    nav_panel(
      title = tags$span(icon("table-cells", class = "me-1"), "Data Table"),
      card(
        card_body(
          tabsetPanel(
            id = "data_table_tabs",
            # PC Scores Tab
            tabPanel(
              title = "PC Scores",
              tags$div(
                class = "mt-3",
                tags$h5(class = "mb-3", icon("database", class = "me-2 text-primary"), "PC Scores Data"),
                tags$p(class = "text-muted", "Principal component scores for each specimen, showing position in morphospace."),
                DTOutput("scores_table"),
                hr(),
                downloadButton("download_scores_csv", "Download PC Scores (.csv)", class = "btn-sm btn-success")
              )
            ),
            # Average Shape Tab
            tabPanel(
              title = "Average Shape",
              tags$div(
                class = "mt-3",
                tags$h5(class = "mb-3", icon("shapes", class = "me-2 text-info"), "Average (Mean) Shape Coordinates"),
                tags$p(class = "text-muted", "Landmark coordinates of the consensus/mean shape from Procrustes superimposition."),
                DTOutput("avg_shape_table"),
                hr(),
                downloadButton("download_avg_shape_csv", "Download Average Shape (.csv)", class = "btn-sm btn-success")
              )
            ),
            # PC Coefficients Tab
            tabPanel(
              title = "PC Coefficients",
              tags$div(
                class = "mt-3",
                tags$h5(class = "mb-3", icon("chart-line", class = "me-2 text-warning"), "Principal Component Coefficients (Loadings)"),
                tags$p(class = "text-muted", "Eigenvectors showing how each landmark coordinate contributes to each PC."),
                DTOutput("pc_coef_table"),
                hr(),
                downloadButton("download_pc_coef_csv", "Download PC Coefficients (.csv)", class = "btn-sm btn-success")
              )
            ),
            # Centroid Size Tab
            tabPanel(
              title = "Centroid Size",
              tags$div(
                class = "mt-3",
                tags$h5(class = "mb-3", icon("ruler-combined", class = "me-2 text-danger"), "Centroid Size Data"),
                tags$p(class = "text-muted", "Centroid size (overall size measure) for each specimen."),
                DTOutput("centroid_size_table"),
                hr(),
                downloadButton("download_centroid_size_csv", "Download Centroid Size (.csv)", class = "btn-sm btn-success")
              )
            ),
            # Procrustes Coordinates Tab
            tabPanel(
              title = "Procrustes Coords",
              tags$div(
                class = "mt-3",
                tags$h5(class = "mb-3", icon("layer-group", class = "me-2 text-success"), "Procrustes-Aligned Coordinates"),
                tags$p(class = "text-muted", "Full Procrustes-superimposed landmark coordinates for all specimens."),
                DTOutput("procrustes_coords_table"),
                hr(),
                downloadButton("download_procrustes_csv", "Download Procrustes Coordinates (.csv)", class = "btn-sm btn-success")
              )
            ),
            # Variance Explained Tab
            tabPanel(
              title = "Variance",
              tags$div(
                class = "mt-3",
                tags$h5(class = "mb-3", icon("percentage", class = "me-2 text-secondary"), "Variance Explained by PCs"),
                tags$p(class = "text-muted", "Proportion of total shape variance explained by each principal component."),
                DTOutput("variance_table"),
                hr(),
                downloadButton("download_variance_csv", "Download Variance Table (.csv)", class = "btn-sm btn-success")
              )
            )
          )
        )
      )
    ),
    nav_panel(
      title = tags$span(icon("circle-question", class = "me-1"), "Help"),
      card(
        card_header(
          class = "bg-light border-bottom",
          tags$div(
            class = "d-flex align-items-center",
            icon("book-open", class = "me-2 text-primary"),
            tags$span("How to Use MorphoStat", style = "font-weight: 500;")
          )
        ),
        card_body(
          class = "p-4",
          # Getting Started
          tags$div(
            class = "help-section mb-4 p-3 bg-light rounded-3",
            tags$div(
              class = "d-flex align-items-center mb-3",
              icon("rocket", class = "text-primary me-2 fa-lg"),
              tags$h5("Getting Started", class = "mb-0", style = "font-weight: 600;")
            ),
            tags$ol(
              class = "mb-0",
              tags$li("Select your input data type (Morphologika, TPS, or NTS)"),
              tags$li("Upload your data file using the file input"),
              tags$li("Configure wireframe links if needed"),
              tags$li("Customize colors and analysis settings"),
              tags$li("Click 'Run Analysis' to process your data")
            )
          ),
          # Input Formats
          tags$div(
            class = "help-section mb-4 p-3 bg-light rounded-3",
            tags$div(
              class = "d-flex align-items-center mb-3",
              icon("file-import", class = "text-info me-2 fa-lg"),
              tags$h5("Input Formats", class = "mb-0", style = "font-weight: 600;")
            ),
            # Morphologika Format
            tags$div(
              class = "mb-3",
              tags$p(class = "mb-1", tags$strong("Morphologika"), tags$span(class = "text-muted", " (.txt)")),
              tags$pre(
                class = "bg-white p-2 rounded border small mb-1",
                style = "font-family: 'Fira Code', monospace; font-size: 0.75rem; max-height: 150px; overflow-y: auto;",
"[individuals]
10
[landmarks]
48
[dimensions]
3
[names]
Specimen_01
Specimen_02
...
[labels]
Group_A
Group_A
...
[rawpoints]
12.345 23.456 5.678
13.456 24.567 6.789
..."
              ),
              tags$p(class = "text-muted small mb-0", "Contains metadata headers followed by 3D/2D coordinates")
            ),
            # TPS Format
            tags$div(
              class = "mb-3",
              tags$p(class = "mb-1", tags$strong("TPS"), tags$span(class = "text-muted", " (.tps)")),
              tags$pre(
                class = "bg-white p-2 rounded border small mb-1",
                style = "font-family: 'Fira Code', monospace; font-size: 0.75rem; max-height: 150px; overflow-y: auto;",
"LM=48
12.345 23.456
13.456 24.567
14.567 25.678
...
ID=Specimen_01
IMAGE=image01.jpg

LM=48
15.678 26.789
16.789 27.890
..."
              ),
              tags$p(class = "text-muted small mb-0", "Each specimen starts with LM=n (landmark count), followed by coordinates")
            ),
            # NTS Format
            tags$div(
              class = "mb-3",
              tags$p(class = "mb-1", tags$strong("NTS"), tags$span(class = "text-muted", " (.nts)")),
              tags$pre(
                class = "bg-white p-2 rounded border small mb-1",
                style = "font-family: 'Fira Code', monospace; font-size: 0.75rem; max-height: 150px; overflow-y: auto;",
"1 10L 48 3 0
Specimen_01
12.345 23.456 5.678
13.456 24.567 6.789
...
Specimen_02
15.678 26.789 7.890
..."
              ),
              tags$p(class = "text-muted small mb-0", "Header line: 1=rectangular matrix, nL=specimens, landmarks, dimensions, 0=no missing")
            ),
            # FCSV Format
            tags$div(
              class = "mb-0",
              tags$p(class = "mb-1", tags$strong("FCSV"), tags$span(class = "text-muted", " (.fcsv)")),
              tags$pre(
                class = "bg-white p-2 rounded border small mb-1",
                style = "font-family: 'Fira Code', monospace; font-size: 0.75rem; max-height: 150px; overflow-y: auto;",
"# Markups fiducial file version = 4.11
# columns = id,x,y,z,ow,ox,oy,oz,vis,sel,lock,label,desc,associatedNodeID
vtkMRMLMarkupsFiducialNode_0,-12.5,34.2,56.7,0,0,0,1,1,1,0,LM1,,
vtkMRMLMarkupsFiducialNode_1,-10.3,32.1,54.5,0,0,0,1,1,1,0,LM2,,
vtkMRMLMarkupsFiducialNode_2,-8.7,30.5,52.3,0,0,0,1,1,1,0,LM3,,
..."
              ),
              tags$p(class = "text-muted small mb-0", "3D Slicer fiducial markup format. Each row is a landmark with id, x, y, z coordinates and metadata. Multiple FCSV files (one per specimen) should be provided.")
            )
          ),
          # Wireframe Links
          tags$div(
            class = "help-section mb-4 p-3 bg-light rounded-3",
            tags$div(
              class = "d-flex align-items-center mb-3",
              icon("bezier-curve", class = "text-warning me-2 fa-lg"),
              tags$h5("Wireframe Links", class = "mb-0", style = "font-weight: 600;")
            ),
            tags$p("Define landmark connections as comma-separated pairs:"),
            tags$code(class = "d-block p-2 bg-white rounded mb-2", "1,2, 2,3, 3,4, ..."),
            tags$p(class = "text-muted small mb-0", "Each pair represents two landmarks to be connected in the wireframe.")
          ),
          # Features
          tags$div(
            class = "help-section mb-4 p-3 bg-light rounded-3",
            tags$div(
              class = "d-flex align-items-center mb-3",
              icon("star", class = "text-success me-2 fa-lg"),
              tags$h5("Features", class = "mb-0", style = "font-weight: 600;")
            ),
            tags$ul(
              class = "mb-0",
              tags$li(tags$strong("PCA Visualization:"), " 2D colored/B&W plots and 3D interactive plots"),
              tags$li(tags$strong("PC Deformations:"), " Mean/min/max shape views with anatomical projections"),
              tags$li(tags$strong("Statistical Analysis:"), " PERMANOVA (shape and PC-based), PERMDISP"),
              tags$li(tags$strong("Statistical Plots:"), " Centroid size boxplots, allometry, eigenvalues"),
              tags$li(tags$strong("Procrustes Visualization:"), " View all specimens or individual samples"),
              tags$li(tags$strong("Customization:"), " Group renaming, custom colors, multiple export formats")
            )
          ),
          # Statistical Methods
          tags$div(
            class = "help-section p-3 bg-light rounded-3",
            tags$div(
              class = "d-flex align-items-center mb-3",
              icon("flask", class = "text-danger me-2 fa-lg"),
              tags$h5("Statistical Methods", class = "mb-0", style = "font-weight: 600;")
            ),
            tags$p("This application implements robust geometric morphometric workflows:"),
            tags$ul(
              class = "mb-0",
              tags$li(tags$strong("GPA Alignment:"), " Generalized Procrustes Analysis removes size, position, and rotation effects"),
              tags$li(tags$strong("PCA:"), " Principal Component Analysis identifies major axes of shape variation"),
              tags$li(tags$strong("PERMANOVA:"), " Permutational MANOVA tests for significant shape differences"),
              tags$li(tags$strong("PERMDISP:"), " Tests for homogeneity of group dispersions"),
              tags$li(tags$strong("Allometry:"), " Procrustes ANOVA evaluates shape-size relationships")
            )
          )
        )
      )
    ),
    nav_panel(
      title = tags$span(icon("circle-info", class = "me-1"), "About"),
      card(
        card_header(
          class = "bg-light border-bottom",
          tags$div(
            class = "d-flex align-items-center",
            icon("info-circle", class = "me-2 text-primary"),
            tags$span("About MorphoStat", style = "font-weight: 500;")
          )
        ),
        card_body(
          class = "p-4",
          # App Info
          tags$div(
            class = "text-center mb-4 p-4 bg-primary bg-gradient rounded-3 text-white",
            icon("cube", class = "fa-3x mb-3"),
            tags$h3("MorphoStat", class = "mb-1", style = "font-weight: 700;"),
            tags$p(class = "lead mb-0", "v1.0"),
            tags$p(class = "small mb-0 opacity-75", "Statistical Morphological Analysis")
          ),
          tags$p(class = "text-center text-muted mb-4", 
            "An interactive tool for statistical comparison and visualization of morphological measurements."
          ),
          tags$p(class = "text-center mb-4", 
            tags$strong("Developed by Dinuka Adasooriya"),
            tags$br(),
            tags$span(class = "text-muted", "Yonsei University College of Dentistry")
          ),
          hr(),
          # R Packages
          tags$div(
            class = "mb-4",
            tags$div(
              class = "d-flex align-items-center mb-3",
              icon("box-open", class = "text-primary me-2"),
              tags$h5("R Packages Used", class = "mb-0", style = "font-weight: 600;")
            ),
            tags$div(
              class = "row g-2",
              tags$div(class = "col-md-6",
                tags$div(class = "p-2 bg-light rounded",
                  tags$small(class = "text-muted", "UI Framework"),
                  tags$br(),
                  tags$strong("shiny, bslib, shinyjs")
                )
              ),
              tags$div(class = "col-md-6",
                tags$div(class = "p-2 bg-light rounded",
                  tags$small(class = "text-muted", "Morphometrics"),
                  tags$br(),
                  tags$strong("geomorph, Morpho, RRPP")
                )
              ),
              tags$div(class = "col-md-6",
                tags$div(class = "p-2 bg-light rounded",
                  tags$small(class = "text-muted", "Visualization"),
                  tags$br(),
                  tags$strong("ggplot2, scatterplot3d, rgl")
                )
              ),
              tags$div(class = "col-md-6",
                tags$div(class = "p-2 bg-light rounded",
                  tags$small(class = "text-muted", "Statistics"),
                  tags$br(),
                  tags$strong("vegan, rstatix, DT")
                )
              )
            )
          ),
          hr(),
          # Citation
          tags$div(
            class = "mb-4",
            tags$div(
              class = "d-flex align-items-center mb-3",
              icon("quote-left", class = "text-secondary me-2"),
              tags$h5("Citation", class = "mb-0", style = "font-weight: 600;")
            ),
            tags$p(class = "small", "If you use MorphoStat in your research, please cite the relevant R packages:"),
            tags$ul(
              class = "small text-muted",
              tags$li("Adams DC, Collyer ML, Kaliontzopoulou A. 2024. Geomorph: Software for geometric morphometric analyses. R package version 4.0.8."),
              tags$li("Oksanen J, et al. 2022. vegan: Community Ecology Package. R package version 2.6-4.")
            )
          ),
          hr(),
          tags$p(class = "text-center text-muted small mb-0", 
            icon("copyright", class = "me-1"),
            "2025 Dinuka Adasooriya. All rights reserved."
          )
        )
      )
    )
  )
)

# =========================================================
# Server Logic
# =========================================================
server <- function(input, output, session) {
  # Reactive values to store analysis results
  rv <- reactiveValues(
    coords = NULL,
    groups = NULL,
    gpa = NULL,
    pca = NULL,
    scores = NULL,
    pc_var = NULL,
    links = NULL,
    fit = NULL,
    pairwise = NULL,
    centroids = NULL,
    shape_minus = NULL,
    shape_plus = NULL,
    ref = NULL,
    betadisper = NULL,
    betadisper_anova = NULL,
    group_names = c(),
    # New morphometric analysis results
    centroid_size = NULL,
    cs_normality = NULL,
    cs_ttest = NULL,
    allometry_fit = NULL,
    allometry_residuals = NULL,
    pca_95 = NULL
  )

  # Dynamic UI for group renaming
  output$group_rename_ui <- renderUI({
    req(rv$groups)
    group_levels <- levels(rv$groups)

    lapply(seq_along(group_levels), function(i) {
      textInput(paste0("group_name_", i),
        paste0("Group ", i, ":"),
        value = group_levels[i]
      )
    })
  })

  # Update vector comparison choices based on available groups
  observe({
    req(rv$groups)
    group_levels <- levels(rv$groups)
    if (length(group_levels) >= 2) {
      choices <- list()
      for (i in 1:(length(group_levels)-1)) {
        for (j in (i+1):length(group_levels)) {
          label <- paste(group_levels[i], "vs", group_levels[j])
          value <- paste0(i, "vs", j)
          choices[[label]] <- value
        }
      }
      updateSelectInput(session, "vector_comparison", choices = choices)
    }
  })
  
  # Update group selection choices for centroid size and dispersion plots
  observe({
    req(rv$groups)
    group_levels <- levels(rv$groups)
    
    # Update centroid size group selection with validation
    if (length(group_levels) > 0) {
      current_cs_selection <- input$cs_groups_select
      # If no current selection or invalid selection, select all in original order
      if (is.null(current_cs_selection) || length(current_cs_selection) == 0 || 
          !all(current_cs_selection %in% group_levels)) {
        selected_groups <- group_levels
      } else {
        selected_groups <- intersect(current_cs_selection, group_levels)
      }
      
      updateSelectInput(session, "cs_groups_select", 
                        choices = group_levels, 
                        selected = selected_groups)
      
      # Update dispersion plot group selection with validation
      current_disp_selection <- input$disp_groups_select
      # If no current selection or invalid selection, select all in original order
      if (is.null(current_disp_selection) || length(current_disp_selection) == 0 || 
          !all(current_disp_selection %in% group_levels)) {
        selected_groups_disp <- group_levels
      } else {
        selected_groups_disp <- intersect(current_disp_selection, group_levels)
      }
      
      updateSelectInput(session, "disp_groups_select", 
                        choices = group_levels, 
                        selected = selected_groups_disp)
    }
  })

  # Dynamic UI for group colors
  output$group_color_ui <- renderUI({
    req(rv$groups)
    group_levels <- levels(rv$groups)
    default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")

    lapply(seq_along(group_levels), function(i) {
      colourpicker::colourInput(paste0("group_color_", i),
        paste0("Group ", i, " Color:"),
        value = default_colors[((i - 1) %% 5) + 1]
      )
    })
  })

  # Dynamic UI for PCA spread custom colors
  output$pca_spread_color_ui <- renderUI({
    req(rv$groups)
    group_levels <- levels(rv$groups)
    default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")

    lapply(seq_along(group_levels), function(i) {
      colourpicker::colourInput(paste0("pca_spread_color_", i),
        paste0(group_levels[i], " Spread Color:"),
        value = default_colors[((i - 1) %% 5) + 1]
      )
    })
  })

  # Get renamed groups
  get_renamed_groups <- reactive({
    req(rv$groups)
    group_levels <- levels(rv$groups)

    renamed <- sapply(seq_along(group_levels), function(i) {
      input_name <- paste0("group_name_", i)
      if (!is.null(input[[input_name]])) {
        input[[input_name]]
      } else {
        group_levels[i]
      }
    })

    factor(rv$groups, levels = group_levels, labels = renamed)
  })

  # Get group colors
  get_group_colors <- reactive({
    req(rv$groups)
    group_levels <- levels(rv$groups)
    default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")

    if (input$use_custom_colors) {
      sapply(seq_along(group_levels), function(i) {
        input_name <- paste0("group_color_", i)
        if (!is.null(input[[input_name]])) {
          input[[input_name]]
        } else {
          default_colors[((i - 1) %% length(default_colors)) + 1]
        }
      })
    } else {
      # Return default colors even when custom colors not enabled
      default_colors[seq_len(min(length(group_levels), length(default_colors)))]
    }
  })

  # Generate UI for allometry group colors
  output$allo_group_colors_ui <- renderUI({
    req(rv$groups)
    group_levels <- levels(rv$groups)
    default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
    
    lapply(seq_along(group_levels), function(i) {
      colourpicker::colourInput(
        paste0("allo_group_color_", i),
        paste0(group_levels[i], ":"),
        value = default_colors[min(i, length(default_colors))]
      )
    })
  })
  
  # Get allometry group colors
  get_allo_group_colors <- reactive({
    req(rv$groups)
    group_levels <- levels(rv$groups)
    default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
    
    sapply(seq_along(group_levels), function(i) {
      input_name <- paste0("allo_group_color_", i)
      if (!is.null(input[[input_name]])) {
        input[[input_name]]
      } else {
        default_colors[min(i, length(default_colors))]
      }
    })
  })

  # Generate UI for centroid size individual group colors
  output$cs_group_colors_ui <- renderUI({
    req(input$cs_groups_select)
    selected_groups <- input$cs_groups_select
    if (is.null(selected_groups) || length(selected_groups) == 0) return(NULL)
    
    default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
    
    lapply(seq_along(selected_groups), function(i) {
      colourpicker::colourInput(
        paste0("cs_group_color_", i),
        paste0(selected_groups[i], ":"),
        value = default_colors[min(i, length(default_colors))]
      )
    })
  })
  
  # Generate UI for dispersion individual group colors  
  output$disp_group_colors_ui <- renderUI({
    req(input$disp_groups_select)
    selected_groups <- input$disp_groups_select
    if (is.null(selected_groups) || length(selected_groups) == 0) return(NULL)
    
    default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
    
    lapply(seq_along(selected_groups), function(i) {
      colourpicker::colourInput(
        paste0("disp_group_color_", i),
        paste0(selected_groups[i], ":"),
        value = default_colors[min(i, length(default_colors))]
      )
    })
  })

  # Parse links from text input - returns NULL if empty or invalid
  parse_links <- reactive({
    # Allow empty input - return NULL gracefully
    if (is.null(input$links_input) || nchar(trimws(input$links_input)) == 0) {
      return(NULL)
    }

    tryCatch(
      {
        # Parse comma-separated pairs
        link_str <- gsub("\\s+", "", input$links_input)
        
        # Check if string is empty after cleanup
        if (nchar(link_str) == 0) {
          return(NULL)
        }
        
        nums <- as.numeric(unlist(strsplit(link_str, ",")))
        
        # Filter out NAs (invalid numbers)
        nums <- nums[!is.na(nums)]
        
        if (length(nums) == 0) {
          return(NULL)
        }

        if (length(nums) %% 2 != 0) {
          showNotification("Invalid link format: must have even number of values",
            type = "warning"
          )
          return(NULL)
        }

        matrix(nums, ncol = 2, byrow = TRUE)
      },
      error = function(e) {
        return(NULL)
      }
    )
  })
  
  # Handle links file upload - populate the text input with file contents
  observeEvent(input$links_file, {
    req(input$links_file)
    
    tryCatch({
      # Read the file content
      file_content <- readLines(input$links_file$datapath, warn = FALSE)
      # Combine all lines and clean up
      links_text <- paste(file_content, collapse = ",")
      # Remove any extra whitespace and ensure proper comma separation
      links_text <- gsub("\\s+", "", links_text)
      # Remove any trailing commas
      links_text <- gsub(",+$", "", links_text)
      links_text <- gsub("^,+", "", links_text)
      # Replace multiple commas with single comma
      links_text <- gsub(",+", ",", links_text)
      
      # Format nicely with spaces after commas for readability
      # Split by comma, group in pairs
      nums <- unlist(strsplit(links_text, ","))
      if (length(nums) %% 2 == 0) {
        pairs <- paste(nums[seq(1, length(nums), by = 2)], 
                      nums[seq(2, length(nums), by = 2)], sep = ",")
        links_text <- paste(pairs, collapse = ", ")
      }
      
      # Update the text area input
      updateTextAreaInput(session, "links_input", value = links_text)
      
      showNotification("Links file loaded successfully!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error reading links file:", e$message), type = "error")
    })
  })
  
  # Reactive links that update in real-time for Procrustes plot
  reactive_links <- reactive({
    parse_links()
  })
  
  # Helper function to get validated links (filters out invalid indices)
  get_valid_links <- function(links, n_landmarks) {
    if (is.null(links) || nrow(links) == 0 || is.null(n_landmarks) || n_landmarks < 2) {
      return(NULL)
    }
    # Filter links to only include those with valid indices
    valid_rows <- apply(links, 1, function(row) {
      all(row >= 1 & row <= n_landmarks)
    })
    if (sum(valid_rows) == 0) return(NULL)
    links[valid_rows, , drop = FALSE]
  }
  
  # Clear links button handler
  observeEvent(input$clear_links, {
    updateTextAreaInput(session, "links_input", value = "")
    showNotification("Landmark links cleared", type = "message", duration = 2)
  })
  
  # Dimension mismatch warning observer
  observeEvent(input$dimension, {
    if (!is.null(rv$coords)) {
      data_dims <- dim(rv$coords)[2]
      selected_dim <- ifelse(input$dimension == "2d", 2, 3)
      if (data_dims != selected_dim) {
        showNotification(
          paste0("Warning: Your data has ", data_dims, "D landmarks, but you selected ", 
                 selected_dim, "D. Some features may not work correctly."),
          type = "warning",
          duration = 8
        )
      }
    }
  }, ignoreInit = TRUE)
  
  # Save links to file handler
  output$save_links <- downloadHandler(
    filename = function() {
      paste0("landmark_links_", Sys.Date(), ".txt")
    },
    content = function(file) {
      links_text <- input$links_input
      if (is.null(links_text) || nchar(trimws(links_text)) == 0) {
        writeLines("# No landmark links defined", file)
      } else {
        writeLines(links_text, file)
      }
    }
  )

  # Load and process data
  observeEvent(input$run_analysis, {
    req(input$data_file)

    withProgress(message = "Processing data...", value = 0, {
      tryCatch(
        {
          # Step 1: Load data
          incProgress(0.1, detail = "Loading data file...")

          if (input$input_type == "morphologika") {
            morpho_data <- read.morphologika(input$data_file$datapath)
            rv$coords <- morpho_data$coords
            rv$groups <- factor(morpho_data$labels)
          } else if (input$input_type == "tps") {
            rv$coords <- readland.tps(input$data_file$datapath)

            # Handle classifiers if provided
            if (!is.null(input$classifiers_file)) {
              classifiers <- read.table(input$classifiers_file$datapath,
                header = FALSE, stringsAsFactors = FALSE
              )
              rv$groups <- factor(classifiers[, 1])
            } else {
              rv$groups <- factor(rep("Group1", dim(rv$coords)[3]))
            }
          } else if (input$input_type == "nts") {
            rv$coords <- readland.nts(input$data_file$datapath)
            rv$groups <- factor(rep("Group1", dim(rv$coords)[3]))
          } else if (input$input_type == "fcsv") {
            # Read FCSV file(s) from 3D Slicer using geomorph's readland.fcsv
            rv$coords <- readland.fcsv(input$data_file$datapath)
            
            # Handle classifiers if provided
            if (!is.null(input$classifiers_file)) {
              classifiers <- read.table(input$classifiers_file$datapath,
                header = FALSE, stringsAsFactors = FALSE
              )
              rv$groups <- factor(classifiers[, 1])
            } else {
              rv$groups <- factor(rep("Group1", dim(rv$coords)[3]))
            }
          }

          # Auto-detect 2D/3D dimensions from data
          detected_dims <- dim(rv$coords)[2]
          if (detected_dims == 2) {
            updateRadioButtons(session, "dimension", selected = "2d")
            showNotification("Auto-detected 2D landmark data", type = "message", duration = 4)
          } else if (detected_dims == 3) {
            updateRadioButtons(session, "dimension", selected = "3d")
            showNotification("Auto-detected 3D landmark data", type = "message", duration = 4)
          }

          # Step 2: GPA (Generalized Procrustes Superimposition)
          incProgress(0.15, detail = "Performing Procrustes alignment...")
          rv$gpa <- gpagen(rv$coords, print.progress = FALSE)

          # ===== CENTROID SIZE ANALYSIS =====
          incProgress(0.20, detail = "Computing centroid sizes...")
          # Calculate centroid size for each specimen
          rv$centroid_size <- rv$gpa$Csize
          cs_data <- data.frame(
            CS = rv$centroid_size,
            Group = rv$groups
          )

          # Normality test on CS values (Shapiro-Wilk test)
          incProgress(0.22, detail = "Testing CS normality...")
          rv$cs_normality <- by(cs_data$CS, cs_data$Group, function(x) {
            if (length(x) >= 3) {
              shapiro.test(x)
            } else {
              list(
                statistic = NA, p.value = NA,
                message = "Sample size too small for Shapiro-Wilk test"
              )
            }
          })

          # Inter-group comparison of CS (t-test)
          incProgress(0.24, detail = "Comparing CS between groups...")
          if (length(levels(rv$groups)) == 2) {
            rv$cs_ttest <- t.test(CS ~ Group, data = cs_data)
          } else if (length(levels(rv$groups)) > 2) {
            # Use ANOVA for more than 2 groups
            rv$cs_ttest <- aov(CS ~ Group, data = cs_data)
          }

          # ===== ALLOMETRY CORRECTION =====
          incProgress(0.26, detail = "Performing allometry correction...")
          # Multivariate regression of shape on log(CS)
          gdf_allometry <- geomorph.data.frame(
            shape = rv$gpa$coords,
            logCS = log(rv$centroid_size),
            group = rv$groups
          )

          rv$allometry_fit <- procD.lm(shape ~ logCS,
            data = gdf_allometry,
            iter = input$allometry_perms,
            RRPP = TRUE,
            print.progress = FALSE
          )

          # Extract residuals (size-corrected shape variables) - for reference only
          rv$allometry_residuals <- arrayspecs(
            residuals(rv$allometry_fit),
            p = dim(rv$gpa$coords)[1],
            k = dim(rv$gpa$coords)[2]
          )

          # Step 3: PCA on GPA-aligned coordinates (not allometry-corrected)
          incProgress(0.30, detail = "Computing PCA...")
          rv$pca <- gm.prcomp(rv$gpa$coords)

          # Step 4: Extract PC scores (following gm.prcomp output format)
          pc_scores <- rv$pca$x
          rv$scores <- as.data.frame(pc_scores)
          colnames(rv$scores) <- gsub("Comp", "PC", colnames(rv$scores))
          rv$scores$Group <- rv$groups
          rv$scores$Specimen <- rownames(pc_scores)

          # Store individual PC scores for easy access
          rv$pc1_scores <- pc_scores[, 1]
          rv$pc2_scores <- pc_scores[, 2]

          # Step 5: Calculate proportion of variance explained (prcomp-style)
          incProgress(0.38, detail = "Calculating variance...")
          pc_variance <- rv$pca$sdev^2
          prop_var <- pc_variance / sum(pc_variance)
          rv$pc_var <- round(100 * prop_var, 2)

          # Identify PCs explaining user-defined variance threshold
          cumsum_var <- cumsum(prop_var)
          var_threshold <- input$pca_variance_threshold / 100
          n_pcs_threshold <- which(cumsum_var >= var_threshold)[1]
          if (is.na(n_pcs_threshold)) n_pcs_threshold <- length(cumsum_var)
          rv$pca_95 <- list(
            n_components = n_pcs_threshold,
            variance_explained = cumsum_var[n_pcs_threshold] * 100,
            threshold = input$pca_variance_threshold
          )

          # Step 6: PERMANOVA using PCs accounting for 95% variance
          incProgress(0.42, detail = "Running PERMANOVA on 95% PCs...")
          # Extract PC scores for variance threshold
          pc_scores_95 <- rv$pca$x[, 1:n_pcs_threshold]
          # Create data frame with PC scores
          pc_data_95 <- as.data.frame(pc_scores_95)
          pc_data_95$group <- rv$groups

          # Run PERMANOVA using adonis2 with configurable permutations
          dist_matrix_pc95 <- dist(pc_scores_95, method = "euclidean")
          rv$fit_pc95 <- adonis2(dist_matrix_pc95 ~ group,
            data = pc_data_95,
            permutations = input$n_perms_pc95,
            method = "euclidean"
          )

          # Shape-based PERMANOVA using GPA-aligned coordinates
          gdf <- geomorph.data.frame(shape = rv$gpa$coords, group = rv$groups)
          rv$fit <- procD.lm(shape ~ group,
            data = gdf,
            iter = input$n_perms, RRPP = TRUE
          )

          # Step 7: PERMDISP test (betadisper)
          incProgress(0.6, detail = "Running PERMDISP test...")
          # Create distance matrix from Procrustes-aligned coordinates
          coords_matrix <- two.d.array(rv$gpa$coords)
          dist_mat <- dist(coords_matrix)
          rv$betadisper <- betadisper(dist_mat, rv$groups)
          rv$betadisper_anova <- anova(rv$betadisper)

          # Step 8: Pairwise comparisons
          incProgress(0.65, detail = "Performing pairwise tests...")
          rv$pairwise <- pairwise(rv$fit, groups = rv$groups)

          # Step 9: Centroids
          incProgress(0.75, detail = "Computing centroids...")
          rv$centroids <- aggregate(rv$scores[, c("PC1", "PC2")],
            by = list(Group = rv$scores$Group), FUN = mean
          )

          # Step 10: Parse links
          incProgress(0.85, detail = "Setting up wireframes...")
          rv$links <- parse_links()
          
          # Show warning if links are not provided
          if (is.null(rv$links) || nrow(rv$links) == 0) {
            showNotification(
              "⚠️ Landmark links not found. Some wireframe plots will not be visible. You can add links later in the Wireframe Settings.",
              type = "warning",
              duration = 8
            )
          }

          # Step 11: Shape reconstruction
          incProgress(0.95, detail = "Reconstructing shapes...")
          rv$ref <- mshape(rv$gpa$coords)

          incProgress(1, detail = "Complete!")

          showNotification("Analysis completed successfully!",
            type = "message", duration = 3
          )
        },
        error = function(e) {
          showNotification(paste("Error:", e$message), type = "error", duration = 10)
        }
      )
    })
  })

  # Update specimen choices for Procrustes plot
  observe({
    req(rv$scores)
    if (!is.null(rv$scores$Specimen)) {
      updateSelectInput(session, "procrustes_specimen", 
                        choices = rv$scores$Specimen)
    }
  })

  # Main PCA Plot (single plot with type selection including 3D)
  output$pca_plot_main <- renderPlot({
    req(rv$scores, rv$pc_var, input$pc_x, input$pc_y)

    # Handle 3D plot separately
    if (input$pca_plot_type == "3d") {
      req(input$pc_z)
      # For 3D, use plotly or scatterplot3d - using base R 3D for now
      library(scatterplot3d)
      
      pc_x_num <- as.numeric(gsub("PC", "", input$pc_x))
      pc_y_num <- as.numeric(gsub("PC", "", input$pc_y))
      pc_z_num <- as.numeric(gsub("PC", "", input$pc_z))
      
      plot_data <- rv$scores
      plot_data$Group <- get_renamed_groups()
      
      comp_x <- paste0("PC", pc_x_num)
      comp_y <- paste0("PC", pc_y_num)
      comp_z <- paste0("PC", pc_z_num)
      
      # Apply axis flipping for 3D plot (Display Options)
      if (input$pca_flip_x) {
        plot_data[[comp_x]] <- -plot_data[[comp_x]]
      }
      if (input$pca_flip_y) {
        plot_data[[comp_y]] <- -plot_data[[comp_y]]
      }
      
      x_label <- paste0(input$pc_x, " (", rv$pc_var[pc_x_num], "%)") 
      y_label <- paste0(input$pc_y, " (", rv$pc_var[pc_y_num], "%)") 
      z_label <- paste0(input$pc_z, " (", rv$pc_var[pc_z_num], "%)") 
      
      colors <- get_group_colors()
      if (is.null(colors) || length(colors) == 0) {
        colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
      }
      group_cols <- colors[as.numeric(plot_data$Group)]
      
      # Set background color
      par(bg = input$plot_bg)
      
      s3d <- scatterplot3d(plot_data[[comp_x]], plot_data[[comp_y]], plot_data[[comp_z]],
                   color = group_cols, pch = 19, cex.symbols = input$pca_3d_point_size,
                   xlab = x_label, ylab = y_label, zlab = z_label,
                   main = input$pca_title,
                   grid = TRUE, box = TRUE)
      
      # Add data point labels if enabled
      if (isTRUE(input$pca_show_labels)) {
        coords_2d <- s3d$xyz.convert(plot_data[[comp_x]], plot_data[[comp_y]], plot_data[[comp_z]])
        text(coords_2d$x, coords_2d$y, labels = plot_data$Specimen, pos = 3, cex = 0.7, offset = 0.3)
      }
      
      legend("topright", legend = levels(plot_data$Group),
             col = colors[seq_along(levels(plot_data$Group))], 
             pch = 19, cex = 0.8, bg = "white")
      
      return()
    }

    # Get PC indices
    pc_x_num <- as.numeric(gsub("PC", "", input$pc_x))
    pc_y_num <- as.numeric(gsub("PC", "", input$pc_y))

    # Prepare data with renamed groups
    plot_data <- rv$scores
    plot_data$Group <- get_renamed_groups()

    # Map PC selection to PC column names
    comp_x <- paste0("PC", pc_x_num)
    comp_y <- paste0("PC", pc_y_num)

    # Apply axis flipping
    if (input$pca_flip_x) {
      plot_data[[comp_x]] <- -plot_data[[comp_x]]
    }
    if (input$pca_flip_y) {
      plot_data[[comp_y]] <- -plot_data[[comp_y]]
    }

    # Get axis labels with variance percentages
    x_label <- paste0(input$pc_x, " (", rv$pc_var[pc_x_num], "%)")
    y_label <- paste0(input$pc_y, " (", rv$pc_var[pc_y_num], "%)")

    if (input$pca_plot_type == "color") {
      # Colored plot - base plot without ellipse
      p <- ggplot(plot_data, aes_string(
        x = comp_x, y = comp_y,
        color = "Group", fill = "Group"
      )) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")
      
      # Add data spread visualization based on selection
      spread_type <- if (!is.null(input$pca_spread_type)) input$pca_spread_type else "ellipse"
      
      # Get spread customization settings
      spread_fill <- if (!is.null(input$pca_spread_fill) && input$pca_spread_customize) input$pca_spread_fill else TRUE
      spread_outline <- if (!is.null(input$pca_spread_outline) && input$pca_spread_customize) input$pca_spread_outline else TRUE
      spread_alpha <- if (!is.null(input$pca_spread_fill_alpha) && input$pca_spread_customize) input$pca_spread_fill_alpha else 0.2
      spread_lwd <- if (!is.null(input$pca_spread_line_width) && input$pca_spread_customize) input$pca_spread_line_width else 1.5
      ellipse_level <- if (!is.null(input$pca_ellipse_level) && input$pca_spread_customize) input$pca_ellipse_level else input$confidence
      
      if (spread_type == "ellipse") {
        if (spread_fill && spread_outline) {
          p <- p + stat_ellipse(type = "norm", alpha = spread_alpha, level = ellipse_level, size = spread_lwd, geom = "polygon")
        } else if (spread_fill) {
          p <- p + stat_ellipse(type = "norm", alpha = spread_alpha, level = ellipse_level, size = 0, geom = "polygon")
        } else if (spread_outline) {
          p <- p + stat_ellipse(type = "norm", level = ellipse_level, size = spread_lwd, geom = "path")
        }
      } else if (spread_type == "hull") {
        # Convex hulls
        hull_data <- do.call(rbind, lapply(split(plot_data, plot_data$Group), function(df) {
          df[chull(df[[comp_x]], df[[comp_y]]), ]
        }))
        if (spread_fill && spread_outline) {
          p <- p + geom_polygon(data = hull_data, aes_string(x = comp_x, y = comp_y, fill = "Group", color = "Group"),
                               alpha = spread_alpha, size = spread_lwd)
        } else if (spread_fill) {
          p <- p + geom_polygon(data = hull_data, aes_string(x = comp_x, y = comp_y, fill = "Group"),
                               alpha = spread_alpha, color = NA)
        } else if (spread_outline) {
          p <- p + geom_polygon(data = hull_data, aes_string(x = comp_x, y = comp_y, color = "Group"),
                               fill = NA, size = spread_lwd)
        }
      } else if (spread_type == "density") {
        # Density contour lines
        density_bins <- if (!is.null(input$pca_density_bins) && input$pca_spread_customize) input$pca_density_bins else 5
        if (spread_fill && spread_outline) {
          p <- p + geom_density_2d_filled(aes_string(x = comp_x, y = comp_y), alpha = spread_alpha, bins = density_bins, show.legend = FALSE) +
                   geom_density_2d(aes_string(x = comp_x, y = comp_y, color = "Group"), size = spread_lwd, bins = density_bins)
        } else if (spread_fill) {
          p <- p + geom_density_2d_filled(aes_string(x = comp_x, y = comp_y), alpha = spread_alpha, bins = density_bins, show.legend = FALSE)
        } else if (spread_outline) {
          p <- p + geom_density_2d(aes_string(x = comp_x, y = comp_y, color = "Group"), size = spread_lwd, bins = density_bins)
        }
      }
      # spread_type == "none" - no visualization added
      
      p <- p + geom_point(size = 4, shape = 21, stroke = 1.5) +
        theme_classic(base_size = 14) +
        theme(
          legend.position = "right",
          legend.title = element_blank(),
          plot.background = element_rect(fill = input$plot_bg),
          panel.background = element_rect(fill = input$plot_bg)
        ) +
        labs(x = x_label, y = y_label, title = input$pca_title)

      # Add labels if requested
      if (input$pca_show_labels) {
        p <- p + geom_text(aes(label = Specimen), vjust = -1, size = 3)
      }

      # Apply custom colors if enabled
      colors <- get_group_colors()
      if (!is.null(colors)) {
        p <- p + scale_color_manual(values = colors) +
          scale_fill_manual(values = colors)
      }
    } else {
      # Black & White plot
      n_groups <- length(unique(plot_data$Group))
      group_names <- levels(plot_data$Group)
      
      # Get shapes - either auto or custom
      if (input$pca_bw_auto_shapes) {
        # Auto mode: alternate between filled and unfilled shapes
        base_shapes <- c(21, 1, 24, 2, 22, 0, 23)  # Alternating filled/unfilled
        shape_values <- base_shapes[seq_len(n_groups)]
      } else {
        # Custom shapes from user input
        shape_values <- sapply(seq_len(n_groups), function(i) {
          shape_input <- paste0("pca_bw_shape_", i)
          if (!is.null(input[[shape_input]])) as.numeric(input[[shape_input]]) else 21
        })
      }
      
      # Determine fill based on shape type
      # Filled shapes (21-25) get black fill, unfilled shapes (0-6) get white
      fill_values <- sapply(shape_values, function(shape_val) {
        if (shape_val %in% c(21, 22, 23, 24, 25)) "black" else "white"
      })
      
      point_size <- if (!is.null(input$pca_bw_point_size)) input$pca_bw_point_size else 5
      
      p <- ggplot(plot_data, aes_string(
        x = comp_x, y = comp_y,
        shape = "Group", fill = "Group"
      )) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")
      
      # Add data spread visualization for B&W plot
      spread_type <- if (!is.null(input$pca_spread_type)) input$pca_spread_type else "ellipse"
      spread_outline <- if (!is.null(input$pca_spread_outline) && input$pca_spread_customize) input$pca_spread_outline else TRUE
      spread_lwd <- if (!is.null(input$pca_spread_line_width) && input$pca_spread_customize) input$pca_spread_line_width else 1.5
      ellipse_level <- if (!is.null(input$pca_ellipse_level) && input$pca_spread_customize) input$pca_ellipse_level else input$confidence
      
      if (spread_type == "ellipse" && spread_outline) {
        p <- p + stat_ellipse(level = ellipse_level, color = "black", size = spread_lwd)
      } else if (spread_type == "hull" && spread_outline) {
        hull_data <- do.call(rbind, lapply(split(plot_data, plot_data$Group), function(df) {
          df[chull(df[[comp_x]], df[[comp_y]]), ]
        }))
        p <- p + geom_polygon(data = hull_data, aes_string(x = comp_x, y = comp_y, group = "Group"),
                             fill = NA, color = "black", size = spread_lwd)
      } else if (spread_type == "density" && spread_outline) {
        density_bins <- if (!is.null(input$pca_density_bins) && input$pca_spread_customize) input$pca_density_bins else 5
        p <- p + geom_density_2d(aes_string(x = comp_x, y = comp_y, group = "Group"), color = "black", size = spread_lwd, bins = density_bins)
      }
      
      p <- p + geom_point(size = point_size, stroke = 1.5, color = "black") +
        scale_shape_manual(values = shape_values) +
        scale_fill_manual(values = fill_values) +
        theme_classic(base_size = 14) +
        theme(
          legend.position = "right",
          legend.title = element_blank(),
          plot.background = element_rect(fill = input$plot_bg),
          panel.background = element_rect(fill = input$plot_bg)
        ) +
        labs(x = x_label, y = y_label, title = input$pca_title)
      
      # Add labels if requested
      if (input$pca_show_labels) {
        p <- p + geom_text(aes(label = Specimen), vjust = -1, size = 3)
      }
    }

    p
  })

  # Dynamic UI for PCA B&W custom shape selection
  output$pca_bw_shape_ui <- renderUI({
    req(rv$groups, !input$pca_bw_auto_shapes)
    
    shape_choices <- list(
      "Circle (Filled)" = 21,
      "Circle (Unfilled)" = 1,
      "Triangle (Filled)" = 24,
      "Triangle (Unfilled)" = 2,
      "Square (Filled)" = 22,
      "Square (Unfilled)" = 0,
      "Diamond (Filled)" = 23,
      "Diamond (Unfilled)" = 5,
      "Inverted Triangle (Filled)" = 25,
      "Inverted Triangle (Unfilled)" = 6,
      "Cross" = 3,
      "Plus" = 4,
      "Star" = 8
    )
    
    group_names <- levels(rv$groups)
    shape_inputs <- lapply(seq_along(group_names), function(i) {
      selectInput(
        inputId = paste0("pca_bw_shape_", i),
        label = group_names[i],
        choices = shape_choices,
        selected = shape_choices[[((i - 1) %% length(shape_choices)) + 1]]
      )
    })
    
    tagList(shape_inputs)
  })

  # Data Summary Output
  output$data_summary <- renderPrint({
    req(rv$coords, rv$groups, rv$pc_var)

    cat("=================================================\n")
    cat("       DATA SUMMARY                              \n")
    cat("=================================================\n\n")
    cat("Total specimens:", dim(rv$coords)[3], "\n")
    cat("Number of landmarks:", dim(rv$coords)[1], "\n")
    cat("Dimensions:", if (input$dimension == "3d") "3D" else "2D", "\n")
    cat("Groups:", paste(levels(rv$groups), collapse = ", "), "\n")

    if (!is.null(rv$pca_95)) {
      cat("\nPCs explaining 95% of variance:", rv$pca_95$n_components, "\n")
      cat("Cumulative variance explained:", round(rv$pca_95$variance_explained, 2), "%\n")
    }
  })

  # PC Variance Output
  output$pc_variance_output <- renderPrint({
    req(rv$pca, rv$pc_var)

    cat("=================================================\n")
    cat("       PROPORTION OF VARIANCE EXPLAINED          \n")
    cat("=================================================\n\n")

    # Calculate variance
    pc_variance <- rv$pca$sdev^2
    prop_var <- pc_variance / sum(pc_variance)
    pc_var_pct <- round(100 * prop_var, 2)

    # Show first 9 PCs
    n_show <- min(9, length(pc_var_pct))
    cat("Variance explained by first", n_show, "PCs (%):\n\n")

    var_df <- data.frame(
      PC = paste0("PC", 1:n_show),
      Variance = pc_var_pct[1:n_show],
      Cumulative = round(cumsum(prop_var[1:n_show]) * 100, 2)
    )
    print(var_df, row.names = FALSE)

    cat("\n")
    cat(
      "Total variance explained by first", n_show, "PCs:",
      round(sum(prop_var[1:n_show]) * 100, 2), "%\n"
    )
  })

  # Centroid Size Analysis
  output$cs_analysis <- renderPrint({
    req(rv$centroid_size, rv$groups, rv$cs_normality)

    cat("=================================================\n")
    cat("       CENTROID SIZE ANALYSIS                    \n")
    cat("=================================================\n\n")

    # Summary statistics by group
    cat("Centroid Size Summary by Group:\n")
    cs_summary <- aggregate(rv$centroid_size ~ rv$groups,
      FUN = function(x) {
        c(
          mean = mean(x),
          sd = sd(x),
          min = min(x),
          max = max(x)
        )
      }
    )
    print(cs_summary)
    cat("\n")

    # Shapiro-Wilk normality test results
    cat("--- Shapiro-Wilk Normality Test on CS ---\n")
    for (group_name in names(rv$cs_normality)) {
      test_result <- rv$cs_normality[[group_name]]
      cat("\nGroup:", group_name, "\n")
      if (!is.null(test_result$statistic)) {
        cat("  W-statistic:", round(test_result$statistic, 4), "\n")
        cat("  p-value:", format.pval(test_result$p.value, digits = 4), "\n")
        if (test_result$p.value > 0.05) {
          cat("  Interpretation: CS values are normally distributed (p > 0.05)\n")
        } else {
          cat("  Interpretation: CS values deviate from normality (p < 0.05)\n")
        }
      } else {
        cat("  ", test_result$message, "\n")
      }
    }
    cat("\n")

    # Inter-group comparison
    cat("--- Inter-group Comparison of CS ---\n")
    if (!is.null(rv$cs_ttest)) {
      if (inherits(rv$cs_ttest, "htest")) {
        # Two-sample t-test
        cat("Independent Two-Sample t-test:\n")
        print(rv$cs_ttest)
        cat("\nInterpretation:\n")
        if (rv$cs_ttest$p.value < 0.05) {
          cat("Significant difference in centroid size between groups (p < 0.05)\n")
        } else {
          cat("No significant difference in centroid size between groups (p >= 0.05)\n")
        }
      } else {
        # ANOVA for multiple groups
        cat("One-way ANOVA:\n")
        print(summary(rv$cs_ttest))
      }
    }
  })

  # Allometry Results
  output$allometry_results <- renderPrint({
    req(rv$allometry_fit)

    cat("=================================================\n")
    cat("     ALLOMETRY REGRESSION (Shape ~ log(CS))     \n")
    cat("=================================================\n\n")
    cat("Multivariate regression: 10,000 permutations\n")
    cat("Residuals used for downstream analyses (size-corrected shape)\n\n")

    print(summary(rv$allometry_fit))

    cat("\nInterpretation:\n")
    fit_summary <- summary(rv$allometry_fit)
    p_value_allom <- fit_summary$table$`Pr(>F)`[1]

    if (!is.na(p_value_allom)) {
      if (p_value_allom < 0.05) {
        cat("Significant allometric effect detected (p < 0.05)\n")
        cat("Shape is significantly related to size.\n")
        cat("Using size-corrected residuals for subsequent analyses.\n")
      } else {
        cat("No significant allometric effect (p >= 0.05)\n")
        cat("Shape is not significantly related to size.\n")
      }
    }
  })

  # PERMANOVA on 95% PCs
  output$permanova_pc95_results <- renderPrint({
    req(rv$fit_pc95)

    cat("=================================================\n")
    cat(sprintf("   PERMANOVA ON %.0f%% PCs (%d iterations)     \n", rv$pca_95$threshold, input$n_perms_pc95))
    cat("=================================================\n\n")
    cat(sprintf("Using Euclidean distance on first %d PCs (%.2f%% variance)\n\n", 
                rv$pca_95$n_components, rv$pca_95$variance_explained))

    print(rv$fit_pc95)

    cat("\nInterpretation:\n")
    p_value_pc95 <- rv$fit_pc95$`Pr(>F)`[1]

    if (!is.na(p_value_pc95)) {
      if (p_value_pc95 < 0.001) {
        cat("Highly significant difference between groups (p < 0.001)\n")
      } else if (p_value_pc95 < 0.05) {
        cat("Significant difference between groups (p < 0.05)\n")
      } else {
        cat("No significant difference between groups (p >= 0.05)\n")
      }
    }
  })

  # PERMANOVA Results (Shape-based)
  output$permanova_results <- renderPrint({
    req(rv$fit)

    cat("=================================================\n")
    cat("       PERMANOVA RESULTS                         \n")
    cat("=================================================\n\n")
    cat("Test: shape ~ group (", input$n_perms, " permutations)\n\n")
    print(summary(rv$fit))
  })

  # PERMANOVA Interpretation
  output$permanova_interpretation <- renderUI({
    req(rv$fit)

    fit_summary <- summary(rv$fit)
    p_value <- fit_summary$table$`Pr(>F)`[1]

    if (is.na(p_value)) {
      return(NULL)
    }

    if (p_value < 0.001) {
      div(
        class = "alert alert-success",
        style = "margin-top: 10px;",
        h5(icon("check-circle"), " Highly Significant Difference"),
        p(sprintf("p-value = %.4f (< 0.001)", p_value)),
        p("There is very strong evidence of significant morphological differences between groups.")
      )
    } else if (p_value < 0.01) {
      div(
        class = "alert alert-success",
        style = "margin-top: 10px;",
        h5(icon("check-circle"), " Significant Difference"),
        p(sprintf("p-value = %.4f (< 0.01)", p_value)),
        p("There is strong evidence of significant morphological differences between groups.")
      )
    } else if (p_value < 0.05) {
      div(
        class = "alert alert-info",
        style = "margin-top: 10px;",
        h5(icon("info-circle"), " Significant Difference"),
        p(sprintf("p-value = %.4f (< 0.05)", p_value)),
        p("There is evidence of significant morphological differences between groups.")
      )
    } else {
      div(
        class = "alert alert-warning",
        style = "margin-top: 10px;",
        h5(icon("times-circle"), " No Significant Difference"),
        p(sprintf("p-value = %.4f (≥ 0.05)", p_value)),
        p("No significant morphological differences detected between groups. Groups may have similar shapes.")
      )
    }
  })

  # PERMDISP Results
  output$permdisp_results <- renderPrint({
    req(rv$betadisper, rv$betadisper_anova)

    cat("=================================================\n")
    cat("  PERMDISP (Multivariate Dispersion Test)       \n")
    cat("=================================================\n\n")
    cat("Testing homogeneity of group dispersions\n")
    cat("Null hypothesis: Group dispersions are equal\n\n")

    print(rv$betadisper_anova)

    cat("\nGroup Distances to Centroid:\n")
    print(rv$betadisper$group.distances)
  })

  # PERMDISP Interpretation
  output$permdisp_interpretation <- renderUI({
    req(rv$betadisper_anova)

    p_value <- rv$betadisper_anova$`Pr(>F)`[1]

    if (p_value > 0.05) {
      div(
        class = "alert alert-success",
        style = "margin-top: 10px;",
        h5(icon("check-circle"), " PERMANOVA is Reliable"),
        p(sprintf("p-value = %.4f (> 0.05)", p_value)),
        p("Group dispersions are homogeneous. The PERMANOVA results are reliable and differences are driven by mean shape differences, not variance.")
      )
    } else {
      div(
        class = "alert alert-warning",
        style = "margin-top: 10px;",
        h5(icon("exclamation-triangle"), " PERMANOVA May Show False Positives"),
        p(sprintf("p-value = %.4f (< 0.05)", p_value)),
        p("Group dispersions differ significantly. PERMANOVA results may be influenced by variance differences rather than mean shape differences. Interpret with caution.")
      )
    }
  })

  # Pairwise Results
  output$pairwise_results <- renderPrint({
    req(rv$pairwise)

    cat("=================================================\n")
    cat("       PAIRWISE COMPARISONS                      \n")
    cat("=================================================\n\n")
    pairwise_summary <- summary(rv$pairwise,
      test.type = "dist",
      confidence = input$confidence, stat.table = TRUE
    )
    print(pairwise_summary)
    
    cat("\n")
    cat("--- Interpretation ---\n")
    # Extract p-values from pairwise results
    if (!is.null(pairwise_summary$summary.table)) {
      p_vals <- pairwise_summary$summary.table[, "Pr > d"]
      sig_pairs <- sum(p_vals < 0.05, na.rm = TRUE)
      total_pairs <- length(p_vals)
      
      cat(sprintf("Conclusion: %d out of %d pairwise comparisons are statistically significant (p < 0.05).\n",
                  sig_pairs, total_pairs))
      
      if (sig_pairs == 0) {
        cat("Result: No significant pairwise differences detected between groups.\n")
      } else if (sig_pairs == total_pairs) {
        cat("Result: All group pairs show significant morphological differences.\n")
      } else {
        cat("Result: Some group pairs show significant morphological differences.\n")
      }
    }
  })

  # Centroids Table
  centroids_df <- reactive({
    # Prefer precomputed centroids (fast path)
    if (!is.null(rv$centroids) && is.data.frame(rv$centroids) && nrow(rv$centroids) > 0) {
      return(rv$centroids)
    }

    # Fallback: compute from the scores table
    if (!is.null(rv$scores) && all(c("Group", "PC1", "PC2") %in% colnames(rv$scores))) {
      return(aggregate(
        rv$scores[, c("PC1", "PC2")],
        by = list(Group = rv$scores$Group),
        FUN = mean
      ))
    }

    # Last-resort fallback: compute directly from PCA scores
    if (!is.null(rv$pca) && !is.null(rv$groups) && !is.null(rv$pca$x) && ncol(rv$pca$x) >= 2) {
      return(aggregate(
        cbind(PC1 = rv$pca$x[, 1], PC2 = rv$pca$x[, 2]),
        by = list(Group = rv$groups),
        FUN = mean
      ))
    }

    data.frame(Message = "Run analysis to populate centroids.")
  })

  output$centroids_ui <- renderUI({
    tagList(
      DTOutput("centroids_table"),
      tableOutput("centroids_table_fallback")
    )
  })

  output$centroids_table <- renderDT({
    df <- centroids_df()
    if (!all(c("PC1", "PC2") %in% colnames(df))) {
      return(NULL)
    }

    datatable(df,
      options = list(pageLength = 10, dom = "t"),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("PC1", "PC2"), digits = 4)
  })

  output$centroids_table_fallback <- renderTable({
    df <- centroids_df()
    if (all(c("PC1", "PC2") %in% colnames(df))) {
      return(NULL)
    }
    df
  }, rownames = FALSE)

  # Scores Table
  output$scores_table <- renderDT({
    req(rv$scores)
    datatable(rv$scores,
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("PC1", "PC2"), digits = 4)
  })

  # Average Shape Table
  output$avg_shape_table <- renderDT({
    req(rv$ref)
    n_landmarks <- nrow(rv$ref)
    n_dims <- ncol(rv$ref)
    
    if (n_dims == 2) {
      df <- data.frame(
        Landmark = seq_len(n_landmarks),
        `Axis 1 (x)` = rv$ref[, 1],
        `Axis 2 (y)` = rv$ref[, 2],
        check.names = FALSE
      )
    } else {
      df <- data.frame(
        Landmark = seq_len(n_landmarks),
        `Axis 1 (x)` = rv$ref[, 1],
        `Axis 2 (y)` = rv$ref[, 2],
        `Axis 3 (z)` = rv$ref[, 3],
        check.names = FALSE
      )
    }
    
    datatable(df,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(columns = if (n_dims == 2) c("Axis 1 (x)", "Axis 2 (y)") else c("Axis 1 (x)", "Axis 2 (y)", "Axis 3 (z)"), digits = 8)
  })

  # PC Coefficients Table
  output$pc_coef_table <- renderDT({
    req(rv$pca)
    
    pc_coef <- rv$pca$rotation
    n_dims <- if (input$dimension == "3d") 3 else 2
    n_landmarks <- nrow(pc_coef) / n_dims
    n_pcs <- min(ncol(pc_coef), 10)  # Show up to 10 PCs
    
    # Create row labels (x1, y1, z1, x2, y2, z2, ...)
    axis_labels <- if (n_dims == 2) c("x", "y") else c("x", "y", "z")
    row_labels <- paste0(rep(axis_labels, n_landmarks), rep(seq_len(n_landmarks), each = n_dims))
    
    df <- as.data.frame(pc_coef[, seq_len(n_pcs)])
    colnames(df) <- paste0("PC", seq_len(n_pcs))
    df <- cbind(Coord = row_labels, df)
    
    datatable(df,
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(columns = paste0("PC", seq_len(n_pcs)), digits = 6)
  })

  # Centroid Size Table
  output$centroid_size_table <- renderDT({
    req(rv$centroid_size, rv$groups)
    
    spec_names <- if (!is.null(rv$scores$Specimen)) rv$scores$Specimen else paste0("Specimen_", seq_along(rv$centroid_size))
    
    df <- data.frame(
      Specimen = spec_names,
      Group = rv$groups,
      `Centroid Size` = rv$centroid_size,
      check.names = FALSE
    )
    
    datatable(df,
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(columns = "Centroid Size", digits = 6)
  })

  # Procrustes Coordinates Table
  output$procrustes_coords_table <- renderDT({
    req(rv$gpa)
    
    coords <- rv$gpa$coords
    n_specimens <- dim(coords)[3]
    n_landmarks <- dim(coords)[1]
    n_dims <- dim(coords)[2]
    
    spec_names <- if (!is.null(rv$scores$Specimen)) rv$scores$Specimen else paste0("Specimen_", seq_len(n_specimens))
    
    # Convert to 2D array format
    coords_2d <- two.d.array(coords)
    
    # Create column names
    axis_labels <- if (n_dims == 2) c("x", "y") else c("x", "y", "z")
    col_names <- paste0(rep(axis_labels, n_landmarks), rep(seq_len(n_landmarks), each = n_dims))
    
    df <- as.data.frame(coords_2d)
    colnames(df) <- col_names
    df <- cbind(Specimen = spec_names, df)
    
    datatable(df,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(columns = col_names, digits = 8)
  })

  # Variance Explained Table
  output$variance_table <- renderDT({
    req(rv$pc_var)
    
    n_pcs <- length(rv$pc_var)
    cumulative_var <- cumsum(rv$pc_var)
    
    df <- data.frame(
      PC = paste0("PC", seq_len(n_pcs)),
      `Variance (%)` = rv$pc_var,
      `Cumulative (%)` = cumulative_var,
      check.names = FALSE
    )
    
    datatable(df,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("Variance (%)", "Cumulative (%)"), digits = 4)
  })

  # Reactive shapes based on wire_pc selection
  get_wireframe_shapes <- reactive({
    req(rv$ref, rv$pca, input$wire_pc, input$wire_pc_min, input$wire_pc_max)

    pc_num <- as.numeric(gsub("PC", "", input$wire_pc))
    ref_vec <- as.vector(rv$ref)
    pc_loadings <- rv$pca$rotation
    n_dims <- if (input$dimension == "3d") 3 else 2

    # Determine PC flip multiplier
    # If sync with PCA is enabled, use the PCA axis settings based on which PC is selected
    pc_multiplier <- 1
    if (isTRUE(input$wire_sync_pca_axis)) {
      # Check if the selected wireframe PC matches PC X or Y from PCA plot
      if (input$wire_pc == input$pc_x && isTRUE(input$pca_flip_x)) {
        pc_multiplier <- -1
      } else if (input$wire_pc == input$pc_y && isTRUE(input$pca_flip_y)) {
        pc_multiplier <- -1
      }
    } else if (isTRUE(input$wire_flip_pc_axis)) {
      pc_multiplier <- -1
    }

    make_shape <- function(score) {
      arrayspecs(
        matrix(ref_vec + pc_loadings[, pc_num] * score * pc_multiplier, ncol = n_dims),
        p = length(ref_vec) / n_dims, k = n_dims
      )[, , 1]
    }

    list(
      shape_minus = make_shape(input$wire_pc_min),
      shape_plus = make_shape(input$wire_pc_max),
      ref = rv$ref
    )
  })

  # Dorsal View (XY plane - top view)
  output$dorsal_minus <- renderPlot({
    req(input$dimension == "3d")
    shapes <- get_wireframe_shapes()
    req(shapes, rv$links)

    ref_2D <- shapes$ref[, c(1, 2)]
    shape_2D <- shapes$shape_minus[, c(1, 2)]
    
    # Apply transformations
    transform_wire_coords <- function(coords) {
      if (input$wire_rotate != "0") {
        angle <- as.numeric(input$wire_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$wire_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$wire_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref_2D <- transform_wire_coords(ref_2D)
    shape_2D <- transform_wire_coords(shape_2D)

    # Determine if wireframe should be shown
    show_wireframe_links <- if (isTRUE(input$wire_show_wireframe)) rv$links else NULL

    par(bg = input$plot_bg)
    plotRefToTarget(ref_2D, shape_2D,
      links = show_wireframe_links,
      method = "vector",
      mag = input$wire_mag,
      gridPars = gridPar(
        pt.bg = input$point_color,
        pt.size = 1.5,
        link.col = input$wire_color_minus,
        link.lwd = input$wire_thickness
      )
    )
    if (isTRUE(input$show_average) && isTRUE(input$wire_show_wireframe)) {
      for (i in seq_len(nrow(rv$links))) {
        lines(ref_2D[rv$links[i, ], 1], ref_2D[rv$links[i, ], 2], col = input$wire_color_avg, lwd = input$wire_thickness)
      }
    }
    # Add landmark numbers if requested
    if (isTRUE(input$wire_show_landmarks)) {
      lm_size <- if (!is.null(input$wire_landmark_size)) input$wire_landmark_size else 1
      lm_color <- if (!is.null(input$wire_landmark_color)) input$wire_landmark_color else "#000000"
      text(ref_2D[, 1], ref_2D[, 2], labels = 1:nrow(ref_2D), 
           cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
    }
    title(paste0(input$wire_pc, " = ", input$wire_pc_min))
  })

  output$dorsal_plus <- renderPlot({
    req(input$dimension == "3d")
    shapes <- get_wireframe_shapes()
    req(shapes, rv$links)

    ref_2D <- shapes$ref[, c(1, 2)]
    shape_2D <- shapes$shape_plus[, c(1, 2)]
    
    # Apply transformations
    transform_wire_coords <- function(coords) {
      if (input$wire_rotate != "0") {
        angle <- as.numeric(input$wire_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$wire_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$wire_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref_2D <- transform_wire_coords(ref_2D)
    shape_2D <- transform_wire_coords(shape_2D)

    # Determine if wireframe should be shown
    show_wireframe_links <- if (isTRUE(input$wire_show_wireframe)) rv$links else NULL

    par(bg = input$plot_bg)
    plotRefToTarget(ref_2D, shape_2D,
      links = show_wireframe_links,
      method = "vector",
      mag = input$wire_mag,
      gridPars = gridPar(
        pt.bg = input$point_color,
        pt.size = 1.5,
        link.col = input$wire_color_plus,
        link.lwd = input$wire_thickness
      )
    )
    if (isTRUE(input$show_average) && isTRUE(input$wire_show_wireframe)) {
      for (i in seq_len(nrow(rv$links))) {
        lines(ref_2D[rv$links[i, ], 1], ref_2D[rv$links[i, ], 2], col = input$wire_color_avg, lwd = input$wire_thickness)
      }
    }
    # Add landmark numbers if requested
    if (isTRUE(input$wire_show_landmarks)) {
      lm_size <- if (!is.null(input$wire_landmark_size)) input$wire_landmark_size else 1
      lm_color <- if (!is.null(input$wire_landmark_color)) input$wire_landmark_color else "#000000"
      text(ref_2D[, 1], ref_2D[, 2], labels = 1:nrow(ref_2D), 
           cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
    }
    title(paste0(input$wire_pc, " = ", input$wire_pc_max))
  })

  # Sagittal View (XZ plane - side view)
  output$sagittal_minus <- renderPlot({
    req(input$dimension == "3d")
    shapes <- get_wireframe_shapes()
    req(shapes, rv$links)

    ref_2D <- shapes$ref[, c(1, 3)]
    shape_2D <- shapes$shape_minus[, c(1, 3)]
    
    # Apply transformations
    transform_wire_coords <- function(coords) {
      if (input$wire_rotate != "0") {
        angle <- as.numeric(input$wire_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$wire_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$wire_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref_2D <- transform_wire_coords(ref_2D)
    shape_2D <- transform_wire_coords(shape_2D)

    # Determine if wireframe should be shown
    show_wireframe_links <- if (isTRUE(input$wire_show_wireframe)) rv$links else NULL

    par(bg = input$plot_bg)
    plotRefToTarget(ref_2D, shape_2D,
      links = show_wireframe_links,
      method = "vector",
      mag = input$wire_mag,
      gridPars = gridPar(
        pt.bg = input$point_color,
        pt.size = 1.5,
        link.col = input$wire_color_minus,
        link.lwd = input$wire_thickness
      )
    )
    if (isTRUE(input$show_average) && isTRUE(input$wire_show_wireframe)) {
      for (i in seq_len(nrow(rv$links))) {
        lines(ref_2D[rv$links[i, ], 1], ref_2D[rv$links[i, ], 2], col = input$wire_color_avg, lwd = input$wire_thickness)
      }
    }
    # Add landmark numbers if requested
    if (isTRUE(input$wire_show_landmarks)) {
      lm_size <- if (!is.null(input$wire_landmark_size)) input$wire_landmark_size else 1
      lm_color <- if (!is.null(input$wire_landmark_color)) input$wire_landmark_color else "#000000"
      text(ref_2D[, 1], ref_2D[, 2], labels = 1:nrow(ref_2D), 
           cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
    }
    title(paste0(input$wire_pc, " = ", input$wire_pc_min))
  })

  output$sagittal_plus <- renderPlot({
    req(input$dimension == "3d")
    shapes <- get_wireframe_shapes()
    req(shapes, rv$links)

    ref_2D <- shapes$ref[, c(1, 3)]
    shape_2D <- shapes$shape_plus[, c(1, 3)]
    
    # Apply transformations
    transform_wire_coords <- function(coords) {
      if (input$wire_rotate != "0") {
        angle <- as.numeric(input$wire_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$wire_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$wire_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref_2D <- transform_wire_coords(ref_2D)
    shape_2D <- transform_wire_coords(shape_2D)

    # Determine if wireframe should be shown
    show_wireframe_links <- if (isTRUE(input$wire_show_wireframe)) rv$links else NULL

    par(bg = input$plot_bg)
    plotRefToTarget(ref_2D, shape_2D,
      links = show_wireframe_links,
      method = "vector",
      mag = input$wire_mag,
      gridPars = gridPar(
        pt.bg = input$point_color,
        pt.size = 1.5,
        link.col = input$wire_color_plus,
        link.lwd = input$wire_thickness
      )
    )
    if (isTRUE(input$show_average) && isTRUE(input$wire_show_wireframe)) {
      for (i in seq_len(nrow(rv$links))) {
        lines(ref_2D[rv$links[i, ], 1], ref_2D[rv$links[i, ], 2], col = input$wire_color_avg, lwd = input$wire_thickness)
      }
    }
    # Add landmark numbers if requested
    if (isTRUE(input$wire_show_landmarks)) {
      lm_size <- if (!is.null(input$wire_landmark_size)) input$wire_landmark_size else 1
      lm_color <- if (!is.null(input$wire_landmark_color)) input$wire_landmark_color else "#000000"
      text(ref_2D[, 1], ref_2D[, 2], labels = 1:nrow(ref_2D), 
           cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
    }
    title(paste0(input$wire_pc, " = ", input$wire_pc_max))
  })

  # Coronal View (YZ plane - frontal view)
  output$coronal_minus <- renderPlot({
    req(input$dimension == "3d")
    shapes <- get_wireframe_shapes()
    req(shapes, rv$links)

    ref_2D <- shapes$ref[, c(2, 3)]
    shape_2D <- shapes$shape_minus[, c(2, 3)]
    
    # Apply transformations
    transform_wire_coords <- function(coords) {
      if (input$wire_rotate != "0") {
        angle <- as.numeric(input$wire_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$wire_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$wire_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref_2D <- transform_wire_coords(ref_2D)
    shape_2D <- transform_wire_coords(shape_2D)

    # Determine if wireframe should be shown
    show_wireframe_links <- if (isTRUE(input$wire_show_wireframe)) rv$links else NULL

    par(bg = input$plot_bg)
    plotRefToTarget(ref_2D, shape_2D,
      links = show_wireframe_links,
      method = "vector",
      mag = input$wire_mag,
      gridPars = gridPar(
        pt.bg = input$point_color,
        pt.size = 1.5,
        link.col = input$wire_color_minus,
        link.lwd = input$wire_thickness
      )
    )
    if (isTRUE(input$show_average) && isTRUE(input$wire_show_wireframe)) {
      for (i in seq_len(nrow(rv$links))) {
        lines(ref_2D[rv$links[i, ], 1], ref_2D[rv$links[i, ], 2], col = input$wire_color_avg, lwd = input$wire_thickness)
      }
    }
    # Add landmark numbers if requested
    if (isTRUE(input$wire_show_landmarks)) {
      lm_size <- if (!is.null(input$wire_landmark_size)) input$wire_landmark_size else 1
      lm_color <- if (!is.null(input$wire_landmark_color)) input$wire_landmark_color else "#000000"
      text(ref_2D[, 1], ref_2D[, 2], labels = 1:nrow(ref_2D), 
           cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
    }
    title(paste0(input$wire_pc, " = ", input$wire_pc_min))
  })

  output$coronal_plus <- renderPlot({
    req(input$dimension == "3d")
    shapes <- get_wireframe_shapes()
    req(shapes, rv$links)

    ref_2D <- shapes$ref[, c(2, 3)]
    shape_2D <- shapes$shape_plus[, c(2, 3)]
    
    # Apply transformations
    transform_wire_coords <- function(coords) {
      if (input$wire_rotate != "0") {
        angle <- as.numeric(input$wire_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$wire_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$wire_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref_2D <- transform_wire_coords(ref_2D)
    shape_2D <- transform_wire_coords(shape_2D)

    # Determine if wireframe should be shown
    show_wireframe_links <- if (isTRUE(input$wire_show_wireframe)) rv$links else NULL

    par(bg = input$plot_bg)
    plotRefToTarget(ref_2D, shape_2D,
      links = show_wireframe_links,
      method = "vector",
      mag = input$wire_mag,
      gridPars = gridPar(
        pt.bg = input$point_color,
        pt.size = 1.5,
        link.col = input$wire_color_plus,
        link.lwd = input$wire_thickness
      )
    )
    if (isTRUE(input$show_average) && isTRUE(input$wire_show_wireframe)) {
      for (i in seq_len(nrow(rv$links))) {
        lines(ref_2D[rv$links[i, ], 1], ref_2D[rv$links[i, ], 2], col = input$wire_color_avg, lwd = input$wire_thickness)
      }
    }
    # Add landmark numbers if requested
    if (isTRUE(input$wire_show_landmarks)) {
      lm_size <- if (!is.null(input$wire_landmark_size)) input$wire_landmark_size else 1
      lm_color <- if (!is.null(input$wire_landmark_color)) input$wire_landmark_color else "#000000"
      text(ref_2D[, 1], ref_2D[, 2], labels = 1:nrow(ref_2D), 
           cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
    }
    title(paste0(input$wire_pc, " = ", input$wire_pc_max))
  })

  # 2D Wireframes
  output$wireframe_2d_minus <- renderPlot({
    req(input$dimension == "2d")
    shapes <- get_wireframe_shapes()
    req(shapes, rv$links)
    
    ref_shape <- shapes$ref
    minus_shape <- shapes$shape_minus
    
    # Apply transformations
    transform_wire_coords <- function(coords) {
      if (input$wire_rotate != "0") {
        angle <- as.numeric(input$wire_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$wire_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$wire_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref_shape <- transform_wire_coords(ref_shape)
    minus_shape <- transform_wire_coords(minus_shape)

    # Determine if wireframe should be shown
    show_wireframe_links <- if (isTRUE(input$wire_show_wireframe)) rv$links else NULL

    par(bg = input$plot_bg)
    plotRefToTarget(ref_shape, minus_shape,
      links = show_wireframe_links,
      method = "vector",
      mag = input$wire_mag,
      gridPars = gridPar(
        pt.bg = input$point_color,
        pt.size = 1.5,
        link.col = input$wire_color_minus,
        link.lwd = input$wire_thickness
      )
    )
    if (isTRUE(input$show_average) && isTRUE(input$wire_show_wireframe)) {
      for (i in seq_len(nrow(rv$links))) {
        lines(ref_shape[rv$links[i, ], 1], ref_shape[rv$links[i, ], 2], col = input$wire_color_avg, lwd = input$wire_thickness)
      }
    }
    # Add landmark numbers if requested
    if (isTRUE(input$wire_show_landmarks)) {
      lm_size <- if (!is.null(input$wire_landmark_size)) input$wire_landmark_size else 1
      lm_color <- if (!is.null(input$wire_landmark_color)) input$wire_landmark_color else "#000000"
      text(ref_shape[, 1], ref_shape[, 2], labels = 1:nrow(ref_shape), 
           cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
    }
    title(paste0(input$wire_pc, " = ", input$wire_pc_min))
  })

  output$wireframe_2d_plus <- renderPlot({
    req(input$dimension == "2d")
    shapes <- get_wireframe_shapes()
    req(shapes, rv$links)
    
    ref_shape <- shapes$ref
    plus_shape <- shapes$shape_plus
    
    # Apply transformations
    transform_wire_coords <- function(coords) {
      if (input$wire_rotate != "0") {
        angle <- as.numeric(input$wire_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$wire_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$wire_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref_shape <- transform_wire_coords(ref_shape)
    plus_shape <- transform_wire_coords(plus_shape)

    # Determine if wireframe should be shown
    show_wireframe_links <- if (isTRUE(input$wire_show_wireframe)) rv$links else NULL

    par(bg = input$plot_bg)
    plotRefToTarget(ref_shape, plus_shape,
      links = show_wireframe_links,
      method = "vector",
      mag = input$wire_mag,
      gridPars = gridPar(
        pt.bg = input$point_color,
        pt.size = 1.5,
        link.col = input$wire_color_plus,
        link.lwd = input$wire_thickness
      )
    )
    if (isTRUE(input$show_average) && isTRUE(input$wire_show_wireframe)) {
      for (i in seq_len(nrow(rv$links))) {
        lines(ref_shape[rv$links[i, ], 1], ref_shape[rv$links[i, ], 2], col = input$wire_color_avg, lwd = input$wire_thickness)
      }
    }
    # Add landmark numbers if requested
    if (isTRUE(input$wire_show_landmarks)) {
      lm_size <- if (!is.null(input$wire_landmark_size)) input$wire_landmark_size else 1
      lm_color <- if (!is.null(input$wire_landmark_color)) input$wire_landmark_color else "#000000"
      text(ref_shape[, 1], ref_shape[, 2], labels = 1:nrow(ref_shape), 
           cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
    }
    title(paste0(input$wire_pc, " = ", input$wire_pc_max))
  })

  # Lollipop Plot (Landmark Displacement Graph)
  output$lollipop_plot <- renderPlot({
    req(rv$ref, rv$pca, input$disp_pc, input$disp_pc_value)
    
    # Get shapes for displacement calculation
    pc_num <- as.numeric(gsub("PC", "", input$disp_pc))
    ref_vec <- as.vector(rv$ref)
    pc_loadings <- rv$pca$rotation
    n_dims <- if (input$dimension == "3d") 3 else 2
    
    # Determine if PC axis should be flipped
    pc_flip_active <- FALSE
    if (isTRUE(input$disp_sync_pca_axis)) {
      if (input$disp_pc == input$pc_x && isTRUE(input$pca_flip_x)) {
        pc_flip_active <- TRUE
      } else if (input$disp_pc == input$pc_y && isTRUE(input$pca_flip_y)) {
        pc_flip_active <- TRUE
      }
    } else if (isTRUE(input$disp_flip_pc_axis)) {
      pc_flip_active <- TRUE
    }
    
    # Create shape at specified PC score with PC axis flip support
    make_shape <- function(score) {
      pc_multiplier <- if (pc_flip_active) -1 else 1
      arrayspecs(
        matrix(ref_vec + pc_loadings[, pc_num] * score * pc_multiplier, ncol = n_dims),
        p = length(ref_vec) / n_dims, k = n_dims
      )[, , 1]
    }
    
    ref_shape <- rv$ref
    target_shape <- make_shape(input$disp_pc_value)
    
    n_landmarks <- nrow(ref_shape)
    
    # Calculate Euclidean distance for each landmark from mean to target PC score
    displacement <- sqrt(rowSums((target_shape - ref_shape)^2))
    
    # Apply magnification
    mag <- if (!is.null(input$disp_mag)) input$disp_mag else 1
    
    # Create data frame for plotting
    lollipop_data <- data.frame(
      Landmark = factor(1:n_landmarks),
      Displacement = displacement * mag
    )
    
    # Find max displacement for y-axis limit
    max_disp <- max(lollipop_data$Displacement) * 1.1
    
    par(mar = c(5, 4, 4, 2), bg = "#FFFFFF")
    
    # Get color
    bar_color <- if (!is.null(input$disp_color)) input$disp_color else "#3498db"
    
    # Create axis direction indicator for title
    axis_indicator <- if (pc_flip_active) " (axis flipped)" else ""
    
    barplot(lollipop_data$Displacement, 
            col = bar_color,
            names.arg = 1:n_landmarks,
            main = paste0("Landmark Displacement - ", input$disp_pc, " = ", input$disp_pc_value, axis_indicator),
            xlab = "Landmark Number",
            ylab = paste0("Displacement", if (mag > 1) paste0(" (x", mag, " magnified)") else ""),
            ylim = c(0, max_disp),
            border = NA)
  })

  # TPS Grid Plot (2D only)
  output$tps_grid_minus <- renderPlot({
    req(input$dimension == "2d", rv$ref, rv$pca, input$wire_pc)
    
    shapes <- get_wireframe_shapes()
    req(shapes, rv$links)
    
    # Apply transformations to shapes before TPS plotting
    transform_wire_coords <- function(coords) {
      if (input$wire_rotate != "0") {
        angle <- as.numeric(input$wire_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$wire_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$wire_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref_transformed <- transform_wire_coords(shapes$ref)
    minus_transformed <- transform_wire_coords(shapes$shape_minus)
    
    par(bg = input$plot_bg)
    plotRefToTarget(ref_transformed, minus_transformed,
      links = rv$links,
      method = "TPS",
      mag = input$wire_mag,
      gridPars = gridPar(
        pt.bg = input$point_color,
        pt.size = 1.5,
        link.col = input$wire_color_minus,
        link.lwd = input$wire_thickness,
        tar.pt.bg = input$wire_color_minus
      )
    )
    # Add landmark numbers if requested
    if (isTRUE(input$wire_show_landmarks)) {
      lm_size <- if (!is.null(input$wire_landmark_size)) input$wire_landmark_size else 1
      lm_color <- if (!is.null(input$wire_landmark_color)) input$wire_landmark_color else "#000000"
      text(ref_transformed[, 1], ref_transformed[, 2], labels = 1:nrow(ref_transformed), 
           cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
    }
    title(paste0("TPS Grid - ", input$wire_pc, " = ", input$wire_pc_min))
  })
  
  output$tps_grid_plus <- renderPlot({
    req(input$dimension == "2d", rv$ref, rv$pca, input$wire_pc)
    
    shapes <- get_wireframe_shapes()
    req(shapes, rv$links)
    
    # Apply transformations to shapes before TPS plotting
    transform_wire_coords <- function(coords) {
      if (input$wire_rotate != "0") {
        angle <- as.numeric(input$wire_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$wire_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$wire_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref_transformed <- transform_wire_coords(shapes$ref)
    plus_transformed <- transform_wire_coords(shapes$shape_plus)
    
    par(bg = input$plot_bg)
    plotRefToTarget(ref_transformed, plus_transformed,
      links = rv$links,
      method = "TPS",
      mag = input$wire_mag,
      gridPars = gridPar(
        pt.bg = input$point_color,
        pt.size = 1.5,
        link.col = input$wire_color_plus,
        link.lwd = input$wire_thickness,
        tar.pt.bg = input$wire_color_plus
      )
    )
    # Add landmark numbers if requested
    if (isTRUE(input$wire_show_landmarks)) {
      lm_size <- if (!is.null(input$wire_landmark_size)) input$wire_landmark_size else 1
      lm_color <- if (!is.null(input$wire_landmark_color)) input$wire_landmark_color else "#000000"
      text(ref_transformed[, 1], ref_transformed[, 2], labels = 1:nrow(ref_transformed), 
           cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
    }
    title(paste0("TPS Grid - ", input$wire_pc, " = ", input$wire_pc_max))
  })

  # Wrapped Outline Plot (2D only)
  output$outline_minus <- renderPlot({
    req(input$dimension == "2d", rv$ref, rv$pca, input$wire_pc)
    
    shapes <- get_wireframe_shapes()
    req(shapes, rv$links)
    
    ref_shape <- shapes$ref
    minus_shape <- shapes$shape_minus
    
    # Apply transformations
    transform_wire_coords <- function(coords) {
      if (input$wire_rotate != "0") {
        angle <- as.numeric(input$wire_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$wire_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$wire_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref_shape <- transform_wire_coords(ref_shape)
    minus_shape <- transform_wire_coords(minus_shape)
    
    # Get plot range
    all_coords <- rbind(ref_shape, minus_shape)
    xlim <- range(all_coords[, 1]) * 1.1
    ylim <- range(all_coords[, 2]) * 1.1
    
    par(bg = input$plot_bg)
    plot(ref_shape[,1], ref_shape[,2], type = "n", xlim = xlim, ylim = ylim,
         xlab = "X", ylab = "Y", asp = 1,
         main = paste0("Outline - ", input$wire_pc, " = ", input$wire_pc_min))
    
    # Draw filled polygon for reference shape
    if (!is.null(rv$links) && nrow(rv$links) > 0) {
      # Create outline order from links
      polygon(ref_shape[, 1], ref_shape[, 2], 
              col = adjustcolor(input$wire_color_avg, alpha.f = 0.3),
              border = input$wire_color_avg, lwd = input$wire_thickness)
    }
    
    # Draw filled polygon for minus shape
    polygon(minus_shape[, 1], minus_shape[, 2], 
            col = adjustcolor(input$wire_color_minus, alpha.f = 0.3),
            border = input$wire_color_minus, lwd = input$wire_thickness)
    
    points(ref_shape[,1], ref_shape[,2], pch = 21, bg = input$wire_color_avg, cex = 1.5)
    points(minus_shape[,1], minus_shape[,2], pch = 21, bg = input$wire_color_minus, cex = 1.5)
    
    # Add landmark numbers if requested
    if (isTRUE(input$wire_show_landmarks)) {
      lm_size <- if (!is.null(input$wire_landmark_size)) input$wire_landmark_size else 1
      lm_color <- if (!is.null(input$wire_landmark_color)) input$wire_landmark_color else "#000000"
      text(ref_shape[, 1], ref_shape[, 2], labels = 1:nrow(ref_shape), 
           cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
    }
  })
  
  output$outline_plus <- renderPlot({
    req(input$dimension == "2d", rv$ref, rv$pca, input$wire_pc)
    
    shapes <- get_wireframe_shapes()
    req(shapes, rv$links)
    
    ref_shape <- shapes$ref
    plus_shape <- shapes$shape_plus
    
    # Apply transformations
    transform_wire_coords <- function(coords) {
      if (input$wire_rotate != "0") {
        angle <- as.numeric(input$wire_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$wire_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$wire_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref_shape <- transform_wire_coords(ref_shape)
    plus_shape <- transform_wire_coords(plus_shape)
    
    # Get plot range
    all_coords <- rbind(ref_shape, plus_shape)
    xlim <- range(all_coords[, 1]) * 1.1
    ylim <- range(all_coords[, 2]) * 1.1
    
    par(bg = input$plot_bg)
    plot(ref_shape[,1], ref_shape[,2], type = "n", xlim = xlim, ylim = ylim,
         xlab = "X", ylab = "Y", asp = 1,
         main = paste0("Outline - ", input$wire_pc, " = ", input$wire_pc_max))
    
    # Draw filled polygon for reference shape
    if (!is.null(rv$links) && nrow(rv$links) > 0) {
      polygon(ref_shape[, 1], ref_shape[, 2], 
              col = adjustcolor(input$wire_color_avg, alpha.f = 0.3),
              border = input$wire_color_avg, lwd = input$wire_thickness)
    }
    
    # Draw filled polygon for plus shape
    polygon(plus_shape[, 1], plus_shape[, 2], 
            col = adjustcolor(input$wire_color_plus, alpha.f = 0.3),
            border = input$wire_color_plus, lwd = input$wire_thickness)
    
    points(ref_shape[,1], ref_shape[,2], pch = 21, bg = input$wire_color_avg, cex = 1.5)
    points(plus_shape[,1], plus_shape[,2], pch = 21, bg = input$wire_color_plus, cex = 1.5)
    
    # Add landmark numbers if requested
    if (isTRUE(input$wire_show_landmarks)) {
      lm_size <- if (!is.null(input$wire_landmark_size)) input$wire_landmark_size else 1
      lm_color <- if (!is.null(input$wire_landmark_color)) input$wire_landmark_color else "#000000"
      text(ref_shape[, 1], ref_shape[, 2], labels = 1:nrow(ref_shape), 
           cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
    }
  })

  # PC Deformation Plot (Mean + Min + Max together)
  output$pc_deformation_plot <- renderPlot({
    req(rv$ref, rv$pca, input$deform_pc, input$deform_pc_min, input$deform_pc_max)

    pc_num <- as.numeric(gsub("PC", "", input$deform_pc))
    ref_vec <- as.vector(rv$ref)
    pc_loadings <- rv$pca$rotation
    n_dims <- if (input$dimension == "3d") 3 else 2

    # Create shapes
    make_shape <- function(score) {
      # Determine PC flip multiplier with sync option
      pc_multiplier <- 1
      if (isTRUE(input$deform_sync_pca_axis)) {
        # Check if the selected deform PC matches PC X or Y from PCA plot
        if (input$deform_pc == input$pc_x && isTRUE(input$pca_flip_x)) {
          pc_multiplier <- -1
        } else if (input$deform_pc == input$pc_y && isTRUE(input$pca_flip_y)) {
          pc_multiplier <- -1
        }
      } else if (isTRUE(input$deform_flip_pc_axis)) {
        pc_multiplier <- -1
      }
      arrayspecs(
        matrix(ref_vec + pc_loadings[, pc_num] * score * pc_multiplier, ncol = n_dims),
        p = length(ref_vec) / n_dims, k = n_dims
      )[, , 1]
    }

    shape_minus <- make_shape(input$deform_pc_min)
    shape_plus <- make_shape(input$deform_pc_max)

    # For 3D, select axes based on user choice
    if (input$dimension == "3d") {
      axes <- switch(input$deform_axes,
        "xy" = c(1, 2),
        "xz" = c(1, 3),
        "yz" = c(2, 3)
      )
      ref_plot <- rv$ref[, axes]
      shape_minus_plot <- shape_minus[, axes]
      shape_plus_plot <- shape_plus[, axes]
    } else {
      ref_plot <- rv$ref
      shape_minus_plot <- shape_minus
      shape_plus_plot <- shape_plus
    }
    
    # Apply transformations
    transform_coords <- function(coords) {
      # Apply rotation
      if (input$deform_rotate != "0") {
        angle <- as.numeric(input$deform_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      
      # Apply flipping
      if (input$deform_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$deform_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      
      return(coords)
    }
    
    ref_plot <- transform_coords(ref_plot)
    shape_minus_plot <- transform_coords(shape_minus_plot)
    shape_plus_plot <- transform_coords(shape_plus_plot)

    # Get variance for title
    var_pct <- if (!is.null(rv$pc_var)) rv$pc_var[pc_num] else NA

    # Plot all three shapes together
    par(bg = input$plot_bg, mar = c(4, 4, 3, 1))

    # Determine plot range
    all_coords <- rbind(ref_plot, shape_minus_plot, shape_plus_plot)
    xlim <- range(all_coords[, 1]) * 1.1
    ylim <- range(all_coords[, 2]) * 1.1

    # Initialize plot
    plot(ref_plot[,1], ref_plot[,2],
      type = "n", xlim = xlim, ylim = ylim,
      xlab = "X", ylab = "Y", asp = 1,
      main = if (!is.na(var_pct)) {
        paste0(input$deform_pc, " Shape Deformations (", var_pct, "% variance)")
      } else {
        paste0(input$deform_pc, " Shape Deformations")
      }
    )

    # Check if wireframe should be shown and links are available
    show_wireframe <- isTRUE(input$deform_show_wireframe) && !is.null(rv$links) && nrow(rv$links) > 0
    show_mean <- isTRUE(input$deform_show_mean)

    # Draw mean shape (links and points)
    if (show_mean) {
      if (show_wireframe) {
        for (i in seq_len(nrow(rv$links))) {
          lines(ref_plot[rv$links[i, ], 1], ref_plot[rv$links[i, ], 2],
            col = input$deform_mean_color, lwd = input$deform_line_width
          )
        }
      }
      points(ref_plot[,1], ref_plot[,2], pch = 21, bg = input$deform_point_color, cex = input$deform_point_size)
    }

    # Draw minus shape (links and points)
    if (show_wireframe) {
      for (i in seq_len(nrow(rv$links))) {
        lines(shape_minus_plot[rv$links[i, ], 1], shape_minus_plot[rv$links[i, ], 2],
          col = input$deform_minus_color, lwd = input$deform_line_width, lty = 2
        )
      }
    }
    points(shape_minus_plot[,1], shape_minus_plot[,2], pch = 21, bg = input$deform_minus_color, cex = input$deform_point_size * 0.8)

    # Draw plus shape (links and points)
    if (show_wireframe) {
      for (i in seq_len(nrow(rv$links))) {
        lines(shape_plus_plot[rv$links[i, ], 1], shape_plus_plot[rv$links[i, ], 2],
          col = input$deform_plus_color, lwd = input$deform_line_width, lty = 2
        )
      }
    }
    points(shape_plus_plot[,1], shape_plus_plot[,2], pch = 21, bg = input$deform_plus_color, cex = input$deform_point_size * 0.8)

    # Add landmark numbers if requested
    if (isTRUE(input$deform_show_landmarks)) {
      lm_size <- if (!is.null(input$deform_landmark_size)) input$deform_landmark_size else 1
      lm_color <- if (!is.null(input$deform_landmark_color)) input$deform_landmark_color else "#000000"
      text(ref_plot[, 1], ref_plot[, 2], labels = 1:nrow(ref_plot), 
           cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
    }

    # Add legend (dynamically based on what's shown)
    if (show_mean) {
      legend("topright",
        legend = c(
          "Mean Shape",
          paste0(input$deform_pc, " = ", input$deform_pc_min),
          paste0(input$deform_pc, " = ", input$deform_pc_max)
        ),
        col = c(input$deform_mean_color, input$deform_minus_color, input$deform_plus_color),
        lwd = input$deform_line_width,
        lty = c(1, 2, 2),
        bty = "n"
      )
    } else {
      legend("topright",
        legend = c(
          paste0(input$deform_pc, " = ", input$deform_pc_min),
          paste0(input$deform_pc, " = ", input$deform_pc_max)
        ),
        col = c(input$deform_minus_color, input$deform_plus_color),
        lwd = input$deform_line_width,
        lty = c(2, 2),
        bty = "n"
      )
    }
  })

  # ========== Individual Download Handlers ==========

  # PCA Plot Downloads (unified for current plot type)
  output$download_pca_png <- downloadHandler(
    filename = function() {
      paste0("PCA_", input$pca_plot_type, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(rv$scores, rv$pc_var, input$pc_x, input$pc_y)

      # Get custom dimensions
      dl_width <- if (!is.null(input$pca_download_width)) input$pca_download_width else 800
      dl_height <- if (!is.null(input$pca_download_height)) input$pca_download_height else 600

      pc_x_num <- as.numeric(gsub("PC", "", input$pc_x))
      pc_y_num <- as.numeric(gsub("PC", "", input$pc_y))
      plot_data <- rv$scores
      plot_data$Group <- get_renamed_groups()
      comp_x <- paste0("PC", pc_x_num)
      comp_y <- paste0("PC", pc_y_num)
      x_label <- paste0(input$pc_x, " (", rv$pc_var[pc_x_num], "%)")
      y_label <- paste0(input$pc_y, " (", rv$pc_var[pc_y_num], "%)")

      png(file, width = dl_width, height = dl_height, res = 100)

      # Handle 3D plot
      if (input$pca_plot_type == "3d") {
        req(input$pc_z)
        library(scatterplot3d)
        
        pc_z_num <- as.numeric(gsub("PC", "", input$pc_z))
        comp_z <- paste0("PC", pc_z_num)
        z_label <- paste0(input$pc_z, " (", rv$pc_var[pc_z_num], "%)")
        
        # Apply axis flipping for 3D plot
        if (input$pca_flip_x) {
          plot_data[[comp_x]] <- -plot_data[[comp_x]]
        }
        if (input$pca_flip_y) {
          plot_data[[comp_y]] <- -plot_data[[comp_y]]
        }
        
        colors <- get_group_colors()
        if (is.null(colors) || length(colors) == 0) {
          colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
        }
        group_cols <- colors[as.numeric(plot_data$Group)]
        
        # Set background color
        par(bg = input$plot_bg)
        
        s3d <- scatterplot3d(plot_data[[comp_x]], plot_data[[comp_y]], plot_data[[comp_z]],
                     color = group_cols, pch = 19, cex.symbols = input$pca_3d_point_size,
                     xlab = x_label, ylab = y_label, zlab = z_label,
                     main = input$pca_title,
                     grid = TRUE, box = TRUE)
        
        # Add data point labels if enabled
        if (isTRUE(input$pca_show_labels)) {
          coords_2d <- s3d$xyz.convert(plot_data[[comp_x]], plot_data[[comp_y]], plot_data[[comp_z]])
          text(coords_2d$x, coords_2d$y, labels = plot_data$Specimen, pos = 3, cex = 0.7, offset = 0.3)
        }
        
        legend("topright", legend = levels(plot_data$Group),
               col = colors[seq_along(levels(plot_data$Group))], 
               pch = 19, cex = 0.8, bg = "white")
        
        dev.off()
        return()
      }

      # Apply axis flipping for 2D plots
      if (input$pca_flip_x) {
        plot_data[[comp_x]] <- -plot_data[[comp_x]]
      }
      if (input$pca_flip_y) {
        plot_data[[comp_y]] <- -plot_data[[comp_y]]
      }

      if (input$pca_plot_type == "color") {
        # Colored plot - base plot
        p <- ggplot(plot_data, aes_string(
          x = comp_x, y = comp_y,
          color = "Group", fill = "Group"
        )) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
          geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")
        
        # Add data spread visualization based on selection (match render logic)
        spread_type <- if (!is.null(input$pca_spread_type)) input$pca_spread_type else "ellipse"
        
        # Get spread customization settings
        spread_fill <- if (!is.null(input$pca_spread_fill) && input$pca_spread_customize) input$pca_spread_fill else TRUE
        spread_outline <- if (!is.null(input$pca_spread_outline) && input$pca_spread_customize) input$pca_spread_outline else TRUE
        spread_alpha <- if (!is.null(input$pca_spread_fill_alpha) && input$pca_spread_customize) input$pca_spread_fill_alpha else 0.2
        spread_lwd <- if (!is.null(input$pca_spread_line_width) && input$pca_spread_customize) input$pca_spread_line_width else 1.5
        ellipse_level <- if (!is.null(input$pca_ellipse_level) && input$pca_spread_customize) input$pca_ellipse_level else input$confidence
        
        if (spread_type == "ellipse") {
          if (spread_fill && spread_outline) {
            p <- p + stat_ellipse(type = "norm", alpha = spread_alpha, level = ellipse_level, size = spread_lwd, geom = "polygon")
          } else if (spread_fill) {
            p <- p + stat_ellipse(type = "norm", alpha = spread_alpha, level = ellipse_level, size = 0, geom = "polygon")
          } else if (spread_outline) {
            p <- p + stat_ellipse(type = "norm", level = ellipse_level, size = spread_lwd, geom = "path")
          }
        } else if (spread_type == "hull") {
          # Convex hulls
          hull_data <- do.call(rbind, lapply(split(plot_data, plot_data$Group), function(df) {
            df[chull(df[[comp_x]], df[[comp_y]]), ]
          }))
          if (spread_fill && spread_outline) {
            p <- p + geom_polygon(data = hull_data, aes_string(x = comp_x, y = comp_y, fill = "Group", color = "Group"),
                                 alpha = spread_alpha, size = spread_lwd)
          } else if (spread_fill) {
            p <- p + geom_polygon(data = hull_data, aes_string(x = comp_x, y = comp_y, fill = "Group"),
                                 alpha = spread_alpha, color = NA)
          } else if (spread_outline) {
            p <- p + geom_polygon(data = hull_data, aes_string(x = comp_x, y = comp_y, color = "Group"),
                                 fill = NA, size = spread_lwd)
          }
        } else if (spread_type == "density") {
          # Density contour lines
          density_bins <- if (!is.null(input$pca_density_bins) && input$pca_spread_customize) input$pca_density_bins else 5
          if (spread_fill && spread_outline) {
            p <- p + geom_density_2d_filled(aes_string(x = comp_x, y = comp_y), alpha = spread_alpha, bins = density_bins, show.legend = FALSE) +
                     geom_density_2d(aes_string(x = comp_x, y = comp_y, color = "Group"), size = spread_lwd, bins = density_bins)
          } else if (spread_fill) {
            p <- p + geom_density_2d_filled(aes_string(x = comp_x, y = comp_y), alpha = spread_alpha, bins = density_bins, show.legend = FALSE)
          } else if (spread_outline) {
            p <- p + geom_density_2d(aes_string(x = comp_x, y = comp_y, color = "Group"), size = spread_lwd, bins = density_bins)
          }
        }
        # spread_type == "none" - no visualization added
        
        p <- p + geom_point(size = 4, shape = 21, stroke = 1.5) +
          theme_classic(base_size = 14) +
          theme(
            legend.position = "right",
            legend.title = element_blank(),
            plot.background = element_rect(fill = input$plot_bg),
            panel.background = element_rect(fill = input$plot_bg)
          ) +
          labs(x = x_label, y = y_label, title = input$pca_title)

        # Apply custom colors if enabled
        colors <- get_group_colors()
        if (!is.null(colors)) {
          p <- p + scale_color_manual(values = colors) + scale_fill_manual(values = colors)
        }
        
        # Add labels if requested
        if (input$pca_show_labels) {
          p <- p + geom_text(aes(label = Specimen), vjust = -1, size = 3)
        }
      } else {
        # B&W plot with custom shapes
        n_groups <- length(unique(plot_data$Group))
        
        # Get shapes - either auto or custom (match render logic)
        if (input$pca_bw_auto_shapes) {
          base_shapes <- c(21, 1, 24, 2, 22, 0, 23)
          shape_values <- base_shapes[seq_len(n_groups)]
        } else {
          shape_values <- sapply(seq_len(n_groups), function(i) {
            shape_input <- paste0("pca_bw_shape_", i)
            if (!is.null(input[[shape_input]])) as.numeric(input[[shape_input]]) else 21
          })
        }
        
        # Determine fill based on shape type
        fill_values <- sapply(shape_values, function(shape_val) {
          if (shape_val %in% c(21, 22, 23, 24, 25)) "black" else "white"
        })
        
        point_size <- if (!is.null(input$pca_bw_point_size)) input$pca_bw_point_size else 5
        
        p <- ggplot(plot_data, aes_string(
          x = comp_x, y = comp_y,
          shape = "Group", fill = "Group"
        )) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
          geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")
        
        # Add data spread visualization for B&W plot
        spread_type <- if (!is.null(input$pca_spread_type)) input$pca_spread_type else "ellipse"
        spread_outline <- if (!is.null(input$pca_spread_outline) && input$pca_spread_customize) input$pca_spread_outline else TRUE
        spread_lwd <- if (!is.null(input$pca_spread_line_width) && input$pca_spread_customize) input$pca_spread_line_width else 1.5
        ellipse_level <- if (!is.null(input$pca_ellipse_level) && input$pca_spread_customize) input$pca_ellipse_level else input$confidence
        
        if (spread_type == "ellipse" && spread_outline) {
          p <- p + stat_ellipse(level = ellipse_level, color = "black", size = spread_lwd)
        } else if (spread_type == "hull" && spread_outline) {
          hull_data <- do.call(rbind, lapply(split(plot_data, plot_data$Group), function(df) {
            df[chull(df[[comp_x]], df[[comp_y]]), ]
          }))
          p <- p + geom_polygon(data = hull_data, aes_string(x = comp_x, y = comp_y, group = "Group"),
                               fill = NA, color = "black", size = spread_lwd)
        } else if (spread_type == "density" && spread_outline) {
          density_bins <- if (!is.null(input$pca_density_bins) && input$pca_spread_customize) input$pca_density_bins else 5
          p <- p + geom_density_2d(aes_string(x = comp_x, y = comp_y, group = "Group"), color = "black", size = spread_lwd, bins = density_bins)
        }
        # spread_type == "none" - no visualization added
        
        p <- p + geom_point(size = point_size, stroke = 1.5, color = "black") +
          scale_shape_manual(values = shape_values) +
          scale_fill_manual(values = fill_values) +
          theme_classic(base_size = 14) +
          theme(
            legend.position = "right",
            legend.title = element_blank(),
            plot.background = element_rect(fill = input$plot_bg),
            panel.background = element_rect(fill = input$plot_bg)
          ) +
          labs(x = x_label, y = y_label, title = input$pca_title)
        
        # Add labels if requested
        if (input$pca_show_labels) {
          p <- p + geom_text(aes(label = Specimen), vjust = -1, size = 3)
        }
      }

      print(p)
      dev.off()
    }
  )

  output$download_pca_svg <- downloadHandler(
    filename = function() {
      paste0("PCA_", input$pca_plot_type, "_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(rv$scores, rv$pc_var, input$pc_x, input$pc_y)

      # Get custom dimensions (convert pixels to inches for SVG)
      dl_width <- if (!is.null(input$pca_download_width)) input$pca_download_width / 80 else 10
      dl_height <- if (!is.null(input$pca_download_height)) input$pca_download_height / 80 else 7.5

      pc_x_num <- as.numeric(gsub("PC", "", input$pc_x))
      pc_y_num <- as.numeric(gsub("PC", "", input$pc_y))
      plot_data <- rv$scores
      plot_data$Group <- get_renamed_groups()
      comp_x <- paste0("PC", pc_x_num)
      comp_y <- paste0("PC", pc_y_num)
      x_label <- paste0(input$pc_x, " (", rv$pc_var[pc_x_num], "%)")
      y_label <- paste0(input$pc_y, " (", rv$pc_var[pc_y_num], "%)")

      svg(file, width = dl_width, height = dl_height)

      # Handle 3D plot
      if (input$pca_plot_type == "3d") {
        req(input$pc_z)
        library(scatterplot3d)
        
        pc_z_num <- as.numeric(gsub("PC", "", input$pc_z))
        comp_z <- paste0("PC", pc_z_num)
        z_label <- paste0(input$pc_z, " (", rv$pc_var[pc_z_num], "%)")
        
        # Apply axis flipping for 3D plot
        if (input$pca_flip_x) {
          plot_data[[comp_x]] <- -plot_data[[comp_x]]
        }
        if (input$pca_flip_y) {
          plot_data[[comp_y]] <- -plot_data[[comp_y]]
        }
        
        colors <- get_group_colors()
        if (is.null(colors) || length(colors) == 0) {
          colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
        }
        group_cols <- colors[as.numeric(plot_data$Group)]
        
        # Set background color
        par(bg = input$plot_bg)
        
        s3d <- scatterplot3d(plot_data[[comp_x]], plot_data[[comp_y]], plot_data[[comp_z]],
                     color = group_cols, pch = 19, cex.symbols = input$pca_3d_point_size,
                     xlab = x_label, ylab = y_label, zlab = z_label,
                     main = input$pca_title,
                     grid = TRUE, box = TRUE)
        
        # Add data point labels if enabled
        if (isTRUE(input$pca_show_labels)) {
          coords_2d <- s3d$xyz.convert(plot_data[[comp_x]], plot_data[[comp_y]], plot_data[[comp_z]])
          text(coords_2d$x, coords_2d$y, labels = plot_data$Specimen, pos = 3, cex = 0.7, offset = 0.3)
        }
        
        legend("topright", legend = levels(plot_data$Group),
               col = colors[seq_along(levels(plot_data$Group))], 
               pch = 19, cex = 0.8, bg = "white")
        
        dev.off()
        return()
      }

      # Apply axis flipping for 2D plots
      if (input$pca_flip_x) {
        plot_data[[comp_x]] <- -plot_data[[comp_x]]
      }
      if (input$pca_flip_y) {
        plot_data[[comp_y]] <- -plot_data[[comp_y]]
      }

      if (input$pca_plot_type == "color") {
        # Colored plot - base plot
        p <- ggplot(plot_data, aes_string(
          x = comp_x, y = comp_y,
          color = "Group", fill = "Group"
        )) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
          geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")
        
        # Add data spread visualization based on selection (match render logic)
        spread_type <- if (!is.null(input$pca_spread_type)) input$pca_spread_type else "ellipse"
        
        # Get spread customization settings
        spread_fill <- if (!is.null(input$pca_spread_fill) && input$pca_spread_customize) input$pca_spread_fill else TRUE
        spread_outline <- if (!is.null(input$pca_spread_outline) && input$pca_spread_customize) input$pca_spread_outline else TRUE
        spread_alpha <- if (!is.null(input$pca_spread_fill_alpha) && input$pca_spread_customize) input$pca_spread_fill_alpha else 0.2
        spread_lwd <- if (!is.null(input$pca_spread_line_width) && input$pca_spread_customize) input$pca_spread_line_width else 1.5
        ellipse_level <- if (!is.null(input$pca_ellipse_level) && input$pca_spread_customize) input$pca_ellipse_level else input$confidence
        
        if (spread_type == "ellipse") {
          if (spread_fill && spread_outline) {
            p <- p + stat_ellipse(type = "norm", alpha = spread_alpha, level = ellipse_level, size = spread_lwd, geom = "polygon")
          } else if (spread_fill) {
            p <- p + stat_ellipse(type = "norm", alpha = spread_alpha, level = ellipse_level, size = 0, geom = "polygon")
          } else if (spread_outline) {
            p <- p + stat_ellipse(type = "norm", level = ellipse_level, size = spread_lwd, geom = "path")
          }
        } else if (spread_type == "hull") {
          # Convex hulls
          hull_data <- do.call(rbind, lapply(split(plot_data, plot_data$Group), function(df) {
            df[chull(df[[comp_x]], df[[comp_y]]), ]
          }))
          if (spread_fill && spread_outline) {
            p <- p + geom_polygon(data = hull_data, aes_string(x = comp_x, y = comp_y, fill = "Group", color = "Group"),
                                 alpha = spread_alpha, size = spread_lwd)
          } else if (spread_fill) {
            p <- p + geom_polygon(data = hull_data, aes_string(x = comp_x, y = comp_y, fill = "Group"),
                                 alpha = spread_alpha, color = NA)
          } else if (spread_outline) {
            p <- p + geom_polygon(data = hull_data, aes_string(x = comp_x, y = comp_y, color = "Group"),
                                 fill = NA, size = spread_lwd)
          }
        } else if (spread_type == "density") {
          # Density contour lines
          density_bins <- if (!is.null(input$pca_density_bins) && input$pca_spread_customize) input$pca_density_bins else 5
          if (spread_fill && spread_outline) {
            p <- p + geom_density_2d_filled(aes_string(x = comp_x, y = comp_y), alpha = spread_alpha, bins = density_bins, show.legend = FALSE) +
                     geom_density_2d(aes_string(x = comp_x, y = comp_y, color = "Group"), size = spread_lwd, bins = density_bins)
          } else if (spread_fill) {
            p <- p + geom_density_2d_filled(aes_string(x = comp_x, y = comp_y), alpha = spread_alpha, bins = density_bins, show.legend = FALSE)
          } else if (spread_outline) {
            p <- p + geom_density_2d(aes_string(x = comp_x, y = comp_y, color = "Group"), size = spread_lwd, bins = density_bins)
          }
        }
        # spread_type == "none" - no visualization added
        
        p <- p + geom_point(size = 4, shape = 21, stroke = 1.5) +
          theme_classic(base_size = 14) +
          theme(
            legend.position = "right",
            legend.title = element_blank(),
            plot.background = element_rect(fill = input$plot_bg),
            panel.background = element_rect(fill = input$plot_bg)
          ) +
          labs(x = x_label, y = y_label, title = input$pca_title)

        # Apply custom colors if enabled
        colors <- get_group_colors()
        if (!is.null(colors)) {
          p <- p + scale_color_manual(values = colors) + scale_fill_manual(values = colors)
        }
        
        # Add labels if requested
        if (input$pca_show_labels) {
          p <- p + geom_text(aes(label = Specimen), vjust = -1, size = 3)
        }
      } else {
        # B&W plot with custom shapes
        n_groups <- length(unique(plot_data$Group))
        
        # Get shapes - either auto or custom (match render logic)
        if (input$pca_bw_auto_shapes) {
          base_shapes <- c(21, 1, 24, 2, 22, 0, 23)
          shape_values <- base_shapes[seq_len(n_groups)]
        } else {
          shape_values <- sapply(seq_len(n_groups), function(i) {
            shape_input <- paste0("pca_bw_shape_", i)
            if (!is.null(input[[shape_input]])) as.numeric(input[[shape_input]]) else 21
          })
        }
        
        # Determine fill based on shape type
        fill_values <- sapply(shape_values, function(shape_val) {
          if (shape_val %in% c(21, 22, 23, 24, 25)) "black" else "white"
        })
        
        point_size <- if (!is.null(input$pca_bw_point_size)) input$pca_bw_point_size else 5
        
        p <- ggplot(plot_data, aes_string(
          x = comp_x, y = comp_y,
          shape = "Group", fill = "Group"
        )) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
          geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")
        
        # Add data spread visualization for B&W plot
        spread_type <- if (!is.null(input$pca_spread_type)) input$pca_spread_type else "ellipse"
        spread_outline <- if (!is.null(input$pca_spread_outline) && input$pca_spread_customize) input$pca_spread_outline else TRUE
        spread_lwd <- if (!is.null(input$pca_spread_line_width) && input$pca_spread_customize) input$pca_spread_line_width else 1.5
        ellipse_level <- if (!is.null(input$pca_ellipse_level) && input$pca_spread_customize) input$pca_ellipse_level else input$confidence
        
        if (spread_type == "ellipse" && spread_outline) {
          p <- p + stat_ellipse(level = ellipse_level, color = "black", size = spread_lwd)
        } else if (spread_type == "hull" && spread_outline) {
          hull_data <- do.call(rbind, lapply(split(plot_data, plot_data$Group), function(df) {
            df[chull(df[[comp_x]], df[[comp_y]]), ]
          }))
          p <- p + geom_polygon(data = hull_data, aes_string(x = comp_x, y = comp_y, group = "Group"),
                               fill = NA, color = "black", size = spread_lwd)
        } else if (spread_type == "density" && spread_outline) {
          density_bins <- if (!is.null(input$pca_density_bins) && input$pca_spread_customize) input$pca_density_bins else 5
          p <- p + geom_density_2d(aes_string(x = comp_x, y = comp_y, group = "Group"), color = "black", size = spread_lwd, bins = density_bins)
        }
        # spread_type == "none" - no visualization added
        
        p <- p + geom_point(size = point_size, stroke = 1.5, color = "black") +
          scale_shape_manual(values = shape_values) +
          scale_fill_manual(values = fill_values) +
          theme_classic(base_size = 14) +
          theme(
            legend.position = "right",
            legend.title = element_blank(),
            plot.background = element_rect(fill = input$plot_bg),
            panel.background = element_rect(fill = input$plot_bg)
          ) +
          labs(x = x_label, y = y_label, title = input$pca_title)
        
        # Add labels if requested
        if (input$pca_show_labels) {
          p <- p + geom_text(aes(label = Specimen), vjust = -1, size = 3)
        }
      }

      print(p)
      dev.off()
    }
  )

  # Statistics Downloads - Single combined file
  output$download_all_statistics <- downloadHandler(
    filename = function() {
      paste0("All_Statistics_", Sys.Date(), ".txt")
    },
    content = function(file) {
      sink(file)
      cat("==========================================================\n")
      cat("       MORPHOSTAT - COMPLETE STATISTICAL ANALYSIS       \n")
      cat("       Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
      cat("==========================================================\n\n")

      cat("=================================================\n")
      cat("       DATA SUMMARY                              \n")
      cat("=================================================\n\n")
      cat("Total specimens:", dim(rv$coords)[3], "\n")
      cat("Number of landmarks:", dim(rv$coords)[1], "\n")
      cat("Dimensions:", if (input$dimension == "3d") "3D" else "2D", "\n")
      cat("Groups:", paste(levels(rv$groups), collapse = ", "), "\n")
      n_show <- min(5, length(rv$pc_var))
      if (n_show > 0) {
        cat("\nVariance explained by first", n_show, "PCs (%):\n")
        print(rv$pc_var[seq_len(n_show)])
      }
      if (!is.null(rv$pca_95)) {
        cat("\nPCs explaining 95% of variance:", rv$pca_95$n_components, "\n")
        cat("Cumulative variance explained:", round(rv$pca_95$variance_explained, 2), "%\n")
      }
      cat("\n\n")

      # Centroid Size Analysis
      cat("=================================================\n")
      cat("       CENTROID SIZE ANALYSIS                    \n")
      cat("=================================================\n\n")

      cs_summary <- aggregate(rv$centroid_size ~ rv$groups,
        FUN = function(x) {
          c(
            mean = mean(x),
            sd = sd(x),
            min = min(x),
            max = max(x)
          )
        }
      )
      cat("Centroid Size Summary by Group:\n")
      print(cs_summary)
      cat("\n")

      cat("--- Shapiro-Wilk Normality Test on CS ---\n")
      for (group_name in names(rv$cs_normality)) {
        test_result <- rv$cs_normality[[group_name]]
        cat("\nGroup:", group_name, "\n")
        if (!is.null(test_result$statistic)) {
          cat("  W-statistic:", round(test_result$statistic, 4), "\n")
          cat("  p-value:", format.pval(test_result$p.value, digits = 4), "\n")
        }
      }
      cat("\n")

      cat("--- Inter-group Comparison of CS ---\n")
      if (!is.null(rv$cs_ttest)) {
        print(rv$cs_ttest)
      }
      cat("\n\n")

      # Allometry Results
      cat("=================================================\n")
      cat("     ALLOMETRY REGRESSION (Shape ~ log(CS))     \n")
      cat("=================================================\n\n")
      cat("Multivariate regression: 10,000 permutations\n\n")
      print(summary(rv$allometry_fit))
      cat("\n\n")

      # PERMANOVA on 95% PCs
      cat("=================================================\n")
      cat("   PERMANOVA ON 95% PCs (10,000 iterations)     \n")
      cat("=================================================\n\n")
      cat("Using Euclidean distance on PCs explaining 95% variance\n\n")
      print(rv$fit_pc95)
      cat("\n\n")

      cat("=================================================\n")
      cat("    PERMANOVA RESULTS (Shape-based)              \n")
      cat("=================================================\n\n")
      cat("Test: shape ~ group (", input$n_perms, " permutations)\n\n")
      print(summary(rv$fit))

      fit_summary <- summary(rv$fit)
      p_value_perm <- fit_summary$table$`Pr(>F)`[1]
      cat("\nInterpretation:\n")
      if (!is.na(p_value_perm)) {
        if (p_value_perm < 0.001) {
          cat("p-value < 0.001: Highly significant morphological differences between groups.\n")
        } else if (p_value_perm < 0.01) {
          cat(sprintf("p-value = %.4f: Significant morphological differences between groups.\n", p_value_perm))
        } else if (p_value_perm < 0.05) {
          cat(sprintf("p-value = %.4f: Significant morphological differences between groups.\n", p_value_perm))
        } else {
          cat(sprintf("p-value = %.4f: No significant morphological differences detected.\n", p_value_perm))
        }
      }
      cat("\n\n")

      cat("=================================================\n")
      cat("  PERMDISP (Multivariate Dispersion Test)       \n")
      cat("=================================================\n\n")
      cat("Testing homogeneity of group dispersions\n")
      cat("Null hypothesis: Group dispersions are equal\n\n")
      print(rv$betadisper_anova)
      cat("\nGroup Distances to Centroid:\n")
      print(rv$betadisper$group.distances)
      cat("\nInterpretation:\n")
      p_value_disp <- rv$betadisper_anova$`Pr(>F)`[1]
      if (p_value_disp > 0.05) {
        cat(sprintf("p-value = %.4f (> 0.05)\n", p_value_disp))
        cat("Group dispersions are homogeneous. PERMANOVA results are reliable.\n")
        cat("Differences are driven by mean shape differences, not variance.\n")
      } else {
        cat(sprintf("p-value = %.4f (< 0.05)\n", p_value_disp))
        cat("WARNING: Group dispersions differ significantly!\n")
        cat("PERMANOVA may show false positives.\n")
      }
      cat("\n\n")

      cat("=================================================\n")
      cat("       PAIRWISE COMPARISONS                      \n")
      cat("=================================================\n\n")
      print(summary(rv$pairwise,
        test.type = "dist",
        confidence = input$confidence, stat.table = TRUE
      ))
      cat("\n\n")

      cat("=================================================\n")
      cat("       GROUP CENTROIDS (PC1, PC2)                \n")
      cat("=================================================\n\n")
      print(rv$centroids)
      cat("\n\n")

      sink()
    }
  )

  output$download_scores_csv <- downloadHandler(
    filename = function() {
      paste0("PC_Scores_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$scores, file, row.names = FALSE)
    }
  )

  # Download Average Shape
  output$download_avg_shape_csv <- downloadHandler(
    filename = function() {
      paste0("Average_Shape_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$ref)
      n_landmarks <- nrow(rv$ref)
      n_dims <- ncol(rv$ref)
      
      if (n_dims == 2) {
        df <- data.frame(
          Landmark = seq_len(n_landmarks),
          `Axis 1 (x)` = rv$ref[, 1],
          `Axis 2 (y)` = rv$ref[, 2],
          check.names = FALSE
        )
      } else {
        df <- data.frame(
          Landmark = seq_len(n_landmarks),
          `Axis 1 (x)` = rv$ref[, 1],
          `Axis 2 (y)` = rv$ref[, 2],
          `Axis 3 (z)` = rv$ref[, 3],
          check.names = FALSE
        )
      }
      
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Download PC Coefficients
  output$download_pc_coef_csv <- downloadHandler(
    filename = function() {
      paste0("PC_Coefficients_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$pca)
      
      pc_coef <- rv$pca$rotation
      n_dims <- if (input$dimension == "3d") 3 else 2
      n_landmarks <- nrow(pc_coef) / n_dims
      n_pcs <- ncol(pc_coef)
      
      # Create row labels (x1, y1, z1, x2, y2, z2, ...)
      axis_labels <- if (n_dims == 2) c("x", "y") else c("x", "y", "z")
      row_labels <- paste0(rep(axis_labels, n_landmarks), rep(seq_len(n_landmarks), each = n_dims))
      
      df <- as.data.frame(pc_coef)
      colnames(df) <- paste0("PC", seq_len(n_pcs))
      df <- cbind(Coord = row_labels, df)
      
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Download Centroid Size
  output$download_centroid_size_csv <- downloadHandler(
    filename = function() {
      paste0("Centroid_Size_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$centroid_size, rv$groups)
      
      spec_names <- if (!is.null(rv$scores$Specimen)) rv$scores$Specimen else paste0("Specimen_", seq_along(rv$centroid_size))
      
      df <- data.frame(
        Specimen = spec_names,
        Group = rv$groups,
        `Centroid Size` = rv$centroid_size,
        check.names = FALSE
      )
      
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Download Procrustes Coordinates
  output$download_procrustes_csv <- downloadHandler(
    filename = function() {
      paste0("Procrustes_Coordinates_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$gpa)
      
      coords <- rv$gpa$coords
      n_specimens <- dim(coords)[3]
      n_landmarks <- dim(coords)[1]
      n_dims <- dim(coords)[2]
      
      spec_names <- if (!is.null(rv$scores$Specimen)) rv$scores$Specimen else paste0("Specimen_", seq_len(n_specimens))
      
      # Convert to 2D array format
      coords_2d <- two.d.array(coords)
      
      # Create column names
      axis_labels <- if (n_dims == 2) c("x", "y") else c("x", "y", "z")
      col_names <- paste0(rep(axis_labels, n_landmarks), rep(seq_len(n_landmarks), each = n_dims))
      
      df <- as.data.frame(coords_2d)
      colnames(df) <- col_names
      df <- cbind(Specimen = spec_names, df)
      
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Download Variance Table
  output$download_variance_csv <- downloadHandler(
    filename = function() {
      paste0("Variance_Explained_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$pc_var)
      
      n_pcs <- length(rv$pc_var)
      cumulative_var <- cumsum(rv$pc_var)
      
      df <- data.frame(
        PC = paste0("PC", seq_len(n_pcs)),
        `Variance (%)` = rv$pc_var,
        `Cumulative (%)` = cumulative_var,
        check.names = FALSE
      )
      
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Wireframe Downloads - Helper function
  save_wireframe_pair <- function(file_path, ref, shape_minus, shape_plus, links,
                                  view_axes = NULL, view_name = "", format = "png") {
    if (format == "png") {
      png(file_path, width = 1200, height = 600, res = 100)
    } else {
      svg(file_path, width = 15, height = 7.5)
    }

    par(mfrow = c(1, 2), bg = input$plot_bg)

    if (!is.null(view_axes)) {
      ref <- ref[, view_axes]
      shape_minus <- shape_minus[, view_axes]
      shape_plus <- shape_plus[, view_axes]
    }

    # Apply transformations (rotation and flip) to match display
    transform_wire_coords <- function(coords) {
      if (input$wire_rotate != "0") {
        angle <- as.numeric(input$wire_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$wire_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$wire_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref <- transform_wire_coords(ref)
    shape_minus <- transform_wire_coords(shape_minus)
    shape_plus <- transform_wire_coords(shape_plus)

    # Determine plot method based on visualization type
    viz_type <- if (!is.null(input$wire_viz_type)) input$wire_viz_type else "wireframe"
    
    # Handle outline plots differently - they use polygon() not plotRefToTarget
    if (viz_type == "outline") {
      # Calculate plot limits
      all_coords <- rbind(ref, shape_minus, shape_plus)
      xlim <- range(all_coords[, 1]) * 1.1
      ylim <- range(all_coords[, 2]) * 1.1
      
      # Plot minus outline
      plot(ref[,1], ref[,2], type = "n", xlim = xlim, ylim = ylim,
           xlab = "X", ylab = "Y", asp = 1,
           main = paste0("Outline - ", input$wire_pc, " = ", input$wire_pc_min))
      polygon(ref[, 1], ref[, 2], 
              col = adjustcolor(input$wire_color_avg, alpha.f = 0.3),
              border = input$wire_color_avg, lwd = input$wire_thickness)
      polygon(shape_minus[, 1], shape_minus[, 2], 
              col = adjustcolor(input$wire_color_minus, alpha.f = 0.3),
              border = input$wire_color_minus, lwd = input$wire_thickness)
      points(ref[,1], ref[,2], pch = 21, bg = input$wire_color_avg, cex = 1.5)
      points(shape_minus[,1], shape_minus[,2], pch = 21, bg = input$wire_color_minus, cex = 1.5)
      if (isTRUE(input$wire_show_landmarks)) {
        text(ref[, 1], ref[, 2], labels = seq_len(nrow(ref)),
             cex = input$wire_landmark_size, col = input$wire_landmark_color,
             pos = 3, offset = 0.3)
      }
      
      # Plot plus outline
      plot(ref[,1], ref[,2], type = "n", xlim = xlim, ylim = ylim,
           xlab = "X", ylab = "Y", asp = 1,
           main = paste0("Outline - ", input$wire_pc, " = ", input$wire_pc_max))
      polygon(ref[, 1], ref[, 2], 
              col = adjustcolor(input$wire_color_avg, alpha.f = 0.3),
              border = input$wire_color_avg, lwd = input$wire_thickness)
      polygon(shape_plus[, 1], shape_plus[, 2], 
              col = adjustcolor(input$wire_color_plus, alpha.f = 0.3),
              border = input$wire_color_plus, lwd = input$wire_thickness)
      points(ref[,1], ref[,2], pch = 21, bg = input$wire_color_avg, cex = 1.5)
      points(shape_plus[,1], shape_plus[,2], pch = 21, bg = input$wire_color_plus, cex = 1.5)
      if (isTRUE(input$wire_show_landmarks)) {
        text(ref[, 1], ref[, 2], labels = seq_len(nrow(ref)),
             cex = input$wire_landmark_size, col = input$wire_landmark_color,
             pos = 3, offset = 0.3)
      }
    } else {
      # Use plotRefToTarget for wireframe and TPS grid
      plot_method <- switch(viz_type,
        "tps_grid" = "TPS",
        "vector"  # default for wireframe
      )

      plotRefToTarget(ref, shape_minus,
        links = links, method = plot_method, mag = input$wire_mag,
        gridPars = gridPar(
          pt.bg = input$point_color, pt.size = 1.5,
          link.col = input$wire_color_minus, link.lwd = input$wire_thickness,
          tar.pt.bg = input$wire_color_minus
        )
      )
      title(paste0(view_name, " ", input$wire_pc, " = ", input$wire_pc_min))
      
      # Add landmark numbers for minus shape if enabled
      if (isTRUE(input$wire_show_landmarks)) {
        text(ref[, 1], ref[, 2], labels = seq_len(nrow(ref)),
             cex = input$wire_landmark_size, col = input$wire_landmark_color,
             pos = 3, offset = 0.3)
      }

      plotRefToTarget(ref, shape_plus,
        links = links, method = plot_method, mag = input$wire_mag,
        gridPars = gridPar(
          pt.bg = input$point_color, pt.size = 1.5,
          link.col = input$wire_color_plus, link.lwd = input$wire_thickness,
          tar.pt.bg = input$wire_color_plus
        )
      )
      title(paste0(view_name, " ", input$wire_pc, " = ", input$wire_pc_max))
      
      # Add landmark numbers for plus shape if enabled
      if (isTRUE(input$wire_show_landmarks)) {
        text(ref[, 1], ref[, 2], labels = seq_len(nrow(ref)),
             cex = input$wire_landmark_size, col = input$wire_landmark_color,
             pos = 3, offset = 0.3)
      }
    }

    dev.off()
  }

  # Unified wireframe downloads
  output$download_wireframe_png <- downloadHandler(
    filename = function() {
      view_name <- if (input$dimension == "3d") input$wire_view else "2d"
      viz_type <- if (!is.null(input$wire_viz_type)) input$wire_viz_type else "wireframe"
      viz_label <- switch(viz_type,
        "tps_grid" = "TPS_Grid",
        "outline" = "Outline",
        "Wireframe"
      )
      paste0(viz_label, "_", view_name, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      shapes <- get_wireframe_shapes()
      req(shapes, rv$links)

      view_axes <- NULL
      if (input$dimension == "3d") {
        view_axes <- switch(input$wire_view,
          "dorsal" = c(1, 2),
          "sagittal" = c(1, 3),
          "coronal" = c(2, 3)
        )
      }
      save_wireframe_pair(
        file, shapes$ref, shapes$shape_minus, shapes$shape_plus,
        rv$links, view_axes, "", "png"
      )
    }
  )

  output$download_wireframe_svg <- downloadHandler(
    filename = function() {
      view_name <- if (input$dimension == "3d") input$wire_view else "2d"
      viz_type <- if (!is.null(input$wire_viz_type)) input$wire_viz_type else "wireframe"
      viz_label <- switch(viz_type,
        "tps_grid" = "TPS_Grid",
        "outline" = "Outline",
        "Wireframe"
      )
      paste0(viz_label, "_", view_name, "_", Sys.Date(), ".svg")
    },
    content = function(file) {
      shapes <- get_wireframe_shapes()
      req(shapes, rv$links)

      view_axes <- NULL
      if (input$dimension == "3d") {
        view_axes <- switch(input$wire_view,
          "dorsal" = c(1, 2),
          "sagittal" = c(1, 3),
          "coronal" = c(2, 3)
        )
      }
      save_wireframe_pair(
        file, shapes$ref, shapes$shape_minus, shapes$shape_plus,
        rv$links, view_axes, "", "svg"
      )
    }
  )

  # Download deformation plots
  output$download_deformation_png <- downloadHandler(
    filename = function() {
      paste0("PC_Deformation_", input$deform_pc, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(rv$ref, rv$pca, input$deform_pc)
      
      # Get custom dimensions
      dl_width <- if (!is.null(input$deform_download_width)) input$deform_download_width else 1000
      dl_height <- if (!is.null(input$deform_download_height)) input$deform_download_height else 800

      pc_num <- as.numeric(gsub("PC", "", input$deform_pc))
      ref_vec <- as.vector(rv$ref)
      pc_loadings <- rv$pca$rotation
      n_dims <- if (input$dimension == "3d") 3 else 2

      # Create shapes with PC axis flip support (with sync option)
      make_shape <- function(score) {
        # Determine PC flip multiplier with sync option
        pc_multiplier <- 1
        if (isTRUE(input$deform_sync_pca_axis)) {
          if (input$deform_pc == input$pc_x && isTRUE(input$pca_flip_x)) {
            pc_multiplier <- -1
          } else if (input$deform_pc == input$pc_y && isTRUE(input$pca_flip_y)) {
            pc_multiplier <- -1
          }
        } else if (isTRUE(input$deform_flip_pc_axis)) {
          pc_multiplier <- -1
        }
        arrayspecs(
          matrix(ref_vec + pc_loadings[, pc_num] * score * pc_multiplier, ncol = n_dims),
          p = length(ref_vec) / n_dims, k = n_dims
        )[, , 1]
      }

      shape_minus <- make_shape(input$deform_pc_min)
      shape_plus <- make_shape(input$deform_pc_max)

      # For 3D, select axes based on user choice
      if (input$dimension == "3d") {
        axes <- switch(input$deform_axes,
          "xy" = c(1, 2),
          "xz" = c(1, 3),
          "yz" = c(2, 3)
        )
        ref_plot <- rv$ref[, axes]
        shape_minus_plot <- shape_minus[, axes]
        shape_plus_plot <- shape_plus[, axes]
      } else {
        ref_plot <- rv$ref
        shape_minus_plot <- shape_minus
        shape_plus_plot <- shape_plus
      }
      
      # Apply transformations
      transform_coords <- function(coords) {
        if (input$deform_rotate != "0") {
          angle <- as.numeric(input$deform_rotate) * pi / 180
          rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
          coords <- t(rotation_matrix %*% t(coords))
        }
        if (input$deform_flip == "horizontal") {
          coords[, 1] <- -coords[, 1]
        } else if (input$deform_flip == "vertical") {
          coords[, 2] <- -coords[, 2]
        }
        return(coords)
      }
      
      ref_plot <- transform_coords(ref_plot)
      shape_minus_plot <- transform_coords(shape_minus_plot)
      shape_plus_plot <- transform_coords(shape_plus_plot)

      png(file, width = dl_width, height = dl_height, res = 120)
      par(bg = input$plot_bg, mar = c(4, 4, 3, 1))

      all_coords <- rbind(ref_plot, shape_minus_plot, shape_plus_plot)
      xlim <- range(all_coords[, 1]) * 1.1
      ylim <- range(all_coords[, 2]) * 1.1

      var_pct <- if (!is.null(rv$pc_var)) rv$pc_var[pc_num] else NA

      plot(ref_plot[,1], ref_plot[,2],
        type = "n", xlim = xlim, ylim = ylim,
        xlab = "X", ylab = "Y", asp = 1,
        main = if (!is.na(var_pct)) {
          paste0(input$deform_pc, " Shape Deformations (", var_pct, "% variance)")
        } else {
          paste0(input$deform_pc, " Shape Deformations")
        }
      )

      # Check display options
      show_wireframe <- isTRUE(input$deform_show_wireframe) && !is.null(rv$links) && nrow(rv$links) > 0
      show_mean <- isTRUE(input$deform_show_mean)

      # Draw mean shape
      if (show_mean) {
        if (show_wireframe) {
          for (i in seq_len(nrow(rv$links))) {
            lines(ref_plot[rv$links[i, ], 1], ref_plot[rv$links[i, ], 2],
              col = input$deform_mean_color, lwd = input$deform_line_width
            )
          }
        }
        points(ref_plot[,1], ref_plot[,2], pch = 21, bg = input$deform_point_color, cex = 1.5)
      }

      # Draw minus shape
      if (show_wireframe) {
        for (i in seq_len(nrow(rv$links))) {
          lines(shape_minus_plot[rv$links[i, ], 1], shape_minus_plot[rv$links[i, ], 2],
            col = input$deform_minus_color, lwd = input$deform_line_width, lty = 2
          )
        }
      }
      points(shape_minus_plot[,1], shape_minus_plot[,2], pch = 21, bg = input$deform_minus_color, cex = 1.2)

      # Draw plus shape
      if (show_wireframe) {
        for (i in seq_len(nrow(rv$links))) {
          lines(shape_plus_plot[rv$links[i, ], 1], shape_plus_plot[rv$links[i, ], 2],
            col = input$deform_plus_color, lwd = input$deform_line_width, lty = 2
          )
        }
      }
      points(shape_plus_plot[,1], shape_plus_plot[,2], pch = 21, bg = input$deform_plus_color, cex = 1.2)

      # Add landmark numbers if enabled
      if (isTRUE(input$deform_show_landmarks)) {
        text(ref_plot[, 1], ref_plot[, 2], labels = seq_len(nrow(ref_plot)),
             cex = input$deform_landmark_size, col = input$deform_landmark_color,
             pos = 3, offset = 0.3)
      }

      # Legend based on what's shown
      if (show_mean) {
        legend("topright",
          legend = c(
            "Mean Shape",
            paste0(input$deform_pc, " = ", input$deform_pc_min),
            paste0(input$deform_pc, " = ", input$deform_pc_max)
          ),
          col = c(input$deform_mean_color, input$deform_minus_color, input$deform_plus_color),
          lwd = input$deform_line_width,
          lty = c(1, 2, 2),
          bty = "n"
        )
      } else {
        legend("topright",
          legend = c(
            paste0(input$deform_pc, " = ", input$deform_pc_min),
            paste0(input$deform_pc, " = ", input$deform_pc_max)
          ),
          col = c(input$deform_minus_color, input$deform_plus_color),
          lwd = input$deform_line_width,
          lty = c(2, 2),
          bty = "n"
        )
      }

      dev.off()
    }
  )

  output$download_deformation_svg <- downloadHandler(
    filename = function() {
      paste0("PC_Deformation_", input$deform_pc, "_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(rv$ref, rv$pca, input$deform_pc)
      
      # Get custom dimensions (convert pixels to inches for SVG)
      dl_width <- if (!is.null(input$deform_download_width)) input$deform_download_width / 80 else 12
      dl_height <- if (!is.null(input$deform_download_height)) input$deform_download_height / 80 else 10

      pc_num <- as.numeric(gsub("PC", "", input$deform_pc))
      ref_vec <- as.vector(rv$ref)
      pc_loadings <- rv$pca$rotation
      n_dims <- if (input$dimension == "3d") 3 else 2

      # Create shapes with PC axis flip support (with sync option)
      make_shape <- function(score) {
        # Determine PC flip multiplier with sync option
        pc_multiplier <- 1
        if (isTRUE(input$deform_sync_pca_axis)) {
          if (input$deform_pc == input$pc_x && isTRUE(input$pca_flip_x)) {
            pc_multiplier <- -1
          } else if (input$deform_pc == input$pc_y && isTRUE(input$pca_flip_y)) {
            pc_multiplier <- -1
          }
        } else if (isTRUE(input$deform_flip_pc_axis)) {
          pc_multiplier <- -1
        }
        arrayspecs(
          matrix(ref_vec + pc_loadings[, pc_num] * score * pc_multiplier, ncol = n_dims),
          p = length(ref_vec) / n_dims, k = n_dims
        )[, , 1]
      }

      shape_minus <- make_shape(input$deform_pc_min)
      shape_plus <- make_shape(input$deform_pc_max)

      # For 3D, select axes based on user choice
      if (input$dimension == "3d") {
        axes <- switch(input$deform_axes,
          "xy" = c(1, 2),
          "xz" = c(1, 3),
          "yz" = c(2, 3)
        )
        ref_plot <- rv$ref[, axes]
        shape_minus_plot <- shape_minus[, axes]
        shape_plus_plot <- shape_plus[, axes]
      } else {
        ref_plot <- rv$ref
        shape_minus_plot <- shape_minus
        shape_plus_plot <- shape_plus
      }
      
      # Apply transformations
      transform_coords <- function(coords) {
        if (input$deform_rotate != "0") {
          angle <- as.numeric(input$deform_rotate) * pi / 180
          rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
          coords <- t(rotation_matrix %*% t(coords))
        }
        if (input$deform_flip == "horizontal") {
          coords[, 1] <- -coords[, 1]
        } else if (input$deform_flip == "vertical") {
          coords[, 2] <- -coords[, 2]
        }
        return(coords)
      }
      
      ref_plot <- transform_coords(ref_plot)
      shape_minus_plot <- transform_coords(shape_minus_plot)
      shape_plus_plot <- transform_coords(shape_plus_plot)

      svg(file, width = dl_width, height = dl_height)
      par(bg = input$plot_bg, mar = c(4, 4, 3, 1))

      all_coords <- rbind(ref_plot, shape_minus_plot, shape_plus_plot)
      xlim <- range(all_coords[, 1]) * 1.1
      ylim <- range(all_coords[, 2]) * 1.1

      var_pct <- if (!is.null(rv$pc_var)) rv$pc_var[pc_num] else NA

      plot(ref_plot[,1], ref_plot[,2],
        type = "n", xlim = xlim, ylim = ylim,
        xlab = "X", ylab = "Y", asp = 1,
        main = if (!is.na(var_pct)) {
          paste0(input$deform_pc, " Shape Deformations (", var_pct, "% variance)")
        } else {
          paste0(input$deform_pc, " Shape Deformations")
        }
      )

      # Check display options
      show_wireframe <- isTRUE(input$deform_show_wireframe) && !is.null(rv$links) && nrow(rv$links) > 0
      show_mean <- isTRUE(input$deform_show_mean)

      # Draw mean shape
      if (show_mean) {
        if (show_wireframe) {
          for (i in seq_len(nrow(rv$links))) {
            lines(ref_plot[rv$links[i, ], 1], ref_plot[rv$links[i, ], 2],
              col = input$deform_mean_color, lwd = input$deform_line_width
            )
          }
        }
        points(ref_plot[,1], ref_plot[,2], pch = 21, bg = input$deform_point_color, cex = 1.5)
      }

      # Draw minus shape
      if (show_wireframe) {
        for (i in seq_len(nrow(rv$links))) {
          lines(shape_minus_plot[rv$links[i, ], 1], shape_minus_plot[rv$links[i, ], 2],
            col = input$deform_minus_color, lwd = input$deform_line_width, lty = 2
          )
        }
      }
      points(shape_minus_plot[,1], shape_minus_plot[,2], pch = 21, bg = input$deform_minus_color, cex = 1.2)

      # Draw plus shape
      if (show_wireframe) {
        for (i in seq_len(nrow(rv$links))) {
          lines(shape_plus_plot[rv$links[i, ], 1], shape_plus_plot[rv$links[i, ], 2],
            col = input$deform_plus_color, lwd = input$deform_line_width, lty = 2
          )
        }
      }
      points(shape_plus_plot[,1], shape_plus_plot[,2], pch = 21, bg = input$deform_plus_color, cex = 1.2)

      # Add landmark numbers if enabled
      if (isTRUE(input$deform_show_landmarks)) {
        text(ref_plot[, 1], ref_plot[, 2], labels = seq_len(nrow(ref_plot)),
             cex = input$deform_landmark_size, col = input$deform_landmark_color,
             pos = 3, offset = 0.3)
      }

      # Legend based on what's shown
      if (show_mean) {
        legend("topright",
          legend = c(
            "Mean Shape",
            paste0(input$deform_pc, " = ", input$deform_pc_min),
            paste0(input$deform_pc, " = ", input$deform_pc_max)
          ),
          col = c(input$deform_mean_color, input$deform_minus_color, input$deform_plus_color),
          lwd = input$deform_line_width,
          lty = c(1, 2, 2),
          bty = "n"
        )
      } else {
        legend("topright",
          legend = c(
            paste0(input$deform_pc, " = ", input$deform_pc_min),
            paste0(input$deform_pc, " = ", input$deform_pc_max)
          ),
          col = c(input$deform_minus_color, input$deform_plus_color),
          lwd = input$deform_line_width,
          lty = c(2, 2),
          bty = "n"
        )
      }

      dev.off()
    }
  )
  
  # Landmark Displacement Download Handlers
  output$download_displacement_png <- downloadHandler(
    filename = function() {
      paste0("Landmark_Displacement_", input$disp_pc, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(rv$ref, rv$pca, input$disp_pc)
      
      dl_width <- if (!is.null(input$disp_download_width)) input$disp_download_width else 1000
      dl_height <- if (!is.null(input$disp_download_height)) input$disp_download_height else 500
      
      png(file, width = dl_width, height = dl_height, res = 120)
      
      # Get shapes for displacement calculation
      pc_num <- as.numeric(gsub("PC", "", input$disp_pc))
      ref_vec <- as.vector(rv$ref)
      pc_loadings <- rv$pca$rotation
      n_dims <- if (input$dimension == "3d") 3 else 2
      
      make_shape <- function(score) {
        pc_multiplier <- 1
        if (isTRUE(input$disp_sync_pca_axis)) {
          if (input$disp_pc == input$pc_x && isTRUE(input$pca_flip_x)) {
            pc_multiplier <- -1
          } else if (input$disp_pc == input$pc_y && isTRUE(input$pca_flip_y)) {
            pc_multiplier <- -1
          }
        } else if (isTRUE(input$disp_flip_pc_axis)) {
          pc_multiplier <- -1
        }
        arrayspecs(
          matrix(ref_vec + pc_loadings[, pc_num] * score * pc_multiplier, ncol = n_dims),
          p = length(ref_vec) / n_dims, k = n_dims
        )[, , 1]
      }
      
      ref_shape <- rv$ref
      pc_value <- if (!is.null(input$disp_pc_value)) input$disp_pc_value else 0.05
      target_shape <- make_shape(pc_value)
      
      n_landmarks <- nrow(ref_shape)
      displacement <- sqrt(rowSums((target_shape - ref_shape)^2))
      
      mag <- if (!is.null(input$disp_mag)) input$disp_mag else 1
      bar_color <- if (!is.null(input$disp_color)) input$disp_color else "#3498db"
      
      lollipop_data <- data.frame(
        Landmark = factor(1:n_landmarks),
        Displacement = displacement * mag
      )
      
      max_disp <- max(lollipop_data$Displacement) * 1.1
      
      par(mar = c(5, 4, 4, 2), bg = "#FFFFFF")
      
      barplot(lollipop_data$Displacement, 
              col = bar_color,
              names.arg = 1:n_landmarks,
              main = paste0("Landmark Displacement - ", input$disp_pc, " = ", pc_value),
              xlab = "Landmark Number",
              ylab = paste0("Displacement", if (mag > 1) paste0(" (x", mag, " magnified)") else ""),
              ylim = c(0, max_disp),
              border = NA)
      
      dev.off()
    }
  )
  
  output$download_displacement_svg <- downloadHandler(
    filename = function() {
      paste0("Landmark_Displacement_", input$disp_pc, "_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(rv$ref, rv$pca, input$disp_pc)
      
      dl_width <- if (!is.null(input$disp_download_width)) input$disp_download_width / 80 else 12
      dl_height <- if (!is.null(input$disp_download_height)) input$disp_download_height / 80 else 6
      
      svg(file, width = dl_width, height = dl_height)
      
      pc_num <- as.numeric(gsub("PC", "", input$disp_pc))
      ref_vec <- as.vector(rv$ref)
      pc_loadings <- rv$pca$rotation
      n_dims <- if (input$dimension == "3d") 3 else 2
      
      make_shape <- function(score) {
        pc_multiplier <- 1
        if (isTRUE(input$disp_sync_pca_axis)) {
          if (input$disp_pc == input$pc_x && isTRUE(input$pca_flip_x)) {
            pc_multiplier <- -1
          } else if (input$disp_pc == input$pc_y && isTRUE(input$pca_flip_y)) {
            pc_multiplier <- -1
          }
        } else if (isTRUE(input$disp_flip_pc_axis)) {
          pc_multiplier <- -1
        }
        arrayspecs(
          matrix(ref_vec + pc_loadings[, pc_num] * score * pc_multiplier, ncol = n_dims),
          p = length(ref_vec) / n_dims, k = n_dims
        )[, , 1]
      }
      
      ref_shape <- rv$ref
      pc_value <- if (!is.null(input$disp_pc_value)) input$disp_pc_value else 0.05
      target_shape <- make_shape(pc_value)
      
      n_landmarks <- nrow(ref_shape)
      displacement <- sqrt(rowSums((target_shape - ref_shape)^2))
      
      mag <- if (!is.null(input$disp_mag)) input$disp_mag else 1
      bar_color <- if (!is.null(input$disp_color)) input$disp_color else "#3498db"
      
      lollipop_data <- data.frame(
        Landmark = factor(1:n_landmarks),
        Displacement = displacement * mag
      )
      
      max_disp <- max(lollipop_data$Displacement) * 1.1
      
      par(mar = c(5, 4, 4, 2), bg = "#FFFFFF")
      
      barplot(lollipop_data$Displacement, 
              col = bar_color,
              names.arg = 1:n_landmarks,
              main = paste0("Landmark Displacement - ", input$disp_pc, " = ", pc_value),
              xlab = "Landmark Number",
              ylab = paste0("Displacement", if (mag > 1) paste0(" (x", mag, " magnified)") else ""),
              ylim = c(0, max_disp),
              border = NA)
      
      dev.off()
    }
  )

  # Procrustes Plot Download Handlers
  output$download_procrustes_png <- downloadHandler(
    filename = function() {
      paste0("Procrustes_Plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(rv$gpa, rv$ref, rv$pca)
      
      # Get custom dimensions
      dl_width <- if (!is.null(input$procrustes_download_width)) input$procrustes_download_width else 1000
      dl_height <- if (!is.null(input$procrustes_download_height)) input$procrustes_download_height else 800
      
      png(file, width = dl_width, height = dl_height, res = 120)
      
      pc_num <- as.numeric(gsub("PC", "", input$procrustes_pc))
      ref_vec <- as.vector(rv$ref)
      pc_loadings <- rv$pca$rotation
      n_dims <- if (input$dimension == "3d") 3 else 2
      n_landmarks <- nrow(rv$ref)
      
      # Get current links from reactive source and validate
      raw_links <- reactive_links()
      current_links <- get_valid_links(raw_links, n_landmarks)
      
      # Create shape at specific PC value with optional PC axis flip (with sync support)
      make_shape <- function(score) {
        # Determine PC flip multiplier with sync option
        pc_multiplier <- 1
        if (isTRUE(input$procrustes_sync_pca_axis)) {
          if (input$procrustes_pc == input$pc_x && isTRUE(input$pca_flip_x)) {
            pc_multiplier <- -1
          } else if (input$procrustes_pc == input$pc_y && isTRUE(input$pca_flip_y)) {
            pc_multiplier <- -1
          }
        } else if (isTRUE(input$procrustes_flip_pc_axis)) {
          pc_multiplier <- -1
        }
        arrayspecs(
          matrix(ref_vec + pc_loadings[, pc_num] * score * pc_multiplier, ncol = n_dims),
          p = length(ref_vec) / n_dims, k = n_dims
        )[, , 1]
      }
      
      deformed_shape <- make_shape(input$procrustes_pc_value)
      
      # Project to 2D if 3D data - select axes based on user choice
      if (input$dimension == "3d") {
        axes <- switch(input$procrustes_axes,
          "xy" = c(1, 2),
          "xz" = c(1, 3),
          "yz" = c(2, 3)
        )
        ref_plot <- rv$ref[, axes]
        deformed_plot <- deformed_shape[, axes]
        coords_plot <- rv$gpa$coords[, axes, ]
      } else {
        ref_plot <- rv$ref
        deformed_plot <- deformed_shape
        coords_plot <- rv$gpa$coords
      }
      
      # Apply transformations
      transform_coords <- function(coords) {
        if (input$procrustes_rotate != "0") {
          angle <- as.numeric(input$procrustes_rotate) * pi / 180
          rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
          coords <- t(rotation_matrix %*% t(coords))
        }
        if (input$procrustes_flip == "horizontal") {
          coords[, 1] <- -coords[, 1]
        } else if (input$procrustes_flip == "vertical") {
          coords[, 2] <- -coords[, 2]
        }
        return(coords)
      }
      
      ref_plot <- transform_coords(ref_plot)
      deformed_plot <- transform_coords(deformed_plot)
      
      # Transform all specimen coordinates
      for (i in seq_len(dim(coords_plot)[3])) {
        coords_plot[, , i] <- transform_coords(coords_plot[, , i])
      }
      
      par(bg = "#FFFFFF", mar = c(4, 4, 3, 1))
      
      if (input$procrustes_view == "all") {
        xlim <- range(coords_plot[, 1, ])
        ylim <- range(coords_plot[, 2, ])
        
        plot(coords_plot[, 1, 1], coords_plot[, 2, 1], 
             type = "n", xlim = xlim, ylim = ylim,
             xlab = "X", ylab = "Y", asp = 1,
             main = paste0("Procrustes Shapes - ", input$procrustes_pc, " = ", input$procrustes_pc_value))
        
        for (i in seq_len(dim(coords_plot)[3])) {
          points(coords_plot[, 1, i], coords_plot[, 2, i], 
                 col = input$procrustes_point_color, 
                 pch = 19, cex = input$procrustes_point_size * 0.5)
          
          if (input$procrustes_show_wireframe && !is.null(current_links) && nrow(current_links) > 0) {
            for (j in seq_len(nrow(current_links))) {
              lines(coords_plot[current_links[j, ], 1, i], 
                    coords_plot[current_links[j, ], 2, i],
                    col = input$procrustes_wire_color, 
                    lwd = input$procrustes_line_width * 0.5)
            }
          }
        }
        
        if (input$procrustes_show_mean) {
          points(ref_plot, pch = 21, bg = input$procrustes_mean_color, 
                 cex = input$procrustes_point_size * 1.2)
          if (!is.null(current_links) && nrow(current_links) > 0) {
            for (i in seq_len(nrow(current_links))) {
              lines(ref_plot[current_links[i, ], 1], ref_plot[current_links[i, ], 2],
                    col = input$procrustes_mean_color, 
                    lwd = input$procrustes_line_width * 1.2)
            }
          }
          # Add landmark numbers for mean shape if enabled
          if (isTRUE(input$procrustes_show_landmarks)) {
            text(ref_plot[, 1], ref_plot[, 2], labels = seq_len(nrow(ref_plot)),
                 cex = input$procrustes_landmark_size, col = input$procrustes_landmark_color,
                 pos = 3, offset = 0.3)
          }
        }
      } else {
        # Plot individual specimen
        req(input$procrustes_specimen)
        spec_idx <- which(rv$scores$Specimen == input$procrustes_specimen)[1]
        
        if (!is.na(spec_idx)) {
          spec_coords <- coords_plot[, , spec_idx]
          
          xlim <- range(c(spec_coords[, 1], deformed_plot[, 1]))
          ylim <- range(c(spec_coords[, 2], deformed_plot[, 2]))
          
          plot(spec_coords, type = "n", xlim = xlim, ylim = ylim,
               xlab = "X", ylab = "Y", asp = 1,
               main = paste0("Specimen: ", input$procrustes_specimen, " - ", 
                            input$procrustes_pc, " = ", input$procrustes_pc_value))
          
          # Draw specimen
          points(spec_coords, pch = 21, bg = input$procrustes_point_color, 
                 cex = input$procrustes_point_size)
          
          if (input$procrustes_show_wireframe && !is.null(current_links) && nrow(current_links) > 0) {
            for (i in seq_len(nrow(current_links))) {
              lines(spec_coords[current_links[i, ], 1], spec_coords[current_links[i, ], 2],
                    col = input$procrustes_wire_color, lwd = input$procrustes_line_width)
            }
          }
          
          # Add landmark numbers for specimen if enabled
          if (isTRUE(input$procrustes_show_landmarks)) {
            text(spec_coords[, 1], spec_coords[, 2], labels = seq_len(nrow(spec_coords)),
                 cex = input$procrustes_landmark_size, col = input$procrustes_landmark_color,
                 pos = 3, offset = 0.3)
          }
          
          # Draw deformed shape if requested
          if (input$procrustes_show_mean) {
            points(deformed_plot, pch = 21, bg = input$procrustes_mean_color, 
                   cex = input$procrustes_point_size * 0.8)
            if (!is.null(current_links) && nrow(current_links) > 0) {
              for (i in seq_len(nrow(current_links))) {
                lines(deformed_plot[current_links[i, ], 1], deformed_plot[current_links[i, ], 2],
                      col = input$procrustes_mean_color, 
                      lwd = input$procrustes_line_width * 0.8)
              }
            }
          }
        }
      }
      
      dev.off()
    }
  )
  
  output$download_procrustes_svg <- downloadHandler(
    filename = function() {
      paste0("Procrustes_Plot_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(rv$gpa, rv$ref, rv$pca)
      
      # Get custom dimensions (convert pixels to inches for SVG)
      dl_width <- if (!is.null(input$procrustes_download_width)) input$procrustes_download_width / 80 else 12
      dl_height <- if (!is.null(input$procrustes_download_height)) input$procrustes_download_height / 80 else 10
      
      svg(file, width = dl_width, height = dl_height)
      
      pc_num <- as.numeric(gsub("PC", "", input$procrustes_pc))
      ref_vec <- as.vector(rv$ref)
      pc_loadings <- rv$pca$rotation
      n_dims <- if (input$dimension == "3d") 3 else 2
      n_landmarks <- nrow(rv$ref)
      
      # Get current links from reactive source and validate
      raw_links <- reactive_links()
      current_links <- get_valid_links(raw_links, n_landmarks)
      
      # Create shape at specific PC value with optional PC axis flip (with sync support)
      make_shape <- function(score) {
        # Determine PC flip multiplier with sync option
        pc_multiplier <- 1
        if (isTRUE(input$procrustes_sync_pca_axis)) {
          if (input$procrustes_pc == input$pc_x && isTRUE(input$pca_flip_x)) {
            pc_multiplier <- -1
          } else if (input$procrustes_pc == input$pc_y && isTRUE(input$pca_flip_y)) {
            pc_multiplier <- -1
          }
        } else if (isTRUE(input$procrustes_flip_pc_axis)) {
          pc_multiplier <- -1
        }
        arrayspecs(
          matrix(ref_vec + pc_loadings[, pc_num] * score * pc_multiplier, ncol = n_dims),
          p = length(ref_vec) / n_dims, k = n_dims
        )[, , 1]
      }
      
      deformed_shape <- make_shape(input$procrustes_pc_value)
      
      # Project to 2D if 3D data - select axes based on user choice
      if (input$dimension == "3d") {
        axes <- switch(input$procrustes_axes,
          "xy" = c(1, 2),
          "xz" = c(1, 3),
          "yz" = c(2, 3)
        )
        ref_plot <- rv$ref[, axes]
        deformed_plot <- deformed_shape[, axes]
        coords_plot <- rv$gpa$coords[, axes, ]
      } else {
        ref_plot <- rv$ref
        deformed_plot <- deformed_shape
        coords_plot <- rv$gpa$coords
      }
      
      # Apply transformations
      transform_coords <- function(coords) {
        if (input$procrustes_rotate != "0") {
          angle <- as.numeric(input$procrustes_rotate) * pi / 180
          rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
          coords <- t(rotation_matrix %*% t(coords))
        }
        if (input$procrustes_flip == "horizontal") {
          coords[, 1] <- -coords[, 1]
        } else if (input$procrustes_flip == "vertical") {
          coords[, 2] <- -coords[, 2]
        }
        return(coords)
      }
      
      ref_plot <- transform_coords(ref_plot)
      deformed_plot <- transform_coords(deformed_plot)
      
      # Transform all specimen coordinates
      for (i in seq_len(dim(coords_plot)[3])) {
        coords_plot[, , i] <- transform_coords(coords_plot[, , i])
      }
      
      par(bg = "#FFFFFF", mar = c(4, 4, 3, 1))
      
      if (input$procrustes_view == "all") {
        xlim <- range(coords_plot[, 1, ])
        ylim <- range(coords_plot[, 2, ])
        
        plot(coords_plot[, 1, 1], coords_plot[, 2, 1], 
             type = "n", xlim = xlim, ylim = ylim,
             xlab = "X", ylab = "Y", asp = 1,
             main = paste0("Procrustes Shapes - ", input$procrustes_pc, " = ", input$procrustes_pc_value))
        
        for (i in seq_len(dim(coords_plot)[3])) {
          points(coords_plot[, 1, i], coords_plot[, 2, i], 
                 col = input$procrustes_point_color, 
                 pch = 19, cex = input$procrustes_point_size * 0.5)
          
          if (input$procrustes_show_wireframe && !is.null(current_links) && nrow(current_links) > 0) {
            for (j in seq_len(nrow(current_links))) {
              lines(coords_plot[current_links[j, ], 1, i], 
                    coords_plot[current_links[j, ], 2, i],
                    col = input$procrustes_wire_color, 
                    lwd = input$procrustes_line_width * 0.5)
            }
          }
        }
        
        if (input$procrustes_show_mean) {
          points(ref_plot, pch = 21, bg = input$procrustes_mean_color, 
                 cex = input$procrustes_point_size * 1.2)
          if (!is.null(current_links) && nrow(current_links) > 0) {
            for (i in seq_len(nrow(current_links))) {
              lines(ref_plot[current_links[i, ], 1], ref_plot[current_links[i, ], 2],
                    col = input$procrustes_mean_color, 
                    lwd = input$procrustes_line_width * 1.2)
            }
          }
          # Add landmark numbers for mean shape if enabled
          if (isTRUE(input$procrustes_show_landmarks)) {
            text(ref_plot[, 1], ref_plot[, 2], labels = seq_len(nrow(ref_plot)),
                 cex = input$procrustes_landmark_size, col = input$procrustes_landmark_color,
                 pos = 3, offset = 0.3)
          }
        }
      } else {
        # Plot individual specimen
        req(input$procrustes_specimen)
        spec_idx <- which(rv$scores$Specimen == input$procrustes_specimen)[1]
        
        if (!is.na(spec_idx)) {
          spec_coords <- coords_plot[, , spec_idx]
          
          xlim <- range(c(spec_coords[, 1], deformed_plot[, 1]))
          ylim <- range(c(spec_coords[, 2], deformed_plot[, 2]))
          
          plot(spec_coords, type = "n", xlim = xlim, ylim = ylim,
               xlab = "X", ylab = "Y", asp = 1,
               main = paste0("Specimen: ", input$procrustes_specimen, " - ", 
                            input$procrustes_pc, " = ", input$procrustes_pc_value))
          
          # Draw specimen
          points(spec_coords, pch = 21, bg = input$procrustes_point_color, 
                 cex = input$procrustes_point_size)
          
          if (input$procrustes_show_wireframe && !is.null(current_links) && nrow(current_links) > 0) {
            for (i in seq_len(nrow(current_links))) {
              lines(spec_coords[current_links[i, ], 1], spec_coords[current_links[i, ], 2],
                    col = input$procrustes_wire_color, lwd = input$procrustes_line_width)
            }
          }
          
          # Add landmark numbers for specimen if enabled
          if (isTRUE(input$procrustes_show_landmarks)) {
            text(spec_coords[, 1], spec_coords[, 2], labels = seq_len(nrow(spec_coords)),
                 cex = input$procrustes_landmark_size, col = input$procrustes_landmark_color,
                 pos = 3, offset = 0.3)
          }
          
          # Draw deformed shape if requested
          if (input$procrustes_show_mean) {
            points(deformed_plot, pch = 21, bg = input$procrustes_mean_color, 
                   cex = input$procrustes_point_size * 0.8)
            if (!is.null(current_links) && nrow(current_links) > 0) {
              for (i in seq_len(nrow(current_links))) {
                lines(deformed_plot[current_links[i, ], 1], deformed_plot[current_links[i, ], 2],
                      col = input$procrustes_mean_color, 
                      lwd = input$procrustes_line_width * 0.8)
              }
            }
          }
        }
      }
      
      dev.off()
    }
  )

  # Statistical Plots Outputs
  
  # Centroid Size Box Plot
  output$centroid_size_plot <- renderPlot({
    req(rv$centroid_size, rv$groups, input$cs_groups_select)
    
    # Get selected groups
    selected_groups <- input$cs_groups_select
    if (is.null(selected_groups) || length(selected_groups) == 0) {
      selected_groups <- levels(rv$groups)
    }
    
    # Filter data based on selected groups
    selected_indices <- rv$groups %in% selected_groups
    cs_data <- data.frame(
      CentroidSize = rv$centroid_size[selected_indices],
      Group = rv$groups[selected_indices]
    )
    cs_data$Group <- droplevels(cs_data$Group)
    
    # Order groups according to selection order in dropdown
    cs_data$Group <- factor(cs_data$Group, levels = selected_groups)
    
    # Get individual group colors
    group_colors <- c()
    for (i in seq_along(selected_groups)) {
      color_input <- paste0("cs_group_color_", i)
      if (!is.null(input[[color_input]])) {
        group_colors[selected_groups[i]] <- input[[color_input]]
      } else {
        default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
        group_colors[selected_groups[i]] <- default_colors[min(i, length(default_colors))]
      }
    }
    
    p <- ggplot(cs_data, aes(x = Group, y = CentroidSize)) +
      geom_boxplot(aes(fill = Group), color = input$cs_outline_color, alpha = 0.7) +
      scale_fill_manual(values = group_colors) +
      theme_classic() +
      labs(
        title = "Centroid Size Comparison Between Groups",
        x = "Group",
        y = "Centroid Size"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = input$cs_title_size, face = "bold"),
        axis.title = element_text(size = input$cs_axis_title_size),
        axis.text = element_text(size = input$cs_axis_text_size),
        axis.text.x = element_text(angle = 45, hjust = 1, size = input$cs_axis_text_size),
        legend.position = "none"
      )
    
    if (input$cs_show_points) {
      p <- p + geom_jitter(width = 0.2, alpha = 0.6, size = 2)
    }
    
    if (input$cs_show_mean) {
      p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red")
    }
    
    p
  })
  
  # Allometry Regression Plot
  output$allometry_plot <- renderPlot({
    req(rv$centroid_size, rv$pca, rv$groups)
    
    # Use first PC for allometry
    pc1_scores <- rv$pca$x[, 1]
    
    allo_data <- data.frame(
      CentroidSize = log(rv$centroid_size),
      PC1 = pc1_scores,
      Group = rv$groups
    )
    
    # Apply axis flipping - with sync option for PC1
    if (input$allo_flip_x) {
      allo_data$CentroidSize <- -allo_data$CentroidSize
    }
    
    # Determine PC1 flip - either manual or synced with PCA
    flip_pc1 <- FALSE
    if (isTRUE(input$allo_sync_pca_axis)) {
      # If PC1 is X-axis in PCA and flipped, flip allometry PC1
      if (input$pc_x == "PC1" && isTRUE(input$pca_flip_x)) {
        flip_pc1 <- TRUE
      } else if (input$pc_y == "PC1" && isTRUE(input$pca_flip_y)) {
        flip_pc1 <- TRUE
      }
    } else if (isTRUE(input$allo_flip_y)) {
      flip_pc1 <- TRUE
    }
    
    if (flip_pc1) {
      allo_data$PC1 <- -allo_data$PC1
    }
    
    if (input$allo_show_groups) {
      # Get custom group colors
      allo_colors <- get_allo_group_colors()
      
      p <- ggplot(allo_data, aes(x = CentroidSize, y = PC1, color = Group)) +
        geom_point(size = input$allo_point_size, alpha = 0.7) +
        scale_color_manual(values = allo_colors)
    } else {
      # Use single color when groups disabled
      single_color <- input$allo_single_color
      if (is.null(single_color)) single_color <- "#3498db"
      
      p <- ggplot(allo_data, aes(x = CentroidSize, y = PC1)) +
        geom_point(size = input$allo_point_size, alpha = 0.7, color = single_color)
    }
    
    # Get line color
    line_color <- input$allo_line_color
    if (is.null(line_color)) line_color <- "#2c3e50"
    
    p <- p + geom_smooth(method = "lm", se = input$allo_show_ci, 
                        color = line_color, size = input$allo_line_width) +
      theme_classic() +
      labs(
        title = "Allometry: Shape (PC1) vs Size Relationship",
        x = "Log Centroid Size",
        y = "PC1 Scores"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = input$allo_title_size, face = "bold"),
        axis.title = element_text(size = input$allo_axis_title_size),
        axis.text = element_text(size = input$allo_axis_text_size),
        legend.position = "bottom"
      )
    
    p
  })
  
  # Shape Difference Vectors Plot
  output$shape_vectors_plot <- renderPlot({
    req(rv$ref, rv$groups, input$vector_comparison)
    
    # Parse comparison groups
    comp_parts <- strsplit(input$vector_comparison, "vs")[[1]]
    group1_idx <- as.numeric(comp_parts[1])
    group2_idx <- as.numeric(comp_parts[2])
    
    group_levels <- levels(rv$groups)
    group1_name <- group_levels[group1_idx]
    group2_name <- group_levels[group2_idx]
    
    # Calculate group means
    coords_matrix <- two.d.array(rv$coords)
    group1_indices <- which(rv$groups == group1_name)
    group2_indices <- which(rv$groups == group2_name)
    
    group1_mean <- colMeans(coords_matrix[group1_indices, ])
    group2_mean <- colMeans(coords_matrix[group2_indices, ])
    
    # Calculate difference vectors
    diff_vector <- group2_mean - group1_mean
    
    # Reshape for plotting
    n_dims <- if (input$dimension == "3d") 3 else 2
    n_landmarks <- length(group1_mean) / n_dims
    
    if (input$dimension == "3d") {
      # Use XY projection for 3D data
      ref_coords <- matrix(rv$ref[, 1:2], ncol = 2)
      diff_coords <- matrix(diff_vector[seq(1, length(diff_vector), by = 3)], ncol = 1)
      diff_coords <- cbind(diff_coords, matrix(diff_vector[seq(2, length(diff_vector), by = 3)], ncol = 1))
    } else {
      ref_coords <- matrix(rv$ref, ncol = 2)
      diff_coords <- matrix(diff_vector, ncol = 2, byrow = FALSE)
      diff_coords <- matrix(diff_coords, ncol = 2)
    }
    
    # Create plot
    plot(ref_coords, pch = 21, bg = "#95a5a6", cex = 1.5,
         main = paste("Shape Difference Vectors:", group1_name, "→", group2_name),
         xlab = "X Coordinate", ylab = "Y Coordinate",
         asp = 1, cex.main = input$vector_title_size/10,
         cex.lab = input$vector_axis_title_size/10,
         cex.axis = input$vector_axis_text_size/10)
    
    # Add vectors
    arrows(ref_coords[, 1], ref_coords[, 2],
           ref_coords[, 1] + diff_coords[, 1] * input$vector_scale,
           ref_coords[, 2] + diff_coords[, 2] * input$vector_scale,
           col = input$vector_color, lwd = input$vector_width,
           length = 0.1, angle = 20)
    
    # Add wireframe if available (validate links against landmark count)
    valid_links <- get_valid_links(rv$links, nrow(ref_coords))
    if (!is.null(valid_links) && nrow(valid_links) > 0) {
      for (i in seq_len(nrow(valid_links))) {
        lines(ref_coords[valid_links[i, ], 1], ref_coords[valid_links[i, ], 2],
              col = "gray", lwd = 1)
      }
    }
  })
  
  # Dispersion Plot (Beta diversity)
  output$dispersion_plot <- renderPlot({
    req(rv$betadisper, rv$groups)
    
    # Get selected groups with validation
    selected_groups <- input$disp_groups_select
    if (is.null(selected_groups) || length(selected_groups) == 0) {
      selected_groups <- levels(rv$groups)
    }
    
    # Filter data based on selected groups
    selected_indices <- rv$groups %in% selected_groups
    if (!any(selected_indices)) {
      return(NULL)  # No data to plot
    }
    
    distances <- rv$betadisper$distances[selected_indices]
    groups_filtered <- rv$groups[selected_indices]
    
    disp_data <- data.frame(
      Group = droplevels(groups_filtered),
      Distance_to_Centroid = distances
    )
    
    # Order groups according to selection order in dropdown
    disp_data$Group <- factor(disp_data$Group, levels = selected_groups)
    
    # Get individual group colors
    group_colors <- c()
    for (i in seq_along(selected_groups)) {
      color_input <- paste0("disp_group_color_", i)
      if (!is.null(input[[color_input]])) {
        group_colors[selected_groups[i]] <- input[[color_input]]
      } else {
        default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
        group_colors[selected_groups[i]] <- default_colors[min(i, length(default_colors))]
      }
    }
    
    p <- ggplot(disp_data, aes(x = Group, y = Distance_to_Centroid)) +
      geom_boxplot(aes(fill = Group), color = input$disp_outline_color, alpha = 0.7) +
      scale_fill_manual(values = group_colors) +
      theme_classic() +
      labs(
        title = "Morphological Dispersion by Group",
        x = "Group",
        y = "Distance to Centroid"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = input$disp_title_size, face = "bold"),
        axis.title = element_text(size = input$disp_axis_title_size),
        axis.text = element_text(size = input$disp_axis_text_size),
        axis.text.x = element_text(angle = 45, hjust = 1, size = input$disp_axis_text_size),
        legend.position = "none"
      )
    
    if (input$disp_show_points) {
      p <- p + geom_jitter(width = 0.2, alpha = 0.6, size = 2)
    }
    
    if (input$disp_show_mean) {
      p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red")
    }
    
    if (input$disp_show_centroids) {
      # Add horizontal line for each group mean
      group_means <- aggregate(Distance_to_Centroid ~ Group, disp_data, mean)
      for (i in seq_len(nrow(group_means))) {
        p <- p + annotate("segment", 
                         x = i - 0.4, xend = i + 0.4,
                         y = group_means$Distance_to_Centroid[i], 
                         yend = group_means$Distance_to_Centroid[i],
                         color = "red", size = 2)
      }
    }
    
    p
  })
  
  # Eigenvalues Plot (Scree Plot)
  output$eigenvalues_plot <- renderPlot({
    req(rv$pca, rv$pc_var)
    
    # Get the number of components to display
    n_components <- min(input$eigen_n_components, length(rv$pc_var))
    
    # Prepare data
    eigen_data <- data.frame(
      PC = paste0("PC", 1:n_components),
      Variance = rv$pc_var[1:n_components],
      Cumulative = cumsum(rv$pc_var[1:n_components])
    )
    eigen_data$PC <- factor(eigen_data$PC, levels = eigen_data$PC)
    
    # Create bar plot
    p <- ggplot(eigen_data, aes(x = PC, y = Variance)) +
      geom_bar(stat = "identity", fill = input$eigen_bar_color, alpha = 0.7) +
      theme_classic() +
      labs(
        title = "Eigenvalues: Variance Explained by Principal Components",
        x = "Principal Component",
        y = "% Variance Explained"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = input$eigen_title_size, face = "bold"),
        axis.title = element_text(size = input$eigen_axis_title_size),
        axis.text = element_text(size = input$eigen_axis_text_size),
        axis.text.x = element_text(angle = 45, hjust = 1, size = input$eigen_axis_text_size)
      )
    
    # Add cumulative variance line if requested
    if (input$eigen_show_cumulative) {
      p <- p + geom_line(aes(y = Cumulative, group = 1), 
                        color = input$eigen_line_color, size = 1.5) +
               geom_point(aes(y = Cumulative), 
                         color = input$eigen_line_color, size = 3) +
               scale_y_continuous(
                 name = "% Variance Explained",
                 sec.axis = sec_axis(~., name = "Cumulative % Variance")
               )
    }
    
    p
  })
  
  # Procrustes Plot - uses reactive links for real-time updates
  output$procrustes_plot <- renderPlot({
    req(rv$gpa, rv$ref, rv$pca)
    
    # Use reactive links that update in real-time and validate against landmark count
    raw_links <- reactive_links()
    n_landmarks <- nrow(rv$ref)
    current_links <- get_valid_links(raw_links, n_landmarks)
    
    pc_num <- as.numeric(gsub("PC", "", input$procrustes_pc))
    ref_vec <- as.vector(rv$ref)
    pc_loadings <- rv$pca$rotation
    n_dims <- if (input$dimension == "3d") 3 else 2
    
    # Create shape at specific PC value with optional PC axis flip (with sync support)
    make_shape <- function(score) {
      # Determine PC flip multiplier with sync option
      pc_multiplier <- 1
      if (isTRUE(input$procrustes_sync_pca_axis)) {
        if (input$procrustes_pc == input$pc_x && isTRUE(input$pca_flip_x)) {
          pc_multiplier <- -1
        } else if (input$procrustes_pc == input$pc_y && isTRUE(input$pca_flip_y)) {
          pc_multiplier <- -1
        }
      } else if (isTRUE(input$procrustes_flip_pc_axis)) {
        pc_multiplier <- -1
      }
      arrayspecs(
        matrix(ref_vec + pc_loadings[, pc_num] * score * pc_multiplier, ncol = n_dims),
        p = length(ref_vec) / n_dims, k = n_dims
      )[, , 1]
    }
    
    deformed_shape <- make_shape(input$procrustes_pc_value)
    
    # Project to 2D if 3D data - select axes based on user choice
    if (input$dimension == "3d") {
      axes <- switch(input$procrustes_axes,
        "xy" = c(1, 2),
        "xz" = c(1, 3),
        "yz" = c(2, 3)
      )
      ref_plot <- rv$ref[, axes]
      deformed_plot <- deformed_shape[, axes]
      coords_plot <- rv$gpa$coords[, axes, ]
    } else {
      ref_plot <- rv$ref
      deformed_plot <- deformed_shape
      coords_plot <- rv$gpa$coords
    }
    
    # Apply transformations
    transform_coords <- function(coords) {
      if (input$procrustes_rotate != "0") {
        angle <- as.numeric(input$procrustes_rotate) * pi / 180
        rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), 2, 2)
        coords <- t(rotation_matrix %*% t(coords))
      }
      if (input$procrustes_flip == "horizontal") {
        coords[, 1] <- -coords[, 1]
      } else if (input$procrustes_flip == "vertical") {
        coords[, 2] <- -coords[, 2]
      }
      return(coords)
    }
    
    ref_plot <- transform_coords(ref_plot)
    deformed_plot <- transform_coords(deformed_plot)
    
    # Transform all specimen coordinates
    if (input$dimension == "3d") {
      for (i in seq_len(dim(coords_plot)[3])) {
        coords_plot[, , i] <- transform_coords(coords_plot[, , i])
      }
    } else {
      for (i in seq_len(dim(coords_plot)[3])) {
        coords_plot[, , i] <- transform_coords(coords_plot[, , i])
      }
    }
    
    # Set up plot
    par(bg = "#FFFFFF", mar = c(4, 4, 3, 1))
    
    if (input$procrustes_view == "all") {
      # Plot all specimens
      xlim <- range(coords_plot[, 1, ])
      ylim <- range(coords_plot[, 2, ])
      
      plot(coords_plot[, 1, 1], coords_plot[, 2, 1], 
           type = "n", xlim = xlim, ylim = ylim,
           xlab = "X", ylab = "Y", asp = 1,
           main = paste0("Procrustes Shapes - ", input$procrustes_pc, " = ", input$procrustes_pc_value))
      
      # Draw all specimens
      for (i in seq_len(dim(coords_plot)[3])) {
        points(coords_plot[, 1, i], coords_plot[, 2, i], 
               col = input$procrustes_point_color, 
               pch = 19, cex = input$procrustes_point_size * 0.5)
        
        if (input$procrustes_show_wireframe && !is.null(current_links) && nrow(current_links) > 0) {
          # Use adjustcolor for transparency (alpha not supported in lines())
          wire_col_transparent <- adjustcolor(input$procrustes_wire_color, alpha.f = 0.3)
          for (j in seq_len(nrow(current_links))) {
            lines(coords_plot[current_links[j, ], 1, i], 
                  coords_plot[current_links[j, ], 2, i],
                  col = wire_col_transparent, 
                  lwd = input$procrustes_line_width * 0.5)
          }
        }
      }
      
      # Draw mean shape if requested
      if (input$procrustes_show_mean) {
        points(ref_plot, pch = 21, bg = input$procrustes_mean_color, 
               cex = input$procrustes_point_size * 1.2)
        if (!is.null(current_links) && nrow(current_links) > 0) {
          for (i in seq_len(nrow(current_links))) {
            lines(ref_plot[current_links[i, ], 1], ref_plot[current_links[i, ], 2],
                  col = input$procrustes_mean_color, 
                  lwd = input$procrustes_line_width * 1.2)
          }
        }
        
        # Add landmark numbers if requested
        if (isTRUE(input$procrustes_show_landmarks)) {
          lm_size <- if (!is.null(input$procrustes_landmark_size)) input$procrustes_landmark_size else 1
          lm_color <- if (!is.null(input$procrustes_landmark_color)) input$procrustes_landmark_color else "#000000"
          text(ref_plot[, 1], ref_plot[, 2], labels = 1:nrow(ref_plot), 
               cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
        }
      }
      
    } else {
      # Plot individual specimen
      req(input$procrustes_specimen)
      spec_idx <- which(rv$scores$Specimen == input$procrustes_specimen)[1]
      
      if (is.na(spec_idx)) return()
      
      spec_coords <- coords_plot[, , spec_idx]
      
      xlim <- range(c(spec_coords[, 1], deformed_plot[, 1]))
      ylim <- range(c(spec_coords[, 2], deformed_plot[, 2]))
      
      plot(spec_coords, type = "n", xlim = xlim, ylim = ylim,
           xlab = "X", ylab = "Y", asp = 1,
           main = paste0("Specimen: ", input$procrustes_specimen, " - ", 
                        input$procrustes_pc, " = ", input$procrustes_pc_value))
      
      # Draw specimen
      points(spec_coords, pch = 21, bg = input$procrustes_point_color, 
             cex = input$procrustes_point_size)
      
      if (input$procrustes_show_wireframe && !is.null(current_links) && nrow(current_links) > 0) {
        for (i in seq_len(nrow(current_links))) {
          lines(spec_coords[current_links[i, ], 1], spec_coords[current_links[i, ], 2],
                col = input$procrustes_wire_color, lwd = input$procrustes_line_width)
        }
      }
      
      # Add landmark numbers if requested
      if (isTRUE(input$procrustes_show_landmarks)) {
        lm_size <- if (!is.null(input$procrustes_landmark_size)) input$procrustes_landmark_size else 1
        lm_color <- if (!is.null(input$procrustes_landmark_color)) input$procrustes_landmark_color else "#000000"
        text(spec_coords[, 1], spec_coords[, 2], labels = 1:nrow(spec_coords), 
             cex = lm_size, col = lm_color, pos = 3, offset = 0.3)
      }
      
      # Draw deformed shape if requested
      if (input$procrustes_show_mean) {
        points(deformed_plot, pch = 21, bg = input$procrustes_mean_color, 
               cex = input$procrustes_point_size * 0.8)
        if (!is.null(current_links) && nrow(current_links) > 0) {
          for (i in seq_len(nrow(current_links))) {
            lines(deformed_plot[current_links[i, ], 1], deformed_plot[current_links[i, ], 2],
                  col = input$procrustes_mean_color, 
                  lwd = input$procrustes_line_width * 0.8,
                  lty = 2)
          }
        }
      }
    }
  })
  
  # Outlier Detection Plot (plotOutliers style)
  output$outlier_plot <- renderPlot({
    req(rv$gpa, rv$ref)
    
    # Calculate Procrustes distances from mean shape
    n_specimens <- dim(rv$gpa$coords)[3]
    distances <- numeric(n_specimens)
    
    # Flatten coordinates for distance calculation
    ref_flat <- as.vector(rv$ref)
    for (i in seq_len(n_specimens)) {
      spec_flat <- as.vector(rv$gpa$coords[, , i])
      distances[i] <- sqrt(sum((spec_flat - ref_flat)^2))
    }
    
    # Get specimen names
    spec_names <- if (!is.null(rv$scores$Specimen)) rv$scores$Specimen else paste0("Specimen_", 1:n_specimens)
    
    # Calculate threshold for outliers
    threshold_sd <- if (!is.null(input$outlier_threshold)) input$outlier_threshold else 2.5
    mean_dist <- mean(distances)
    sd_dist <- sd(distances)
    threshold <- mean_dist + threshold_sd * sd_dist
    
    # Identify outliers
    is_outlier <- distances > threshold
    
    # Create plot
    par(mar = c(8, 4, 4, 2), bg = "#FFFFFF")
    
    # Get colors
    normal_color <- if (!is.null(input$outlier_point_color)) input$outlier_point_color else "#3498db"
    outlier_color <- if (!is.null(input$outlier_highlight_color)) input$outlier_highlight_color else "#e74c3c"
    
    # Point colors based on outlier status
    point_colors <- ifelse(is_outlier & isTRUE(input$outlier_highlight), outlier_color, normal_color)
    
    # Create bar plot
    barplot_heights <- barplot(distances, 
            col = point_colors,
            border = NA,
            main = "Outlier Detection: Procrustes Distance from Mean Shape",
            ylab = "Procrustes Distance",
            xlab = "",
            ylim = c(0, max(distances) * 1.15))
    
    # Add threshold line
    abline(h = threshold, col = "#e74c3c", lty = 2, lwd = 2)
    text(par("usr")[2] * 0.85, threshold, 
         paste0("Threshold (", threshold_sd, " SD)"), 
         col = "#e74c3c", pos = 3, cex = 0.8)
    
    # Add mean line
    abline(h = mean_dist, col = "#2c3e50", lty = 3, lwd = 1.5)
    text(par("usr")[2] * 0.85, mean_dist, "Mean", col = "#2c3e50", pos = 3, cex = 0.8)
    
    # Add specimen labels if requested
    if (isTRUE(input$outlier_show_labels)) {
      text(barplot_heights, par("usr")[3] - 0.02 * diff(par("usr")[3:4]), 
           labels = spec_names, srt = 45, adj = 1, xpd = TRUE, cex = 0.7)
    }
    
    # Mark outliers with special symbols
    if (isTRUE(input$outlier_highlight) && any(is_outlier)) {
      outlier_indices <- which(is_outlier)
      points(barplot_heights[outlier_indices], distances[outlier_indices] + max(distances) * 0.03,
             pch = 25, col = outlier_color, bg = outlier_color, cex = 1.5)
    }
    
    # Add legend
    legend("topright", 
           legend = c("Normal", "Potential Outlier", paste0("Threshold (", threshold_sd, " SD)")),
           fill = c(normal_color, outlier_color, NA),
           border = c(NA, NA, NA),
           lty = c(NA, NA, 2),
           col = c(NA, NA, "#e74c3c"),
           lwd = c(NA, NA, 2),
           bty = "n",
           cex = 0.8)
  })
  
  # Outlier Summary Output
  output$outlier_summary <- renderPrint({
    req(rv$gpa, rv$ref)
    
    # Calculate Procrustes distances from mean shape
    n_specimens <- dim(rv$gpa$coords)[3]
    distances <- numeric(n_specimens)
    
    ref_flat <- as.vector(rv$ref)
    for (i in seq_len(n_specimens)) {
      spec_flat <- as.vector(rv$gpa$coords[, , i])
      distances[i] <- sqrt(sum((spec_flat - ref_flat)^2))
    }
    
    spec_names <- if (!is.null(rv$scores$Specimen)) rv$scores$Specimen else paste0("Specimen_", 1:n_specimens)
    
    threshold_sd <- if (!is.null(input$outlier_threshold)) input$outlier_threshold else 2.5
    mean_dist <- mean(distances)
    sd_dist <- sd(distances)
    threshold <- mean_dist + threshold_sd * sd_dist
    
    is_outlier <- distances > threshold
    
    cat("=================================================\n")
    cat("       OUTLIER DETECTION SUMMARY                 \n")
    cat("=================================================\n\n")
    cat("Total specimens:", n_specimens, "\n")
    cat("Mean Procrustes distance:", round(mean_dist, 6), "\n")
    cat("Standard deviation:", round(sd_dist, 6), "\n")
    cat("Threshold (", threshold_sd, " SD):", round(threshold, 6), "\n\n")
    
    cat("--- Outlier Status ---\n")
    cat("Potential outliers:", sum(is_outlier), "\n\n")
    
    if (sum(is_outlier) > 0) {
      cat("Flagged specimens:\n")
      outlier_data <- data.frame(
        Specimen = spec_names[is_outlier],
        Distance = round(distances[is_outlier], 6),
        SD_from_Mean = round((distances[is_outlier] - mean_dist) / sd_dist, 2)
      )
      outlier_data <- outlier_data[order(-outlier_data$Distance), ]
      print(outlier_data, row.names = FALSE)
    } else {
      cat("No outliers detected at the current threshold.\n")
    }
    
    cat("\n--- All Specimens (sorted by distance) ---\n")
    all_data <- data.frame(
      Specimen = spec_names,
      Distance = round(distances, 6),
      SD_from_Mean = round((distances - mean_dist) / sd_dist, 2),
      Outlier = ifelse(is_outlier, "Yes", "No")
    )
    all_data <- all_data[order(-all_data$Distance), ]
    print(head(all_data, 20), row.names = FALSE)
    if (n_specimens > 20) {
      cat(paste0("\n... and ", n_specimens - 20, " more specimens\n"))
    }
  })
  
  # Outlier Plot Download Handlers
  output$download_outlier_png <- downloadHandler(
    filename = function() {
      paste0("Outlier_Detection_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(rv$gpa, rv$ref)
      
      dl_width <- if (!is.null(input$outlier_download_width)) input$outlier_download_width else 800
      dl_height <- if (!is.null(input$outlier_download_height)) input$outlier_download_height else 600
      
      png(file, width = dl_width, height = dl_height, res = 120)
      
      # Same plot code as above
      n_specimens <- dim(rv$gpa$coords)[3]
      distances <- numeric(n_specimens)
      ref_flat <- as.vector(rv$ref)
      for (i in seq_len(n_specimens)) {
        spec_flat <- as.vector(rv$gpa$coords[, , i])
        distances[i] <- sqrt(sum((spec_flat - ref_flat)^2))
      }
      spec_names <- if (!is.null(rv$scores$Specimen)) rv$scores$Specimen else paste0("Specimen_", 1:n_specimens)
      threshold_sd <- if (!is.null(input$outlier_threshold)) input$outlier_threshold else 2.5
      mean_dist <- mean(distances)
      sd_dist <- sd(distances)
      threshold <- mean_dist + threshold_sd * sd_dist
      is_outlier <- distances > threshold
      
      par(mar = c(8, 4, 4, 2), bg = "#FFFFFF")
      normal_color <- if (!is.null(input$outlier_point_color)) input$outlier_point_color else "#3498db"
      outlier_color <- if (!is.null(input$outlier_highlight_color)) input$outlier_highlight_color else "#e74c3c"
      point_colors <- ifelse(is_outlier & isTRUE(input$outlier_highlight), outlier_color, normal_color)
      
      barplot_heights <- barplot(distances, col = point_colors, border = NA,
              main = "Outlier Detection: Procrustes Distance from Mean Shape",
              ylab = "Procrustes Distance", xlab = "", ylim = c(0, max(distances) * 1.15))
      abline(h = threshold, col = "#e74c3c", lty = 2, lwd = 2)
      abline(h = mean_dist, col = "#2c3e50", lty = 3, lwd = 1.5)
      
      if (isTRUE(input$outlier_show_labels)) {
        text(barplot_heights, par("usr")[3] - 0.02 * diff(par("usr")[3:4]), 
             labels = spec_names, srt = 45, adj = 1, xpd = TRUE, cex = 0.7)
      }
      
      legend("topright", 
             legend = c("Normal", "Potential Outlier", paste0("Threshold (", threshold_sd, " SD)")),
             fill = c(normal_color, outlier_color, NA), border = c(NA, NA, NA),
             lty = c(NA, NA, 2), col = c(NA, NA, "#e74c3c"), lwd = c(NA, NA, 2), bty = "n", cex = 0.8)
      
      dev.off()
    }
  )
  
  # Outlier Summary Text Download Handler
  output$download_outlier_summary <- downloadHandler(
    filename = function() {
      paste0("Outlier_Summary_", Sys.Date(), ".txt")
    },
    content = function(file) {
      req(rv$gpa, rv$ref)
      
      # Calculate Procrustes distances
      n_specimens <- dim(rv$gpa$coords)[3]
      distances <- numeric(n_specimens)
      ref_flat <- as.vector(rv$ref)
      for (i in seq_len(n_specimens)) {
        spec_flat <- as.vector(rv$gpa$coords[, , i])
        distances[i] <- sqrt(sum((spec_flat - ref_flat)^2))
      }
      
      spec_names <- if (!is.null(rv$scores$Specimen)) rv$scores$Specimen else paste0("Specimen_", 1:n_specimens)
      threshold_sd <- if (!is.null(input$outlier_threshold)) input$outlier_threshold else 2.5
      mean_dist <- mean(distances)
      sd_dist <- sd(distances)
      threshold <- mean_dist + threshold_sd * sd_dist
      is_outlier <- distances > threshold
      
      # Write to file
      sink(file)
      cat("=================================================\n")
      cat("       OUTLIER DETECTION SUMMARY                 \n")
      cat("       Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
      cat("=================================================\n\n")
      cat("Total specimens:", n_specimens, "\n")
      cat("Mean Procrustes distance:", round(mean_dist, 6), "\n")
      cat("Standard deviation:", round(sd_dist, 6), "\n")
      cat("Threshold (", threshold_sd, " SD):", round(threshold, 6), "\n\n")
      
      cat("--- Outlier Status ---\n")
      cat("Potential outliers:", sum(is_outlier), "\n\n")
      
      if (sum(is_outlier) > 0) {
        cat("Flagged specimens:\n")
        outlier_data <- data.frame(
          Specimen = spec_names[is_outlier],
          Distance = round(distances[is_outlier], 6),
          SD_from_Mean = round((distances[is_outlier] - mean_dist) / sd_dist, 2)
        )
        outlier_data <- outlier_data[order(-outlier_data$Distance), ]
        print(outlier_data, row.names = FALSE)
      } else {
        cat("No outliers detected at the current threshold.\n")
      }
      
      cat("\n--- All Specimens (sorted by distance) ---\n")
      all_data <- data.frame(
        Specimen = spec_names,
        Distance = round(distances, 6),
        SD_from_Mean = round((distances - mean_dist) / sd_dist, 2),
        Outlier = ifelse(is_outlier, "Yes", "No")
      )
      all_data <- all_data[order(-all_data$Distance), ]
      print(all_data, row.names = FALSE)
      sink()
    }
  )
  
  output$download_outlier_svg <- downloadHandler(
    filename = function() {
      paste0("Outlier_Detection_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(rv$gpa, rv$ref)
      
      dl_width <- if (!is.null(input$outlier_download_width)) input$outlier_download_width / 80 else 10
      dl_height <- if (!is.null(input$outlier_download_height)) input$outlier_download_height / 80 else 7.5
      
      svg(file, width = dl_width, height = dl_height)
      
      n_specimens <- dim(rv$gpa$coords)[3]
      distances <- numeric(n_specimens)
      ref_flat <- as.vector(rv$ref)
      for (i in seq_len(n_specimens)) {
        spec_flat <- as.vector(rv$gpa$coords[, , i])
        distances[i] <- sqrt(sum((spec_flat - ref_flat)^2))
      }
      spec_names <- if (!is.null(rv$scores$Specimen)) rv$scores$Specimen else paste0("Specimen_", 1:n_specimens)
      threshold_sd <- if (!is.null(input$outlier_threshold)) input$outlier_threshold else 2.5
      mean_dist <- mean(distances)
      sd_dist <- sd(distances)
      threshold <- mean_dist + threshold_sd * sd_dist
      is_outlier <- distances > threshold
      
      par(mar = c(8, 4, 4, 2), bg = "#FFFFFF")
      normal_color <- if (!is.null(input$outlier_point_color)) input$outlier_point_color else "#3498db"
      outlier_color <- if (!is.null(input$outlier_highlight_color)) input$outlier_highlight_color else "#e74c3c"
      point_colors <- ifelse(is_outlier & isTRUE(input$outlier_highlight), outlier_color, normal_color)
      
      barplot_heights <- barplot(distances, col = point_colors, border = NA,
              main = "Outlier Detection: Procrustes Distance from Mean Shape",
              ylab = "Procrustes Distance", xlab = "", ylim = c(0, max(distances) * 1.15))
      abline(h = threshold, col = "#e74c3c", lty = 2, lwd = 2)
      abline(h = mean_dist, col = "#2c3e50", lty = 3, lwd = 1.5)
      
      if (isTRUE(input$outlier_show_labels)) {
        text(barplot_heights, par("usr")[3] - 0.02 * diff(par("usr")[3:4]), 
             labels = spec_names, srt = 45, adj = 1, xpd = TRUE, cex = 0.7)
      }
      
      legend("topright", 
             legend = c("Normal", "Potential Outlier", paste0("Threshold (", threshold_sd, " SD)")),
             fill = c(normal_color, outlier_color, NA), border = c(NA, NA, NA),
             lty = c(NA, NA, 2), col = c(NA, NA, "#e74c3c"), lwd = c(NA, NA, 2), bty = "n", cex = 0.8)
      
      dev.off()
    }
  )
  
  # Download handlers for statistical plots
  output$download_stat_plot_png <- downloadHandler(
    filename = function() {
      plot_name <- switch(input$stat_plot_tabs,
        "Centroid Size" = "Centroid_Size_Plot",
        "Allometry Regression" = "Allometry_Plot", 
        "Shape Difference Vectors" = "Shape_Vectors_Plot",
        "Dispersion Plot" = "Dispersion_Plot"
      )
      paste0(plot_name, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Get custom dimensions
      dl_width <- if (!is.null(input$stat_download_width)) input$stat_download_width else 800
      dl_height <- if (!is.null(input$stat_download_height)) input$stat_download_height else 600
      
      png(file, width = dl_width, height = dl_height, res = 120)
      
      if (input$stat_plot_tabs == "Centroid Size") {
        req(rv$centroid_size, rv$groups, input$cs_groups_select)
        
        # Get selected groups
        selected_groups <- input$cs_groups_select
        if (is.null(selected_groups) || length(selected_groups) == 0) {
          selected_groups <- levels(rv$groups)
        }
        
        # Filter data based on selected groups
        selected_indices <- rv$groups %in% selected_groups
        cs_data <- data.frame(
          CentroidSize = rv$centroid_size[selected_indices],
          Group = rv$groups[selected_indices]
        )
        cs_data$Group <- droplevels(cs_data$Group)
        
        # Order groups according to selection order in dropdown
        cs_data$Group <- factor(cs_data$Group, levels = selected_groups)
        
        # Get individual group colors
        group_colors <- c()
        for (i in seq_along(selected_groups)) {
          color_input <- paste0("cs_group_color_", i)
          if (!is.null(input[[color_input]])) {
            group_colors[selected_groups[i]] <- input[[color_input]]
          } else {
            default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
            group_colors[selected_groups[i]] <- default_colors[min(i, length(default_colors))]
          }
        }
        
        p <- ggplot(cs_data, aes(x = Group, y = CentroidSize)) +
          geom_boxplot(aes(fill = Group), color = input$cs_outline_color, alpha = 0.7) +
          scale_fill_manual(values = group_colors) +
          theme_classic() +
          labs(title = "Centroid Size Comparison Between Groups", x = "Group", y = "Centroid Size") +
          theme(
            plot.title = element_text(hjust = 0.5, size = input$cs_title_size, face = "bold"),
            axis.title = element_text(size = input$cs_axis_title_size),
            axis.text = element_text(size = input$cs_axis_text_size),
            axis.text.x = element_text(angle = 45, hjust = 1, size = input$cs_axis_text_size),
            legend.position = "none"
          )
        if (input$cs_show_points) p <- p + geom_jitter(width = 0.2, alpha = 0.6, size = 2)
        if (input$cs_show_mean) p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red")
        print(p)
        
      } else if (input$stat_plot_tabs == "Allometry Regression") {
        req(rv$centroid_size, rv$pca, rv$groups)
        pc1_scores <- rv$pca$x[, 1]
        allo_data <- data.frame(
          CentroidSize = log(rv$centroid_size),
          PC1 = pc1_scores,
          Group = rv$groups
        )
        
        # Apply axis flipping - with sync option for PC1
        if (input$allo_flip_x) {
          allo_data$CentroidSize <- -allo_data$CentroidSize
        }
        
        # Determine PC1 flip - either manual or synced with PCA
        flip_pc1 <- FALSE
        if (isTRUE(input$allo_sync_pca_axis)) {
          if (input$pc_x == "PC1" && isTRUE(input$pca_flip_x)) {
            flip_pc1 <- TRUE
          } else if (input$pc_y == "PC1" && isTRUE(input$pca_flip_y)) {
            flip_pc1 <- TRUE
          }
        } else if (isTRUE(input$allo_flip_y)) {
          flip_pc1 <- TRUE
        }
        
        if (flip_pc1) {
          allo_data$PC1 <- -allo_data$PC1
        }
        
        if (input$allo_show_groups) {
          allo_colors <- get_allo_group_colors()
          p <- ggplot(allo_data, aes(x = CentroidSize, y = PC1, color = Group)) +
            geom_point(size = input$allo_point_size, alpha = 0.7) +
            scale_color_manual(values = allo_colors)
        } else {
          single_color <- input$allo_single_color
          if (is.null(single_color)) single_color <- "#3498db"
          p <- ggplot(allo_data, aes(x = CentroidSize, y = PC1)) +
            geom_point(size = input$allo_point_size, alpha = 0.7, color = single_color)
        }
        
        line_color <- input$allo_line_color
        if (is.null(line_color)) line_color <- "#2c3e50"
        
        p <- p + geom_smooth(method = "lm", se = input$allo_show_ci, 
                            color = line_color, size = input$allo_line_width) +
          theme_classic() +
          labs(title = "Allometry: Shape (PC1) vs Size Relationship", x = "Log Centroid Size", y = "PC1 Scores") +
          theme(
            plot.title = element_text(hjust = 0.5, size = input$allo_title_size, face = "bold"),
            axis.title = element_text(size = input$allo_axis_title_size),
            axis.text = element_text(size = input$allo_axis_text_size),
            legend.position = "bottom"
          )
        print(p)
        
      } else if (input$stat_plot_tabs == "Shape Difference Vectors") {
        req(rv$ref, rv$groups, input$vector_comparison)
        comp_parts <- strsplit(input$vector_comparison, "vs")[[1]]
        group1_idx <- as.numeric(comp_parts[1])
        group2_idx <- as.numeric(comp_parts[2])
        group_levels <- levels(rv$groups)
        group1_name <- group_levels[group1_idx]
        group2_name <- group_levels[group2_idx]
        
        coords_matrix <- two.d.array(rv$coords)
        group1_indices <- which(rv$groups == group1_name)
        group2_indices <- which(rv$groups == group2_name)
        group1_mean <- colMeans(coords_matrix[group1_indices, ])
        group2_mean <- colMeans(coords_matrix[group2_indices, ])
        diff_vector <- group2_mean - group1_mean
        
        n_dims <- if (input$dimension == "3d") 3 else 2
        if (input$dimension == "3d") {
          ref_coords <- matrix(rv$ref[, 1:2], ncol = 2)
          diff_coords <- matrix(diff_vector[seq(1, length(diff_vector), by = 3)], ncol = 1)
          diff_coords <- cbind(diff_coords, matrix(diff_vector[seq(2, length(diff_vector), by = 3)], ncol = 1))
        } else {
          ref_coords <- matrix(rv$ref, ncol = 2)
          diff_coords <- matrix(diff_vector, ncol = 2, byrow = FALSE)
          diff_coords <- matrix(diff_coords, ncol = 2)
        }
        
        plot(ref_coords, pch = 21, bg = "#95a5a6", cex = 1.5,
             main = paste("Shape Difference Vectors:", group1_name, "→", group2_name),
             xlab = "X Coordinate", ylab = "Y Coordinate", asp = 1)
        arrows(ref_coords[, 1], ref_coords[, 2],
               ref_coords[, 1] + diff_coords[, 1] * input$vector_scale,
               ref_coords[, 2] + diff_coords[, 2] * input$vector_scale,
               col = input$vector_color, lwd = input$vector_width,
               length = 0.1, angle = 20)
        # Validate links before drawing wireframe
        valid_links <- get_valid_links(rv$links, nrow(ref_coords))
        if (!is.null(valid_links) && nrow(valid_links) > 0) {
          for (i in seq_len(nrow(valid_links))) {
            lines(ref_coords[valid_links[i, ], 1], ref_coords[valid_links[i, ], 2], col = "gray", lwd = 1)
          }
        }
        
      } else if (input$stat_plot_tabs == "Dispersion Plot") {
        req(rv$betadisper, rv$groups, input$disp_groups_select)
        
        # Get selected groups
        selected_groups <- input$disp_groups_select
        if (is.null(selected_groups) || length(selected_groups) == 0) {
          selected_groups <- levels(rv$groups)
        }
        
        # Filter data based on selected groups
        selected_indices <- rv$groups %in% selected_groups
        distances <- rv$betadisper$distances[selected_indices]
        groups_filtered <- rv$groups[selected_indices]
        disp_data <- data.frame(
          Group = droplevels(groups_filtered),
          Distance_to_Centroid = distances
        )
        
        # Order groups according to selection order in dropdown
        disp_data$Group <- factor(disp_data$Group, levels = selected_groups)
        
        # Get individual group colors
        group_colors <- c()
        for (i in seq_along(selected_groups)) {
          color_input <- paste0("disp_group_color_", i)
          if (!is.null(input[[color_input]])) {
            group_colors[selected_groups[i]] <- input[[color_input]]
          } else {
            default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
            group_colors[selected_groups[i]] <- default_colors[min(i, length(default_colors))]
          }
        }
        
        p <- ggplot(disp_data, aes(x = Group, y = Distance_to_Centroid)) +
          geom_boxplot(aes(fill = Group), color = input$disp_outline_color, alpha = 0.7) +
          scale_fill_manual(values = group_colors) +
          theme_classic() +
          labs(title = "Morphological Dispersion by Group", x = "Group", y = "Distance to Centroid") +
          theme(
            plot.title = element_text(hjust = 0.5, size = input$disp_title_size, face = "bold"),
            axis.title = element_text(size = input$disp_axis_title_size),
            axis.text = element_text(size = input$disp_axis_text_size),
            axis.text.x = element_text(angle = 45, hjust = 1, size = input$disp_axis_text_size),
            legend.position = "none"
          )
        if (input$disp_show_points) p <- p + geom_jitter(width = 0.2, alpha = 0.6, size = 2)
        if (input$disp_show_mean) p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red")
        print(p)
      } else if (input$stat_plot_tabs == "Eigenvalues") {
        req(rv$pca, rv$pc_var)
        
        n_components <- min(input$eigen_n_components, length(rv$pc_var))
        eigen_data <- data.frame(
          PC = paste0("PC", 1:n_components),
          Variance = rv$pc_var[1:n_components],
          Cumulative = cumsum(rv$pc_var[1:n_components])
        )
        eigen_data$PC <- factor(eigen_data$PC, levels = eigen_data$PC)
        
        p <- ggplot(eigen_data, aes(x = PC, y = Variance)) +
          geom_bar(stat = "identity", fill = input$eigen_bar_color, alpha = 0.7) +
          theme_classic() +
          labs(title = "Eigenvalues: Variance Explained by Principal Components",
               x = "Principal Component", y = "% Variance Explained") +
          theme(
            plot.title = element_text(hjust = 0.5, size = input$eigen_title_size, face = "bold"),
            axis.title = element_text(size = input$eigen_axis_title_size),
            axis.text = element_text(size = input$eigen_axis_text_size),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
        
        if (input$eigen_show_cumulative) {
          p <- p + geom_line(aes(y = Cumulative, group = 1), 
                            color = input$eigen_line_color, size = 1.5) +
                   geom_point(aes(y = Cumulative), 
                             color = input$eigen_line_color, size = 3) +
                   scale_y_continuous(
                     name = "% Variance Explained",
                     sec.axis = sec_axis(~., name = "Cumulative % Variance")
                   )
        }
        print(p)
      }
      
      dev.off()
    }
  )
  
  output$download_stat_plot_svg <- downloadHandler(
    filename = function() {
      plot_name <- switch(input$stat_plot_tabs,
        "Centroid Size" = "Centroid_Size_Plot",
        "Allometry Regression" = "Allometry_Plot", 
        "Shape Difference Vectors" = "Shape_Vectors_Plot",
        "Dispersion Plot" = "Dispersion_Plot",
        "Eigenvalues" = "Eigenvalues_Plot"
      )
      paste0(plot_name, "_", Sys.Date(), ".svg")
    },
    content = function(file) {
      # Get custom dimensions (convert pixels to inches for SVG)
      dl_width <- if (!is.null(input$stat_download_width)) input$stat_download_width / 80 else 10
      dl_height <- if (!is.null(input$stat_download_height)) input$stat_download_height / 80 else 8
      
      svg(file, width = dl_width, height = dl_height)
      
      if (input$stat_plot_tabs == "Centroid Size") {
        req(rv$centroid_size, rv$groups, input$cs_groups_select)
        
        # Get selected groups
        selected_groups <- input$cs_groups_select
        if (is.null(selected_groups) || length(selected_groups) == 0) {
          selected_groups <- levels(rv$groups)
        }
        
        # Filter data based on selected groups
        selected_indices <- rv$groups %in% selected_groups
        cs_data <- data.frame(
          CentroidSize = rv$centroid_size[selected_indices],
          Group = rv$groups[selected_indices]
        )
        cs_data$Group <- droplevels(cs_data$Group)
        
        # Order groups according to selection order in dropdown
        cs_data$Group <- factor(cs_data$Group, levels = selected_groups)
        
        # Get individual group colors
        group_colors <- c()
        for (i in seq_along(selected_groups)) {
          color_input <- paste0("cs_group_color_", i)
          if (!is.null(input[[color_input]])) {
            group_colors[selected_groups[i]] <- input[[color_input]]
          } else {
            default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
            group_colors[selected_groups[i]] <- default_colors[min(i, length(default_colors))]
          }
        }
        
        p <- ggplot(cs_data, aes(x = Group, y = CentroidSize)) +
          geom_boxplot(aes(fill = Group), color = input$cs_outline_color, alpha = 0.7) +
          scale_fill_manual(values = group_colors) +
          theme_classic() +
          labs(title = "Centroid Size Comparison Between Groups", x = "Group", y = "Centroid Size") +
          theme(
            plot.title = element_text(hjust = 0.5, size = input$cs_title_size, face = "bold"),
            axis.title = element_text(size = input$cs_axis_title_size),
            axis.text = element_text(size = input$cs_axis_text_size),
            axis.text.x = element_text(angle = 45, hjust = 1, size = input$cs_axis_text_size),
            legend.position = "none"
          )
        if (input$cs_show_points) p <- p + geom_jitter(width = 0.2, alpha = 0.6, size = 2)
        if (input$cs_show_mean) p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red")
        print(p)
        
      } else if (input$stat_plot_tabs == "Allometry Regression") {
        req(rv$centroid_size, rv$pca, rv$groups)
        pc1_scores <- rv$pca$x[, 1]
        allo_data <- data.frame(
          CentroidSize = log(rv$centroid_size),
          PC1 = pc1_scores,
          Group = rv$groups
        )
        
        # Apply axis flipping - with sync option for PC1
        if (input$allo_flip_x) {
          allo_data$CentroidSize <- -allo_data$CentroidSize
        }
        
        # Determine PC1 flip - either manual or synced with PCA
        flip_pc1 <- FALSE
        if (isTRUE(input$allo_sync_pca_axis)) {
          if (input$pc_x == "PC1" && isTRUE(input$pca_flip_x)) {
            flip_pc1 <- TRUE
          } else if (input$pc_y == "PC1" && isTRUE(input$pca_flip_y)) {
            flip_pc1 <- TRUE
          }
        } else if (isTRUE(input$allo_flip_y)) {
          flip_pc1 <- TRUE
        }
        
        if (flip_pc1) {
          allo_data$PC1 <- -allo_data$PC1
        }
        
        if (input$allo_show_groups) {
          allo_colors <- get_allo_group_colors()
          p <- ggplot(allo_data, aes(x = CentroidSize, y = PC1, color = Group)) +
            geom_point(size = input$allo_point_size, alpha = 0.7) +
            scale_color_manual(values = allo_colors)
        } else {
          single_color <- input$allo_single_color
          if (is.null(single_color)) single_color <- "#3498db"
          p <- ggplot(allo_data, aes(x = CentroidSize, y = PC1)) +
            geom_point(size = input$allo_point_size, alpha = 0.7, color = single_color)
        }
        
        line_color <- input$allo_line_color
        if (is.null(line_color)) line_color <- "#2c3e50"
        
        p <- p + geom_smooth(method = "lm", se = input$allo_show_ci, 
                            color = line_color, size = input$allo_line_width) +
          theme_classic() +
          labs(title = "Allometry: Shape (PC1) vs Size Relationship", x = "Log Centroid Size", y = "PC1 Scores") +
          theme(
            plot.title = element_text(hjust = 0.5, size = input$allo_title_size, face = "bold"),
            axis.title = element_text(size = input$allo_axis_title_size),
            axis.text = element_text(size = input$allo_axis_text_size),
            legend.position = "bottom"
          )
        print(p)
        
      } else if (input$stat_plot_tabs == "Shape Difference Vectors") {
        req(rv$ref, rv$groups, input$vector_comparison)
        comp_parts <- strsplit(input$vector_comparison, "vs")[[1]]
        group1_idx <- as.numeric(comp_parts[1])
        group2_idx <- as.numeric(comp_parts[2])
        group_levels <- levels(rv$groups)
        group1_name <- group_levels[group1_idx]
        group2_name <- group_levels[group2_idx]
        
        coords_matrix <- two.d.array(rv$coords)
        group1_indices <- which(rv$groups == group1_name)
        group2_indices <- which(rv$groups == group2_name)
        group1_mean <- colMeans(coords_matrix[group1_indices, ])
        group2_mean <- colMeans(coords_matrix[group2_indices, ])
        diff_vector <- group2_mean - group1_mean
        
        n_dims <- if (input$dimension == "3d") 3 else 2
        if (input$dimension == "3d") {
          ref_coords <- matrix(rv$ref[, 1:2], ncol = 2)
          diff_coords <- matrix(diff_vector[seq(1, length(diff_vector), by = 3)], ncol = 1)
          diff_coords <- cbind(diff_coords, matrix(diff_vector[seq(2, length(diff_vector), by = 3)], ncol = 1))
        } else {
          ref_coords <- matrix(rv$ref, ncol = 2)
          diff_coords <- matrix(diff_vector, ncol = 2, byrow = FALSE)
          diff_coords <- matrix(diff_coords, ncol = 2)
        }
        
        plot(ref_coords, pch = 21, bg = "#95a5a6", cex = 1.5,
             main = paste("Shape Difference Vectors:", group1_name, "→", group2_name),
             xlab = "X Coordinate", ylab = "Y Coordinate", asp = 1)
        arrows(ref_coords[, 1], ref_coords[, 2],
               ref_coords[, 1] + diff_coords[, 1] * input$vector_scale,
               ref_coords[, 2] + diff_coords[, 2] * input$vector_scale,
               col = input$vector_color, lwd = input$vector_width,
               length = 0.1, angle = 20)
        # Validate links before drawing wireframe
        valid_links <- get_valid_links(rv$links, nrow(ref_coords))
        if (!is.null(valid_links) && nrow(valid_links) > 0) {
          for (i in seq_len(nrow(valid_links))) {
            lines(ref_coords[valid_links[i, ], 1], ref_coords[valid_links[i, ], 2], col = "gray", lwd = 1)
          }
        }
        
      } else if (input$stat_plot_tabs == "Dispersion Plot") {
        req(rv$betadisper, rv$groups, input$disp_groups_select)
        
        # Get selected groups
        selected_groups <- input$disp_groups_select
        if (is.null(selected_groups) || length(selected_groups) == 0) {
          selected_groups <- levels(rv$groups)
        }
        
        # Filter data based on selected groups
        selected_indices <- rv$groups %in% selected_groups
        distances <- rv$betadisper$distances[selected_indices]
        groups_filtered <- rv$groups[selected_indices]
        disp_data <- data.frame(
          Group = droplevels(groups_filtered),
          Distance_to_Centroid = distances
        )
        
        # Order groups according to selection order in dropdown
        disp_data$Group <- factor(disp_data$Group, levels = selected_groups)
        
        # Get individual group colors
        group_colors <- c()
        for (i in seq_along(selected_groups)) {
          color_input <- paste0("disp_group_color_", i)
          if (!is.null(input[[color_input]])) {
            group_colors[selected_groups[i]] <- input[[color_input]]
          } else {
            default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
            group_colors[selected_groups[i]] <- default_colors[min(i, length(default_colors))]
          }
        }
        
        p <- ggplot(disp_data, aes(x = Group, y = Distance_to_Centroid)) +
          geom_boxplot(aes(fill = Group), color = input$disp_outline_color, alpha = 0.7) +
          scale_fill_manual(values = group_colors) +
          theme_classic() +
          labs(title = "Morphological Dispersion by Group", x = "Group", y = "Distance to Centroid") +
          theme(
            plot.title = element_text(hjust = 0.5, size = input$disp_title_size, face = "bold"),
            axis.title = element_text(size = input$disp_axis_title_size),
            axis.text = element_text(size = input$disp_axis_text_size),
            axis.text.x = element_text(angle = 45, hjust = 1, size = input$disp_axis_text_size),
            legend.position = "none"
          )
        if (input$disp_show_points) p <- p + geom_jitter(width = 0.2, alpha = 0.6, size = 2)
        if (input$disp_show_mean) p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red")
        print(p)
      } else if (input$stat_plot_tabs == "Eigenvalues") {
        req(rv$pca, rv$pc_var)
        
        n_components <- min(input$eigen_n_components, length(rv$pc_var))
        eigen_data <- data.frame(
          PC = paste0("PC", 1:n_components),
          Variance = rv$pc_var[1:n_components],
          Cumulative = cumsum(rv$pc_var[1:n_components])
        )
        eigen_data$PC <- factor(eigen_data$PC, levels = eigen_data$PC)
        
        p <- ggplot(eigen_data, aes(x = PC, y = Variance)) +
          geom_bar(stat = "identity", fill = input$eigen_bar_color, alpha = 0.7) +
          theme_classic() +
          labs(title = "Eigenvalues: Variance Explained by Principal Components",
               x = "Principal Component", y = "% Variance Explained") +
          theme(
            plot.title = element_text(hjust = 0.5, size = input$eigen_title_size, face = "bold"),
            axis.title = element_text(size = input$eigen_axis_title_size),
            axis.text = element_text(size = input$eigen_axis_text_size),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
        
        if (input$eigen_show_cumulative) {
          p <- p + geom_line(aes(y = Cumulative, group = 1), 
                            color = input$eigen_line_color, size = 1.5) +
                   geom_point(aes(y = Cumulative), 
                             color = input$eigen_line_color, size = 3) +
                   scale_y_continuous(
                     name = "% Variance Explained",
                     sec.axis = sec_axis(~., name = "Cumulative % Variance")
                   )
        }
        print(p)
      }
      
      dev.off()
    }
  )
  
  output$download_all_stat_plots <- downloadHandler(
    filename = function() {
      paste0("Statistical_Plots_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir()
      
      # Generate all five plots
      plots <- c("Centroid Size", "Allometry Regression", "Shape Difference Vectors", "Dispersion Plot", "Eigenvalues")
      files_to_zip <- c()
      
      for (plot_type in plots) {
        plot_file <- file.path(temp_dir, paste0(gsub(" ", "_", plot_type), "_Plot.png"))
        files_to_zip <- c(files_to_zip, plot_file)
        
        png(plot_file, width = 800, height = 600)
        
        if (plot_type == "Centroid Size") {
          req(rv$centroid_size, rv$groups)
          
          # Get selected groups (same as individual download handler)
          selected_groups <- input$cs_groups_select
          if (is.null(selected_groups) || length(selected_groups) == 0) {
            selected_groups <- levels(rv$groups)
          }
          
          # Filter data based on selected groups
          selected_indices <- rv$groups %in% selected_groups
          cs_data <- data.frame(
            CentroidSize = rv$centroid_size[selected_indices],
            Group = rv$groups[selected_indices]
          )
          cs_data$Group <- droplevels(cs_data$Group)
          cs_data$Group <- factor(cs_data$Group, levels = selected_groups)
          
          # Get individual group colors (same as individual download handler)
          group_colors <- c()
          for (i in seq_along(selected_groups)) {
            color_input <- paste0("cs_group_color_", i)
            if (!is.null(input[[color_input]])) {
              group_colors[selected_groups[i]] <- input[[color_input]]
            } else {
              default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
              group_colors[selected_groups[i]] <- default_colors[min(i, length(default_colors))]
            }
          }
          
          p <- ggplot(cs_data, aes(x = Group, y = CentroidSize)) +
            geom_boxplot(aes(fill = Group), color = input$cs_outline_color, alpha = 0.7) +
            scale_fill_manual(values = group_colors) +
            theme_classic() +
            labs(title = "Centroid Size Comparison Between Groups", x = "Group", y = "Centroid Size") +
            theme(
              plot.title = element_text(hjust = 0.5, size = input$cs_title_size, face = "bold"),
              axis.title = element_text(size = input$cs_axis_title_size),
              axis.text = element_text(size = input$cs_axis_text_size),
              axis.text.x = element_text(angle = 45, hjust = 1, size = input$cs_axis_text_size),
              legend.position = "none"
            )
          if (input$cs_show_points) p <- p + geom_jitter(width = 0.2, alpha = 0.6, size = 2)
          if (input$cs_show_mean) p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red")
          print(p)
          
        } else if (plot_type == "Allometry Regression") {
          req(rv$centroid_size, rv$pca, rv$groups)
          pc1_scores <- rv$pca$x[, 1]
          allo_data <- data.frame(
            CentroidSize = log(rv$centroid_size),
            PC1 = pc1_scores,
            Group = rv$groups
          )
          
          # Apply axis flipping - same as individual download handler
          if (input$allo_flip_x) {
            allo_data$CentroidSize <- -allo_data$CentroidSize
          }
          
          # Determine PC1 flip
          flip_pc1 <- FALSE
          if (isTRUE(input$allo_sync_pca_axis)) {
            if (input$pc_x == "PC1" && isTRUE(input$pca_flip_x)) {
              flip_pc1 <- TRUE
            } else if (input$pc_y == "PC1" && isTRUE(input$pca_flip_y)) {
              flip_pc1 <- TRUE
            }
          } else if (isTRUE(input$allo_flip_y)) {
            flip_pc1 <- TRUE
          }
          
          if (flip_pc1) {
            allo_data$PC1 <- -allo_data$PC1
          }
          
          if (input$allo_show_groups) {
            allo_colors <- get_allo_group_colors()
            p <- ggplot(allo_data, aes(x = CentroidSize, y = PC1, color = Group)) +
              geom_point(size = input$allo_point_size, alpha = 0.7) +
              scale_color_manual(values = allo_colors)
          } else {
            single_color <- input$allo_single_color
            if (is.null(single_color)) single_color <- "#3498db"
            p <- ggplot(allo_data, aes(x = CentroidSize, y = PC1)) +
              geom_point(size = input$allo_point_size, alpha = 0.7, color = single_color)
          }
          
          line_color <- input$allo_line_color
          if (is.null(line_color)) line_color <- "#2c3e50"
          
          p <- p + geom_smooth(method = "lm", se = input$allo_show_ci, 
                              color = line_color, size = input$allo_line_width) +
            theme_classic() +
            labs(title = "Allometry: Shape (PC1) vs Size Relationship", x = "Log Centroid Size", y = "PC1 Scores") +
            theme(
              plot.title = element_text(hjust = 0.5, size = input$allo_title_size, face = "bold"),
              axis.title = element_text(size = input$allo_axis_title_size),
              axis.text = element_text(size = input$allo_axis_text_size),
              legend.position = "bottom"
            )
          print(p)
          
        } else if (plot_type == "Shape Difference Vectors") {
          req(rv$ref, rv$groups, input$vector_comparison)
          comp_parts <- strsplit(input$vector_comparison, "vs")[[1]]
          group1_idx <- as.numeric(comp_parts[1])
          group2_idx <- as.numeric(comp_parts[2])
          group_levels <- levels(rv$groups)
          group1_name <- group_levels[group1_idx]
          group2_name <- group_levels[group2_idx]
          
          coords_matrix <- two.d.array(rv$coords)
          group1_indices <- which(rv$groups == group1_name)
          group2_indices <- which(rv$groups == group2_name)
          group1_mean <- colMeans(coords_matrix[group1_indices, ])
          group2_mean <- colMeans(coords_matrix[group2_indices, ])
          diff_vector <- group2_mean - group1_mean
          
          n_dims <- if (input$dimension == "3d") 3 else 2
          if (input$dimension == "3d") {
            ref_coords <- matrix(rv$ref[, 1:2], ncol = 2)
            diff_coords <- matrix(diff_vector[seq(1, length(diff_vector), by = 3)], ncol = 1)
            diff_coords <- cbind(diff_coords, matrix(diff_vector[seq(2, length(diff_vector), by = 3)], ncol = 1))
          } else {
            ref_coords <- matrix(rv$ref, ncol = 2)
            diff_coords <- matrix(diff_vector, ncol = 2, byrow = FALSE)
            diff_coords <- matrix(diff_coords, ncol = 2)
          }
          
          plot(ref_coords, pch = 21, bg = "#95a5a6", cex = 1.5,
               main = paste("Shape Difference Vectors:", group1_name, "→", group2_name),
               xlab = "X Coordinate", ylab = "Y Coordinate", asp = 1)
          arrows(ref_coords[, 1], ref_coords[, 2],
                 ref_coords[, 1] + diff_coords[, 1] * input$vector_scale,
                 ref_coords[, 2] + diff_coords[, 2] * input$vector_scale,
                 col = input$vector_color, lwd = input$vector_width,
                 length = 0.1, angle = 20)
          if (!is.null(rv$links) && nrow(rv$links) > 0) {
            valid_links_stat <- get_valid_links(rv$links, nrow(ref_coords))
            if (!is.null(valid_links_stat)) {
              for (i in seq_len(nrow(valid_links_stat))) {
                lines(ref_coords[valid_links_stat[i, ], 1], ref_coords[valid_links_stat[i, ], 2], col = "gray", lwd = 1)
              }
            }
          }
          
        } else if (plot_type == "Dispersion Plot") {
          req(rv$betadisper, rv$groups)
          
          # Get selected groups (same as individual download handler)
          selected_groups <- input$disp_groups_select
          if (is.null(selected_groups) || length(selected_groups) == 0) {
            selected_groups <- levels(rv$groups)
          }
          
          # Filter data based on selected groups
          selected_indices <- rv$groups %in% selected_groups
          distances <- rv$betadisper$distances[selected_indices]
          groups_filtered <- rv$groups[selected_indices]
          disp_data <- data.frame(
            Group = droplevels(groups_filtered),
            Distance_to_Centroid = distances
          )
          disp_data$Group <- factor(disp_data$Group, levels = selected_groups)
          
          # Get individual group colors (same as individual download handler)
          group_colors <- c()
          for (i in seq_along(selected_groups)) {
            color_input <- paste0("disp_group_color_", i)
            if (!is.null(input[[color_input]])) {
              group_colors[selected_groups[i]] <- input[[color_input]]
            } else {
              default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
              group_colors[selected_groups[i]] <- default_colors[min(i, length(default_colors))]
            }
          }
          
          p <- ggplot(disp_data, aes(x = Group, y = Distance_to_Centroid)) +
            geom_boxplot(aes(fill = Group), color = input$disp_outline_color, alpha = 0.7) +
            scale_fill_manual(values = group_colors) +
            theme_classic() +
            labs(title = "Morphological Dispersion by Group", x = "Group", y = "Distance to Centroid") +
            theme(
              plot.title = element_text(hjust = 0.5, size = input$disp_title_size, face = "bold"),
              axis.title = element_text(size = input$disp_axis_title_size),
              axis.text = element_text(size = input$disp_axis_text_size),
              axis.text.x = element_text(angle = 45, hjust = 1, size = input$disp_axis_text_size),
              legend.position = "none"
            )
          if (input$disp_show_points) p <- p + geom_jitter(width = 0.2, alpha = 0.6, size = 2)
          if (input$disp_show_mean) p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red")
          print(p)
          
        } else if (plot_type == "Eigenvalues") {
          req(rv$pca, rv$pc_var)
          
          n_components <- min(input$eigen_n_components, length(rv$pc_var))
          eigen_data <- data.frame(
            PC = paste0("PC", 1:n_components),
            Variance = rv$pc_var[1:n_components],
            Cumulative = cumsum(rv$pc_var[1:n_components])
          )
          eigen_data$PC <- factor(eigen_data$PC, levels = eigen_data$PC)
          
          p <- ggplot(eigen_data, aes(x = PC, y = Variance)) +
            geom_bar(stat = "identity", fill = input$eigen_bar_color, alpha = 0.7) +
            theme_classic() +
            labs(title = "Eigenvalues: Variance Explained by Principal Components",
                 x = "Principal Component", y = "% Variance Explained") +
            theme(
              plot.title = element_text(hjust = 0.5, size = input$eigen_title_size, face = "bold"),
              axis.title = element_text(size = input$eigen_axis_title_size),
              axis.text = element_text(size = input$eigen_axis_text_size),
              axis.text.x = element_text(angle = 45, hjust = 1)
            )
          
          if (input$eigen_show_cumulative) {
            p <- p + geom_line(aes(y = Cumulative, group = 1), 
                              color = input$eigen_line_color, size = 1.5) +
                     geom_point(aes(y = Cumulative), 
                               color = input$eigen_line_color, size = 3) +
                     scale_y_continuous(
                       name = "% Variance Explained",
                       sec.axis = sec_axis(~., name = "Cumulative % Variance")
                     )
          }
          print(p)
        }
        
        dev.off()
      }
      
      zip(file, files_to_zip, flags = "-j")
    }
  )

  # Download Results (Complete Package)
  output$download_results <- downloadHandler(
    filename = function() {
      # Use project name as prefix for the zip file
      project_name <- gsub("[^a-zA-Z0-9_-]", "_", input$project_name)
      if (nchar(project_name) == 0) project_name <- "Morphostat_Project"
      paste0(project_name, "_Results_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create temporary directory
      temp_dir <- tempdir()
      files_to_zip <- c()

      # ===== Save comprehensive results to text file =====
      results_file <- file.path(temp_dir, "Analysis_Results.txt")
      sink(results_file)

      cat("=================================================\n")
      cat("       MORPHOSTAT ANALYSIS RESULTS              \n")
      cat("       Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
      cat("=================================================\n\n")

      cat("--- Data Summary ---\n")
      if (!is.null(rv$coords)) {
        cat("Total specimens:", dim(rv$coords)[3], "\n")
        cat("Number of landmarks:", dim(rv$coords)[1], "\n")
        cat("Dimensions:", if (input$dimension == "3d") "3D" else "2D", "\n")
        cat("Groups:", paste(levels(rv$groups), collapse = ", "), "\n")
      }
      
      if (!is.null(rv$pc_var)) {
        n_show <- min(10, length(rv$pc_var))
        cat("\nVariance explained by first", n_show, "PCs (%):\n")
        print(rv$pc_var[seq_len(n_show)])
        
        if (!is.null(rv$pca_95)) {
          cat("\nPCs explaining", rv$pca_95$threshold, "% of variance:", rv$pca_95$n_components, "\n")
          cat("Cumulative variance explained:", round(rv$pca_95$variance_explained, 2), "%\n")
        }
      }
      cat("\n")

      # Centroid Size Analysis
      if (!is.null(rv$centroid_size)) {
        cat("--- Centroid Size Analysis ---\n")
        cs_summary <- aggregate(rv$centroid_size ~ rv$groups,
          FUN = function(x) c(mean = round(mean(x), 4), sd = round(sd(x), 4)))
        print(cs_summary)
        cat("\n")
      }

      # Allometry Results
      if (!is.null(rv$allometry_fit)) {
        cat("--- Allometry Regression (Shape ~ log(CS)) ---\n")
        print(summary(rv$allometry_fit))
        cat("\n")
      }

      # PERMANOVA Results
      if (!is.null(rv$fit)) {
        cat("--- PERMANOVA Results (Shape-based) ---\n")
        print(summary(rv$fit))
        cat("\n")
      }

      # PERMANOVA on 95% PCs
      if (!is.null(rv$fit_pc95)) {
        cat("--- PERMANOVA on 95% PCs (Euclidean Distance) ---\n")
        print(rv$fit_pc95)
        cat("\n")
      }

      # PERMDISP
      if (!is.null(rv$betadisper_anova)) {
        cat("--- PERMDISP Test (Homogeneity of Dispersions) ---\n")
        cat("Testing for differences in group dispersions\n")
        cat("Null hypothesis: Group dispersions are equal\n\n")
        print(rv$betadisper_anova)
        if (!is.null(rv$betadisper)) {
          cat("\nGroup Distances to Centroid:\n")
          print(rv$betadisper$group.distances)
        }
        cat("\nInterpretation:\n")
        p_value <- rv$betadisper_anova$`Pr(>F)`[1]
        if (p_value > 0.05) {
          cat(sprintf("p-value = %.4f (> 0.05)\n", p_value))
          cat("Group dispersions are homogeneous. PERMANOVA results are reliable.\n")
        } else {
          cat(sprintf("p-value = %.4f (< 0.05)\n", p_value))
          cat("WARNING: Group dispersions differ significantly!\n")
          cat("PERMANOVA may show false positives.\n")
        }
        cat("\n")
      }

      # Pairwise Comparisons
      if (!is.null(rv$pairwise)) {
        cat("--- Pairwise Comparisons ---\n")
        print(summary(rv$pairwise,
          test.type = "dist",
          confidence = input$confidence, stat.table = TRUE
        ))
        cat("\n")
      }

      # Group Centroids
      if (!is.null(rv$centroids)) {
        cat("--- Group Centroids ---\n")
        print(rv$centroids)
      }

      sink()
      files_to_zip <- c(files_to_zip, results_file)

      # ===== Save PC scores =====
      if (!is.null(rv$scores)) {
        scores_file <- file.path(temp_dir, "PC_Scores.csv")
        write.csv(rv$scores, scores_file, row.names = FALSE)
        files_to_zip <- c(files_to_zip, scores_file)
      }

      # ===== Save Outlier Summary =====
      if (!is.null(rv$gpa) && !is.null(rv$ref)) {
        outlier_file <- file.path(temp_dir, "Outlier_Summary.txt")
        sink(outlier_file)
        
        n_specimens <- dim(rv$gpa$coords)[3]
        distances <- numeric(n_specimens)
        ref_flat <- as.vector(rv$ref)
        for (i in seq_len(n_specimens)) {
          spec_flat <- as.vector(rv$gpa$coords[, , i])
          distances[i] <- sqrt(sum((spec_flat - ref_flat)^2))
        }
        spec_names <- if (!is.null(rv$scores$Specimen)) rv$scores$Specimen else paste0("Specimen_", 1:n_specimens)
        threshold_sd <- 2.5
        mean_dist <- mean(distances)
        sd_dist <- sd(distances)
        threshold <- mean_dist + threshold_sd * sd_dist
        is_outlier <- distances > threshold
        
        cat("=================================================\n")
        cat("       OUTLIER DETECTION SUMMARY                 \n")
        cat("=================================================\n\n")
        cat("Total specimens:", n_specimens, "\n")
        cat("Mean Procrustes distance:", round(mean_dist, 6), "\n")
        cat("Standard deviation:", round(sd_dist, 6), "\n")
        cat("Threshold (2.5 SD):", round(threshold, 6), "\n")
        cat("Potential outliers:", sum(is_outlier), "\n\n")
        
        all_data <- data.frame(
          Specimen = spec_names,
          Distance = round(distances, 6),
          SD_from_Mean = round((distances - mean_dist) / sd_dist, 2),
          Outlier = ifelse(is_outlier, "Yes", "No")
        )
        all_data <- all_data[order(-all_data$Distance), ]
        print(all_data, row.names = FALSE)
        
        sink()
        files_to_zip <- c(files_to_zip, outlier_file)
      }

      # ===== Save PCA Plot (Color) =====
      if (!is.null(rv$scores) && !is.null(rv$pc_var)) {
        pca_color_file <- file.path(temp_dir, "PCA_Plot_Color.png")
        png(pca_color_file, width = 800, height = 600, res = 100)
        tryCatch({
          plot_data <- rv$scores
          plot_data$Group <- get_renamed_groups()
          
          p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Group, fill = Group)) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
            geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")
          
          # Add data spread visualization based on selection
          spread_type <- if (!is.null(input$pca_spread_type)) input$pca_spread_type else "ellipse"
          spread_fill <- if (!is.null(input$pca_spread_fill) && input$pca_spread_customize) input$pca_spread_fill else TRUE
          spread_outline <- if (!is.null(input$pca_spread_outline) && input$pca_spread_customize) input$pca_spread_outline else TRUE
          spread_alpha <- if (!is.null(input$pca_spread_fill_alpha) && input$pca_spread_customize) input$pca_spread_fill_alpha else 0.2
          spread_lwd <- if (!is.null(input$pca_spread_line_width) && input$pca_spread_customize) input$pca_spread_line_width else 1.5
          ellipse_level <- if (!is.null(input$pca_ellipse_level) && input$pca_spread_customize) input$pca_ellipse_level else input$confidence
          
          if (spread_type == "ellipse") {
            if (spread_fill && spread_outline) {
              p <- p + stat_ellipse(type = "norm", alpha = spread_alpha, level = ellipse_level, size = spread_lwd, geom = "polygon")
            } else if (spread_fill) {
              p <- p + stat_ellipse(type = "norm", alpha = spread_alpha, level = ellipse_level, size = 0, geom = "polygon")
            } else if (spread_outline) {
              p <- p + stat_ellipse(type = "norm", level = ellipse_level, size = spread_lwd, geom = "path")
            }
          } else if (spread_type == "hull") {
            hull_data <- do.call(rbind, lapply(split(plot_data, plot_data$Group), function(df) {
              df[chull(df[["PC1"]], df[["PC2"]]), ]
            }))
            if (spread_fill && spread_outline) {
              p <- p + geom_polygon(data = hull_data, aes(x = PC1, y = PC2, fill = Group, color = Group),
                                   alpha = spread_alpha, size = spread_lwd)
            } else if (spread_fill) {
              p <- p + geom_polygon(data = hull_data, aes(x = PC1, y = PC2, fill = Group),
                                   alpha = spread_alpha, color = NA)
            } else if (spread_outline) {
              p <- p + geom_polygon(data = hull_data, aes(x = PC1, y = PC2, color = Group),
                                   fill = NA, size = spread_lwd)
            }
          } else if (spread_type == "density") {
            density_bins <- if (!is.null(input$pca_density_bins) && input$pca_spread_customize) input$pca_density_bins else 5
            if (spread_fill && spread_outline) {
              p <- p + geom_density_2d_filled(aes(x = PC1, y = PC2), alpha = spread_alpha, bins = density_bins, show.legend = FALSE) +
                       geom_density_2d(aes(x = PC1, y = PC2, color = Group), size = spread_lwd, bins = density_bins)
            } else if (spread_fill) {
              p <- p + geom_density_2d_filled(aes(x = PC1, y = PC2), alpha = spread_alpha, bins = density_bins, show.legend = FALSE)
            } else if (spread_outline) {
              p <- p + geom_density_2d(aes(x = PC1, y = PC2, color = Group), size = spread_lwd, bins = density_bins)
            }
          }
          
          p <- p + geom_point(size = 4, shape = 21, stroke = 1.5) +
            theme_classic(base_size = 14) +
            labs(
              title = input$pca_title,
              x = paste0("PC1 (", rv$pc_var[1], "%)"),
              y = paste0("PC2 (", rv$pc_var[2], "%)")
            ) +
            theme(
              legend.position = "right",
              legend.title = element_blank(),
              plot.background = element_rect(fill = input$plot_bg),
              panel.background = element_rect(fill = input$plot_bg)
            )
          
          colors <- get_group_colors()
          if (!is.null(colors)) {
            p <- p + scale_color_manual(values = colors) + scale_fill_manual(values = colors)
          }
          
          print(p)
        }, error = function(e) NULL)
        dev.off()
        files_to_zip <- c(files_to_zip, pca_color_file)
      }

      # ===== Save Centroid Size Plot =====
      if (!is.null(rv$centroid_size) && !is.null(rv$groups)) {
        cs_file <- file.path(temp_dir, "Centroid_Size_Plot.png")
        png(cs_file, width = 800, height = 600, res = 100)
        tryCatch({
          # Get selected groups
          selected_groups <- input$cs_groups_select
          if (is.null(selected_groups) || length(selected_groups) == 0) {
            selected_groups <- levels(rv$groups)
          }
          
          # Filter data based on selected groups
          selected_indices <- rv$groups %in% selected_groups
          cs_data <- data.frame(
            CentroidSize = rv$centroid_size[selected_indices],
            Group = rv$groups[selected_indices]
          )
          cs_data$Group <- droplevels(cs_data$Group)
          cs_data$Group <- factor(cs_data$Group, levels = selected_groups)
          
          # Get individual group colors
          group_colors <- c()
          for (i in seq_along(selected_groups)) {
            color_input <- paste0("cs_group_color_", i)
            if (!is.null(input[[color_input]])) {
              group_colors[selected_groups[i]] <- input[[color_input]]
            } else {
              default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6", "#1abc9c")
              group_colors[selected_groups[i]] <- default_colors[min(i, length(default_colors))]
            }
          }
          
          p <- ggplot(cs_data, aes(x = Group, y = CentroidSize)) +
            geom_boxplot(aes(fill = Group), color = input$cs_outline_color, alpha = 0.7) +
            scale_fill_manual(values = group_colors) +
            theme_classic(base_size = 14) +
            labs(title = "Centroid Size by Group", y = "Centroid Size") +
            theme(legend.position = "none")
          if (input$cs_show_points) p <- p + geom_jitter(width = 0.2, alpha = 0.5, size = 2)
          if (input$cs_show_mean) p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red")
          print(p)
        }, error = function(e) NULL)
        dev.off()
        files_to_zip <- c(files_to_zip, cs_file)
      }

      # ===== Save Allometry Plot =====
      if (!is.null(rv$centroid_size) && !is.null(rv$pca)) {
        allo_file <- file.path(temp_dir, "Allometry_Plot.png")
        png(allo_file, width = 800, height = 600, res = 100)
        tryCatch({
          allo_data <- data.frame(
            logCS = log(rv$centroid_size),
            PC1 = rv$pca$x[, 1],
            Group = rv$groups
          )
          
          # Apply axis flipping
          if (input$allo_flip_x) {
            allo_data$logCS <- -allo_data$logCS
          }
          
          # Determine PC1 flip
          flip_pc1 <- FALSE
          if (isTRUE(input$allo_sync_pca_axis)) {
            if (input$pc_x == "PC1" && isTRUE(input$pca_flip_x)) {
              flip_pc1 <- TRUE
            } else if (input$pc_y == "PC1" && isTRUE(input$pca_flip_y)) {
              flip_pc1 <- TRUE
            }
          } else if (isTRUE(input$allo_flip_y)) {
            flip_pc1 <- TRUE
          }
          
          if (flip_pc1) {
            allo_data$PC1 <- -allo_data$PC1
          }
          
          if (input$allo_show_groups) {
            allo_colors <- get_allo_group_colors()
            p <- ggplot(allo_data, aes(x = logCS, y = PC1, color = Group)) +
              geom_point(size = input$allo_point_size, alpha = 0.7) +
              scale_color_manual(values = allo_colors)
          } else {
            single_color <- input$allo_single_color
            if (is.null(single_color)) single_color <- "#3498db"
            p <- ggplot(allo_data, aes(x = logCS, y = PC1)) +
              geom_point(size = input$allo_point_size, alpha = 0.7, color = single_color)
          }
          
          line_color <- input$allo_line_color
          if (is.null(line_color)) line_color <- "#2c3e50"
          
          p <- p + geom_smooth(method = "lm", se = input$allo_show_ci, 
                              color = line_color, size = input$allo_line_width) +
            theme_classic(base_size = 14) +
            labs(title = "Allometry: Shape vs Size",
                 x = "log(Centroid Size)", y = "PC1 (Shape)")
          print(p)
        }, error = function(e) NULL)
        dev.off()
        files_to_zip <- c(files_to_zip, allo_file)
      }

      # ===== Save Eigenvalues Plot =====
      if (!is.null(rv$pca) && !is.null(rv$pc_var)) {
        eigen_file <- file.path(temp_dir, "Eigenvalues_Plot.png")
        png(eigen_file, width = 800, height = 600, res = 100)
        tryCatch({
          n_pcs <- min(input$eigen_n_components, length(rv$pc_var))
          eigen_data <- data.frame(
            PC = paste0("PC", 1:n_pcs),
            Variance = rv$pc_var[1:n_pcs],
            Cumulative = cumsum(rv$pc_var[1:n_pcs])
          )
          eigen_data$PC <- factor(eigen_data$PC, levels = eigen_data$PC)
          
          p <- ggplot(eigen_data, aes(x = PC, y = Variance)) +
            geom_bar(stat = "identity", fill = input$eigen_bar_color, alpha = 0.7) +
            theme_classic(base_size = 14) +
            labs(title = "Variance Explained by Principal Components",
                 x = "Principal Component", y = "% Variance")
          
          if (input$eigen_show_cumulative) {
            p <- p + geom_line(aes(y = Cumulative, group = 1), color = input$eigen_line_color, size = 1.5) +
              geom_point(aes(y = Cumulative), color = input$eigen_line_color, size = 3) +
              scale_y_continuous(
                name = "% Variance Explained",
                sec.axis = sec_axis(~., name = "Cumulative % Variance")
              )
          }
          print(p)
        }, error = function(e) NULL)
        dev.off()
        files_to_zip <- c(files_to_zip, eigen_file)
      }

      # ===== Save Outlier Detection Plot =====
      if (!is.null(rv$gpa) && !is.null(rv$ref)) {
        outlier_plot_file <- file.path(temp_dir, "Outlier_Detection_Plot.png")
        png(outlier_plot_file, width = 900, height = 600, res = 100)
        tryCatch({
          n_specimens <- dim(rv$gpa$coords)[3]
          distances <- numeric(n_specimens)
          ref_flat <- as.vector(rv$ref)
          for (i in seq_len(n_specimens)) {
            spec_flat <- as.vector(rv$gpa$coords[, , i])
            distances[i] <- sqrt(sum((spec_flat - ref_flat)^2))
          }
          mean_dist <- mean(distances)
          sd_dist <- sd(distances)
          threshold <- mean_dist + 2.5 * sd_dist
          is_outlier <- distances > threshold
          point_colors <- ifelse(is_outlier, "#e74c3c", "#3498db")
          
          par(mar = c(5, 4, 4, 2), bg = "#FFFFFF")
          bp <- barplot(distances, col = point_colors, border = NA,
                  main = "Outlier Detection: Procrustes Distance from Mean Shape",
                  ylab = "Procrustes Distance", ylim = c(0, max(distances) * 1.15))
          abline(h = threshold, col = "#e74c3c", lty = 2, lwd = 2)
          abline(h = mean_dist, col = "#2c3e50", lty = 3, lwd = 1.5)
          legend("topright", legend = c("Normal", "Outlier", "Threshold (2.5 SD)"),
                 fill = c("#3498db", "#e74c3c", NA), border = NA,
                 lty = c(NA, NA, 2), col = c(NA, NA, "#e74c3c"), lwd = c(NA, NA, 2), bty = "n")
        }, error = function(e) NULL)
        dev.off()
        files_to_zip <- c(files_to_zip, outlier_plot_file)
      }

      # ===== Save PC Deformation Plot =====
      if (!is.null(rv$ref) && !is.null(rv$pca)) {
        deform_file <- file.path(temp_dir, "PC1_Deformation_Plot.png")
        png(deform_file, width = 800, height = 600, res = 100)
        tryCatch({
          pc_num <- 1
          ref_vec <- as.vector(rv$ref)
          pc_loadings <- rv$pca$rotation
          n_dims <- if (input$dimension == "3d") 3 else 2
          
          make_shape <- function(score) {
            arrayspecs(
              matrix(ref_vec + pc_loadings[, pc_num] * score, ncol = n_dims),
              p = length(ref_vec) / n_dims, k = n_dims
            )[, , 1]
          }
          
          shape_minus <- make_shape(-0.05)
          shape_plus <- make_shape(0.05)
          
          if (input$dimension == "3d") {
            ref_plot <- rv$ref[, c(1, 2)]
            shape_minus_plot <- shape_minus[, c(1, 2)]
            shape_plus_plot <- shape_plus[, c(1, 2)]
          } else {
            ref_plot <- rv$ref
            shape_minus_plot <- shape_minus
            shape_plus_plot <- shape_plus
          }
          
          par(bg = "#FFFFFF", mar = c(4, 4, 3, 1))
          all_coords <- rbind(ref_plot, shape_minus_plot, shape_plus_plot)
          xlim <- range(all_coords[, 1]) * 1.1
          ylim <- range(all_coords[, 2]) * 1.1
          
          plot(ref_plot[,1], ref_plot[,2], type = "n", xlim = xlim, ylim = ylim,
               xlab = "X", ylab = "Y", asp = 1,
               main = paste0("PC1 Shape Deformations (", rv$pc_var[1], "% variance)"))
          
          if (!is.null(rv$links) && nrow(rv$links) > 0) {
            for (i in seq_len(nrow(rv$links))) {
              lines(ref_plot[rv$links[i, ], 1], ref_plot[rv$links[i, ], 2], col = "#95a5a6", lwd = 2)
              lines(shape_minus_plot[rv$links[i, ], 1], shape_minus_plot[rv$links[i, ], 2], col = "#e74c3c", lwd = 2, lty = 2)
              lines(shape_plus_plot[rv$links[i, ], 1], shape_plus_plot[rv$links[i, ], 2], col = "#3498db", lwd = 2, lty = 2)
            }
          }
          points(ref_plot[,1], ref_plot[,2], pch = 21, bg = "#2c3e50", cex = 1.5)
          points(shape_minus_plot[,1], shape_minus_plot[,2], pch = 21, bg = "#e74c3c", cex = 1.2)
          points(shape_plus_plot[,1], shape_plus_plot[,2], pch = 21, bg = "#3498db", cex = 1.2)
          legend("topright", legend = c("Mean", "PC1 = -0.05", "PC1 = +0.05"),
                 col = c("#95a5a6", "#e74c3c", "#3498db"), lwd = 2, lty = c(1, 2, 2), bty = "n")
        }, error = function(e) NULL)
        dev.off()
        files_to_zip <- c(files_to_zip, deform_file)
      }

      # Create zip file
      zip(file, files_to_zip, flags = "-j")
    }
  )

  # =========================================================
  # Save Project Handler
  # =========================================================
  output$save_project <- downloadHandler(
    filename = function() {
      project_name <- gsub("[^a-zA-Z0-9_-]", "_", input$project_name)
      if (nchar(project_name) == 0) project_name <- "Morphostat_Project"
      paste0(project_name, ".rds")
    },
    content = function(file) {
      # Collect all input values to save
      all_inputs <- reactiveValuesToList(input)
      
      # Collect all reactive values (analysis results)
      all_rv <- reactiveValuesToList(rv)
      
      # Create project object with all settings and data
      project_data <- list(
        project_name = input$project_name,
        inputs = all_inputs,
        reactive_values = all_rv,
        save_date = Sys.time(),
        morphostat_version = "1.0.0"
      )
      
      # Save as RDS file
      saveRDS(project_data, file)
    }
  )

  # =========================================================
  # Load Project Observer
  # =========================================================
  observeEvent(input$project_file, {
    req(input$project_file)
    req(input$input_type == "morphostat_project")
    
    withProgress(message = "Loading project...", value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Reading project file...")
        project_data <- readRDS(input$project_file$datapath)
        
        # Validate project file
        if (!is.list(project_data) || is.null(project_data$inputs) || is.null(project_data$reactive_values)) {
          showNotification("Invalid MorphoStat project file.", type = "error")
          return()
        }
        
        incProgress(0.4, detail = "Restoring analysis data...")
        # Restore reactive values (analysis results)
        for (name in names(project_data$reactive_values)) {
          if (name %in% names(rv)) {
            rv[[name]] <- project_data$reactive_values[[name]]
          }
        }
        
        incProgress(0.5, detail = "Restoring settings...")
        # Restore input values
        saved_inputs <- project_data$inputs
        
        # Helper functions to safely update inputs with proper named parameters
        safe_update_text <- function(input_id, value) {
          if (!is.null(value)) {
            tryCatch(updateTextInput(session, input_id, value = value), error = function(e) NULL)
          }
        }
        safe_update_textarea <- function(input_id, value) {
          if (!is.null(value)) {
            tryCatch(updateTextAreaInput(session, input_id, value = value), error = function(e) NULL)
          }
        }
        safe_update_slider <- function(input_id, value) {
          if (!is.null(value)) {
            tryCatch(updateSliderInput(session, input_id, value = value), error = function(e) NULL)
          }
        }
        safe_update_numeric <- function(input_id, value) {
          if (!is.null(value)) {
            tryCatch(updateNumericInput(session, input_id, value = value), error = function(e) NULL)
          }
        }
        safe_update_checkbox <- function(input_id, value) {
          if (!is.null(value)) {
            tryCatch(updateCheckboxInput(session, input_id, value = value), error = function(e) NULL)
          }
        }
        safe_update_select <- function(input_id, selected) {
          if (!is.null(selected)) {
            tryCatch(updateSelectInput(session, input_id, selected = selected), error = function(e) NULL)
          }
        }
        safe_update_radio <- function(input_id, selected) {
          if (!is.null(selected)) {
            tryCatch(updateRadioButtons(session, input_id, selected = selected), error = function(e) NULL)
          }
        }
        safe_update_colour <- function(input_id, value) {
          if (!is.null(value)) {
            tryCatch(colourpicker::updateColourInput(session, input_id, value = value), error = function(e) NULL)
          }
        }
        
        # ===== Project Settings =====
        safe_update_text("project_name", saved_inputs$project_name)
        safe_update_radio("dimension", saved_inputs$dimension)
        
        # ===== Analysis Settings =====
        safe_update_slider("n_perms", saved_inputs$n_perms)
        safe_update_slider("n_perms_pc95", saved_inputs$n_perms_pc95)
        safe_update_slider("allometry_perms", saved_inputs$allometry_perms)
        safe_update_slider("confidence", saved_inputs$confidence)
        safe_update_slider("pca_variance_threshold", saved_inputs$pca_variance_threshold)
        
        # ===== Links Input =====
        safe_update_textarea("links_input", saved_inputs$links_input)
        
        incProgress(0.6, detail = "Restoring Procrustes settings...")
        # ===== Procrustes Plot Settings =====
        safe_update_select("procrustes_view", saved_inputs$procrustes_view)
        safe_update_select("procrustes_pc", saved_inputs$procrustes_pc)
        safe_update_slider("procrustes_pc_value", saved_inputs$procrustes_pc_value)
        safe_update_checkbox("procrustes_flip_pc_axis", saved_inputs$procrustes_flip_pc_axis)
        safe_update_checkbox("procrustes_sync_pca_axis", saved_inputs$procrustes_sync_pca_axis)
        safe_update_select("procrustes_flip", saved_inputs$procrustes_flip)
        safe_update_select("procrustes_rotate", saved_inputs$procrustes_rotate)
        safe_update_select("procrustes_axes", saved_inputs$procrustes_axes)
        safe_update_checkbox("procrustes_show_wireframe", saved_inputs$procrustes_show_wireframe)
        safe_update_checkbox("procrustes_show_mean", saved_inputs$procrustes_show_mean)
        safe_update_checkbox("procrustes_show_landmarks", saved_inputs$procrustes_show_landmarks)
        safe_update_slider("procrustes_landmark_size", saved_inputs$procrustes_landmark_size)
        safe_update_colour("procrustes_landmark_color", saved_inputs$procrustes_landmark_color)
        safe_update_slider("procrustes_point_size", saved_inputs$procrustes_point_size)
        safe_update_slider("procrustes_line_width", saved_inputs$procrustes_line_width)
        safe_update_colour("procrustes_point_color", saved_inputs$procrustes_point_color)
        safe_update_colour("procrustes_wire_color", saved_inputs$procrustes_wire_color)
        safe_update_colour("procrustes_mean_color", saved_inputs$procrustes_mean_color)
        safe_update_numeric("procrustes_download_width", saved_inputs$procrustes_download_width)
        safe_update_numeric("procrustes_download_height", saved_inputs$procrustes_download_height)
        
        # ===== Outlier Detection Settings =====
        safe_update_slider("outlier_threshold", saved_inputs$outlier_threshold)
        safe_update_checkbox("outlier_show_labels", saved_inputs$outlier_show_labels)
        safe_update_checkbox("outlier_highlight", saved_inputs$outlier_highlight)
        safe_update_colour("outlier_point_color", saved_inputs$outlier_point_color)
        safe_update_colour("outlier_highlight_color", saved_inputs$outlier_highlight_color)
        safe_update_numeric("outlier_download_width", saved_inputs$outlier_download_width)
        safe_update_numeric("outlier_download_height", saved_inputs$outlier_download_height)
        
        incProgress(0.7, detail = "Restoring PCA settings...")
        # ===== PCA Plot Settings =====
        safe_update_text("pca_title", saved_inputs$pca_title)
        safe_update_select("pca_plot_type", saved_inputs$pca_plot_type)
        safe_update_checkbox("pca_show_labels", saved_inputs$pca_show_labels)
        safe_update_checkbox("pca_flip_x", saved_inputs$pca_flip_x)
        safe_update_checkbox("pca_flip_y", saved_inputs$pca_flip_y)
        safe_update_select("pca_spread_type", saved_inputs$pca_spread_type)
        safe_update_checkbox("pca_spread_customize", saved_inputs$pca_spread_customize)
        safe_update_slider("pca_ellipse_level", saved_inputs$pca_ellipse_level)
        safe_update_slider("pca_density_bins", saved_inputs$pca_density_bins)
        safe_update_checkbox("pca_spread_fill", saved_inputs$pca_spread_fill)
        safe_update_slider("pca_spread_fill_alpha", saved_inputs$pca_spread_fill_alpha)
        safe_update_checkbox("pca_spread_outline", saved_inputs$pca_spread_outline)
        safe_update_slider("pca_spread_line_width", saved_inputs$pca_spread_line_width)
        safe_update_checkbox("pca_spread_custom_colors", saved_inputs$pca_spread_custom_colors)
        safe_update_select("pc_x", saved_inputs$pc_x)
        safe_update_select("pc_y", saved_inputs$pc_y)
        safe_update_select("pc_z", saved_inputs$pc_z)
        safe_update_slider("pca_3d_point_size", saved_inputs$pca_3d_point_size)
        safe_update_checkbox("pca_bw_auto_shapes", saved_inputs$pca_bw_auto_shapes)
        safe_update_slider("pca_bw_point_size", saved_inputs$pca_bw_point_size)
        safe_update_colour("plot_bg", saved_inputs$plot_bg)
        safe_update_checkbox("use_custom_colors", saved_inputs$use_custom_colors)
        safe_update_numeric("pca_download_width", saved_inputs$pca_download_width)
        safe_update_numeric("pca_download_height", saved_inputs$pca_download_height)
        
        incProgress(0.75, detail = "Restoring Statistical Plot settings...")
        # ===== Centroid Size Plot Settings =====
        # First restore the group selection (order matters for X-axis)
        if (!is.null(saved_inputs$cs_groups_select) && !is.null(rv$groups)) {
          # Validate that saved groups still exist
          valid_groups <- intersect(saved_inputs$cs_groups_select, levels(rv$groups))
          if (length(valid_groups) > 0) {
            updateSelectInput(session, "cs_groups_select", 
                              choices = levels(rv$groups),
                              selected = valid_groups)
          }
        }
        safe_update_checkbox("cs_show_points", saved_inputs$cs_show_points)
        safe_update_checkbox("cs_show_mean", saved_inputs$cs_show_mean)
        safe_update_colour("cs_outline_color", saved_inputs$cs_outline_color)
        safe_update_slider("cs_title_size", saved_inputs$cs_title_size)
        safe_update_slider("cs_axis_title_size", saved_inputs$cs_axis_title_size)
        safe_update_slider("cs_axis_text_size", saved_inputs$cs_axis_text_size)
        
        # ===== Allometry Plot Settings =====
        safe_update_checkbox("allo_flip_x", saved_inputs$allo_flip_x)
        safe_update_checkbox("allo_flip_y", saved_inputs$allo_flip_y)
        safe_update_checkbox("allo_sync_pca_axis", saved_inputs$allo_sync_pca_axis)
        safe_update_checkbox("allo_show_ci", saved_inputs$allo_show_ci)
        safe_update_checkbox("allo_show_groups", saved_inputs$allo_show_groups)
        safe_update_slider("allo_point_size", saved_inputs$allo_point_size)
        safe_update_slider("allo_line_width", saved_inputs$allo_line_width)
        safe_update_colour("allo_single_color", saved_inputs$allo_single_color)
        safe_update_colour("allo_line_color", saved_inputs$allo_line_color)
        safe_update_slider("allo_title_size", saved_inputs$allo_title_size)
        safe_update_slider("allo_axis_title_size", saved_inputs$allo_axis_title_size)
        safe_update_slider("allo_axis_text_size", saved_inputs$allo_axis_text_size)
        
        # ===== Vector Plot Settings =====
        safe_update_slider("vector_scale", saved_inputs$vector_scale)
        safe_update_colour("vector_color", saved_inputs$vector_color)
        safe_update_slider("vector_width", saved_inputs$vector_width)
        safe_update_slider("vector_title_size", saved_inputs$vector_title_size)
        safe_update_slider("vector_axis_title_size", saved_inputs$vector_axis_title_size)
        safe_update_slider("vector_axis_text_size", saved_inputs$vector_axis_text_size)
        
        # ===== Dispersion Plot Settings =====
        safe_update_checkbox("disp_show_points", saved_inputs$disp_show_points)
        safe_update_checkbox("disp_show_mean", saved_inputs$disp_show_mean)
        safe_update_checkbox("disp_show_ellipse", saved_inputs$disp_show_ellipse)
        safe_update_slider("disp_ellipse_level", saved_inputs$disp_ellipse_level)
        safe_update_checkbox("disp_show_centroids", saved_inputs$disp_show_centroids)
        safe_update_slider("disp_point_size", saved_inputs$disp_point_size)
        safe_update_colour("disp_outline_color", saved_inputs$disp_outline_color)
        safe_update_slider("disp_title_size", saved_inputs$disp_title_size)
        safe_update_slider("disp_axis_title_size", saved_inputs$disp_axis_title_size)
        safe_update_slider("disp_axis_text_size", saved_inputs$disp_axis_text_size)
        
        # ===== Eigenvalue Plot Settings =====
        safe_update_slider("eigen_n_components", saved_inputs$eigen_n_components)
        safe_update_checkbox("eigen_show_cumulative", saved_inputs$eigen_show_cumulative)
        safe_update_colour("eigen_bar_color", saved_inputs$eigen_bar_color)
        safe_update_colour("eigen_line_color", saved_inputs$eigen_line_color)
        safe_update_slider("eigen_title_size", saved_inputs$eigen_title_size)
        safe_update_slider("eigen_axis_title_size", saved_inputs$eigen_axis_title_size)
        safe_update_slider("eigen_axis_text_size", saved_inputs$eigen_axis_text_size)
        
        safe_update_numeric("stat_download_width", saved_inputs$stat_download_width)
        safe_update_numeric("stat_download_height", saved_inputs$stat_download_height)
        
        incProgress(0.85, detail = "Restoring Shape Visualization settings...")
        # ===== Deformation Grid Settings =====
        safe_update_select("deform_pc", saved_inputs$deform_pc)
        safe_update_slider("deform_magnitude", saved_inputs$deform_magnitude)
        safe_update_numeric("deform_pc_min", saved_inputs$deform_pc_min)
        safe_update_numeric("deform_pc_max", saved_inputs$deform_pc_max)
        safe_update_checkbox("deform_flip_pc_axis", saved_inputs$deform_flip_pc_axis)
        safe_update_checkbox("deform_sync_pca_axis", saved_inputs$deform_sync_pca_axis)
        safe_update_select("deform_flip", saved_inputs$deform_flip)
        safe_update_select("deform_rotate", saved_inputs$deform_rotate)
        safe_update_select("deform_axes", saved_inputs$deform_axes)
        safe_update_slider("deform_point_size", saved_inputs$deform_point_size)
        safe_update_checkbox("deform_show_wireframe", saved_inputs$deform_show_wireframe)
        safe_update_checkbox("deform_show_mean", saved_inputs$deform_show_mean)
        safe_update_checkbox("deform_show_landmarks", saved_inputs$deform_show_landmarks)
        safe_update_slider("deform_landmark_size", saved_inputs$deform_landmark_size)
        safe_update_colour("deform_landmark_color", saved_inputs$deform_landmark_color)
        safe_update_colour("deform_mean_color", saved_inputs$deform_mean_color)
        safe_update_colour("deform_minus_color", saved_inputs$deform_minus_color)
        safe_update_colour("deform_plus_color", saved_inputs$deform_plus_color)
        safe_update_colour("deform_point_color", saved_inputs$deform_point_color)
        safe_update_slider("deform_line_width", saved_inputs$deform_line_width)
        safe_update_numeric("deform_download_width", saved_inputs$deform_download_width)
        safe_update_numeric("deform_download_height", saved_inputs$deform_download_height)
        
        # ===== Displacement Vector Settings =====
        safe_update_select("disp_pc", saved_inputs$disp_pc)
        safe_update_numeric("disp_pc_value", saved_inputs$disp_pc_value)
        safe_update_slider("disp_mag", saved_inputs$disp_mag)
        safe_update_checkbox("disp_flip_pc_axis", saved_inputs$disp_flip_pc_axis)
        safe_update_checkbox("disp_sync_pca_axis", saved_inputs$disp_sync_pca_axis)
        safe_update_colour("disp_color", saved_inputs$disp_color)
        safe_update_numeric("disp_download_width", saved_inputs$disp_download_width)
        safe_update_numeric("disp_download_height", saved_inputs$disp_download_height)
        
        # ===== Wireframe Settings =====
        safe_update_select("wire_viz_type", saved_inputs$wire_viz_type)
        safe_update_select("wire_view", saved_inputs$wire_view)
        safe_update_select("wire_pc", saved_inputs$wire_pc)
        safe_update_numeric("wire_pc_min", saved_inputs$wire_pc_min)
        safe_update_numeric("wire_pc_max", saved_inputs$wire_pc_max)
        safe_update_checkbox("wire_flip_pc_axis", saved_inputs$wire_flip_pc_axis)
        safe_update_checkbox("wire_sync_pca_axis", saved_inputs$wire_sync_pca_axis)
        safe_update_select("wire_flip", saved_inputs$wire_flip)
        safe_update_select("wire_rotate", saved_inputs$wire_rotate)
        safe_update_slider("wire_thickness", saved_inputs$wire_thickness)
        safe_update_slider("wire_mag", saved_inputs$wire_mag)
        safe_update_checkbox("wire_show_wireframe", saved_inputs$wire_show_wireframe)
        safe_update_checkbox("show_average", saved_inputs$show_average)
        safe_update_checkbox("wire_show_landmarks", saved_inputs$wire_show_landmarks)
        safe_update_slider("wire_landmark_size", saved_inputs$wire_landmark_size)
        safe_update_colour("wire_landmark_color", saved_inputs$wire_landmark_color)
        safe_update_colour("wire_color_minus", saved_inputs$wire_color_minus)
        safe_update_colour("wire_color_plus", saved_inputs$wire_color_plus)
        safe_update_colour("wire_color_avg", saved_inputs$wire_color_avg)
        safe_update_colour("point_color", saved_inputs$point_color)
        
        incProgress(0.95, detail = "Restoring custom colors...")
        # ===== Restore Dynamic Group Colors (delayed to ensure groups are loaded) =====
        # Use shinyjs::delay or just schedule with invalidateLater pattern
        # We need to restore custom group colors after groups are set up
        if (!is.null(rv$groups) && !is.null(saved_inputs$use_custom_colors) && saved_inputs$use_custom_colors) {
          group_levels <- levels(rv$groups)
          for (i in seq_along(group_levels)) {
            color_id <- paste0("group_color_", i)
            if (!is.null(saved_inputs[[color_id]])) {
              tryCatch(
                colourpicker::updateColourInput(session, color_id, value = saved_inputs[[color_id]]),
                error = function(e) NULL
              )
            }
          }
        }
        
        # Restore Centroid Size group colors
        if (!is.null(saved_inputs$cs_groups_select) && length(saved_inputs$cs_groups_select) > 0) {
          for (i in seq_along(saved_inputs$cs_groups_select)) {
            color_id <- paste0("cs_group_color_", i)
            if (!is.null(saved_inputs[[color_id]])) {
              tryCatch(
                colourpicker::updateColourInput(session, color_id, value = saved_inputs[[color_id]]),
                error = function(e) NULL
              )
            }
          }
        }
        
        # Restore spread custom colors
        if (!is.null(rv$groups) && !is.null(saved_inputs$pca_spread_custom_colors) && saved_inputs$pca_spread_custom_colors) {
          group_levels <- levels(rv$groups)
          for (i in seq_along(group_levels)) {
            color_id <- paste0("pca_spread_color_", i)
            if (!is.null(saved_inputs[[color_id]])) {
              tryCatch(
                colourpicker::updateColourInput(session, color_id, value = saved_inputs[[color_id]]),
                error = function(e) NULL
              )
            }
          }
        }
        
        incProgress(1, detail = "Project loaded successfully!")
        
        # Show success notification
        showNotification(
          paste0("Project '", project_data$project_name, "' loaded successfully! ",
                 "Saved on: ", format(project_data$save_date, "%Y-%m-%d %H:%M")),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(paste("Error loading project:", e$message), type = "error")
      })
    })
  })
}

# =========================================================
# Run App
# =========================================================
shinyApp(ui = ui, server = server)
