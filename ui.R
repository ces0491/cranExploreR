library(shiny)
library(bslib)
library(plotly)
library(DT)

ui <- page_navbar(
  id = "main_nav",
  title = "cranExploreR",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50",
    "navbar-bg" = "#2c3e50"
  ),
  fillable = FALSE,

  nav_panel(
    title = "Explorer",
    icon = icon("magnifying-glass"),

    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        title = "Search CRAN",
        textInput(
          "search_query", label = NULL,
          placeholder = "e.g. dplyr, ggplot2"
        ),
        actionButton(
          "search_btn", "Search",
          class = "btn-primary w-100 mb-3",
          icon = icon("magnifying-glass")
        ),
        hr(),
        uiOutput("search_results_ui")
      ),

      # Main content
      conditionalPanel(
        condition = "output.package_loaded",

        # Package header
        uiOutput("package_header"),

        layout_columns(
          col_widths = c(3, 3, 3, 3),
          fill = FALSE,
          value_box(
            title = "Last Day",
            value = textOutput("dl_day"),
            showcase = icon("calendar-day"),
            theme = "primary"
          ),
          value_box(
            title = "Last Week",
            value = textOutput("dl_week"),
            showcase = icon("calendar-week"),
            theme = "info"
          ),
          value_box(
            title = "Last Month",
            value = textOutput("dl_month"),
            showcase = icon("calendar"),
            theme = "success"
          ),
          value_box(
            title = "Last Year",
            value = textOutput("dl_year"),
            showcase = icon("chart-line"),
            theme = "warning"
          )
        ),

        layout_columns(
          col_widths = c(8, 4),

          card(
            card_header(
              "Download Trend (Last 12 Months)"
            ),
            card_body(
              plotlyOutput(
                "download_trend_plot", height = "350px"
              )
            )
          ),

          card(
            card_header("Viability Score"),
            card_body(
              uiOutput("health_score_display"),
              hr(),
              uiOutput("health_details")
            )
          )
        ),

        layout_columns(
          col_widths = c(6, 6),

          card(
            card_header("Package Metadata"),
            card_body(
              uiOutput("metadata_table")
            )
          ),

          card(
            card_header("Dependencies"),
            card_body(
              navset_underline(
                nav_panel(
                  "Imports",
                  DTOutput("imports_table")
                ),
                nav_panel(
                  "Depends",
                  DTOutput("depends_table")
                ),
                nav_panel(
                  "Suggests",
                  DTOutput("suggests_table")
                )
              )
            )
          )
        ),

        layout_columns(
          col_widths = c(6, 6),

          card(
            card_header("Version History"),
            card_body(
              DTOutput("version_table")
            )
          ),

          card(
            card_header("Reverse Dependencies"),
            card_body(
              uiOutput("rev_deps_summary")
            )
          )
        )
      ),

      # Welcome state when no package is selected
      conditionalPanel(
        condition = "!output.package_loaded",
        div(
          class = "text-center py-5",
          icon("box-open", class = "fa-4x text-muted mb-3"),
          h3(
            "Search for a CRAN package",
            class = "text-muted"
          ),
          p(
            "Enter a package name or keyword",
            "in the sidebar to get started.",
            class = "text-muted"
          )
        )
      )
    )
  ),

  nav_panel(
    title = "Compare",
    icon = icon("scale-balanced"),

    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        title = "Compare Packages",
        textInput(
          "compare_search", label = NULL,
          placeholder = "Search for a package..."
        ),
        actionButton(
          "compare_search_btn", "Find",
          class = "btn-outline-secondary w-100 mb-2",
          icon = icon("magnifying-glass")
        ),
        uiOutput("compare_search_results_ui"),
        hr(),
        textInput(
          "compare_pkg1", "Package 1",
          placeholder = "e.g. dplyr"
        ),
        textInput(
          "compare_pkg2", "Package 2",
          placeholder = "e.g. data.table"
        ),
        textInput(
          "compare_pkg3", "Package 3 (optional)",
          placeholder = "e.g. dtplyr"
        ),
        actionButton(
          "compare_btn", "Compare",
          class = "btn-primary w-100",
          icon = icon("scale-balanced")
        )
      ),

      conditionalPanel(
        condition = "output.comparison_loaded",

        card(
          card_header(
            "Download Comparison (Last 12 Months)"
          ),
          card_body(
            plotlyOutput(
              "compare_trend_plot", height = "400px"
            )
          )
        ),

        card(
          card_header("Side-by-Side Comparison"),
          card_body(
            DTOutput("compare_table")
          )
        )
      ),

      conditionalPanel(
        condition = "!output.comparison_loaded",
        div(
          class = "text-center py-5",
          icon(
            "scale-balanced",
            class = "fa-4x text-muted mb-3"
          ),
          h3(
            "Compare CRAN packages",
            class = "text-muted"
          ),
          p(
            "Enter two or three package names to",
            "compare their download stats",
            "and metadata.",
            class = "text-muted"
          )
        )
      )
    )
  ),

  nav_panel(
    title = "Browse",
    icon = icon("compass"),

    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        title = "Browse Packages",

        actionButton(
          "browse_popular_btn", "Popular Packages",
          icon = icon("fire"),
          class = "btn-outline-warning w-100 mb-3"
        ),
        hr(),

        h6("Categories"),
        selectInput(
          "browse_category", NULL,
          choices = c(
            "Select a category..." = "",
            names(BROWSE_CATEGORIES)
          )
        ),
        hr(),

        h6("Alphabetical"),
        tags$div(
          class = "d-flex flex-wrap gap-1",
          lapply(LETTERS, function(l) {
            actionButton(
              paste0("az_", l), l,
              class = paste(
                "btn btn-outline-secondary",
                "btn-sm px-2 py-1"
              )
            )
          })
        )
      ),

      conditionalPanel(
        condition = "output.browse_has_results",
        card(
          card_header(
            class = paste(
              "d-flex justify-content-between",
              "align-items-center"
            ),
            textOutput("browse_results_header"),
            tags$small(
              "Click a row to explore",
              class = "text-muted"
            )
          ),
          card_body(
            DTOutput("browse_table")
          )
        )
      ),

      conditionalPanel(
        condition = "!output.browse_has_results",
        div(
          class = "text-center py-5",
          icon(
            "compass",
            class = "fa-4x text-muted mb-3"
          ),
          h3(
            "Browse CRAN packages",
            class = "text-muted"
          ),
          p(
            "View popular packages, pick a category,",
            "or browse alphabetically.",
            class = "text-muted"
          )
        )
      )
    )
  ),

  nav_panel(
    title = "About",
    icon = icon("circle-info"),
    card(
      card_body(
        class = "mx-auto",
        style = "max-width: 700px;",
        h3("cranExploreR"),
        p(
          "A Shiny app for exploring CRAN package",
          "viability. Search for any R package to",
          "see download statistics, maintenance",
          "health, dependency information,",
          "and version history."
        ),
        h5("Data Sources"),
        tags$ul(
          tags$li(
            tags$a(
              href = "https://crandb.r-pkg.org",
              "crandb"
            ),
            " \u2014 Package metadata and version history"
          ),
          tags$li(
            tags$a(
              href = "https://cranlogs.r-pkg.org",
              "cranlogs"
            ),
            " \u2014 Download statistics"
          ),
          tags$li(
            tags$a(
              href = "https://search.r-pkg.org",
              "R package search"
            ),
            " \u2014 Full-text package search"
          )
        ),
        h5("Viability Score"),
        p(
          "The viability score (0\u2013100)",
          "is a composite indicator based on:"
        ),
        tags$ul(
          tags$li(
            tags$strong("Recency"),
            " \u2014 How recently updated (30%)"
          ),
          tags$li(
            tags$strong("Download momentum"),
            " \u2014 Growing or declining (25%)"
          ),
          tags$li(
            tags$strong("Download volume"),
            " \u2014 Monthly downloads (20%)"
          ),
          tags$li(
            tags$strong("Ecosystem adoption"),
            " \u2014 Reverse dependencies (15%)"
          ),
          tags$li(
            tags$strong("Maturity"),
            " \u2014 Releases over time (10%)"
          )
        ),
        hr(),
        p(
          class = "text-muted",
          "Built with R, Shiny, and bslib."
        )
      )
    )
  )
)
