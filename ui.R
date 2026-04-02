library(shiny)
library(bslib)
library(plotly)
library(DT)

# nolint start: line_length_linter.
custom_js <- tags$script(HTML("
$(document).on('keypress', '#search_query', function(e) {
  if (e.which === 13) { $('#search_btn').click(); }
});
$(document).on('keypress', '#compare_search', function(e) {
  if (e.which === 13) { $('#compare_search_btn').click(); }
});
"))
# nolint end

custom_css <- tags$style(HTML(
  ".bslib-value-box .value-box-value {
    font-size: clamp(1.2rem, 2.5vw, 2rem);
    white-space: nowrap;
  }"
))

ui <- page_navbar(
  id = "main_nav",
  title = "cranExploreR",
  header = tagList(custom_js, custom_css),
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
            title = "Yesterday",
            value = textOutput("dl_day"),
            showcase = icon("calendar-day"),
            theme = "primary"
          ),
          value_box(
            title = "Past 7 Days",
            value = textOutput("dl_week"),
            showcase = icon("calendar-week"),
            theme = "info"
          ),
          value_box(
            title = "Past 30 Days",
            value = textOutput("dl_month"),
            showcase = icon("calendar"),
            theme = "success"
          ),
          value_box(
            title = "Past 365 Days",
            value = textOutput("dl_year"),
            showcase = icon("chart-line"),
            theme = "warning"
          )
        ),

        layout_columns(
          col_widths = c(8, 4),

          card(
            card_header(
              class = paste(
                "d-flex justify-content-between",
                "align-items-center"
              ),
              "Download Trend (Last 12 Months)",
              checkboxGroupInput(
                "trend_series",
                label = NULL,
                choices = c(
                  "Weekly" = "weekly",
                  "Cumulative" = "cumulative",
                  "Average" = "average"
                ),
                selected = "weekly",
                inline = TRUE
              )
            ),
            card_body(
              plotlyOutput(
                "download_trend_plot",
                height = "350px"
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
        style = "max-width: 800px;",
        h3("cranExploreR"),
        p(
          "An interactive dashboard for evaluating",
          "CRAN packages before you add them to",
          "your project. Choosing the right",
          "dependencies is one of the most",
          "consequential decisions in any R",
          "project \u2014 a package that loses its",
          "maintainer, stops getting updates, or",
          "has a shrinking user base can become a",
          "liability that is expensive to replace",
          "later."
        ),

        h5("Why Package Viability Matters"),
        tags$ul(
          tags$li(
            tags$strong("Production systems"),
            " \u2014 If your Shiny app or pipeline",
            "depends on a package that hasn't been",
            "updated in years, a breaking change in",
            "R or another dependency could leave you",
            "scrambling for a fix with no upstream",
            "support."
          ),
          tags$li(
            tags$strong("Reproducibility"),
            " \u2014 Packages with active maintenance",
            "are more likely to stay on CRAN,",
            "get timely bug fixes, and remain",
            "compatible with new R versions."
          ),
          tags$li(
            tags$strong("Team projects"),
            " \u2014 When onboarding contributors,",
            "widely-adopted packages with strong",
            "documentation reduce the learning",
            "curve. Niche or abandoned packages",
            "increase it."
          ),
          tags$li(
            tags$strong("CRAN submissions"),
            " \u2014 If you are building a package",
            "for CRAN, your dependencies are part",
            "of your submission. Depending on",
            "archived or fragile packages can",
            "delay acceptance."
          )
        ),

        h5("What to Look For"),
        p(
          "No single metric tells the whole story.",
          "Use the Explorer tab to build a",
          "complete picture:"
        ),
        tags$ul(
          tags$li(
            tags$strong("Download trend"),
            " \u2014 A steady or growing download",
            "curve suggests an active user base.",
            "A sharp decline may signal that",
            "users are migrating to an alternative."
          ),
          tags$li(
            tags$strong("Recency of updates"),
            " \u2014 Check when the last version was",
            "published. Some stable packages",
            "legitimately go years without updates,",
            "but a long gap combined with open",
            "issues is a red flag."
          ),
          tags$li(
            tags$strong("Reverse dependencies"),
            " \u2014 Packages depended on by many",
            "others have strong ecosystem",
            "incentives to stay maintained.",
            "A package with hundreds of reverse",
            "dependencies is unlikely to be",
            "abandoned quietly."
          ),
          tags$li(
            tags$strong("Version history"),
            " \u2014 A long, consistent release",
            "history signals maturity.",
            "A single release followed by silence",
            "may indicate an experiment that was",
            "never fully supported."
          ),
          tags$li(
            tags$strong("The Compare tab"),
            " \u2014 When choosing between packages",
            "that solve the same problem",
            "(e.g. dplyr vs data.table), compare",
            "them side-by-side on downloads,",
            "viability score, and release activity",
            "to make an informed decision."
          )
        ),

        h5("Viability Score"),
        p(
          "The viability score (0\u2013100)",
          "is a weighted composite that",
          "summarises five dimensions into a",
          "single number. It is a starting point",
          "for evaluation, not a definitive",
          "judgement \u2014 context always matters."
        ),
        tags$table(
          class = "table table-sm",
          tags$thead(
            tags$tr(
              tags$th("Factor"),
              tags$th("Weight"),
              tags$th("What it measures")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("Recency"),
              tags$td("30%"),
              tags$td(
                "How recently the package",
                "was updated on CRAN"
              )
            ),
            tags$tr(
              tags$td("Download momentum"),
              tags$td("25%"),
              tags$td(
                "Whether downloads are growing,",
                "stable, or declining"
              )
            ),
            tags$tr(
              tags$td("Download volume"),
              tags$td("20%"),
              tags$td(
                "Absolute number of monthly",
                "downloads"
              )
            ),
            tags$tr(
              tags$td("Ecosystem adoption"),
              tags$td("15%"),
              tags$td(
                "Number of other packages that",
                "depend on it"
              )
            ),
            tags$tr(
              tags$td("Maturity"),
              tags$td("10%"),
              tags$td(
                "Total number of releases",
                "over the package's lifetime"
              )
            )
          )
        ),

        h5("Data Sources"),
        tags$ul(
          tags$li(
            tags$a(
              href = "https://crandb.r-pkg.org",
              "crandb"
            ),
            " \u2014 Package metadata",
            "and version history"
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
        hr(),
        p(
          class = "text-muted",
          "Built with R, Shiny, and bslib."
        )
      )
    )
  )
)
