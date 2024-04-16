# nav_panel() content page with a collapsible file input/data filtering sidebar and two stacked card() display areas
filter_layout = function(id, top = card("top card"), bottom = card("bottom card"), heights = c(2, 1)) {
    ns = NS(id)

    layout_sidebar(padding = "0px", gap = "0px", fill = T, fillable = T,
        sidebar = sidebar(width = "300px",
            accordion(id = ns("sidebar"), open = T, multiple = T,

                accordion_panel(title = "Dataset", value = "data", icon = bs_icon("file-spreadsheet"),
                    fileInput(ns("files"), NULL, multiple = T, accept = c(".xlsx")),
                    uiOutput(ns("variables"))
                ),

                accordion_panel(title = "Filter", value = "filter", icon = bs_icon("funnel"),
                    uiOutput(ns("filters"))
                ),

                # accordion_panel(title = "ANOVA", value = "analysis", icon = bs_icon("hammer"),
                #     virtualSelectInput_(ns("facet"), label = "Group by...", choices = c(), selected = c())
                # )
            )
        ),

        layout_columns(col_widths = 12, row_heights = heights,  gap = "0px",
            top,
            bottom
        )
    )
}
