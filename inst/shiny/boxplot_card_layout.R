boxplot_card_layout = function(id) {
    ns = NS(id)

    navset_card_tab(full_screen = T,
        title = tagList(

            popover(
                trigger = actionLink(ns("options"), bs_icon("gear")),
                div(style="line-height: 1rem;",
                    prettyCheckboxGroup(ns("toggle"), NULL, choices = c("Show group mean trend lines"="show_mean"), selected = c("show_mean")),

                    virtualSelectInput_(ns("type"), NULL, choices = c("boxplots with points"="box1", "boxplots with outliers"="box2", "boxplots only"="box3", "lines"="line", "trend lines only"="line2"), selected = "box2"),

                    esquisse::palettePicker(ns("palette"), tags$b("Color Palette"), textColor = "#ffffff", selected = "turbo", choices = list(
                        "grays"=gray.colors(10, start = 0, end = 0.9, alpha = 1.0),
                        "cividis"=viridis::viridis_pal(option = "cividis")(10),
                        "inferno"=viridis::viridis_pal(option = "inferno")(10),
                        "magma"=viridis::viridis_pal(option = "magma")(10),
                        "mako"=viridis::viridis_pal(option = "mako")(10),
                        "plasma"=viridis::viridis_pal(option = "plasma")(10),
                        "rocket"=viridis::viridis_pal(option = "rocket")(10),
                        "turbo"=viridis::viridis_pal(option = "turbo")(10))
                    ),
                )
            )
        ),

        nav_panel(
            title = "Boxplots",
            icon = bs_icon("graph-up"),
            plotlyOutput(ns("figure"))
        ),

        nav_panel(
            title = "Statistics",
            icon = bs_icon("menu-button"),

            "TODO: Put more stats tables here"
        ),

        nav_panel(
            title = "Values",
            icon = bs_icon("table"),
            div(style = "font-size: 0.8rem;",
                DT::DTOutput(ns("table"))
            )
        )
    )
}
