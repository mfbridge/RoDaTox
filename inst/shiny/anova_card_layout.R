anova_card_layout = function(id) {
    ns = NS(id)

    layout_columns(col_widths = c(6, 6), gap = "0px",
        navset_card_tab(full_screen = T,

            nav_panel(title = "Estimates and CIs", gt_output(ns("table"))),
            nav_panel(title = "Model Summary", verbatimTextOutput(ns("table-full")))
        ),

        navset_card_tab(full_screen = T,
            title = tagList(
                popover(
                    trigger = actionLink(ns("options"), bs_icon("gear")),
                    div(style="line-height: 1rem;",
                        prettyCheckboxGroup(ns("toggle"), NULL, choices = c("Detrend QQ plot (reduces visual bias)"="detrend"), selected = c())
                    )
                )
            ),

            nav_panel(title = "QQ Plot", icon = bs_icon("graph-up"),
                plotOutput(ns("qq"))
            ),
            nav_panel(title = "Residuals", icon = bs_icon("bar-chart"),
                plotOutput(ns("res"))
            )
        )
    )
}
