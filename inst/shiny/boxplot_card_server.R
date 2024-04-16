boxplot_card_server = function(id, data, value.var, time.var, dose.var, id.var, facet.var = "") {
    moduleServer(id, function(input, output, session) {
        ns = session$ns

        # re-plot whenever the 'subset' element of our dataset changes (filter options change)
        observeEvent(data$subset, {
            req(nrow(data$subset) > 0)

            output$figure = renderPlotly({
                dt = data.table(
                    x = data$subset[, get(data$variables[[time.var]])],
                    y = data$subset[, get(data$variables[[value.var]])],
                    c = data$subset[, get(data$variables[[dose.var]])],
                    i = data$subset[, get(data$variables[[id.var]])]
                )

                dt$y = as.numeric(dt$y)
                dt$c = factor(as.numeric(dt$c), levels = sort(unique(as.numeric(dt$c))), ordered = T)

                phase.day.list = stringi::stri_match_all(dt$x, regex = "(?<phase>[A-Za-z]+)(?<day>[0-9]+)")
                dt$x = factor(dt$x, levels = unlist(Map(\(x) x[1, 1], unique(phase.day.list))), ordered = T) # phase day
                dt$p = factor(unlist(Map(\(x) x[1, 2], phase.day.list)), levels = unique(unlist(Map(\(x) x[1, 2], unique(phase.day.list)))), ordered = T) # phase type
                #browser()

                .p = ggplot(dt, aes(x = x, y = y, color = c, group = c))

                # select a plot type
                if (input$type %in% c("box1", "box2", "box3")) {
                    .p = .p + geom_boxplot(position = position_dodge(), outlier.shape = NA)
                } else if (input$type %in% c("line")) {
                    .p = .p + geom_line(aes(group = i), alpha = 0.75)
                }

                if ("show_mean" %in% input$toggle | input$type == "line2") {
                    .p = .p + stat_summary(geom = "line", fun = mean, linewidth = 1.0, linetype = "solid")
                }

                if (input$palette == "grays") {
                    .p = .p + scale_color_grey(start = 0.0, end = 0.75)
                } else {
                    .p = .p + scale_color_viridis_d(option = input$palette, begin = 0.1, end = 0.9)
                }

                if (facet.var != "") {
                    .p = .p + ggh4x::facet_grid2(. ~ p, drop = T, scales = "free_x", space = "free_x")
                }

                p = ggplotly({
                    .p +
                    theme_minimal() +
                    theme(legend.position = "bottom") +
                    labs(y = value.var, fill = dose.var, x = time.var, color = dose.var)
                }) %>%
                    style(fillcolor = 'NA', boxmean = T, pointpos = 0, marker = list(symbol = 'square', opacity = 0.9, size = 3, line = list(width = 0.5)))

                if (input$type == 'box1') p = p %>% style(boxpoints = 'all')
                if (input$type == 'box3') p = p %>% style(boxpoints = 'none')

                p = p %>% layout(boxmode = 'group', boxgap = 0.1, boxgroupgap = 0.2, legend = list(orientation = 'h')) %>% plotly_build()

                # have to hide points from plotly boxplots because they are included by default
                if (input$type == 'box3') {
                    p$x$data = lapply(p$x$data, \(x){ x$marker$outliercolor = 'rgba(0,0,0,0)'; return(x) })
                    p$x$data <- lapply(p$x$data, \(x){ x$marker$line$color = 'rgba(0,0,0,0)'; x$marker$line$width = 0; x$marker$size = 0; x$marker$color = 'rgba(0,0,0,0)'; return(x) })
                }

                toWebGL(p)
            })

            output$table = DT::renderDT({
                DT::datatable(data$subset, filter = "top")
            })
        })
    })
}
