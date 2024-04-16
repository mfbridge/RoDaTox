
anova_card_server = function(id, data, value.var, time.var, dose.var) {
    moduleServer(id, function(input, output, session) {
        ns = session$ns

        observeEvent(data$subset, {
            req(nrow(data$subset) > 0)

            dt = data$subset[, c(value.var, data$variables[[time.var]], data$variables[[dose.var]]), with = F]
            dt[, value := as.numeric(get(value.var))]
            dt[, dose := as.numeric(get(data$variables[[dose.var]]))]
            dt[, time := as.character(get(data$variables[[time.var]]))]
            dt[, phase := unlist(Map(\(x) x[1, "phase"], stringi::stri_match_all(dt[, time], regex = "(?<phase>[A-Za-z]+)(?<day>[0-9]+)")))]
            dt[, day := as.numeric(unlist(Map(\(x) x[1, "day"], stringi::stri_match_all(dt[, time], regex = "(?<phase>[A-Za-z]+)(?<day>[0-9]+)"))))]

            output$`table-full` = renderPrint({
                for (p in unique(dt$phase)) {
                    print(p)
                    m = lm(value ~ dose * day, data = dt[phase == p, ])
                    print(summary(m))
                    print(car::Anova(m, type = "III"))
                    cat('\n')
                }
            })

            output$table =  render_gt({
                ml = list()
                for (p in unique(dt$phase)) {
                    ml[[p]] = lm(value ~ dose * day, data = dt[phase == p, ])
                }
                t = modelsummary(ml, output = "gt", stars = c('*' = 0.05, '**' = 0.01), statistic = NULL, estimate = "{estimate} [{conf.low}, {conf.high}] {stars}", shape = model ~ term, notes = "* p <= 0.05, ** p <= 0.01")

                t |> gt::tab_options(table.font.size = "75%")
            })

            output$qq = renderPlot({
                ggplot(dt, aes(sample = value, color = phase, group = phase, fill = phase)) +
                    qqplotr::stat_qq_band(bandType = "pointwise", alpha = 0.1, detrend = "detrend" %in% input$toggle) +
                    qqplotr::stat_qq_line(alpha = 0.5, detrend = "detrend" %in% input$toggle) +
                    qqplotr::stat_qq_point(size = 1, alpha = 0.25, detrend = "detrend" %in% input$toggle) +
                    facet_grid(. ~ phase) +
                    scale_color_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
                    scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
                    theme_minimal()
            })

            output$res = renderPlot({
                residual = data.table()
                for (p in unique(dt$phase)) {
                    model = lm(value ~ dose * day, data = dt[phase == p, ])
                    residual = rbindlist(list(residual, data.table(r = residuals(model), phase = p)))
                }
                ggplot(residual, aes(x = r, color = phase, group = phase, fill = phase)) +
                    geom_histogram(position = "identity", alpha = 0.1, bins = 40) +
                    facet_grid(. ~ phase) +
                    scale_color_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
                    scale_fill_viridis_d(option = "turbo", begin = 0.1, end = 0.9) +
                    theme_minimal()
            })
        })
    })
}
