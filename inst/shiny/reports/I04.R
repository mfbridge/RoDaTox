I04UI = function(id) {
    ns = NS(id)

    filter_layout(id = ns("filter"),
        top = boxplot_card_layout(ns("plot")),
        bottom = anova_card_layout(ns("anova"))
    )
}

I04Server = function(id) {
    data = reactiveValues()

    moduleServer(id, function(input, output, session) {
        ns = NS(id)

        filter_server(id = "filter", data = data,
            xlsx.key.sheet = "Key to Column Labels",
            xlsx.data.sheet = "Data",

            factors.numeric = c("Dose", "Animal ID"),
            factors.alpha = c("Litter ID", "Pregnancy Status"),
            factors.phase = c("Time"),

            value = c("Body Weight"),

            variables = list(
                `Body Weight` = "Body Weight",

                `Dose` = "Concentration",
                `Time` = "Observation Day",

                `Animal ID` = "Animal ID",
                `Litter` = "Litter ID",
                `Pregnancy Status` = "Pregnancy Status"
            ),

            facet = "Phase",

            fun = \(dt) {
                # do some custom processing of the dataset first

                dt[, `Phase` := unlist(Map(\(x) x[1, "phase"], stringi::stri_match_all(`Observation Day`, regex = "(?<phase>[A-Za-z]+)(?<day>[0-9]+)")))]

                #View(dt)
                return(dt)
            }
        )

        boxplot_card_server(id = "plot", data = data,
            value.var = "Body Weight",
            time.var = "Time",
            dose.var = "Dose",
            id.var = "Animal ID",
            facet.var = "Phase Type"
        )

        anova_card_server(id = "anova", data = data,
            value.var = "Body Weight",
            time.var = "Time",
            dose.var = "Dose"
        )
    })
}
