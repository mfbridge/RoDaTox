filter_server = function(id, data,
    xlsx.key.sheet = "Key to Column Labels",
    xlsx.data.sheet = "Data",

    factors.numeric = c("Dose"),
    factors.alpha = c(),
    factors.phase = c(),

    value = "Body Weight",

    variables = list(
        `Dose` = "Concentration"
    ),

    facets = "Phase Type",

    fun = \(dt) { return(dt) }
) {
    moduleServer(id, function(input, output, session) {
        ns = session$ns

        output$variables = renderUI({
            var.ui = tagList()

            for (v in 1:length(variables)) {
                var.label = names(variables)[[v]]
                var.name = variables[[v]]

                if (var.name != value) {

                    # update variable lists
                    var.id = sprintf("var-%d", v)
                        var.ui = tagAppendChild(var.ui,
                            virtualSelectInput_(ns(var.id), var.label, choices = c(), selected = c(), multiple = F, search = T)
                        )
                }
            }

            var.ui
        })

        output$filters = renderUI({
            req(data$dataset)

            filter.ui = tagList()

            for (v in 1:length(variables)) {
                var.label = names(variables)[[v]]
                var.name = variables[[v]]

                if (var.name != value) {

                    filter.id = sprintf("value-%d", v)

                    filter.ui = tagAppendChild(filter.ui,
                        virtualSelectInput_(ns(filter.id), var.label,
                            choices = c(),
                            selected = c(), multiple = T, search = T, allOptionsSelectedText = sprintf("All %s", textclean::make_plural(tolower(var.label)))
                        )
                    )
                }
            }

            filter.ui
        })

        # observers for variable dropdown selections
        var.obs = lapply(1:length(variables), \(x) {
            var.id = sprintf("var-%d", x)
            val.id = sprintf("value-%d", x)
            name = names(variables)[variables == variables[[x]]][[1]]

            observeEvent(input[[var.id]], {
                req(data$dataset)
                req(nrow(data$dataset) > 0)
                req(data$columns)

                data.var = input[[var.id]]
                req(data.var %in% data$columns)

                # also apply various sortings/groupings for dropdown elements

                var.values = data$dataset[, unique(get(data.var))]
                var.selected = var.values

                if (name %in% factors.numeric) {
                    var.values = sort(as.numeric(var.values))

                } else if (name %in% factors.alpha) {
                    var.values = sort(as.character(var.values))

                } else if (name %in% factors.phase) {
                    phase.days = unique(var.values)
                    phase.day.list = stringi::stri_match_all(phase.days, regex = "(?<phase>[A-Za-z]+)(?<day>[0-9]+)")
                    phase.types = unique(unlist(Map(\(x) x[1, "phase"], phase.day.list)))

                    phase.list = list()
                    for (p in phase.types) {
                        phase.list[[p]] = Filter(\(x) startsWith(x, p), unique(phase.days))
                    }

                    var.values = phase.list
                    var.selected = phase.days
                }

                if (data.var %in% data$columns) {
                    updateVirtualSelect(val.id, choices = var.values, selected = var.selected)
                }
            })
        })

        # observers for value dropdowns
        val.obs = lapply(sprintf("value-%d", 1:length(variables)), \(x) {
            observeEvent(input[[x]], {
                req(data$dataset)
                req(data$columns)

                subset = copy(data$dataset)

                for (v in 1:length(variables)) {
                    if (variables[[v]] %in% data$columns & variables[[v]] != value) {
                        data.var.name = sprintf("var-%d", v)
                        data.var = input[[data.var.name]]

                        input.name = sprintf("value-%d", v)
                        values = input[[input.name]]

                        subset = subset[get(data.var) %in% values, ]
                    }
                }

                data$subset = subset

                cli::cli_alert_info(sprintf("subset contains %s out of %s rows", cli::col_green(nrow(data$subset)), cli::col_green(nrow(data$dataset))))
            })
        })

        # populate dropdowns on file selection
        observeEvent(input$files, {
            updateVirtualSelect("datasets", choices = input$files[, "name"], selected = input$files[, "name"])

            for (f in 1:nrow(input$files)) {
                filename = input$files[f, "name"]
                path = input$files[f, "datapath"]
                filetype = input$files[f, "type"]

                if (filetype == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet") {
                    sheets = readxl::excel_sheets(path)

                    if (all(c(xlsx.key.sheet, xlsx.data.sheet) %in% sheets)) {

                        # maybe process this tab later?
                        data$metadata = as.data.table(readxl::read_excel(path = path, sheet = xlsx.key.sheet, trim_ws = T, na = c("", ".", "NA"), progress = T, col_types = c("text", "text", "skip", "skip")))

                        # raw data
                        data$dataset = as.data.table(readxl::read_excel(path = path, sheet = xlsx.data.sheet, trim_ws = T, na = c("", ".", "NA")))

                        # pass through user supplied processing function
                        data$dataset = fun(data$dataset)

                        # data columns
                        data$columns = colnames(data$dataset)

                        data$variables = variables

                        updateVirtualSelect("facet", choices = data$columns, selected = facets)

                        for (v in 1:length(variables)) {
                            var.label = names(variables)[[v]]
                            var.name = variables[[v]]

                            if (!(var.name %in% data$columns)) {
                                cli::cli_alert_warning(
                                    sprintf("defined variable for %s (%s) not found in selected file (contains %s)",
                                        cli::col_red(var.label), cli::col_br_red(var.name), cli::col_yellow(paste0(data$columns, collapse = ", "))))
                            } else {
                                if (var.name != value) {
                                    updateVirtualSelect(sprintf("var-%d", v), choices = data$columns, selected = var.name)
                                }
                            }
                        }

                        # if all specified variables were found in the loaded dataset(s), finally hide the data panel
                        if (all(variables %in% data$columns)) {
                            accordion_panel_close("sidebar", "data")
                            accordion_panel_open("sidebar", "filter")
                        }
                    }
                }
            }
        })
    })
}
