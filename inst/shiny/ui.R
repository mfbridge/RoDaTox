page_navbar(
    title = "RoDaTox",
    window_title = "Rodent Data Toxicology Dashboard",

    # some UI CSS tweaks
    theme = bs_theme(preset = "minty", spacer = "0.75rem", `border-radius` = "0") %>%
        bs_add_variables(`popover-border-radius` = "0px"),

    nav_menu(title = "In-life (Ixx)",
        nav_panel("I04 - Body Weight", I04UI("i04"))
    )
)
