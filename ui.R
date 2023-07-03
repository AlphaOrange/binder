ui <- dashboardPage(
  dashboardHeader(title = "Binder"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    sidebarMenu(
      menuItem("Characters", tabName = "tab_characters", icon = icon("dashboard")),
      menuItem("Bestiary", tabName = "tab_bestiary", icon = icon("th"), badgeLabel = "todo", badgeColor = "yellow"),
      menuItem("Artifacts", tabName = "tab_artifacts", icon = icon("th"), badgeLabel = "todo", badgeColor = "yellow"),
      selectizeInput("filter_campaigns", "Kampagnen", choices = .data$selections$campaign,
                     selected = .data$selections$campaign, multiple = TRUE,
                     options = list(plugins = list("remove_button"))),
      selectizeInput("filter_arcs", "Arcs", choices = .data$lists$arcs,
                     selected = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button")))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab_characters",
        fluidRow(
          column(width = 2,
            checkboxGroupInput("filter_char_roles", "Filter: Rollen",
                               choices = .res$roles, selected = .res$roles),
            selectInput("sel_character", "Charaktere", list(), selectize = FALSE, size = 16)
          ),
          column(width = 10,
            tabBox(
              title = textOutput("txt_char_name"),
              id = "tabset_char",
              width = NULL,
              tabPanel("Steckbrief",
                uiOutput("gallery_character", class = "binder-gallery"),
                div(class = "binder-tagbox",
                  uiOutput("ui_char_tags")
                ),
                textOutput("txt_char_sozio"),
                textOutput("txt_char_desc"),
                h3("Kampagnen"),
                textOutput("txt_char_campaigns"),
                h3("Inventar"),
                uiOutput("ui_char_inventory"),
                h3("Stats"),
                uiOutput("ui_char_attr"),
                uiOutput("ui_char_base"),
                h4("Fertigkeiten"),
                uiOutput("ui_char_skills"),
                h4("Kampf"),
                uiOutput("ui_char_fight"),
                h5("Waffen"),
                uiOutput("ui_char_weapons"),
                h4("Magie"),
                uiOutput("ui_char_magic"),
                h4("Götterwirken"),
                uiOutput("ui_char_karmal"),
                h4("Sonderfertigkeiten"),
                uiOutput("ui_char_abilities"),
                h4("Vorteile"),
                uiOutput("ui_char_advantages"),
                h4("Nachteile"),
                uiOutput("ui_char_disadvantages"),
                h4("Sonderregeln"),
                uiOutput("ui_char_rules")
              ),
              tabPanel(tagList(shiny::icon("shield-virus"), "Kampf"),
                h2("Kampf")
              ),
              tabPanel(tagList(shiny::icon("comment", class = "fa-solid"), "Konversation"),
                h2("Konversation")
              ),
              tabPanel(tagList(shiny::icon("person-running"), "Action"),
                h2("Action")
              )
            )
          )
        )
      ),
      tabItem(tabName = "tab_bestiary",
        h2("Bestiarium")
      ),
      tabItem(tabName = "tab_artifacts",
        h2("Artefakte")
      )
    )
  )
)
