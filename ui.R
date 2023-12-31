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
                uiOutput("ui_char_gallery_tab1", class = "binder-gallery"),
                h2(textOutput("txt_char_name_tab1")),
                div(class = "binder-soziobox",
                    textOutput("txt_char_sozio_tab1")
                ),
                div(class = "binder-tagbox",
                  uiOutput("ui_char_tags_tab1")
                ),
                textOutput("txt_char_desc_tab1"),
                conditionalPanel(
                  condition = "output.cond_char_hasAppearance == true",
                  h3("Erscheinung"),
                  uiOutput("ui_char_appearance_tab1")
                ),
                div(em(textOutput("txt_char_languages_tab1"))),
                conditionalPanel(
                  condition = "output.cond_char_hasAnySpecial == true",
                  h3("Besonderes"),
                  conditionalPanel(
                    condition = "output.cond_char_hasAdvantages == true",
                    h4("Vorteile"),
                    uiOutput("ui_char_advantages_tab1")
                  ),
                  conditionalPanel(
                    condition = "output.cond_char_hasDisadvantages == true",
                    h4("Nachteile"),
                    uiOutput("ui_char_disadvantages_tab1")
                  ),
                  conditionalPanel(
                    condition = "output.cond_char_hasRules == true",
                    h4("Sonderregeln"),
                    uiOutput("ui_char_rules_tab1")
                  )
                ),
                conditionalPanel(
                  condition = "output.cond_char_hasCampaigns == true",
                  h3("Kampagnen"),
                  uiOutput("ui_char_campaigns_tab1")
                )
              ),
              tabPanel(tagList(shiny::icon("shield-virus"), "Kampf"),
                uiOutput("ui_char_gallery_tab2", class = "binder-gallery"),
                h2(textOutput("txt_char_name_tab2")),
                div(class = "binder-soziobox",
                  textOutput("txt_char_sozio_tab2")
                ),
                div(class = "binder-tagbox",
                  uiOutput("ui_char_tags_tab2")
                ),
                uiOutput("ui_char_attr_tab2"),
                uiOutput("ui_char_base_tab2"),
                h3("Fertigkeiten"),
                uiOutput("ui_char_skills_tab2"),
                h4("Kampf"),
                uiOutput("ui_char_fight_tab2"),
                h4("Waffen"),
                uiOutput("ui_char_weapons_tab2"),
                h4("Sonderfertigkeiten"),
                uiOutput("ui_char_abilities_tab2"),
                conditionalPanel(
                  condition = "output.cond_char_hasRules == true",
                  h4("Sonderregeln"),
                  uiOutput("ui_char_rules_tab2")
                ),
                h4("Vorteile"),
                uiOutput("ui_char_advantages_tab2"),
                h4("Nachteile"),
                uiOutput("ui_char_disadvantages_tab2"),
                h4("Magie"),
                uiOutput("ui_char_magic_tab2"),
                h4("Götterwirken"),
                uiOutput("ui_char_karmal_tab2")
              ),
              tabPanel(tagList(shiny::icon("comment", class = "fa-solid"), "Konversation"),
                h2("Konversation"),
                h3("Fertigkeiten"),
                uiOutput("ui_char_skills_tab3")
              ),
              tabPanel(tagList(shiny::icon("person-running"), "Action"),
                h2("Action"),
                h3("Fertigkeiten"),
                uiOutput("ui_char_skills_tab4"),
                h4("Magie"),
                uiOutput("ui_char_magic_tab4"),
                h4("Götterwirken"),
                uiOutput("ui_char_karmal_tab4")
              ),
              tabPanel("Alt"
                # uiOutput("gallery_character", class = "binder-gallery"),
                # div(class = "binder-tagbox",
                #   uiOutput("ui_char_tags")
                # ),
                # textOutput("txt_char_sozio"),
                # textOutput("txt_char_desc"),
                # h3("Kampagnen"),
                # textOutput("txt_char_campaigns"),
                # h3("Inventar"),
                # uiOutput("ui_char_inventory"),
                # h3("Stats"),
                # uiOutput("ui_char_attr"),
                # uiOutput("ui_char_base"),
                # h4("Fertigkeiten"),
                # uiOutput("ui_char_skills"),
                # h4("Kampf"),
                # uiOutput("ui_char_fight"),
                # h5("Waffen"),
                # uiOutput("ui_char_weapons"),
                # h4("Magie"),
                # uiOutput("ui_char_magic"),
                # h4("Götterwirken"),
                # uiOutput("ui_char_karmal"),
                # h4("Sonderfertigkeiten"),
                # uiOutput("ui_char_abilities"),
                # h4("Vorteile"),
                # uiOutput("ui_char_advantages"),
                # h4("Nachteile"),
                # uiOutput("ui_char_disadvantages"),
                # h4("Sonderregeln"),
                # uiOutput("ui_char_rules")
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
