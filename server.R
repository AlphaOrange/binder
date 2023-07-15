server <- function(input, output, session) {


  #### Data Pipeline ####

  .data_filtered <- reactive({

    filtered <- .data

    # General filters
    for (category in setdiff(names(filtered), c("campaign", "labels", "selections", "lists"))) {
      filtered[[category]] %<>% list.filter(any(Kampagnen %in% input$filter_campaigns) |
                                              !length(Kampagnen) & "(keine)" %in% input$filter_campaigns)
    }

    # Type-specific filters
    filtered$character %<>% list.filter(Rolle %in% input$filter_char_roles)

    filtered

  })

  .char <- reactive({
    req(input$sel_character)
    .data$character[[input$sel_character]]
  })


  #### Update filtered selections ####

  observe({
    choices <- .data$selections$character %>% intersect(names(.data_filtered()$character))
    selected <- intersect(input$sel_character, choices) %>% ifelse(length(.), ., choices[1])
    updateSelectInput(session, "sel_character",
                      choices = choices,
                      selected = selected)
  })


  #### Preprocessed Reactives and Functions ####

  # Voller Name
  rct_char_name <- reactive({
    if (length(.char()$Titel)) {
      paste(paste(.char()$Titel, collapse = " "), .char()$Name)
    } else {
      .char()$Name
    }
  })

  # Professionsbezeichnung
  rct_char_profession <- reactive({
    ifelse(is.null(.char()$Profession), "ohne Profession", .char()$Profession)
  })

  # Alter, Geburts- und Todeskennzeichnung
  rct_char_age <- reactive({
    dead <- !is.null(.char()$Todesjahr)
    born <- paste0("*", .char()$Geburtsjahr)
    died <- ifelse(dead, paste0("✝", .char()$Todesjahr), "")
    age <- ifelse(dead, .char()$Todesjahr - .char()$Geburtsjahr,
                  .res$current_year - .char()$Geburtsjahr)
    list(born = born, died = died, age = age)
  })

  # Generate Tags Container
  fct_char_tags <- function(data, selection, deadmark = FALSE) {
    tags <- names(data)
    if (!missing(selection)) { tags <- intersect(tags, selection) }
    tags <- lapply(tags, \(tag_type) {
      .char()$Tags[[tag_type]] %>%
        lapply(\(tag) { span(tag, class = c("binder-tag-all", paste0("binder-tag-", tag_type))) }) %>%
        tagList
    }) %>% tagList
    if (deadmark && !is.null(.char()$Todesjahr)) {
      tags <- span("verstorben", class = c("binder-tag-all", "binder-tag-dead")) %>%
        tagList(., tags)
    }
    tags
  }

  # Generate Magic UI
  fct_char_magic <- function(tag = "all") {
    names_spells <- names(.char()$Stats$Magie$Zaubersprüche)
    if (tag == "fight") { names_spells <- names_spells %>% intersect(.res$.proc$magic_fight) }
    magic_spells <- lapply(names_spells, \(action) {
      details    <- .res$magic[[action]]
      value      <- .char()$Stats$Magie$Zaubersprüche[[action]][1] %>% span(class = "binder-value")
      extensions <- .char()$Stats$Magie$Zaubersprüche[[action]][-1] %>% paste(collapse = ", ")
      if (extensions != "") { extensions <- paste0("(", extensions, ")") }
      attributes <- details$attributes %>% lapply(\(attribute) {
        span(.char()$Stats$Eigenschaften[[attribute]],
             class = sprintf("binder-attr-small binder-attr-%s", attribute))
      }) %>% tagList
      tagList(
        attributes,
        value,
        action,
        extensions
      ) %>% div(class = sprintf("binder-magic binder-magic-%s", details$type))
    }) %>% tagList

    magic_rituals <- lapply(names(.char()$Stats$Magie$Rituale), \(action) {
      details    <- .res$magic[[action]]
      value      <- .char()$Stats$Magie$Rituale[[action]][1] %>% span(class = "binder-value")
      extensions <- .char()$Stats$Magie$Rituale[[action]][-1] %>% paste(collapse = ", ")
      if (extensions != "") { extensions <- paste0("(", extensions, ")") }
      attributes <- details$attributes %>% lapply(\(attribute) {
        span(.char()$Stats$Eigenschaften[[attribute]],
             class = sprintf("binder-attr-small binder-attr-%s", attribute))
      }) %>% tagList
      tagList(
        attributes,
        value,
        action,
        extensions
      ) %>% div(class = sprintf("binder-magic binder-magic-%s", details$type))
    }) %>% tagList

    magic_tricks <- lapply(.char()$Stats$Magie$Zaubertricks, \(action) {
      details    <- .res$magic[[action]]
      action %>% div(class = sprintf("binder-magic binder-magic-%s", details$type))
    }) %>% tagList

    if (tag == "fight") {
      div(tagList(magic_spells))
    } else {
      div(tagList(magic_spells, magic_rituals, magic_tricks))
    }
  }

  # Generate Karmal UI
  fct_char_karmal <- function(tag = "all") {
    names_liturgies <- names(.char()$Stats$Götterwirken$Liturgien)
    if (tag == "fight") { names_liturgies <- names_liturgies %>% intersect(.res$.proc$karmal_fight) }
    karmal_liturgies <- lapply(names_liturgies, \(action) {
      details    <- .res$karmal[[action]]
      value      <- .char()$Stats$Götterwirken$Liturgien[[action]][1] %>% span(class = "binder-value")
      extensions <- .char()$Stats$Götterwirken$Liturgien[[action]][-1] %>% paste(collapse = ", ")
      if (extensions != "") { extensions <- paste0("(", extensions, ")") }
      attributes <- details$attributes %>% lapply(\(attribute) {
        span(.char()$Stats$Eigenschaften[[attribute]],
             class = sprintf("binder-attr-small binder-attr-%s", attribute))
      }) %>% tagList
      tagList(
        attributes,
        value,
        action,
        extensions
      ) %>% div(class = sprintf("binder-karmal binder-karmal-%s", details$type))
    }) %>% tagList

    karmal_ceremonies <- lapply(names(.char()$Stats$Götterwirken$Zeremonien), \(action) {
      details    <- .res$karmal[[action]]
      value      <- .char()$Stats$Götterwirken$Zeremonien[[action]][1] %>% span(class = "binder-value")
      extensions <- .char()$Stats$Götterwirken$Zeremonien[[action]][-1] %>% paste(collapse = ", ")
      if (extensions != "") { extensions <- paste0("(", extensions, ")") }
      attributes <- details$attributes %>% lapply(\(attribute) {
        span(.char()$Stats$Eigenschaften[[attribute]],
             class = sprintf("binder-attr-small binder-attr-%s", attribute))
      }) %>% tagList
      tagList(
        attributes,
        value,
        action,
        extensions
      ) %>% div(class = sprintf("binder-karmal binder-karmal-%s", details$type))
    }) %>% tagList

    karmal_blessings <- lapply(.char()$Stats$Götterwirken$Segnungen, \(action) {
      details    <- .res$karmal[[action]]
      action %>% div(class = sprintf("binder-karmal binder-karmal-%s", details$type))
    }) %>% tagList

    if (tag == "fight") {
      div(tagList(karmal_liturgies))
    } else {
      div(tagList(karmal_liturgies, karmal_ceremonies, karmal_blessings))
    }
  }


  #### JS Conditions ####

  # Charakter hat: Erscheinung
  output$cond_char_hasAppearance <- reactive(length(.char()$Erscheinung) > 0)
  outputOptions(output, "cond_char_hasAppearance", suspendWhenHidden = FALSE)

  # Charakter hat: Vorteile
  output$cond_char_hasAdvantages <- reactive(length(.char()$Vorteile) > 0)
  outputOptions(output, "cond_char_hasAdvantages", suspendWhenHidden = FALSE)

  # Charakter hat: Nachteile
  output$cond_char_hasDisadvantages <- reactive(length(.char()$Nachteile) > 0)
  outputOptions(output, "cond_char_hasDisadvantages", suspendWhenHidden = FALSE)

  # Charakter hat: Sonderregeln
  output$cond_char_hasRules <- reactive(length(.char()$Sonderregeln) > 0)
  outputOptions(output, "cond_char_hasRules", suspendWhenHidden = FALSE)

  # Charakter hat: Vorteile oder Nachteile oder Sonderregeln
  output$cond_char_hasAnySpecial <- reactive({
    length(.char()$Vorteile) > 0 ||
    length(.char()$Nachteile) > 0 ||
    length(.char()$Sonderregeln) > 0
  })
  outputOptions(output, "cond_char_hasAnySpecial", suspendWhenHidden = FALSE)

  # Charakter hat: Kampagnen
  output$cond_char_hasCampaigns <- reactive(length(.char()$Kampagnen) > 0)
  outputOptions(output, "cond_char_hasCampaigns", suspendWhenHidden = FALSE)


  #### Character Sheet Outputs ####

  # Bildergalerie
  output$gallery_character <- renderUI({
    lapply(.char()$Bilder, \(image) {
      img(src = file.path("pix", image))
    }) %>% tagList
  })

  # Avatar
  output$ui_avatar <- renderUI({
    if (length(.char()$Bilder)) {
      image <- first(.char()$Bilder)
      img(src = file.path("pix", image))
    }
  })

  # Tags
  output$ui_char_tags <- renderUI({ fct_char_tags(.char()$Tags) })
  output$ui_char_tags_fight <- renderUI({ fct_char_tags(.char()$Tags, selection = "Kampf", deadmark = TRUE) })

  # Name
  output$txt_char_name      <- renderText(.char()$Name)
  output$txt_char_name_tab1 <- renderText(rct_char_name())
  output$txt_char_name_tab2 <- renderText(.char()$Name)

  # Soziodemographie
  output$txt_char_sozio <- renderText({
    sprintf("%s / %s / %s / %s (%i)",
      rct_char_profession(),
      .char()$Spezies,
      .char()$Geschlecht,
      paste(c(rct_char_age()$born, rct_char_age()$died), collapse = " "),
      rct_char_age()$age
    )
  })
  output$txt_char_sozio_tab2 <- renderText({
    sprintf("%s (%i)", .char()$Spezies, rct_char_age()$age)
  })

  # Charakterbeschreibung
  output$txt_char_desc <- renderText({
    .char()$Text
  })

  # Charakterbeschreibung
  output$ui_char_appearance <- renderUI({
    icons <- list(Aussehen = "user", Kleidung = "shirt", Gestik = "hand",
                  Mimik = "face-smile", Stimme = "comment-dots", Besonderheiten = "star")
    lapply(names(.char()$Erscheinung), \(item) {
        tagList(icon(icons[[item]], class = "fa-solid"), .char()$Erscheinung[[item]]) %>%
        (tags$li)
    }) %>% (tags$ul)
  })

  # Soziodemographie
  output$txt_char_languages <- renderText({
    if (length(.char()$Sprachen)) {
      sapply(names(.char()$Sprachen), \(language) {
        sprintf("%s (%s)", language, as.roman(.char()$Sprachen[[language]]))
      }) %>%
        paste(collapse = ", ") %>%
        paste("Spricht:", .)
    }
  })

  # Kampagnen des Charakters
  output$ui_char_campaigns <- renderUI({
    campaigns <- .data$labels$campaign[unlist(.char()$Kampagnen)]
    lapply(campaigns, \(campaign) {
      campaign %>% (tags$li)
    }) %>% (tags$ul)
  })

  # Inventar des Charakters
  output$ui_char_inventory <- renderUI({
    lapply(names(.char()$Inventar), \(item) {
      sprintf("%sx %s", .char()$Inventar[[item]], item) %>%
        (tags$li)
    }) %>% (tags$ul)
  })

  # Attribute
  output$ui_char_attr_tab2 <- renderUI({
    lapply(.res$attributes, \(attr) {
      paste(attr, .char()$Stats$Eigenschaften[[attr]]) %>%
        span(class = c("binder-attr", sprintf("binder-attr-%s", attr)))
    }) %>% tagList %>% div
  })

  # Basiswerte
  output$ui_char_base_tab2 <- renderUI({
    armor <- paste("RS", .char()$Stats$Rüstungsschutz) %>%
      span(class = "binder-base") %>% list
    lapply(.res$basevalues, \(base) {
      paste(base, .char()$Stats$Grundwerte[[base]]) %>%
        span(class = "binder-base")
    }) %>% c(armor) %>% tagList %>% div
  })

  # Fertigkeiten
  output$ui_char_skills_tab2 <- renderUI({
    lapply(.res$fightskills, \(feat) {
      attributes <- .res$skills[[feat]] %>% lapply(\(attribute) {
        span(.char()$Stats$Eigenschaften[[attribute]],
             class = sprintf("binder-attr-small binder-attr-%s", attribute))
      }) %>% tagList
      value <- ifelse(feat %in% names(.char()$Stats$Fertigkeiten),
                      .char()$Stats$Fertigkeiten[[feat]], 0)
      tagList(
        attributes,
        value %>% span(class = "binder-value"),
        feat
      ) %>% div(class = "binder-feat")
    }) %>% tagList %>% div
  })
  output$ui_char_skills_tab3 <- renderUI({
    lapply(.res$talkskills, \(feat) {
      attributes <- .res$skills[[feat]] %>% lapply(\(attribute) {
        span(.char()$Stats$Eigenschaften[[attribute]],
             class = sprintf("binder-attr-small binder-attr-%s", attribute))
      }) %>% tagList
      value <- ifelse(feat %in% names(.char()$Stats$Fertigkeiten),
                      .char()$Stats$Fertigkeiten[[feat]], 0)
      tagList(
        attributes,
        value %>% span(class = "binder-value"),
        feat
      ) %>% div(class = "binder-feat")
    }) %>% tagList %>% div
  })
  output$ui_char_skills_tab4 <- renderUI({
    lapply(names(.res$skills), \(feat) {
      attributes <- .res$skills[[feat]] %>% lapply(\(attribute) {
        span(.char()$Stats$Eigenschaften[[attribute]],
             class = sprintf("binder-attr-small binder-attr-%s", attribute))
      }) %>% tagList
      value <- ifelse(feat %in% names(.char()$Stats$Fertigkeiten),
                      .char()$Stats$Fertigkeiten[[feat]], 0)
      tagList(
        attributes,
        value %>% span(class = "binder-value"),
        feat
      ) %>% div(class = "binder-feat")
    }) %>% tagList %>% div
  })

  # Kampftechniken
  output$ui_char_fight <- renderUI({
    lapply(names(.res$fighting), \(fight) {
      if (fight %in% names(.char()$Stats$Kampftechniken)) {
        value <- .char()$Stats$Kampftechniken[[fight]]
      } else {
        value <- 6
      }
      tagList(
        value %>% span(class = "binder-value"),
        fight
      ) %>% div(class = "binder-fight")
    }) %>% tagList %>% div
  })

  output$ui_char_weapons <- renderUI({
    weapons_NK <- lapply(names(.char()$Stats$Waffen$Nahkampf), \(weapon) {
      item <- .char()$Stats$Waffen$Nahkampf[[weapon]]
      if (is.null(item$PA)) { item$PA <- .char()$Stats$Grundwerte$AW }
      val_AT <- item$AT %>% span(class = "binder-value")
      val_PA <- item$PA %>% span(class = "binder-value")
      val_TP <- sprintf("%iW6 %s",
                        item$TP[[1]],
                        ifelse(item$TP[[2]] >= 0,
                               paste("+", item$TP[[2]]),
                               paste("-", abs(item$TP[[2]])))) %>% span(class = "binder-value")
      block <- tagList(val_AT, val_PA, val_TP, paste0("(", item$RW, ")"), strong(weapon)) %>%
        div(class = "binder-fight")
      if (!is.null(item$Vorteil))  { block <- tagList(block, div(paste0("Vorteil: ", item$Vorteil))) }
      if (!is.null(item$Nachteil)) { block <- tagList(block, div(paste0("Nachteil: ", item$Nachteil))) }
      block
    })
    weapons_FK <- lapply(names(.char()$Stats$Waffen$Fernkampf), \(weapon) {
      item <- .char()$Stats$Waffen$Fernkampf[[weapon]]
      val_AT <- item$FK %>% span(class = "binder-value")
      val_PA <- .char()$Stats$Grundwerte$AW %>% span(class = "binder-value")
      val_TP <- sprintf("%iW6 %s",
                        item$TP[[1]],
                        ifelse(item$TP[[2]] >= 0,
                               paste("+", item$TP[[2]]),
                               paste("-", abs(item$TP[[2]])))) %>% span(class = "binder-value")
      val_load <- paste0("LZ ", item$LZ, " (", item$Munition, ")")
      val_RW <- item$RW %>% paste(collapse = "/") %>% paste("RW")
      block <- tagList(val_AT, val_PA, val_TP, val_load, val_RW, strong(weapon)) %>%
        div(class = "binder-fight")
      if (!is.null(item$Vorteil))  { block <- tagList(block, div(paste0("Vorteil: ", item$Vorteil))) }
      if (!is.null(item$Nachteil)) { block <- tagList(block, div(paste0("Nachteil: ", item$Nachteil))) }
      block
    })
    weapons <- c(weapons_NK, weapons_FK) %>% tagList %>% div
  })

  # Magie: Zauber + Zaubersprüche + Rituale
  output$ui_char_magic_tab2 <- renderUI({ fct_char_magic(tag = "fight") })
  output$ui_char_magic_tab4 <- renderUI({ fct_char_magic() })

  # Götterwirken: Liturgien + Segnungen + Zeremonien
  output$ui_char_karmal_tab2 <- renderUI({ fct_char_karmal(tag = "fight") })
  output$ui_char_karmal_tab4 <- renderUI({ fct_char_karmal() })

  # Funktion: Textbaustein aus unterschiedlich aufgebauten Detaillisten
  fct_details2text <- function(name, details) {
    if (length(details) == 0) {
      name
    } else if (length(details) == 2) {
      paste0(name, " ", as.roman(details[[1]]), ": ", details[[2]])
    } else if (is.numeric(details[[1]])) {
      paste0(name, " ", as.roman(details[[1]]))
    } else {
      paste0(name, ": ", details[[1]])
    }
  }

  # Sonderfertigkeiten
  output$ui_char_abilities <- renderUI({
    lapply(names(.char()$Sonderfertigkeiten), \(item) {
      fct_details2text(item, .char()$Sonderfertigkeiten[[item]])
    }) %>% tagList
  })

  # Vorteile
  output$ui_char_advantages <- renderUI({
    lapply(names(.char()$Vorteile), \(item) {
      fct_details2text(item, .char()$Vorteile[[item]])
    }) %>% tagList
  })

  # Nachteile
  output$ui_char_disadvantages <- renderUI({
    lapply(names(.char()$Nachteile), \(item) {
      fct_details2text(item, .char()$Nachteile[[item]])
    }) %>% tagList
  })

  # Sonderregeln
  output$ui_char_rules <- renderUI({
    lapply(names(.char()$Sonderregeln), \(item) {
      div(class = "binder-specialrules",
        h5(item),
        p(.char()$Sonderregeln[[item]])
      )
    }) %>% tagList
  })

}
