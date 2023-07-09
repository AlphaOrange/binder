server <- function(input, output, session) {


  #### Data Pipeline ####

  .data_filtered <- reactive({

    filtered <- .data

    # General filters
    for (category in setdiff(names(filtered), c("campaign", "labels", "selections", "lists"))) {
      filtered[[category]] %<>% list.filter(any(Kampagnen %in% input$filter_campaigns))
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


  #### Preprocessed Reactives ####

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

  # Tags
  output$ui_char_tags <- renderUI({
    lapply(names(.char()$Tags), \(tag_type) {
      .char()$Tags[[tag_type]] %>%
        lapply(\(tag) { span(tag, class = c("binder-tag-all", paste0("binder-tag-", tag_type))) }) %>%
        tagList
    }) %>% tagList
  })

  # Voller Name
  output$txt_char_name      <- renderText(rct_char_name())
  output$txt_char_name_tab1 <- renderText(rct_char_name())

  # Soziodemographie
  output$txt_char_sozio <- renderText({
    dead <- !is.null(.char()$Todesjahr)
    born <- paste0("*", .char()$Geburtsjahr)
    died <- ifelse(dead, paste0("✝", .char()$Todesjahr), "")
    age <- ifelse(dead, .char()$Todesjahr - .char()$Geburtsjahr,
                        .res$current_year - .char()$Geburtsjahr)

    sprintf("%s / %s / %s / %s (%i)",
      rct_char_profession(),
      .char()$Spezies,
      .char()$Geschlecht,
      paste(c(born, died), collapse = " "),
      age
    )
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
  output$ui_char_attr <- renderUI({
    lapply(.res$attributes, \(attr) {
      paste(attr, .char()$Stats$Eigenschaften[[attr]]) %>%
        span(class = c("binder-attr", sprintf("binder-attr-%s", attr)))
    }) %>% tagList %>% div
  })

  # Basiswerte
  output$ui_char_base <- renderUI({
    armor <- paste("RS", .char()$Stats$Rüstungsschutz) %>%
      span(class = "binder-base") %>% list
    lapply(.res$basevalues, \(base) {
      paste(base, .char()$Stats$Grundwerte[[base]]) %>%
        span(class = "binder-base")
    }) %>% c(armor) %>% tagList %>% div
  })

  # Fertigkeiten
  output$ui_char_skills <- renderUI({
    lapply(names(.char()$Stats$Fertigkeiten), \(feat) {
      attributes <- .res$skills[[feat]] %>% lapply(\(attribute) {
        span(.char()$Stats$Eigenschaften[[attribute]],
             class = sprintf("binder-attr-small binder-attr-%s", attribute))
      }) %>% tagList
      tagList(
        attributes,
        .char()$Stats$Fertigkeiten[[feat]] %>% span(class = "binder-value"),
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
  output$ui_char_magic <- renderUI({

    magic_spells <- lapply(names(.char()$Stats$Magie$Zaubersprüche), \(action) {
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

    div(tagList(magic_spells, magic_rituals, magic_tricks))
  })

  # Götterwirken: Liturgien + Segnungen + Zeremonien
  output$ui_char_karmal <- renderUI({

    karmal_liturgies <- lapply(names(.char()$Stats$Götterwirken$Liturgien), \(action) {
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

    div(tagList(karmal_liturgies, karmal_ceremonies, karmal_blessings))
  })

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
