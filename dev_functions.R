# Load Packages
library(magrittr)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(rlist)
library(dplyr)
library(stringr)
library(crayon)

# Load scripts
source("data_io.R")
source("functions/utils.R")


##### Editor #####

#### Create new json file from template ####
binder_create <- function(type, id, ...) {

  args <- list(...)

  # load template
  tmpl_filename <- sprintf("_%s\\.json", type)
  path <- list.files(path = "data", pattern = sprintf("^%s$", tmpl_filename), full.names = TRUE, recursive = TRUE)[1]
  template <- read_json(path)
  template$ID <- id

  # fill template with arguments
  for (name in names(args)) {
    template[[name]] <- args[[name]]
  }

  # save new json file
  new_filename <- paste0(id, ".json")
  new_path <- path %>% str_replace(tmpl_filename, new_filename)
  if (!file.exists(new_path)) {
    write_json(template, new_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  } else {
    stop(paste("Dateiname existiert bereits:", new_filename))
  }

}
# binder_create("character", "ulfrost",
#               Name = "Ulfrost vom Titzelforst",
#               Rolle = "recurring",
#               Kampagnen = list("feniasspuren"))

#### Quickly apply tag via list selection ####
binder_add_tag <- function(tag, types = "all", tag_type = "Weitere") {
  # loop all json data files: read, modify and write
  data <- list()
  json_files <- list.files(path = "data", pattern = "^[^_].*\\.json$", full.names = TRUE, recursive = TRUE)
  cat(bold(sprintf("Vertaggen mit '%s'\n", tag)))
  for (file in json_files) {
    data <- read_json(file)
    if (is.null(names(data))) {
      # Multi-json
      data <- lapply(data, \(single) {
        if (types == "all" || single$Typ %in% types) {
          line <- sprintf("[%s] %s: (j/n)", single$Typ, single$Name)
          answer <- readline(line)
          if (answer == "j") {
            single$Tags[[tag_type]] <- unique(c(single$Tags[[tag_type]], tag))
          }
          single
        }
      })
      write_json(data, file, pretty = TRUE, auto_unbox = TRUE, null = "null")
    } else {
      # single-json
      if (types == "all" || data$Typ %in% types) {
        line <- sprintf("[%s] %s: (j/n)", data$Typ, data$Name)
        answer <- readline(line)
        if (answer == "j") {
          data$Tags[[tag_type]] <- unique(c(data$Tags[[tag_type]], tag))
          write_json(data, file, pretty = TRUE, auto_unbox = TRUE, null = "null")
        }
      }
    }
  }
}
# binder_add_tag("survival", "character", "Test")
# binder_add_tag("survival", "character", "Orte")
# binder_add_tag("Phantasien", tag_type = "Orte")

#### Update data jsons via updated template ####
binder_add_fields <- function(type, fields) {

  # loop all json data files: read, modify and write
  data <- list()
  json_files <- list.files(path = "data", pattern = "^[^_].*\\.json$", full.names = TRUE, recursive = TRUE)
  for (file in json_files) {
    single <- read_json(file)
    if (single$Typ == type) {
      single <- merge_lists(single, fields)
    }
    write_json(single, file, pretty = TRUE, auto_unbox = TRUE, null = "null")
  }

}
# empty_named_list <- {
#   l <- list(a = "b")
#   l$a <- NULL
#   l
# }
# binder_add_fields(type = "character", list(Stats = list(Kampftechniken = list(
#   Armbrüste = 6, Bögen = 6, Wurfwaffen = 6, Dolche = 6, Fechtwaffen = 6, Hiebwaffen = 6, Kettenwaffen = 6,
#   Lanzen = 6, Raufen = 6, Schilde = 6, Schwerter = 6, Stangenwaffen = 6, Zweihandhiebwaffen = 6,
#   Zweihandschwerter = 6
# ))))


##### Validation #####

#### Validate all data ####
binder_validate_all <- function() {

  # load all schemas
  schemas <- load_schemas()

  # validate all data files
  json_files <- list.files(path = "data", pattern = "^[^_].*\\.json$", full.names = TRUE, recursive = TRUE)
  for (file in json_files) {
    validates <- sapply(names(schemas), \(schema) {
      schemas[[schema]](file)
    })
    if (any(validates)) {
      cat(file, ":", bold("CHECK"), ":", paste(names(schemas)[validates], collapse = ", "), "\n")
    } else {
      cat(red(bold(paste0(file, " : FAIL\n"))))
    }
  }

}
# binder_validate_all()

#### Detailed file validation ####
binder_validate_file <- function(file, type) {

  # create validator
  validator <- sprintf("resources/schemas/schema_%s.json", type) %>%
    jsonvalidate::json_validator()

  # check file
  validator(file, verbose = TRUE)

}
# binder_validate_file("data/characters/morena.json", "character")
# binder_validate_file("data/campaigns/1-nanduskammer.json", "campaign")
# binder_validate_file("data/characters/m_galebfurt.json", "character")

#### Check ids for unambiguousness ####
binder_check_ids <- function() {

  # gather ids from all data files
  json_files <- list.files(path = "data", pattern = "^[^_].*\\.json$", full.names = TRUE, recursive = TRUE)

  dublets <- lapply(json_files, \(file) {
    single <- read_json(file) %>% list.flatten
    ids <- single[names(single) == "ID"] %>% unlist
    bind_cols(file, ids) %>% setNames(c("File", "ID"))
  }) %>% bind_rows %>%
    group_by(ID) %>%
    summarize(Files = paste(File, collapse = "\n"), Count = n()) %>%
    filter(Count > 1)

  if (!nrow(dublets)) {
    cat(bold("CHECK") %+% " keine uneindeutigen IDs gefunden.\n")
  } else {
    for (i in seq_len(nrow(dublets))) {
      cat(bold("ID mehrfach vergeben: ") %+% dublets$ID[i] %+% "\n" %+% dublets$Files[i])
    }
  }

}
# binder_check_ids()

#### Check for missing resources ####
binder_check_missing_resources <- function() {

  # load all data
  data <- load_data()
  res  <- load_resources()

  # check magic resources
  cat("Magie-Ressourcen: ")
  res_magic <- lapply(data$character, \(char) {
    c(
      char$Stats$Magie$Zaubertricks,
      names(char$Stats$Magie$Zaubersprüche),
      names(char$Stats$Magie$Rituale)
    )
  }) %>% unlist %>% unique
  missing_magic <- res_magic %>% setdiff(names(res$magic))
  if (length(missing_magic)) {
    cat(bold(red("FAIL")) %+% "\nMissing Resources: ")
    cat(paste(missing_magic, collapse = ", "), "\n")
  } else {
    cat(bold("CHECK\n"))
  }

  # check karmal resources
  cat("Karmal-Ressourcen: ")
  res_karmal <- lapply(data$character, \(char) {
    c(
      char$Stats$Götterwirken$Segnungen,
      names(char$Stats$Götterwirken$Liturgien),
      names(char$Stats$Götterwirken$Zeremonien)
    )
  }) %>% unlist %>% unique
  missing_karmal <- res_karmal %>% setdiff(names(res$karmal))
  if (length(missing_karmal)) {
    cat(bold(red("FAIL")) %+% "\nMissing Resources: ")
    cat(paste(missing_karmal, collapse = ", "), "\n")
  } else {
    cat(bold("CHECK\n"))
  }

}
# binder_check_missing_resources()

