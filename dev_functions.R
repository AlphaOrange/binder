# Load Packages
library(magrittr)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(rlist)
library(stringr)
library(crayon)

# Load scripts
source("data_io.R")


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
binder_add_tag <- function(tag, types) {

  # loop all json data files: read, modify and write
  data <- list()
  json_files <- list.files(path = "data", pattern = "^[^_].*\\.json$", full.names = TRUE, recursive = TRUE)
  cat(bold(sprintf("Vertaggen mit '%s'\n", tag)))
  for (file in json_files) {
    single <- read_json(file)
    if (missing(types) || single$Typ %in% types) {
      line <- sprintf("[%s] %s: (j/n)", single$Typ, single$name)
      answer <- readline(line)
      if (answer == "j") {
        single$Tags <- unique(c(single$Tag, tag))
        write_json(single, file, pretty = TRUE, auto_unbox = TRUE, null = "null")
      }
    }
  }
}
# binder_add_tag("survival")


#### Update data jsons via updated template ####
binder_add_fields <- function(type, fields) {

  # loop all json data files: read, modify and write
  data <- list()
  json_files <- list.files(path = "data", pattern = "^[^_].*\\.json$", full.names = TRUE, recursive = TRUE)
  for (file in json_files) {
    single <- read_json(file)
    if (single$Typ == type) {
      single <- list.merge(single, fields)
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

#### Detailled file validation ####
binder_validate_file <- function(file, type) {

  # create validator
  validator <- sprintf("resources/schemas/schema_%s.json", type) %>%
    jsonvalidate::json_validator()

  # check file
  validator(file, verbose = TRUE)

}
# binder_validate_file("data/characters/morena.json", "character")
# binder_validate_file("data/campaigns/1-nanduskammer.json", "campaign")
