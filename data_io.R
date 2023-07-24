#################################
##### Binder: Data Handling #####
#################################


# Load resources
load_resources <- function() {

  # Base file
  res <- read_json("resources/_base.json")

  # Add separate files
  json_files <- list.files(path = "resources", pattern = "^[^_].*\\.json$", full.names = TRUE, recursive = TRUE)
  for (file in json_files) {
    single <- read_json(file)
    type <- file %>% str_match("([a-zA-Z]*)(?:_[^\\.]+)?\\.json$") %>% extract(1, 2)
    res[[type]] <- c(res[[type]], single)
  }

  # Processing
  res$.proc$magic_fight  <- res$magic  %>% list.filter("fight" %in% tags) %>% names
  res$.proc$karmal_fight <- res$karmal %>% list.filter("fight" %in% tags) %>% names
  res$.proc$advantages_fight <- res$advantages %>% list.filter("fight" %in% tags) %>% names
  res$.proc$disadvantages_fight <- res$disadvantages %>% list.filter("fight" %in% tags) %>% names

  res

}


# Load all schemas
load_schemas <- function(parse = TRUE) {

  schema_files <- list.files(path = "resources/schemas", pattern = "^[^_].*\\.json$", full.names = TRUE, recursive = TRUE)
  schema_names <- sapply(schema_files, \(file) {
    read_json(file)$title
  })

  schemas <- lapply(schema_files, \(file) {
    if (parse) {
      jsonvalidate::json_validator(file, engine = "ajv")
    } else {
      read_json(file)
    }
  }) %>% setNames(schema_names)

  schemas

}


# Load all defaults
load_defaults <- function() {

  defaults_files <- list.files(path = "resources/defaults", pattern = "^[^_].*\\.json$", full.names = TRUE, recursive = TRUE)
  defaults <- lapply(defaults_files, \(file) {
    read_json(file)
  })
  defaults_names <- sapply(defaults, \(item) { item$Typ })
  defaults <- defaults %>% setNames(defaults_names)

  defaults

}


# Load and validate data
load_data <- function() {

  # load all schemas
  schemas <- load_schemas(parse = FALSE)
  defaults <- load_defaults()

  # load and assign data
  json_files <- list.files(path = "data", pattern = "^[^_].*\\.json$", full.names = TRUE, recursive = TRUE)

  # read all files and split multi-files
  json_data <- list()
  for (file in json_files) {
    data <- read_json(file)
    # check if this is a multi-file
    if (is.null(names(data))) {
      # append multi-file content
      json_data <- c(json_data, data)
    } else {
      # append single-file content
      json_data <- c(json_data, list(data))
    }
  }

  # assign json data
  data <- list()
  for (single in json_data) {
    # add defaults
    single <- merge_lists(single, defaults[[single$Typ]])
    # store data
    data[[single$Typ]][[single$ID]] <- single
  }

  # generate aggregated lists
  unlisted <- data %>% unlist
  data_lists <- list()
  data_lists$arcs <- unlisted %>% names %>% str_which("\\.Arcs[0-9]*$") %>% unlisted[.] %>% unique %>% sort
  data_lists$tags <- unlisted %>% names %>% str_which("\\.Tags[0-9]*$") %>% unlisted[.] %>% unique %>% sort

  # sort data and create labels/selections
  for (type in names(data)) {

    if (type == "campaign") {
      # for campaign use campaign number for ordering
      data[[type]] %<>% .[order(list.mapv(data[[type]], Nummer))]
    } else {
      # for all others use alphabetical order of name
      data[[type]] %<>% .[order(list.mapv(data[[type]], Name))]
    }
    data$labels[[type]] <- list.map(data[[type]], Name) %>%
      setNames(list.map(data[[type]], ID))
    data$selections[[type]] <- list.map(data[[type]], ID) %>%
      setNames(list.map(data[[type]], Name))

    # sort inside of elements
    if (type == "character") {
      for (char in names(data$character)) {
        if (length(data$character[[char]]$Stats$Magie)) {
          data$character[[char]]$Stats$Magie %<>% .[order(names(.))]
        }
      }
    }

  }

  # add general options to selections
  data$selections$campaign %<>% c("(keine)", .)

  # add aggregations to data
  data$lists <- data_lists

  data

}
