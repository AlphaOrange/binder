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
      jsonvalidate::json_validator(file)
    } else {
      read_json(file)
    }
  }) %>% setNames(schema_names)

  schemas

}


# Load and validate data
load_data <- function() {

  # load all schemas
  schemas <- load_schemas(parse = FALSE)

  # load and assign data
  data <- list()
  json_files <- list.files(path = "data", pattern = "^[^_].*\\.json$", full.names = TRUE, recursive = TRUE)
  for (file in json_files) {
    single <- read_json(file)
    # if available auto-complete optional properties with empty vectors/list, single entries remain NULL
    if (single$Typ %in% names(schemas)) {
      for (property in names(schemas[[single$Typ]]$properties)) {
        if (!(property %in% names(single))) {
          type <- schemas[[single$Typ]]$properties[[property]]$type
          if (type == "array") {
            subtype <- schemas[[single$Typ]]$properties[[property]]$type$items$type
            if (subtype == "string") {
              single[[property]] <- character(0)
            } else if (subtype %in% c("integer", "number")) {
              single[[property]] <- numeric(0)
            } else {
              single[[property]] <- list()
            }
          } else if (type == "object") {
            single[[property]] <- list()
          }
        }
      }
    }
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
      # for all others use alphabetical order
      data[[type]] %<>% .[order(names(data[[type]]))]
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

  # add aggregations to data
  data$lists <- data_lists

  data

}
