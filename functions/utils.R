# merge lists by adding missing entries
merge_lists <- function(list1, list2) {
  for (name in names(list2)) {
    if (name %in% names(list1)) {
      list1[[name]] <- merge_lists(list1[[name]], list2[[name]])
    } else {
      list1[[name]] <- list2[[name]]
    }
  }
  list1
}
