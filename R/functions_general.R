#' Jaccard weighting of edge-strenght
#'
#' Jaccard weighting of edge-strenght in an edge list
#'
#' Takes a weighted edge-list and adjusts the weight by dividing the two nodes
#' union by their intercept.
#'
#' @param i The outgoing node in an edgelist.
#' @param j The outgoing node in an edgelist.
#' @param w The weight of the edge between i and j.
#' @export
weight_jaccard <- function(i, j, w = 1) {
  y <- tibble(i = i, j = j, w = w)

  y %<>% group_by(i) %>% mutate(w.i = sum(w)) %>% ungroup() %>%
    group_by(j) %>% mutate(w.j = sum(w)) %>% ungroup() %>%
    mutate(weight_jac = w / (w.i + w.j - w)) %>%
    select(weight_jac) %>% pull()
  return(y) # TODO: Find a solution for the case when you have adittional groupings, eg. year
}
