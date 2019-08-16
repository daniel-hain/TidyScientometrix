##################################################################
##	BEGIN: Jaccard Weight
##################################################################

weight_jaccard <- function(x, i, j, w) {
  y <- x %>% select_(i, j, w)
  colnames(y) <- c("i", "j", "w")

  y %<>% group_by(i) %>% mutate(w.i = sum(w)) %>% ungroup() %>%
    group_by(j) %>% mutate(w.j = sum(w)) %>% ungroup() %>%
    mutate(weight_jac = w / (w.i + w.j - w)) %>%
    select(weight_jac) %>% pull()
  return(y) # TODO: Find a solution for the case when you have adittional groupings, eg. year
}
