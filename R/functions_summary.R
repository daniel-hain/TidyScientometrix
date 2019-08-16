
##################################################################
##	BEGIN: gen_summary():
##################################################################
# Wrapper around the summary functions
gen_summary <- function(x, top_n = 10, level, what = "general", plot = FALSE, select_com = NULL){
  require(kableExtra)

  if(!is.null(select_com)){
    x %<>% filter(com %in% select_com)
  }
  
  if(level == "REF"){
    gen_summary_ref(x = x, top_n = top_n, what = what, plot = plot)
  } else if(level == "PUB"){
    gen_summary_pub(x = x, top_n = top_n, what = what, plot = plot)
  } else{
    cat("No usable level selected. Levels: REF (References), PUB (Publications)")
    break()
  }
  
  # group_rows("Community 1", 1, 10) 
}

##################################################################
##	BEGIN: gen_summary_pub():
##################################################################
# Summaru functions for publications
gen_summary_pub <- function(x, top_n = top_n, what = what, plot = plot){
 
  ### For summery absed on citation counts
  if(what == "count"){
    x_sum <- x %>%
      select(N_AU, AU) %>%
      mutate(n_frac = 1 / N_AU) %>%
      unnest(AU) %>%  drop_na() %>%
      group_by(AU) %>% summarise(n_frac = sum(n_frac)) %>% ungroup() %>%
      arrange(desc(n_frac)) %>% drop_na(AU) %>% slice(1:top_n) %>%
      bind_cols(
        x %>% select(SO) %>% 
          drop_na() %>%
          group_by(SO) %>% summarise(n = n()) %>% ungroup() %>%
          arrange(desc(n)) %>% drop_na(SO) %>% slice(1:top_n) %>%
          mutate(SO = SO %>% str_trunc(45))
      ) %>%
      bind_cols(
        x %>% select(EID, C1) %>%
          unnest(C1) %>% unnest(C1) %>% drop_na() %>%
          group_by(EID) %>% mutate(n_frac = 1 / n() ) %>% ungroup() %>%
          group_by(C1) %>% summarise(n_frac = sum(n_frac)) %>% ungroup() %>%
          arrange(desc(n_frac)) %>% drop_na(C1) %>% slice(1:top_n) 
      )  %>%
      bind_cols(
        x %>% select(EID, C1) %>%
          unnest(C1) %>% unnest(C1_CN) %>% drop_na() %>%
          group_by(EID) %>% mutate(n_frac = 1 / n() ) %>% ungroup() %>%
          group_by(C1_CN) %>% summarise(n_frac = sum(n_frac)) %>% ungroup() %>%
          arrange(desc(n_frac)) %>% drop_na(C1_CN) %>% slice(1:top_n) 
      )  %>%
      bind_cols(
        x %>% select(DE) %>%
          unnest(DE) %>%  drop_na(DE) %>%
          count(DE, sort = TRUE) %>% slice(1:top_n)
      ) %>%
      mutate_at(.vars = vars(contains("_n"), contains("_frac")), round, digits = 0) 
    
    if(plot == TRUE){
      colnames(x_sum)  %<>% str_replace("[:digit:]+", "") %>% str_replace("_int", "") %>% str_replace("_frac", ".f")
      
      x_sum %<>% kable() %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = 10) %>%
        column_spec(c(2, 4, 6, 8), border_right = T) %>% 
        add_header_above(c("Authors" = 2, "Journals" = 2, "Institutions" = 2, "countries" = 2, "Keywords" = 2)) 
    }
    return(x_sum)
  }
  
  ### For top-publications network based
  if(what == "top"){
    x_sum <- x %>%
      select(com, AU1, PY, TI, SO, dgr_int) %>%
      group_by(com) %>% arrange(desc(dgr_int)) %>% slice(1:top_n) %>% ungroup() %>% 
      select(-com) %>%
      mutate(TI = TI %>% str_trunc(150), SO = SO %>% str_trunc(45)) 
    
    if(plot == TRUE){
      colnames(x_sum)  %<>% str_replace("[:digit:]+", "") %>% str_replace("_int", "") %>% str_replace("_frac", ".f")
      x_sum %<>% kable() %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = 10)
    }
    return(x_sum)
  }
  
  ### For general summary network based   
  if(what == "general"){
    
    x_sum <- x %>%
      select(com, EID, dgr_int, AU) %>%
      unnest(AU) %>% drop_na(AU) %>% clean_AU() %>%
      group_by(EID, AU) %>% mutate(dgr_int_frac = dgr_int / n()) %>% ungroup() %>%
      group_by(com, AU) %>% summarise(dgr_int_frac = sum(dgr_int_frac)) %>% ungroup() %>%
      group_by(com) %>% arrange(desc(dgr_int_frac)) %>% drop_na(AU) %>% slice(1:top_n) %>% ungroup() %>%
      select(AU, dgr_int_frac) %>%
      bind_cols(
        x %>% select(com, EID, SO, dgr_int) %>%
          group_by(com, SO) %>% summarise(dgr_int = sum(dgr_int)) %>% ungroup() %>%
          group_by(com) %>% arrange(desc(dgr_int)) %>% drop_na(SO) %>% slice(1:top_n) %>% ungroup() %>%
          select(SO, dgr_int) %>%
          mutate(SO = SO %>% str_trunc(45))
      ) %>%
      bind_cols(
        x %>% select(com, EID, dgr_int, C1) %>%
          unnest(C1) %>% unnest(C1) %>% drop_na(C1) %>%
          group_by(EID, C1) %>% mutate(dgr_int_frac = dgr_int / n()) %>% ungroup() %>%
          group_by(com, C1) %>% summarise(dgr_int_frac = sum(dgr_int_frac)) %>% ungroup() %>%
          group_by(com) %>% arrange(desc(dgr_int_frac)) %>% drop_na(C1) %>% slice(1:top_n) %>% ungroup() %>%
          select(C1, dgr_int_frac) %>%
          mutate(C1 = C1 %>% str_trunc(45)) 
      ) %>%
      bind_cols(
        x %>% select(com, EID, dgr_int, DE) %>%
          unnest(DE) %>% drop_na(DE) %>%
          group_by(com, DE) %>% summarise(dgr_int = sum(dgr_int)) %>% ungroup() %>%
          group_by(com) %>% arrange(desc(dgr_int)) %>% drop_na(DE) %>% slice(1:top_n) %>% ungroup() %>%
          select(DE, dgr_int)
      ) %>%
      bind_cols(
        x %>% select(com, CR) %>%
          unnest(CR) %>% mutate(CR = paste0(CR_AU1, " (" , CR_PY, ") ", CR_SO %>% str_trunc(45) ) )  %>% drop_na(CR_SID) %>%
          group_by(com, CR) %>% summarise(n = n()) %>% ungroup() %>%
          group_by(com) %>% arrange(desc(n)) %>% drop_na(com) %>% slice(1:top_n) %>% ungroup() %>%
          select(CR, n)
      ) %>%
      mutate_at(.vars = vars(contains("_int")), round, digits = 0) 
    
    if(plot == TRUE){
      colnames(x_sum)  %<>% str_replace("[:digit:]+", "") %>% str_replace("_int", "") %>% str_replace("_frac", ".f")
      x_sum %<>% kable() %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = 10) %>%
        column_spec(c(2, 4, 6, 8), border_right = T) %>% 
        add_header_above(c("Authors" = 2, "Journals" = 2, "Institutions" = 2, "Keywords" = 2,"Most cited References" = 2))
    }
    return(x_sum)
  }
}



# M %>%
#   select(com, topic) %>% drop_na() %>%
#   group_by(com, topic) %>%
#   summarize(n = n()) %>%
#   ungroup() %>%
#   group_by(com) %>%
#   mutate(share = n / sum(n)) %>%
#   arrange(com, desc(share)) %>%
#   slice(1:10) %>%
#   ungroup() 

##################################################################
##	BEGIN: gen_summary_ref():
##################################################################
# Summaru functions references
gen_summary_ref <- function(x, top_n = top_n, what = what, plot = plot){
  
  ### For summery absed on citation counts
  if(what == "count"){
    x_sum <- x %>%
      select(AU, N_AU, TC) %>%
      mutate(TC_frac = TC / N_AU, n_frac = 1 / N_AU) %>%
      unnest(AU) %>% drop_na() %>%
      group_by(AU) %>% summarise(TC_frac = sum(TC_frac), n_frac = sum(n_frac), TC_n = sum(TC) / n() ) %>% ungroup() %>%
      arrange(desc(TC_frac)) %>% slice(1:top_n) %>%
      bind_cols(
        x %>% select(SO, TC) %>%
          group_by(SO) %>% summarise(TC = sum(TC), n = n(), TC_n = sum(TC) / n() ) %>% ungroup() %>%
          arrange(desc(TC)) %>% drop_na(SO) %>% slice(1:top_n) %>%
          mutate(SO = str_trunc(SO, 35))
      ) %>%
      bind_cols(
        x %>% 
          select(SR, TC) %>%
          arrange(desc(TC)) %>% drop_na(SR) %>% slice(1:top_n) 
      ) %>%
      mutate_at(.vars = vars(contains("_n"), contains("_frac")), round, digits = 0) 
    
    if(plot == TRUE){
      colnames(x_sum)  %<>% str_replace("[:digit:]+", "") %>% str_replace("_int", "") %>% str_replace("_frac", ".f")
      
      x_sum %<>% kable() %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = 10) %>%
        column_spec(c(4, 8), border_right = T) %>% 
        add_header_above(c("Authors" = 4, "Journals" = 4, "References" = 2))   
    }
  }
  
  ### For top-references network based
  if(what == "top"){
    cat("Sorry, noy available for citation review.")
    break
  }
  
  ### For general summary network based 
  if(what == "general"){
    x_sum <- x %>%
    select(com, SID, AU, dgr_int) %>%
      unnest(AU) %>% drop_na() %>% clean_AU() %>%
      group_by(SID, AU) %>% mutate(dgr_int_frac = dgr_int / n()) %>% ungroup() %>%
      group_by(com, AU) %>% summarise(dgr_int_frac = sum(dgr_int_frac)) %>% ungroup() %>%
      group_by(com) %>% arrange(desc(dgr_int_frac)) %>% drop_na(AU) %>% slice(1:top_n) %>% ungroup()  %>% select(AU, dgr_int_frac) %>%
      bind_cols(
        x %>% select(com, SO, dgr_int) %>% 
          group_by(com, SO) %>% summarise(dgr_int = sum(dgr_int)) %>% ungroup() %>%
          group_by(com) %>% arrange(desc(dgr_int)) %>% drop_na(SO) %>% slice(1:top_n) %>% ungroup() %>% select(SO, dgr_int) %>% 
          mutate(SO = str_trunc(SO, 45)) 
      ) %>%
      bind_cols(
        x %>% 
          select(com, SR, dgr_int) %>%
          group_by(com) %>% arrange(desc(dgr_int)) %>% drop_na(SR) %>% slice(1:top_n) %>% ungroup() %>% select(SR, dgr_int) 
      ) %>%
      mutate_at(.vars = vars(contains("_int")), round, digits = 0) 
    
    if(plot == TRUE){
      colnames(x_sum)  %<>% str_replace("[:digit:]+", "") %>% str_replace("_int", "") %>% str_replace("_frac", ".f")
      x_sum %<>% kable() %>% 
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE, font_size = 10) %>%
        column_spec(c(2, 4), border_right = T) %>% add_header_above(c("Authors" = 2, "Journals" = 2, "References" = 2))  
    }
  }
  
  return(x_sum)
}


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
##	Plotting
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

##################################################################
##	BEGIN: gg_color_hue():
##################################################################
# Little helper for teh standard Ggplot palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

##################################################################
##	BEGIN: gg_color_select():
##################################################################
# Little to make sure the colors of one or more plots are always aligned with a certain palette

gg_color_select <- function(x, cat, pal = NULL) {
  require(RColorBrewer)
  cat <-   rlang::enquo(cat)

  n_col <- x %>% distinct(!!cat) %>% pull()
  
  if(is.null(pal)){
    mycol <- gg_color_hue(length(n_col))
  } 
  if(!is.null(pal)){
    mycol <- brewer.pal(length(n_col), pal)
  }
  names(mycol) <- n_col
  return(mycol)
}
  

##################################################################
##	BEGIN: plot_timeline():
##################################################################
# Nicely plotting a timeline...
plot_summary_timeline <- function(x, y1, y2, by, y1_text = "Number", y2_text = "Share", pal = NULL, select_cat = NULL, PY_min = NULL, PY_max = NULL,
                                  label = FALSE){
  require(RColorBrewer)
  require(directlabels)
  require(ggpubr)
  
  # Define the variables  
  y1 <- rlang::enquo(y1)
  y2 <- rlang::enquo(y2)
  by <- rlang::enquo(by)
  
  x %<>% arrange(!!by, PY)
  
  # select subcategories if necessary
  if(!is.null(select_cat)){ x %<>% filter(!!by  %in% select_cat)}
  if(!is.null(PY_min)){ x %<>% filter(PY >= PY_min)}
  if(!is.null(PY_max)){ x %<>% filter(PY <= PY_max)}

  # colors
  mycol <- x %>% gg_color_select(cat = !!by, pal = pal)
  
  # generate the plots
  x %<>% select(PY, !!y1, !!y2, !!by) 
  
  p1 <- x %>% ggplot(aes(x = PY, y = !!y1, col = factor(!!by) )) +
    geom_line(size = 1, alpha = 0.8, show.legend = FALSE) +
    labs(x = "Year", y = y1_text)  +
    scale_colour_manual(name = "Legend", values = mycol)
  
  p2 <- x %>% ggplot(aes(x = PY, y = !!y2, fill = factor(!!by))) +
    geom_area(position = "stack") +
    labs(x = "Year", y = y2_text)  +
    scale_fill_manual(name = "Legend", values = mycol)
  
  if(label == TRUE){
    p1 <- p1 + geom_dl(aes(label = !!by), method = list("last.bumpup", cex =0.75, hjust = 0.5, colour = "black"))
    p2 <- p2 +  geom_text(data = x %>% filter(PY == max(PY)) %>% arrange(desc(!!by)) %>% mutate(pos = cumsum(!!y2) - (!!y2 / 2)),
                           aes(x = max(PY), y = pos, label =  !!by), size = 3) 
  }
  
  out <- ggarrange(p1, p2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
  
  return(out)
}
  
  
  
