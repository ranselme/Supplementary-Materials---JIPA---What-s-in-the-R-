#--------------------------/PREPARATION/--------------------------#

# Generation of a nice sankey diagrams.
# Use of networkD3 for the diagrams

create_Sankey_diagram_both <- function(table){
  
  #Adapted from https://r-graph-gallery.com/321-introduction-to-interactive-sankey-diagram-2.html
  
  links <- table %>% 
    dplyr::mutate(Phoneme_chart = dplyr::case_when(
      stringr::str_detect(phoneme,"r") & stringr::str_detect(phoneme,"ɾ") == FALSE ~ "r",
      stringr::str_detect(phoneme,"ɾ") & stringr::str_detect(phoneme,"r") == FALSE ~ "ɾ",
      stringr::str_detect(phoneme,"ɾ") & stringr::str_detect(phoneme,"r") ~ "ɾ & r",
      stringr::str_detect(phoneme,"ɾ") == FALSE & stringr::str_detect(phoneme,"r") == FALSE ~ "None"),
      Phonemes = dplyr::case_when(
        phonemic == 0 & tap_pm == 0 ~ "-/r/-/ɾ/",
        phonemic > 0 & tap_pm == 0 ~ "+/r/-/ɾ/",
        phonemic == 0 & tap_pm > 0 ~ "-/r/+/ɾ/",
        phonemic > 0 & tap_pm > 0 ~ "+/r/+/ɾ/"),
      Phones = dplyr::case_when(
        phonetic == 0 & tap_pt == 0 ~ "-[r]-[ɾ]",
        phonetic > 0 & tap_pt == 0 ~ "+[r]-[ɾ]",
        phonetic == 0 & tap_pt > 0 ~ "-[r]+[ɾ]",
        phonetic > 0 & tap_pt > 0 ~ "+[r]+[ɾ]")) %>% 
    dplyr::select(Phoneme_chart, Phonemes, Phones)
  
  links <- dplyr::bind_rows(dplyr::select(dplyr::rename(links,
                                                        source=Phonemes,
                                                        target=Phoneme_chart),source,target),
                            dplyr::select(dplyr::rename(links,
                                                        source=Phoneme_chart,
                                                        target=Phones),source,target)) %>% 
    dplyr::mutate(value = 1) 
  
  
  links <- links %>% 
    dplyr::count(source,target) %>% 
    dplyr::rename(value = n)  
  
  nodes <- data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique()
  )
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  # Make the Network
  p <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                                Source = "IDsource", Target = "IDtarget",
                                Value = "value", NodeID = "name", 
                                sinksRight=FALSE)
  p}

create_Sankey_diagram_phonetic <- function(table){
  
  #Adapted from https://r-graph-gallery.com/321-introduction-to-interactive-sankey-diagram-2.html
  
  links_pt <- table %>% 
    dplyr::mutate(Phoneme_chart = dplyr::case_when(
      stringr::str_detect(phoneme,"r") & stringr::str_detect(phoneme,"ɾ") == FALSE ~ "r",
      stringr::str_detect(phoneme,"ɾ") & stringr::str_detect(phoneme,"r") == FALSE ~ "ɾ",
      stringr::str_detect(phoneme,"ɾ") & stringr::str_detect(phoneme,"r") ~ "ɾ & r",
      stringr::str_detect(phoneme,"ɾ") == FALSE & stringr::str_detect(phoneme,"r") == FALSE ~ "None"),
      Phones = dplyr::case_when(
        phonetic == 0 & tap_pt == 0 ~ "-[r]-[ɾ]",
        phonetic > 0 & tap_pt == 0 ~ "+[r]-[ɾ]",
        phonetic == 0 & tap_pt > 0 ~ "-[r]+[ɾ]",
        phonetic > 0 & tap_pt > 0 ~ "+[r]+[ɾ]")) %>% 
    dplyr::select(Phoneme_chart, Phones) %>%
    dplyr::rename(source=Phoneme_chart,
                  target=Phones) %>% 
    dplyr::mutate(value = 1) %>%
    dplyr::count(source,target) %>% 
    dplyr::rename(value = n) #%>% 
  #dplyr::mutate(target = paste(target ,value, sep = " -> "))
  
  nodes <- data.frame(
    name=c(as.character(links_pt$source), 
           as.character(links_pt$target)) %>% unique()
  )
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links_pt$IDsource <- match(links_pt$source, nodes$name)-1 
  links_pt$IDtarget <- match(links_pt$target, nodes$name)-1
  
  # Make the Network
  p_pt <- networkD3::sankeyNetwork(Links = links_pt, Nodes = nodes,
                                   Source = "IDsource", Target = "IDtarget",
                                   Value = "value", NodeID = "name",
                                   sinksRight=FALSE)
  
  p_pt
    
}

create_Sankey_diagram_phonemic <- function(table){
  
  #Adapted from https://r-graph-gallery.com/321-introduction-to-interactive-sankey-diagram-2.html
  
  links_pm <- table %>% 
    dplyr::mutate(Phoneme_chart = dplyr::case_when(
      stringr::str_detect(phoneme,"r") & stringr::str_detect(phoneme,"ɾ") == FALSE ~ "r",
      stringr::str_detect(phoneme,"ɾ") & stringr::str_detect(phoneme,"r") == FALSE ~ "ɾ",
      stringr::str_detect(phoneme,"ɾ") & stringr::str_detect(phoneme,"r") ~ "ɾ & r",
      stringr::str_detect(phoneme,"ɾ") == FALSE & stringr::str_detect(phoneme,"r") == FALSE ~ "None"),
      Phonemes = dplyr::case_when(
        phonemic == 0 & tap_pm == 0 ~ "-/r/-/ɾ/",
        phonemic > 0 & tap_pm == 0 ~ "+/r/-/ɾ/",
        phonemic == 0 & tap_pm > 0 ~ "-/r/+/ɾ/",
        phonemic > 0 & tap_pm > 0 ~ "+/r/+/ɾ/")) %>% 
    dplyr::select(Phoneme_chart, Phonemes) %>%
    dplyr::rename(source=Phoneme_chart,
                  target=Phonemes) %>% 
    dplyr::mutate(value = 1) %>% 
    
    
    dplyr::count(source,target) %>% 
    dplyr::rename(value = n) #%>% 
  #dplyr::mutate(target = paste(target ,value, sep = " -> "))
  
  
  nodes <- data.frame(
    name=c(as.character(links_pm$source), 
           as.character(links_pm$target)) %>% unique()
  )
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links_pm$IDsource <- match(links_pm$source, nodes$name)-1 
  links_pm$IDtarget <- match(links_pm$target, nodes$name)-1
  
  # Make the Network
  p_pm <- networkD3::sankeyNetwork(Links = links_pm, Nodes = nodes,
                                   Source = "IDsource", Target = "IDtarget",
                                   Value = "value", NodeID = "name", 
                                   sinksRight=FALSE)
  p_pm
  
}
