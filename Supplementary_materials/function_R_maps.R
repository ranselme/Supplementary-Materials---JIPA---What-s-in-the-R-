#--------------------------/PREPARATION/--------------------------#

# Generation of a nice blank map to add data to.
# Removal of Antarctica from the map.
# Use of GGplot2 for the maps

world_map <- ggplot2::map_data("world") %>% 
              dplyr::filter(region != "Antarctica")

basic4map <- ggplot2::ggplot() + 
  ggplot2::coord_fixed() +
  ggplot2::xlab("") +
  ggplot2::ylab("")

#Add map to base plot
base_world <- basic4map +
  ggplot2::geom_polygon(data=world_map,
                        ggplot2::aes(x=long,
                                     y=lat,
                                     group=group), 
                        colour="grey",
                        fill="grey") +
  ggplot2::theme(panel.background = ggplot2::element_rect(
    size = 0.5,
    linetype = "solid"),
    panel.grid.major = ggplot2::element_line(
      size = 0.5,
      linetype = 'solid',
      colour = "gray90"), 
    panel.grid.minor = ggplot2::element_line(
      size = 0.25,
      linetype = 'solid',
      colour = "gray90")) +
  ggplot2::labs(title = "World map of Sounds")

rm(basic4map)
