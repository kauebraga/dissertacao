theme_opts <- function(base_size) {
  
  theme_void(base_family = "Roboto Condensed") %+replace%
  # theme_void() %+replace%
    
    theme(
      # panel.background = element_blank(),
      plot.background = element_rect(fill="white"),
      panel.border=element_blank(),
      panel.background = element_rect(fill = "lightskyblue", colour = NA),
      panel.grid.major = element_line(color = "grey50", linetype = "dashed", 
                                      size = 0.2),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      # axis.text =element_text(size=7),
      # text = element_text(size=9),
      strip.background = element_rect(colour = "white", fill = "white", size = 11), #muda estilo de facet_grid boxes
      strip.text.x = element_text(size = 11, face ="bold"),
      # Legends  
      # legend.position=posicao_legenda, # c("da esquerda", "de baixo")
      legend.direction='vertical',
      legend.box='vertical',
      legend.title = element_blank(),
      legend.text = element_text(size=6),
      legend.key.size = unit(0.5,"line")
      # legend.key = element_rect(color = "transparent")
      #legend.key.size = unit(5, "cm"),
      #plot.title = element_text(size=22)
      
    )
}
