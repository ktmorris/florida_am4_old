theme_bc <- function(...){
  library(ggplot2)
  library(extrafont)
  theme(plot.title = element_text(family = "Roboto Black", 
                                  color = "black", hjust = 0.5, margin = margin(0, 0, 10, 
                                                                                0)), axis.title = element_text(family = "Roboto", 
                                                                                                               color = "#525353"), axis.line = element_line(colour = "black"), 
        text = element_text(family = "Roboto", color = "#525353"), 
        plot.caption = element_text(family = "Roboto", size = 8), 
        panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#dddddb"), plot.background = element_rect(fill = "#dddddb"), 
        panel.grid.minor = element_blank(), legend.key = element_blank(), 
        legend.background = element_rect(color = "black", fill = "#dddddb"), 
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        plot.margin = margin(10, 10, 10, 10))
}