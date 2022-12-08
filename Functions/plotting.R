#Plot a linear model for flipper length as a function of body mass
lm_bodymass_flipperlength<-function(dataset2){
  ggplot(data = dataset2, 
         aes(x = flipper_length_mm,
             y = body_mass_g)) +
    geom_point(aes(color = species, 
                   shape = species),
               size = 2,
               alpha = 0.7) + geom_smooth(method = 'lm', colour = "royalblue3",  se = FALSE) +
    scale_color_manual(values = c("springgreen3","purple","cyan4")) +
    labs(tag = "Figure. 1", title = "Penguin body mass and flipper length, Palmer Station",
         subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins", caption = "Figure 1: Shows the linear model for flipper length as a function of body mass for all 3 species of penguin", 
         x = "Flipper length (mm)",
         y = "Body mass (g)",
         color = "Penguin species",
         shape = "Penguin species") +
    theme(legend.position = c(1, 0), legend.justification = c(1, 0)) + theme( plot.title = element_text(hjust = 0.5, vjust = 0.5),
                                                                              plot.subtitle = element_text(hjust = 0.5),
                                                                              plot.title.position = "plot",
                                                                              plot.caption = element_text(hjust = 0.1, vjust = 0.5, face= "italic"),
                                                                              plot.caption.position = "panel", 
                                                                              plot.tag= element_text(size=12, vjust = 2)) +  theme_bw()

}


#saving images 
#Saving image as a PNG, define the size and scaling
save_plot_png<- function(dataset2, 
                                  filename, width, height, res, scaling){
  agg_png(filename, width   =  width, 
          height  =  height, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  linearmodel <- lm_bodymass_flipperlength(dataset2)
  print(linearmodel)
  dev.off()
}



#Saving image as a SVG
save_plot_svg <- function(dataset2, filename, width, height, scaling){
  width_inches = width/2.54
  height_inches = height/2.54
  svglite(filename, width = width_inches, height = height_inches, scaling = scaling)
  linearmodel <- lm_bodymass_flipperlength(dataset2)
  print(linearmodel)
  dev.off()
}
