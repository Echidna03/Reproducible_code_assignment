
#Function to plot exploratory figure
plot_explo_figure <- function(adelie_bodymass){
  adelie_bodymass %>% 
    ggplot(aes(x = island,  y = body_mass_g)) + # define the x and y axes as species and body mass respectively
    geom_jitter( width= 0.25, aes(colour= island), show.legend = FALSE) + # create a jittered point plot with a width of 0.25, colour coded based on island. Hide legend 
    xlab("Island") + ylab("Body Mass (g)")  +
    # clean up axis titles
    scale_color_manual(values = c( "#1B9E77", "#7570B3", "#E6AB02")) + # apply colourblind-friendly palette
    theme_bw() # remove grey boxes
}
#Function to save exploratory figure as a .svg file

save_explo_plot_svg <- function(adelie_bodymass, 
                                filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  explo_plot <- plot_explo_figure(adelie_bodymass)
  print(explo_plot)
  dev.off()
}
# Function to plot stats figure
stats_figure <-function(adelie_bodymass){
  ggbetweenstats(
    data = adelie_bodymass,
    x = island,
    y = body_mass_g,
    type = "parametric",
    pairwise.display = "significant",
    p.adjust.method = "holm",
    violin.args = list(width = 0),
    xlab = "Island",
    ylab = "Body Mass (g)",
    var.equal = TRUE,
    ggtheme = ggstatsplot::theme_ggstatsplot()
  )}

#Function to save qqplot figure as a .svg file

save_qqplot_svg <- function(adelie_bodymass, 
                                filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  qqplot <- qqnorm(adelie_bodymass$body_mass_g)
  print(qqplot)
  dev.off()}

#Function to save stats figure as a .svg file

save_stats_plot_svg <- function(adelie_bodymass, 
                                filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  stats_plot <- stats_figure(adelie_bodymass)
  print(stats_plot)
  dev.off()}