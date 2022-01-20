'
  Script: Visualizations males results at rest
  Input: output of analysis_males.R
  Output: Figure 4 A-C paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Environment_preparation -------------------------------------------------
source("src/analysis_males.R")

# Visualization -----------------------------------------------------------


fig_mono <- list()
for(i in unique(males_mod$out_mono)) {
  print(i)
  
  fig_mono[[i]] <- males_mod %>%
    filter(out_mono == i) %>%
    mutate(
      ba_grouped = str_replace_all(ba_grouped, "_", " \n"), 
      my_col = ifelse(n_id > 2, "enough", "not_enough"), 
      lab_sig = ifelse(n_id < 3, "", lab_sig)
    ) %>%
    
    ggplot(aes(factor(ba_grouped, levels = ba_order), g), 
           color = my_col) +
    geom_bar(stat = "identity", aes(color = my_col), fill = "white") +
    geom_errorbar(aes(ymin = g-sem, ymax = g+sem, color = my_col), width = 0.2) +
    geom_hline(yintercept = 0, color = "black") +
    
    my_theme +
    labs(x="", y = "Hedge's g") +
    scale_color_manual(values = c("black", "grey75")) + 
    
    facet_grid(str_replace_all(outcome, "_", " \n")~., scales = "free_y",
               switch = "both") +
    labs(x = "Brain areas", y = "g(se)", title = str_replace_all(i, "_", " ")) +
     geom_text(aes(label = lab_sig, y = 0.1)) + 
    theme(text = element_text(size = 20),
          legend.position = "none") + 
    theme(panel.spacing = unit(0.75, "lines")) +
    theme(strip.background = element_rect(color = "white")) +       
    theme(strip.text.y = element_text(size = 10, colour = "black",face = "bold")) 
  
  
}

fig_mono[[1]]      
fig_mono[[2]]
fig_mono[[3]]
fig_mono[[4]]

