projections <- read_rds("~/westcoast-networks/data/clean/Simulation/projectiondataforML.rds")

i <-1
c <- map_dfr(1:length(projections), function(p){
  
  data <- dfassemble(projections[[p]], "Class")$All 
  
  target <- as.numeric(data$Recover)
  cor_with_target <- sapply(data, function(x) cor(as.numeric(x), 
                                                  target,
                                                  use = "complete.obs"))
  
  out <- tibble(Variable = names(cor_with_target),
                 Correlation = cor_with_target,
                 Projection = names(projections[p])) %>% 
    filter(abs(Correlation) > .05,
           Variable != "Recover")
  
})

ggplot(c, aes(x = fct_reorder(Variable, Correlation), 
              y = Correlation, fill = Projection)) +
  geom_col(position = "dodge") +
  geom_point() +
  theme_minimal() +
  scale_fill_manual(values = c("#AAFAC8", "#2D728B", "#1E585C")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(1, 1, .5, 2), "lines")) +
  labs(title = "Correlation with Recovering to Prior Steady State",
       subtitle = "By Projection",
       x = "Variable")

r <- map_dfr(1:length(projections), function(p){
  
  data <- dfassemble(projections[[p]], "Reg")$All 
  
  target <- as.numeric(data$Return)
  cor_with_target <- sapply(data, function(x) cor(as.numeric(x), 
                                                  target,
                                                  use = "complete.obs"))
  
  out <- tibble(Variable = names(cor_with_target),
                Correlation = cor_with_target,
                Projection = names(projections[p])) %>% 
    filter(abs(Correlation) > .05,
           Variable != "Return")
  
})

ggplot(r, aes(x = fct_reorder(Variable, Correlation), 
              y = Correlation, fill = Projection)) +
  geom_col(position = "dodge") +
  geom_point() +
  theme_minimal() +
  scale_fill_manual(values = c("#AAFAC8", "#2D728B", "#1E585C")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(1, 1, 2, 3), "lines")) +
  labs(title = "Correlation with Return Time to Prior Steady State",
       subtitle = "By Projection",
       x = "Variable")


type <- c("Class", "Reg")
t <- type[1]
  p <- 1

  for(p in 1:3){
    data <- dfassemble(projections[[p]], "Class")$All 
  
    res.pca1 <- prcomp(select(data,-Recover), scale = TRUE)
    fviz_pca_var(res.pca1, title = names(projections[p]),
                 subtitle = "Class",
                 col.ind = data$Recover,
                 addEllipses = TRUE, # Concentration ellipses
                 ellipse.type = "confidence",
                 repel = TRUE     # Avoid text overlapping
    )
  }

for(p in 1:3){
  data <- dfassemble(projections[[p]], "Reg")$All 
  
  res.pca1 <- prcomp(select(data,-Return), scale = TRUE)
  fviz_pca_var(res.pca1, title = names(projections[p]),
               subtitle = "Reg",
               col.ind = data$Return,
               addEllipses = TRUE, # Concentration ellipses
               ellipse.type = "confidence",
               repel = TRUE     # Avoid text overlapping
  )
}

