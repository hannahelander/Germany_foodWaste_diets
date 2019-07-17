library(plotly)




data1 <- as.data.frame(t(Diets[1,1:length(Diets)-1]))
data2 <- as.data.frame(t(Diets_clean[2,1:length(Diets_clean)-1]))

# SQ in terms of DGE_groups
data3 <- as.data.frame(t(Diets_DGEgroups))

p <- plot_ly(data3, labels = rownames(data3), values = ~SQ_capita,  type = 'pie') %>%
  layout(title = 'Status Quo',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

p <- plot_ly(data, labels = rownames(data), values = ~SQ_percentage,  type = 'pie') %>%
  layout(title = 'Status Quo',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



p





###########

p <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c(colnames(supply_chain_Y[4:8])),
    color = c("blue", "blue", "blue", "blue", "blue", "blue"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = c(0,1,0,2,3,3),
    target = c(2,3,3,4,4,5),
    value =  c(8,4,2,8,4,2)
  )
) %>% 
  layout(
    title = "Basic Sankey Diagram",
    font = list(
      size = 10
    )
  )

chart_link = api_create(p, filename="sankey-basic-example")
chart_link
