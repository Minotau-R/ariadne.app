
library(ariadne)
library(igraph)
library(shiny)
library(visNetwork)

graph <- ariadne() |>
    as_undirected(mode = "collapse")

server <- function(input, output) {
  output$ariadne <- renderVisNetwork({
    
    visIgraph(graph, randomSeed = 123) |>
        
        visNodes(color = "darkorange") |>
        visEdges(color = "lightgrey", value = "source") |>
        
        visOptions(
            selectedBy = list(variable = "id"),
            highlightNearest = TRUE
        ) |>
        
        visInteraction(multiselect = TRUE)
  })
}

ui <- fluidPage(
  visNetworkOutput("ariadne", height = "100vh", width = "100vw")
)

shinyApp(ui = ui, server = server)
