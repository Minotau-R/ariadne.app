
#' @importFrom ariadne ariadne
#' @importFrom igraph as_undirected
#' @importFrom visNetwork renderVisNetwork visIgraph visNodes visEdges visOptions visInteraction
server <- function(input, output) {
    
    graph <- ariadne() |>
        as_undirected(mode = "collapse")
    
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

#' @importFrom shiny fluidPage
#' @importFrom visNetwork visNetworkOutput
ui <- function(){
    fluidPage(
        visNetworkOutput("ariadne", height = "100vh", width = "100vw")
    )
}

#' @export
#' @importFrom shiny shinyApp
shinePath <- function(){
    shinyApp(ui = ui(), server = server)
}
