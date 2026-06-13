
#' @importFrom ariadne ariadne drawPath
server <- function(input, output) {
    
    graph <- ariadne() |>
        as_undirected(mode = "each")
    
    edge_attr(graph, "id") <- seq_along(E(graph))
    
    edge_df <- as_data_frame(graph, what = "edges")
    
    output$network <- renderVisNetwork({
        
        visIgraph(graph, randomSeed = 123) |>
            visNodes(color = "darkorange") |>
            visEdges(color = list(color = "lightgrey", highlight = "red")) |>
            visInteraction(multiselect = TRUE, selectConnectedEdges = FALSE) |>
            visEvents(
                select = "function(x) {
                    Shiny.onInputChange('net_data', x);
                ;}"
            )
    })
    
    observe({
        
        nodes <- unlist(input$net_data$nodes)
        req(length(nodes) == 1L && !is.null(input$net_data$edges))
        
        edge_df$label <- " "
        
        visNetworkProxy("network") |>
            visUpdateEdges(edges = edge_df) |>
            visSetSelection(nodesId = nodes, highlightEdges = FALSE)
    })
    
    observe({
        
        nodes <- unlist(input$net_data$nodes)
        req(length(nodes) > 1L)
      
        by <- c(nodes[1], nodes[length(nodes)]) |>
            paste(collapse = "~") |>
            as.formula()
        
        include <- if(length(nodes) > 2L) nodes[2:(length(nodes) - 1)] else NULL
        
        path_df <- drawPath(graph, by, k = input$k, include = include)
        
        edges <- get_edge_ids(graph, path_df)
        edge_df$label <- ifelse(edge_df$id %in% edges, edge_df$source, " ")
        
        visNetworkProxy("network") |>
            visUpdateEdges(edges = edge_df) |>
            visSetSelection(
                nodesId = nodes,
                edgesId = edges,
                highlightEdges = FALSE
            )
    })
}


ui <- function(){
    fluidPage(
        numericInput("k", "k:", value = 1, min = 1),
        visNetworkOutput("network", height = "100vh", width = "100vw")
    )
}


#' @export
shinePath <- function(){
    shinyApp(ui = ui(), server = server)
}
