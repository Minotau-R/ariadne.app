
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
    
    observe({
        visNetworkProxy("network_proxy_nodes") %>%
            visNodes(color = input$color)
    })
}


#' @importFrom bslib page_sidebar bs_theme sidebar
#' @importFrom visNetwork visNetworkOutput
ui <- function(){
    
    page_sidebar(
        fillable = FALSE,
        theme = bs_theme(bootswatch = "united"),
        sidebar = sidebar(
            
            textInput("formula", "Path:", placeholder = "from ~ to"),
            
            numericInput("k", "k:", value = 1, min = 1),
            
            selectInput(
                "include", "Include:", choices = NULL, multiple = TRUE
            ),
            
            selectInput(
                "exclude", "Exclude:", choices = NULL, multiple = TRUE
            ),
            
            selectInput(
                "resource", "Resource:", multiple = TRUE,
                choices = NULL
            ),
            
            tags$style(HTML("
                .shiny-options-group .radio-inline {margin-right: 1rem;}
            ")),
            
            radioButtons(
                "init_type", "Initial values as:", choices = c("text", "file"),
                inline = TRUE,
            ),
            
            conditionalPanel(
                condition = "input.init_type == 'text'",
            
                selectizeInput(
                    "init_text", "Initial values:", choices = NULL,
                    multiple = TRUE, options = list(create = TRUE)
                )
            ),
            
            conditionalPanel(
                condition = "input.init_type == 'file'",
            
                fileInput(
                    "init_file", "Initial values:", accept = c("csv", "tsv")
                )
            ),

            checkboxInput("focus", "Focus"),
            checkboxInput("prune", "Prune"),
            
            actionButton("weave", "Weave", class = "btn-warning", icon = icon("pencil"))
        ),
        visNetworkOutput("ariadne", height = "100vh", width = "100vw"),
    )
}


#' @export
shinePath <- function(){
    shinyApp(ui = ui(), server = server)
}
