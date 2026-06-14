
#' @importFrom ariadne ariadne drawPath
server <- function(input, output) {
    
    graph <- ariadne() |>
        as_undirected(mode = "each")
    
    vertex_attr(graph, "id") <- V(graph)$name
    edge_attr(graph, "id") <- seq_along(E(graph))
    
    node_df <- as_data_frame(graph, what = "vertices")
    edge_df <- as_data_frame(graph, what = "edges")
    
    output$network <- renderVisNetwork({
        
        visIgraph(graph, randomSeed = 123) |>
            visNodes(color = "darkorange") |>
            visEdges(color = list(color = "lightgrey", highlight = "red")) |>
            visInteraction(
                dragNodes = FALSE,
                multiselect = TRUE,
                selectConnectedEdges = FALSE
            ) |>
            visEvents(
                select = "function(x) {
                    Shiny.onInputChange('net_data', x);
                ;}"
            )
    })
    
    observe({
        
        if( is.null(input$resource) ){
            node_df$hidden <- FALSE
            edge_df$hidden <- FALSE
        }else{
            edge_selected <- edge_df$source %in% input$resource
            
            nodes <- unique(
                c(edge_df$from[edge_selected], edge_df$to[edge_selected])
            )
            
            edge_df$hidden <- !edge_selected
            node_df$hidden <- !node_df$id %in% nodes
        }
        
        visNetworkProxy("network") |>
            visUpdateNodes(nodes = node_df) |>
            visUpdateEdges(edges = edge_df)
    })
    
    observe({
        
        nodes <- unlist(input$net_data$nodes)
        req(length(nodes) < 2L)
        
        edge_df$label <- " "
        
        if( !is.null(nodes) ){
            
            visNetworkProxy("network") |>
                visUpdateEdges(edges = edge_df) |>
                visSetSelection(nodesId = nodes, highlightEdges = FALSE)
        }
        
        updateTextInput(inputId = "by", value = NA)
        updateNumericInput(inputId = "k", value = 1)
        updateSelectInput(inputId = "include", selected = NA)
        updateSelectInput(inputId = "exclude", selected = NA)
    })
    
    observe({
        
        nodes <- unlist(input$net_data$nodes)
        req(length(nodes) > 1L)
        
        path_by <- c(nodes[1], nodes[length(nodes)]) |>
            paste(collapse = "~") |>
            as.formula()
        
        include <- if(length(nodes) > 2L) nodes[2:(length(nodes) - 1)] else NULL
        
        updateTextInput(inputId = "by", value = deparse(path_by))
        updateSelectInput(inputId = "include", selected = include)
        
        path_df <- drawPath(
            graph,
            by = path_by,
            k = input$k,
            include = input$include,
            exclude = input$exclude,
            res.name = input$resource
        )
        
        edges <- get_edge_ids(graph, path_df)
        
        edge_selected <- edge_df$id %in% edges
        node_selected <- node_df$id %in% union(path_df$from, path_df$to)
        
        edge_df$label <- ifelse(edge_selected, edge_df$source, " ")
        
        node_df$hidden <- if(input$prune) !node_selected else FALSE
        edge_df$hidden <- if(input$prune) !edge_selected else FALSE
        
        visNetworkProxy("network") |>
            visUpdateNodes(nodes = node_df) |>
            visUpdateEdges(edges = edge_df) |>
            visSetSelection(
                nodesId = nodes,
                edgesId = edges,
                highlightEdges = FALSE
            )
        
        if( input$focus ){
            
            visNetworkProxy("network") |>
                visFit(nodes = nodes)
        }
    })
}


#' @importFrom htmltools br div
#' @importFrom bslib page_sidebar bs_theme sidebar navset_tab nav_panel accordion accordion_panel
ui <- function(){
    
    graph <- ariadne()
    
    page_sidebar(
        fillable = FALSE,
        theme = bs_theme(bootswatch = "united"),
        sidebar = sidebar(navset_tab(
            
            nav_panel("Explore", br(),
                
                textInput("by", "Path:", placeholder = "from ~ to"),
                
                numericInput("k", "k:", value = 1, min = 1),
                
                selectInput("include", "Include:", choices = V(graph)$name,
                    selected = NULL, multiple = TRUE),
                
                selectInput("exclude", "Exclude:", choices = V(graph)$name,
                    selected = NULL, multiple = TRUE),
                
                selectInput("resource", "Resource:",
                    choices = unique(E(graph)$source), selected = NULL,
                    multiple = TRUE),
            
            checkboxInput("focus", "Focus"),
            checkboxInput("prune", "Prune"),
            
            accordion(open = FALSE, height = "80%",
                    
                accordion_panel("Advanced",
                    
                    numericInput("buffer", "Buffer factor:", value = 2,
                        min = 1, step = 1),
                        
                    numericInput("max_attempt", "Max attempts:", value = 5,
                        min = 1, step = 1))),
            
            div(style = "margin-top: +20px"),
            
            actionButton("download", "Download", class = "btn-warning",
                icon = icon("download"), style = "float: right;")),
                
            nav_panel("Weave", br(),
                
                tags$style(HTML("
                    .shiny-options-group .radio-inline {margin-right: 1rem;}
                ")),
                
                radioButtons("weave_type", "Type:",
                    choices = c("simple", "complex"), inline = TRUE),
                
                radioButtons("init_type", "Initial values as:",
                    choices = c("text", "file"), inline = TRUE),
            
                conditionalPanel(condition = "input.init_type == 'text'",
            
                    selectizeInput("init_text", "Initial values:",
                        choices = NULL, multiple = TRUE,
                        options = list(create = TRUE))),
            
                conditionalPanel(condition = "input.init_type == 'file'",
            
                    fileInput("init_file", "Initial values:",
                        accept = c("csv", "tsv"))),
                
                checkboxInput("names", "Add names", value = TRUE),
                
                accordion(open = FALSE, height = "80%",
                    
                    accordion_panel("Advanced",
                        
                        checkboxInput("weave_prune", "Prune", value = TRUE),        
                                    
                        numericInput("batch_size", "Batch size:",
                            value = NULL, min = 1),
                        
                        numericInput("factor", "Jobs per unit:", value = 3,
                            min = 1, step = 1))),
                
                div(style = "margin-top: +20px"),
                
                conditionalPanel(condition = "input.weave_type == 'complex'",
                
                    sliderInput("weave_complex", "Threshold:", min = 0, max = 1,
                        value = 0, step = 0.01, ticks = FALSE)),
            
                actionButton("weave", "Weave", class = "btn-warning",
                    icon = icon("pencil"), style = "float: right;")))),
        
        visNetworkOutput("network", height = "100vh", width = "100vw")
    )
}


#' @export
shinePath <- function(){
    shinyApp(ui = ui(), server = server)
}
