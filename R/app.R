
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
            #visOptions(
            #    selectedBy = list(variable = "id"),
            #    highlightNearest = TRUE
            #) |>
            visInteraction(multiselect = TRUE)
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
#' @importFrom shiny shinyApp
shinePath <- function(){
    shinyApp(ui = ui(), server = server)
}
