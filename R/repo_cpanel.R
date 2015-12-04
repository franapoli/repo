
repo_cpanel <- function(reporoot=NULL, env=globalenv()){

    if(require(shiny)) {
        library(shiny)
    } else {
        stop("You need to install the Shiny library to use repo_cpanel")
    }
                          
    
    if(is.null(reporoot))
        repo <- repo_open() else repo <- repo_open(reporoot)
    
    items <- sapply(repo$entries(), get, x="name")    
    
    shinyApp(
        server = function(input, output)
            {
                output$itemDetails_name <- renderText({
                    input$items
                })
                output$itemDetails_description <- renderText({
                    repo$entries()[[which(items==input$items)]]$description
                })
                output$itemDetails_tags <- renderText({
                    paste(repo$entries()[[which(items==input$items)]]$tags,
                          collapse=", ")
                })
                output$itemDetails_dimensions <- renderText({
                    paste(repo$entries()[[which(items==input$items)]]$dims,
                          collapse="x")
                })
                output$itemDetails_size <- renderText({
                    paste(repo$entries()[[which(items==input$items)]]$size)
                })
                output$itemDetails_time <- renderText({
                    paste(repo$entries()[[which(items==input$items)]]$timestamp)
                })
                output$itemDetails_source <- renderText({
                    paste(repo$entries()[[which(items==input$items)]]$source,
                          collapse=", ")
                })
                output$itemDetails_attached <- renderText({
                    paste(repo$entries()[[which(items==input$items)]]$attachedto,
                          collapse=", ")
                })
                output$itemDetails_depends <- renderText({
                    paste(repo$entries()[[which(items==input$items)]]$depends,
                          collapse=", ")
                })
                output$itemDetails_md5 <- renderText({
                    paste(repo$entries()[[which(items==input$items)]]$checksum,
                          collapse=", ")
                })
                output$itemDetails_path <- renderText({
                    paste(file.path(repo$root(), repo$entries()[[which(items==input$items)]]$dump))
                })


                output$items <- renderUI({
                    selectInput("items", "Select item", choices=items)
                })
                output$pies <- renderPlot({
                    repo$pies()
                })

                observeEvent(input$loadButton, {
                                 assign(input$items, repo$get(input$items), envir=env)
                             })
            },

        
        ui=fluidPage(

            titlePanel("Repo Control Panel"),

            sidebarLayout(
                sidebarPanel(
                    h6(paste0("Current repo: ", repo$root())),
                    uiOutput("items"),
                    actionButton("loadButton", "Load into workspace"),
                    h4("Items size overview"),
                    plotOutput("pies")
                    ),

                mainPanel(
                    h2("Item details"),
                    tags$p("Name:"),
                    verbatimTextOutput("itemDetails_name"),
                    tags$p("Description:"),
                    verbatimTextOutput("itemDetails_description"),
                    tags$p("Tags:"),
                    verbatimTextOutput("itemDetails_tags"),
                    tags$p("Dimensions:"),
                    verbatimTextOutput("itemDetails_dimensions"),
                    tags$p("Size on disk:"),
                    verbatimTextOutput("itemDetails_size"),
                    tags$p("Timestamp:"),
                    verbatimTextOutput("itemDetails_time"),
                    tags$p("Source:"),
                    verbatimTextOutput("itemDetails_source"),                                        
                    tags$p("Attached to:"),
                    verbatimTextOutput("itemDetails_attached"),
                    tags$p("Depends on:"),
                    verbatimTextOutput("itemDetails_depends"),
                    tags$p("MD5 checksum:"),
                    verbatimTextOutput("itemDetails_md5"),
                    tags$p("Stored in:"),
                    verbatimTextOutput("itemDetails_path")
                    )
                )
            )
        )
}
