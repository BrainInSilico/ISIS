
# inspired from https://github.com/tbradley1013/tree-subset-shiny

library(shiny)
library(ggtree)
library(ape)
library(ggplot2)

#' @description extract sample name by looking at the id and choosing the "right separator"
#' @param id character - the id of the sequence.
#' @details if a / is present in the id, it is assumed that the sample name is the last string after the last /
#' @details if no / is present but a "_" is present, it is assumed that the sample name is the string before the first "_"
#' @details if no / and no "_" are present, the whole id is returned with warning
#' @author Nicolas Beaume
#' @export
getSampleName <- function(id) {
    if(grepl('/', id)) {
        sampleName <- unlist(strsplit(id, split = '/'))[1]
        if(grepl('\\.', sampleName)) {
            sampleName <- unlist(strsplit(id, split = '\\.'))[1:2]
            sampleName <- paste(sampleName, collapse = '.')
        }
        return(sampleName)
    } else if(grepl('_', id)) {
        return(unlist(strsplit(id, split = '_'))[1])
    } else {
        warning(paste('no split found to differenciate sample name in', id,'. The whole id is returned'))
        return(id)
    }
}

shinyServer(function(input, output) {
    tree <- reactive({
        req(input$upload_tree$datapath)
        tree <- read.tree(input$upload_tree$datapath)
        tree$node.label <- 0:(tree$Nnode -1)
        return(tree)
    })
    
    info <- reactive({
        req(input$upload_info$datapath)
        info <- read.table(input$upload_info$datapath, h=T, sep='\t', stringsAsFactors = T)
        return(info)
    })
    
    # Rendering the ui elements and tree options (text size, height, width)
    output$treeOptions <- renderUI({
        req(input$upload_tree, tree())
        output <- tagList(
            fluidRow(
                column(
                    3,
                    numericInput(
                        inputId = "tree_text_size",
                        label = "Select label text size:",
                        min = 1,
                        value = 3
                    )
                ),
                column(
                    3,
                    numericInput(
                        inputId = "tree_plot_height",
                        label = "Select plot height",
                        value = 1200
                    )
                ),
                column(
                    3, 
                    numericInput(
                        inputId = "tree_width_multiply",
                        label = "Select plot width multiplier:",
                        value = 1.4,
                        min = 1,
                        step = 0.1
                    )
                )
            )
        )
        return(output)
    })
    
    # Rendering the ui elements and tree options (text size, height, width)
    output$infoOptions <- renderUI({
        req(input$upload_info, info())
        output <- tagList(
            fluidRow(
                column(
                    3, 
                    varSelectInput(
                        inputId = "infoToColor",
                        label = "Select the type of information to highlight:",
                        data = info()
                    )
                )
            )
        )
        return(output)
    })
    
    output$nodeSelection <- renderUI({
        req(input$upload_tree, tree())
        output <- tagList(
            fluidRow(
                column(
                    3,
                    textInput(
                        inputId = "selectedNode",
                        label = "Select Node:",
                        value = 0
                    )
                ),
                column(
                    3,
                    numericInput(
                        inputId = "subtree_plot_height",
                        label = "Select plot height",
                        value = 800
                    )
                )
            )
        )
        return(output)
    })

    p <- reactive({
        req(input$tree_width_multiply, 
            input$tree_text_size, tree())
        if(! is.null(input$upload_info$datapath)) {
            colorCol <- as.character(input$infoToColor)
            thisInfo <- data.frame(id=tree()$tip.label,
                               group=info()[,colorCol], 
                               stringsAsFactors = F)
            p <- ggtree(tree()) %<+% thisInfo +
                geom_nodelab(size=input$tree_text_size) +
                geom_tippoint(aes(color = group)) +
                theme_tree2()
        } else {
            p <- ggtree(tree()) +
                geom_nodelab(size=input$tree_text_size) +
                theme_tree2()
        }
        p + lims(x = c(0, max(p$data$x) * input$tree_width_multiply))
    })
    
    output$tree <- renderPlot({
           print(p())
    })
    
    output$downloadTree <- downloadHandler(
        filename = 'tree.jpeg',
        content = function(file) {
            ggsave(filename = file, plot = p(), device = 'jpeg')
        }
    )
    
    output$metadata <- renderPrint({
        req(info(), input$infoToColor)
        colorCol <- as.character(input$infoToColor)
        thisInfo <- data.frame(id=tree()$tip.label,
                               group=info()[,colorCol], 
                               stringsAsFactors = F)
       print(thisInfo)
    })
    
    output$metadataTable <- renderUI({
        req(info())
        verbatimTextOutput('metadata')
    })
    
    subtree <- reactive({
        req(p(), input$selectedNode, tree())
        viewClade(p(), (as.numeric(input$selectedNode)+length(tree()$tip.label)+1))
    })
    
    output$MRCA <- renderPlot({
        print(subtree())
    })
    
    output$downloadMRCA <- downloadHandler(
        filename = 'subtree.jpeg',
        content = function(file) {
            ggsave(filename = file, plot = subtree(), device = 'jpeg')
        }
    )
    
    # output$MRCA <- renderPrint({
    #     req(p(), input$selectedNode, tree())
    #     print((as.numeric(input$selectedNode)+length(tree()$node.label)))
    # })
    
    output$wholeTree <- renderUI({
        req(input$tree_plot_height, tree())
        plotOutput("tree", height = input$tree_plot_height)
    })
    
    output$commonAncestor <- renderUI({
        req(input$selectedNode, p(), input$subtree_plot_height)
        plotOutput("MRCA", height = input$subtree_plot_height)
        #verbatimTextOutput('MRCA')
    })

})
