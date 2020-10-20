# inspired from https://github.com/tbradley1013/tree-subset-shiny

library(shiny)
library(shinyalert)
library(shinyjs)

shinyUI(
    tagList(
        useShinyalert(),
        useShinyjs(),
        navbarPage(
            title = "ISIS : last common ancestor of a clade",
            tabPanel(
                title = "Tree",
                fluidRow(
                    class = "inputs",
                    column(
                        6,
                        fileInput(
                            inputId = "upload_tree",
                            label = "Select Tree File:"
                        )
                    ),
                    column(
                        6,
                        fileInput(
                            inputId = "upload_info",
                            label = "Select information File (.csv, tab-separated):"
                        )
                    )
                ),
                uiOutput("treeOptions"),
                uiOutput('infoOptions'),
                fluidRow(
                    downloadButton("downloadTree", "Download tree"),
                    uiOutput(
                        "wholeTree"
                    )
                )
            ),
            tabPanel(
                title = "last Common Ancestor",
                uiOutput("nodeSelection"),
                fluidRow(
                    downloadButton("downloadMRCA", "Download tree"),
                    uiOutput(
                        "commonAncestor"
                    )
                )
            ),
            tabPanel(
                title = "metadata",
                fluidRow(
                    uiOutput(
                        "metadataTable"
                    )
                )
            )
        ) 
    )
)