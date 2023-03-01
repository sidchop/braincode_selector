library(shiny)
library(DT)

key  <- readRDS("www/key.RDS")
data <- readRDS("www/table.RDS")

ui   <- fluidPage(
  titlePanel("Table of code-base brain visualization tools:"),

  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("language", "Choose language:",
                         unique(data$Language),
                         unique(data$Language)),
      checkboxGroupInput("vtype", "Choose visualization type:",
                         colnames(data)[3:7],
                         colnames(data)[3:7]),
      checkboxGroupInput("ftype", "Choose input file type:",
                         choices = key$FileType,
                         selected = key$FileType)
    ),

    mainPanel(
      DT::dataTableOutput("mytable"),
    )
  ),
  tags$a(href="https://github.com/sidchop/RepoNeuroVis/blob/main/manuscript.pdf", "Paper: A Practical Guide for Generating Reproductivle and Programmatic Neuroimageing Visualizations"),
 hr(),
  tags$a(href="https://sidchop.shinyapps.io/braincode/", "ShinyApp: Generate simple code-based templates for brain visualisation"),
 hr(),
  tags$a(href="https://sidchop.shinyapps.io/braincode_selector/", "GitHub"),

)

server <- function(input, output) {

  key_active <- reactive({
    key_temp <- key
    key_temp <- key_temp$Key[which(key_temp$FileType %in% input$ftype) ]
  })


  filtereddata <- reactive({
    data_filt <- data
    data_filt <- data_filt[
      which(data_filt$Language %in%   input$language), ]

    data_filt <- data_filt[, c("Language","Tool" ,  input$vtype)]

    data_filt <- data_filt[which(!is.na(data_filt[,input$vtype])),]

    data_filt <- janitor::remove_empty(data_filt, which = "rows")

    #keep only tools with selected filetype
    keep <-   keep_tmp <- matrix(0, ncol = length(input$vtype), nrow = nrow(data_filt))

    for (f in 1:length(input$vtype)) {
      tmp <- unlist(c(data_filt[,input$vtype[f]][1]))
      for (t in 1:length(key_active())) {
        keep_tmp[,f] <- stringr::str_detect(tmp, as.character(key_active()[t]))
        keep <- keep+keep_tmp
      }
    }
    keep_filt <- rowSums(keep, na.rm = T)
    data_filt <-  data_filt[which(keep_filt!=0),]

    return( data_filt )
  })

  output$mytable  <- DT::renderDT({
    table <- datatable( filtereddata(),options = list(pageLength = 15))
    table <- formatStyle( table,
                          'Language',
                          backgroundColor = styleEqual(c('R', 'MATLAB', 'Python'), c('#a4c5dd', '#E17B34', '#F3DD73'))
    )
    table <- formatStyle(table ,
                         'Tool',fontWeight = 'bold')
    return(table)
  })


}

shinyApp(ui = ui, server = server)
