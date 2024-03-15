library(shiny)
library(jsonlite)
library(listviewer)
library(shinyWidgets)
library(shinyjs)


server = function(input, output, session){
    ##load helper functions:
    source('helpers.R')

    tags$link(rel = 'stylesheet', type = 'text/css', href = 'local.css')
    
    ## global variables:
    ## --------------------------------------------------------------------------------

    sep <- reactiveVal(';') ## separator used for CSV data
    output$sep <- renderText(sep())
    outputOptions(output, 'sep', suspendWhenHidden = FALSE)

    ## holds a dataframe with the descriptors (path, size ...) of an uploaded file with instance data:
    instanceFile <- reactive(input$filePicker)
    ## instance to be validated
    instance <- reactiveVal(NULL)
    schemas <- reactiveVal(NULL) ## schemas (topic + shared definitions) to validate against
    error_count <- reactiveVal(NULL) ## error count
    is_valid <- reactiveVal(FALSE) ##


    

    ## these are the names of the files (without file extension) from which
    ## to read the schemas (and submit for client-side JS validation)
    schema_names <- c('Station' = 'station', 'Method' = 'method',
                      'Observation data' = 'data_observation', 'Mapping data' = 'data_mapping',
                      'Reference' = 'reference', 'Event' = 'event', 
                      'Sample' = 'sample', 'License' = 'license'
                      )
    schema_urls <- sprintf('www/schemas/%s.json', schema_names) |>
        setNames(schema_names)

    insertUI('#schemaPickerHere',
             ui = radioGroupButtons(inputId = 'schemaPicker', 
                                    disabled = TRUE, ## enable only after instance has been provided
                                    label = NULL,
                                    choices = schema_names, justified = TRUE,
                                    direction = 'vertical',
                                    selected = NA
                                    )
             )

    output$fileInfo <- renderText({d <- input$filePicker[1,]
        ifelse(is.null(d), sprintf('no file uploaded'), sprintf('%s (%s kB)', d$name, d$size))
    }) 
       
    output$schemaInfo <- renderText({sn <- input$schemaPicker
        ifelse(is.null(sn), 'a schema you still need to select', {
            names(schema_names)[grep(input$schemaPicker, schema_names)]
        }
        )
    })
    output$delimiterInfo <- renderText(sep())
    output$verdict <- renderText(ifelse(is_valid(), 'a valid', 'not a valid'))

    adviseSeparator <- function(d){
        if(!is.null(d) & ncol(d) < 2 & length(unlist(strsplit(as.character(d[1, 1]), ','))) > 1){
            show('alertWrongSeparator')} else {hide('alertWrongSeparator')}
    }


    ## upon new instance or new schema:
    ## send instance and schema to the client for validation:
    ## --------------------------------------------------------------------------------
    revalidate <- function(){
        instance_json <- head(instance(), 1) |> as.list() |> jsonlite::toJSON(auto_unbox = TRUE)
        session$sendCustomMessage('validate', 
                                  list(schemas = schemas(), instance = instance_json)
                                  )
    }

    ## update display on incoming validation results:
    observe({
        d_result <- tryCatch({input$validationResult |> fromJSON() |> as.data.frame()},
                             error = \(e) data.frame(message = 'no data')
                             )
        adviseSeparator(d_result)
        ## if result datatable has < 1 items (=errors)
        is_valid(nrow(d_result) < 1)

        output$instancePreview <- instance() |> as.data.frame() |> renderDataTablePlain()
        output$validationResult <- d_result |> renderDataTablePlain()

        removeClass('resultHeader', c('bg-light', 'bg-danger', 'bg-success'))
        addClass('resultHeader', c('bg-danger', 'bg-success')[is_valid() + 1])
        if(is_valid()){hide('errorTable')} else {show('errorTable')}
    }) |>
    bindEvent(input$validationResult)

    ## digest content of textarea upon pasting of CSV data:
    observe({
        req(input$dataPaster)
        read.csv2(text = input$dataPaster, header = TRUE, sep = sep(), dec = '.') |>
            head(1) |>
            instance()
        revalidate()
    }) |>
        bindEvent(input$dataPaster)


    ## digest uploaded file:
    observe({
        req(input$filePicker)
        ## read header and first data row of uploaded file, convert to JSON object description, 
        ## update instance:
        read.csv2(instanceFile()$datapath, header = TRUE, sep = sep(), dec = '.') |>
            head(1) |>
            instance()
        revalidate()
    })|>
        bindEvent(input$filePicker)


    observe(if(!is.null(instance())){
                hide('alertNoInstance')
                updateRadioGroupButtons(inputId = 'schemaPicker',
                                        label = NULL,
                                        choices = schema_names, justified = TRUE,
                                        selected = NA
                                        )
            }
            )


    observe(if(!is.null(schemas()))  hide('alertNoSchema'))
    observe(if(!is.null(schemas()) & !is.null(instance)) show('resultPanel'))



    ## update schemas:
    observe({
        ## update schemas:
        c(input$schemaPicker, 'shared') |>
            Map(f = \(schema_name){                    
                readLines(sprintf('www/schemas/%s.json', schema_name)) |>
                    paste0(collapse = '\n')
            }) |> 
            setNames(c('schemaTopic', 'schemaShared')) |>
            schemas()
        revalidate()
    }) |>
        bindEvent(input$schemaPicker)



   output$schemaView <- renderJsonedit(schemas() |> Map(f = \(s) fromJSON(s)) |> jsonedit())

   pushbar::setup_pushbar()

   observeEvent(input$showSchema, pushbar::pushbar_open(id = 'pushbarSchema'))  
   observeEvent(input$hideSchema, pushbar::pushbar_close()) 


}

