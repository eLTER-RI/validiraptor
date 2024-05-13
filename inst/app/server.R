library(shiny)
library(jsonlite)
library(listviewer)
library(shinyWidgets)
library(shinyjs)
library(cicerone)





server = function(input, output, session){


    guide <- Cicerone$
        new()$
        step(
            "instanceInput", "Input utilities",
            description = "First: provide an example (\"instance\") of your CSV
 data here. You can upload or paste own data or select one of the demo files.")$
 step("schemaPickerHere", "Select schema", "Second: select the schema (ruleset) 
your CSV  data should comply with.")$
    step("ciceronePlaceholderResultPanel", "Results", "Once you have provided
 CSV data and picked a schema, the validation results will show here.")

  guide$init()$start()

    tags$link(rel = "stylesheet", type = "text/css", href = "local.css")

    ## global variables:
    ## ------------------------------------------------------------------------

    sep <- reactiveVal(";") ## separator used for CSV data
    output$sep <- renderText(sep())
    outputOptions(output, "sep", suspendWhenHidden = FALSE)

    ## holds a dataframe with the descriptors (path, size ...) of an
    ##uploaded file with instance data:
    instanceFile <- reactive(input$filePicker)
    ## instance to be validated
    instance <- reactiveVal(NULL)
    schemas <- reactiveVal(NULL) ## schemas (topic + shared definitions) to
                                        # validate against
    is_valid <- reactiveVal(FALSE)

    ## insert actionButtons in upload tabset, one for each example data:
    insertUI(selector = "#exampleSelector",
             ui = radioGroupButtons("exampleFileName", label = "",
                                    choices = c(list.files("www/examples",
                                                           pattern = "\\.csv$")
                                                ),
                                    selected = NA, direction = "vertical"
                                    )
             )


    ## these are the names of the files (without file extension) from which
    ## to read the schemas (and submit for client-side JS validation)
    schema_names <- c("Station" = "station", "Method" = "method",
                      "Observation data" = "data_observation",
                      "Mapping data" = "data_mapping",
                      "Reference" = "reference", "Event" = "event",
                      "Sample" = "sample", "License" = "license"
                      )


    insertUI("#schemaPickerHere",
             ui = radioGroupButtons(inputId = "schemaPicker",
                                    ## enable only after instance  provided:
                                    disabled = TRUE,
                                    label = NULL,
                                    choices = schema_names, justified = TRUE,
                                    direction = "vertical",
                                    selected = NA
                                    )
             )

    output$fileInfo <- renderText({d <- input$filePicker[1,]
        ifelse(is.null(d), sprintf("no file uploaded"),
               sprintf("%s (%s kB)", d$name, d$size)
               )
    })

    output$schemaInfo <- renderText({sn <- input$schemaPicker
        ifelse(is.null(sn), "a schema you still need to select", {
            names(schema_names)[grep(input$schemaPicker, schema_names)]
        }
        )
    })
    output$delimiterInfo <- renderText(sep())
    output$verdict <- renderText(ifelse(is_valid(), "a valid", "not a valid"))

    adviseSeparator <- function(d){
        if(!is.null(d) & ncol(d) < 2 & length(unlist(
                                           strsplit(as.character(d[1, 1]), ","))
                                           ) > 1){
            show("alertWrongSeparator")} else {hide("alertWrongSeparator")}
    }


    ## upon new instance or new schema:
    ## send instance and schema to the client for validation:
    ## ------------------------------------------------------------------------
    revalidate <- function(){
        ## you can"t trigger the shinyEffects effect directly but have to insert
        ##  a UI element containing the shinyEffects expression, e. g. setPulse:
        insertUI(selector = "shinyEffectsPlaceholder",
                 ui = setPulse(id = "H1Schema", duration = 2, iteration = 3)
                 )
        hide("alertWrongSeparator")


        instance_json <- head(instance(), 1) |> as.list() |>
            jsonlite::toJSON(auto_unbox = TRUE)
        session$sendCustomMessage("validate",
                                  list(schemas = schemas(),
                                       instance = instance_json
                                       )
                                  )
    }

    ## update display on incoming validation results:
    observe({
        d_result <- tryCatch({input$validationResult |> fromJSON() |>
                                  as.data.frame()},
                             error = \(e) data.frame(message = "no data")
                             )
        adviseSeparator(d_result)


        ## if result datatable has < 1 items (=errors)
        is_valid(nrow(d_result) < 1)

        ## apply cosmetics to initial error report:
        if(!is_valid()){
            ## unnest list column 4 (with dataframes):
            d_result <- cbind(d_result[-4], d_result[[4]])
            d_result$affected <- d_result[intersect(c("instancePath",
                                                      "missingProperty"),
                                                    names(d_result))
                                          ] |>
                apply(MARGIN = 1, FUN = \(cols) {paste(cols, collapse = " ") |>
                                                     gsub(pattern = "/|NA",
                                                          replacement = ""
                                                          )
                }
                )
            d_result$message <- gsub("must have required property",
                                     "data must contain field", d_result$message
                                     )
            d_result <- d_result[intersect(c("affected", "message"),
                                           names(d_result))
                                 ]
        }

        output$instancePreview <- instance() |> as.data.frame() |>
            renderDataTablePlain()
        output$validationResult <- d_result |> renderDataTablePlain()



        removeClass("resultHeader", c("bg-light", "bg-danger", "bg-success"))
        addClass("resultHeader", c("bg-danger", "bg-success")[is_valid() + 1])
        if(is_valid()){hide("errorTable")} else {show("errorTable")}
    }) |>
    bindEvent(input$validationResult)

    ## digest content of textarea upon pasting of CSV data:
    observe({
        req(input$dataPaster)
        read.csv2(text = input$dataPaster, header = TRUE, sep = sep(),
                  dec = "."
                  ) |>
            head(1) |>
            instance()

        updateRadioGroupButtons(session = session,
                                inputId = "exampleFileName", selected = NA
                                )
        revalidate()
    }) |>
    bindEvent(input$dataPaster)


    ## digest uploaded file:
    observe({
        file_path <- instanceFile()$datapath
        ## read header and first data row of instance file,
        ## convert to JSON object description,
        ## update instance and revalidate:
        read.csv2(file_path, header = TRUE, sep = sep(), dec = ".") |>
            head(1) |>  instance()
        updateRadioGroupButtons(session = session,
                                inputId = "exampleFileName", selected = NA
                                )
        revalidate()
    }) |>
        bindEvent(input$filePicker, ignoreNULL = TRUE)

    ## digest selected example file:
    observe({
        req(input$exampleFileName)
        file_path <- file.path("www/examples", input$exampleFileName)
        tryCatch(read.csv2(file_path, header = TRUE, sep = sep(), dec = ".") |>
                 head(1) |>  instance(),
                 error = \(e) NULL
                 )
        revalidate()
    }) |>
        bindEvent(input$exampleFileName, ignoreNULL = TRUE, ignoreInit = TRUE)


    observe(if(!is.null(instance())){
                hide("alertNoInstance")
                updateRadioGroupButtons(inputId = "schemaPicker",
                                        label = NULL,
                                        choices = schema_names,
                                        justified = TRUE,
                                        selected = NA
                                        )
            }
            )


    observe(if(!is.null(schemas()))  hide("alertNoSchema"))
    observe(if(!is.null(schemas()) & !is.null(instance)) show("resultPanel"))



    ## update schemas:
    observe({
        ## update schemas:
        c(input$schemaPicker, "shared") |>
            Map(f = \(schema_name){
                readLines(sprintf("www/schemas/%s.json", schema_name)) |>
                    paste0(collapse = "\n")
            }) |>
            setNames(c("schemaTopic", "schemaShared")) |>
            schemas()
        revalidate()
    }) |>
        bindEvent(input$schemaPicker)



    output$schemaView <- renderJsonedit(schemas() |>
                                        Map(f = \(s) fromJSON(s)) |> jsonedit()
                                        )







    pushbar::setup_pushbar()

    observeEvent(input$showSchema, pushbar::pushbar_open(id = "pushbarSchema"))
    observeEvent(input$hideSchema, pushbar::pushbar_close())


}

