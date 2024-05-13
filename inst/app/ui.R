library(shiny)
library(listviewer)
library(bslib)
library(pushbar)
library(shinyjs)
library(markdown)
library(shinyEffects) ## to add pulse effect to UI elements
library(cicerone)

bootstrap5 <- bslib::bs_theme(version = 5)



navbarPage(title = "Validiraptor",
           position = "fixed-top",
           theme = bootstrap5,
           useShinyjs(),
           use_cicerone(), ## for step-by-step guide
           pushbar::pushbar_deps(), ## to create drawer (slide-in)
           div(id = "shinyEffectsPlaceholder"),
           img(src = "eLTER-IMAGE-validiraptor.svg", height = "50px"),
           ## broadcast whether one of filePicker, dataPaster or schemaPicker,
           ## because server.R listes to all three in one function:
           tags$head(
             tags$script(
               "$(document).on(\"shiny:inputchanged\", function(event) {
                   if([\"filePicker\", \"dataPaster\", \"schemaPicker\"].indexOf(event.name) != -1){
                     Shiny.setInputValue(\"changedInput\", event.name);
                   }
                 });"
             )
           ),

           tabPanel(id = "tabInput", title = "main",
                    tags$head(tags$script(src = "/js/validate.js" )),
                    verbatimTextOutput("log"),
                    verbatimTextOutput("fileInputLog"),
                    fluidRow(
                      column(2, h1("instance")),
                      column(8, h1("validation")),
                      column(2, h1(span(id = "H1Schema", "schema")))
                    ),

                    fluidRow(
                      column(2, class = "well",
                             tabsetPanel(id = "instanceInput",
                               tabPanel("upload",
                                        fileInputArea("filePicker",
                                                      label = "Drag & drop, or click for file picker.",
                                                      buttonLabel = markdown("accepted format: CSV"),
                                                      multiple = FALSE,
                                                      accept = "text/csv"
                                                      ),
                                        textOutput("fileInfo")
                                        ),
                               tabPanel("paste",
                                        textAreaInput("dataPaster", label = "",
                                                      placeholder = "paste header and first data row here")
                                        ),
                               tabPanel("demo",
                                        tags$small("Pick one of these example data to get to know the app:"),
                                        div(id = "exampleSelector")
                                        )
                             )
                             ),
                      column(8, id = "ciceronePlaceholderResultPanel",
                             div(id = "alertNoInstance", class = "alert alert-warning",
                                 "please provide an instance first (upload or paste data)"
                                 ),
                             div(id = "alertWrongSeparator",
                                 class = "alert alert-warning",
                                 "please make sure to use",
                                 textOutput("sep", inline = TRUE),
                                 " as column separator"
                                 ),
                             div(id = "alertNoSchema",
                                 class = "alert alert-warning",
                                 "after providing an instance, please select a schema (or change the current one)"
                                 ),
                             div(
                                 id = "resultPanel",
                                 card(card_header(id = "resultHeader",
                                                  class = "bg-light",
                                                  p("Judging from the first row, this data is ",
                                                    textOutput("verdict", inline = TRUE),
                                                    "instance of",
                                                    textOutput("schemaInfo", inline = TRUE),
                                                    ":"
                                                    )
                                                  ),
                                      card_body(
                                        DT::DTOutput("instancePreview"),
                                        )
                                      ),
                                 div(id = "errorTable",
                                     card(
                                       card_header(class = "bg-light",
                                                   "due to the following errors:"
                                                   ),
                                       card_body(
                                         div(id = "validationDetails",
                                             DT::DTOutput("validationResult")
                                             )
                                       )
                                     )
                                     ) |> shinyjs::hidden()
                                 ) |> shinyjs::hidden()
                             )
                     ,
                      column(2,
                             div(id = "schemaPickerHere"),
                             actionButton("showSchema", "Schema details"),
                             pushbar::pushbar(id = "pushbarSchema",
                                              from = "right",
                                              style = "padding:5em; width:50%; background-color:#fff",
                                              span(class = "float-right",
                                                   actionButton("hideSchema",
                                                                label = icon(name = "times")
                                                                )
                                                   ),
                                              h2("schema details"),
                                              div(class = "text-info p-2",
                                                  markdown("You can inspect the rules imposed on a  valid instance by exploring the schemas below.
- **schemaTopic** ist the schema relevant to your specific data (e. g. Station or Observation data) and mainly defines what variables (\"properties\") must or can be part of your data).
- **shemaShared**, to which schemaTopic refers, specifies rules on the variable level (type, precision, allowed values ...)
       ")
),
jsoneditOutput("schemaView")
)
)
)
),
tabPanel(id = "tabHelp", title = "help", includeMarkdown("www/help.md")),
tabPanel(id = "tabAbout", title = "about", includeMarkdown("www/about.md"))
)


