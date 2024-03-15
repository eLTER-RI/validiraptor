## helper functions to source by server.R
csv_to_jsonstring  <- function(dataframe){
    dataframe |> head(1) |> unbox() |> toJSON() |> as.character()
}


## render plain dataTableOutput:
renderDataTablePlain <- \(expr) renderDataTable(expr, options = list(paging = FALSE, searching = FALSE, info = FALSE))


## create a drag & drop file input (author: Nan Xiao)
## https://nanx.me/blog/post/shiny-file-input-area/
fileInputArea <- function(inputId, label, multiple = FALSE, accept = NULL,
                          width = NULL, buttonLabel = "Browse...", placeholder = "No file selected") {
    restoredValue <- restoreInput(id = inputId, default = NULL)

                                        # Catch potential edge case - ensure that it's either NULL or a data frame.
    if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
        warning("Restored value for ", inputId, " has incorrect format.")
        restoredValue <- NULL
    }

    if (!is.null(restoredValue)) {
        restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
    }

    inputTag <- tags$input(
                         id = inputId,
                         name = inputId,
                         type = "file",
                         ## Don't use "display: none;" style, which causes keyboard accessibility issue;
                         ##  instead use the following workaround: https://css-tricks.com/places-its-tempting-to-use-display-none-but-dont/
                         style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
                         `data-restore` = restoredValue
                     )

    if (multiple) {
        inputTag$attribs$multiple <- "multiple"
    }
    if (length(accept) > 0) {
        inputTag$attribs$accept <- paste(accept, collapse = ",")
    }

    ## Icon from <https://icons.getbootstrap.com/icons/upload/>
    ## create an icon file
    icon_file <- 'www/validiraptor.svg'
    icon_encoded <- xfun::base64_uri(icon_file)

    div(
        class = "form-group shiny-input-container w-100",
        style = htmltools::css(width = htmltools::validateCssUnit(width)),
        shiny:::shinyInputLabel(inputId, ""),
        div(
            class = "input-group mb-3",
                                        # input-group-prepend is for bootstrap 4 compat
            tags$label(
                     class = "input-group-btn input-group-prepend w-100",
                     span(
                         class = "btn btn-area w-100", inputTag,
                         div(tags$image(src = icon_encoded, width = "180px;"), style = "margin-top: 2rem;"),
                         div(p(label), style = "font-size: 1.2rem; font-weight: 700; padding-top: 2rem;"),
                         div(p(buttonLabel), style = "font-size: 1rem; font-weight: 400; margin-bottom: 2rem;")
                     )
                 )
        ),
        tags$div(
                 id = paste(inputId, "_progress", sep = ""),
                 class = "progress active shiny-file-input-progress",
                 tags$div(class = "progress-bar")
             )
    )

}
