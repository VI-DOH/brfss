#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(lubridate)


#' BRFSS Processing App
#'
#' This Shiny app allows the user to simplify the processing steps for
#' downloading, cleaning, and analyzing BRFSS data
#'
#' @return Nothing
#' @export
#'
#'
process_app <- function() {
  ###############    bootstrap values for colors  ##################################

  info_clr <- function(bg="yellow", fg = "black") {paste0('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-info,
  .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-info {
                                        background: ',bg,';
                                        color: ',fg,';
                                        }')
  }

  color_danger <- function(bg="red", fg = "black") {

    paste0('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-danger,',
           '.bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-danger ',
           '{background: white;',
           'color: blue;}')

  }

  bs_clr <- function(bs = "info", bg="yellow", fg = "black") {
    tags$head(
      tags$style(
        HTML(paste0('.bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-',bs,',
  .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-',bs,' {
                                        background: ',bg,';
                                        color: ',fg,';
                                        }'))))
  }

  #############################################
  ##
  ##      get initial metadata

  my_brf <- brfss.params()

  my_geog <- my_brf["geog"]

  my_geogs <- geog_name(c(my_brf["geog"], my_brf["geogs_other"]))

  df_layout <- get.layout() %>%
    filter(sect_type != "DUMMY") %>%
    left_join(modules_used() %>% select("module","geog"),
              by= c("section" = "module")) %>%
    filter(sect_type != "Module" | !is.na(geog)) %>%
    select(-geog)


  sec_types <- df_layout  %>%
    pull(sect_type)  %>%
    unique() %>% sort()

  shinyApp(ui =
             # Define UI for application that draws a histogram
             fluidPage(
               fluidRow(style = "background-color: #DCDFFD; padding: 4px; margin: 4px",
                        column(width=3, style = "color:blue; padding_left: 5px",
                               sliderTextInput(
                                 inputId = "year_id",
                                 label = "Year:",
                                 choices = seq(from = 2016,
                                               to = lubridate::year(Sys.Date())
                                 ),
                                 selected = my_brf["year"],
                                 grid = TRUE
                               )
                        ),
                        column(width = 2,
                               fluidRow(
                                 pickerInput(
                                   inputId = "geog_id",
                                   label = "Geography",
                                   choices = sort(brfss::geogs$Geog),
                                   selected = my_geogs, multiple = TRUE
                                 )),

                               fluidRow(
                                 column(width = 3,h4("Primary: ")),
                                 column(width = 9,
                                        htmlOutput(outputId = "geog_main",
                                                   style = "background-color: white; padding: 4px;")
                                 )
                               )

                        ),
                        column(width = 2, style="margin-left:10%;",
                               p("Data Format:", style="font-weight: bold"),
                               switchInput(
                                 inputId = "source_id",
                                 onLabel = "SAS",
                                 offLabel = "ASCII",
                                 onStatus = "source_sas",
                                 offStatus = "source_ascii",
                                 value = (my_brf["source"] == "sas")

                               )
                        ),
                        column(width = 3,
                               p("Data Extent:", style="font-weight: bold"),
                               switchInput(
                                 inputId = "extent_id",
                                 onLabel = "Local",
                                 offLabel = "National",
                                 onStatus = "extent_local",
                                 offStatus = "extent_national",
                                 value = (my_brf["extent"] == "local")
                               )
                        )
               ),
               navbarPage("BRFSS",


                          tabPanel("Processing",
                                   fluidPage(
                                     useShinyjs(),

                                     #switchInput colors

                                     bs_clr(bs = "source_ascii", bg = "white", fg = "green"),
                                     bs_clr(bs = "source_sas", bg = "green", fg = "white"),
                                     bs_clr(bs = "extent_local", bg = "#0000ff50", fg = "white"),
                                     bs_clr(bs = "extent_national", bg = "white", fg = "#0044ff"),
                                     bs_clr(bs = "brfss_on", bg = "#ffff44aa", fg = "white"),


                                     # Sidebar with a slider input for number of bins
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    prettySwitch(
                                                      inputId = "select_all_id",
                                                      label = "Select All",
                                                      status = "primary",
                                                      slim = TRUE
                                                    ),
                                                    hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                                                    div(style = "font-size: 20px;",
                                                        div(id = 'dl_div',
                                                            materialSwitch(
                                                              inputId = "download_id",
                                                              label = "Download",
                                                              status = "success",
                                                              right = TRUE
                                                            )),
                                                        materialSwitch(
                                                          inputId = "codebook_id",
                                                          label = "Get Codebook",
                                                          status = "success",
                                                          right = TRUE
                                                        ),
                                                        materialSwitch(
                                                          inputId = "layout_id",
                                                          label = "Layout?",
                                                          status = "success",
                                                          right = TRUE
                                                        ),
                                                        materialSwitch(
                                                          inputId = "convert_id",
                                                          label = "Convert",
                                                          status = "success",
                                                          right = TRUE
                                                        ),
                                                        materialSwitch(
                                                          inputId = "split_id",
                                                          label = "Split Geographies",
                                                          status = "success",
                                                          right = TRUE
                                                        ),
                                                        materialSwitch(
                                                          inputId = "factorize_id",
                                                          label = "Factorize?",
                                                          status = "success",
                                                          right = TRUE
                                                        ),
                                                        materialSwitch(
                                                          inputId = "saq_id",
                                                          label = "State-added Questions?",
                                                          status = "success",
                                                          right = TRUE
                                                        ),
                                                        materialSwitch(
                                                          inputId = "response_id",
                                                          label = "Responses and Modules?",
                                                          status = "success",
                                                          right = TRUE
                                                        )
                                                    ),
                                                    actionBttn(
                                                      inputId = "process_id",
                                                      label = "Process",
                                                      style = "fill",
                                                      color = "success"
                                                    )
                                       ),

                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         fluidRow(
                                           column(width = 6,
                                                  htmlOutput(outputId = "raw_hdr"),
                                                  htmlOutput(outputId = "map_raw_htm")
                                           ),
                                           column(width = 6,style = 'border-left: 1px solid blue;',
                                                  htmlOutput(outputId = "not_raw_hdr"),
                                                  htmlOutput(outputId = "map_htm")
                                           )
                                         )

                                       )
                                     )
                                   )
                          ),
                          tabPanel("View",
                                   fluidPage(sidebarLayout(

                                     #field_size start end  col_name  sect_type sect_num                   section
                                     #coi <- "DIABETE4"
                                     # subset <- c("SEXVAR","_AGE_G")
                                     # subset <- c("_RACEGR3")
                                     # subset <- NULL
                                     # #simple_stats(coi = coi)


                                     #df_stats <-survey_stats(coi = coi, subset = subset, pct = TRUE, digits = 2)

                                     sidebarPanel(width=3,

                                                  pickerInput(
                                                    inputId = "sect_type_id",
                                                    label = "Section Type:",
                                                    choices = sec_types, selected = "Core"

                                                  ),

                                                  pickerInput(
                                                    inputId = "section_id",
                                                    label = "Section:",
                                                    choices = ""

                                                  ),

                                                  pickerInput(
                                                    inputId = "column_id",
                                                    label = "Measure:",
                                                    choices = ""

                                                  )
                                     ),


                                     mainPanel(
                                       tableOutput(outputId = "stats_id")
                                     )
                                   ))
                          )
               )
             ), server = process_server)


  process_server <- function(input, output, session = session) {

    geog0 <- character()

    geog_main <- reactive( {
      x <- rvals$geog_changed
      geog_name(brfss.param(geog))
    })



    #######################################################
    ##
    ##    reactive values

    rvals <- reactiveValues(geog_changed = TRUE,fs_changed = TRUE, coi = "")

    ##########################################################
    ##
    ##    input events

    observeEvent(input$process_id,{

      brfss::process_year(download = input$download_id ,layout = input$layout_id ,
                          convert = input$convert_id,
                          codebook = input$codebook_id, split = input$split_id,
                          factorize = input$factorize_id, responses = input$response_id)

      rvals$fs_changed <- !rvals$fs_changed
    })

    observeEvent(input$select_all_id,{

      tf <- input$select_all_id

      updateSwitchInput(session = session, "download_id", value = tf)
      updateSwitchInput(session = session, "layout_id", value = tf)
      updateSwitchInput(session = session, "convert_id", value = tf)
      updateSwitchInput(session = session, "factorize_id", value = tf)
      updateSwitchInput(session = session, "saq_id", value = tf)
      updateSwitchInput(session = session, "split_id", value = tf)
      updateSwitchInput(session = session, "codebook_id", value = tf)
      updateSwitchInput(session = session, "response_id", value = tf)

      lbl <- ifelse(input$select_all_id,"De-select All","Select All")

      updatePrettySwitch(
        inputId = "select_all_id",
        label = lbl)
    })

    observeEvent(input$year_id, {
      year <- input$year_id

      brfss.param(year = year)
    })

    #####################################################
    ##
    ##    deal with geographies ... multiple allowed

    observeEvent(input$geog_id, {

      cat("observing ",input$geog_id,"\n")
      geogs <- geog_abb(input$geog_id)
      if(length(geogs) == 0) {
        brfss.param(geog = "")
        brfss.param(geogs_other = "")

      } else if(length(geogs) == 1) {
        brfss.param(geog = geogs[1])
        brfss.param(geogs_other = "")

      } else {
        excl <- which(geogs == brfss.param(geog))
        geogs <- geogs[-excl]
        brfss.param(geogs_other = geogs)
      }

      rvals$geog_changed <- !rvals$geog_changed
    }, ignoreNULL = FALSE)

    output$geog_main <- renderText(geog_main())


    show_me <- function(input_id) {
      showElement(
        id = input_id,
        anim = TRUE,
        animType = "fade",
        time = 0.5,
        selector = NULL,
        asis = FALSE
      )
    }

    hide_me <- function(input_id) {
      hideElement(
        id = input_id,
        anim = TRUE,
        animType = "fade",
        time = 0.5,
        selector = NULL,
        asis = FALSE
      )
    }

    observeEvent(input$extent_id, {

      ext <- ifelse(input$extent_id, "local", "national")

      brfss.param(extent = ext)

      if(ext == "local") {
        updateSwitchInput(session = session, "download_id", value = FALSE)
        hide_me("download_id")


      } else {
        show_me("download_id")

      }
    })

    observeEvent(input$source_id, {

      src <- ifelse(input$source_id, "sas", "ascii")

      brfss.param(source = src)

      if(src == "sas") {
        updateSwitchInput(session = session, "saq_id", value = FALSE)
        hide_me("saq_id")
      } else {
        show_me("saq_id")

      }

    })



    ##################################################################################
    ##
    ##      main window with file maps

    rd0 <- 0
    gr0 <- 0
    bl0 <- 255

    rd1 <- 0
    gr1 <- 155
    bl1 <- 0


    dir_font <- paste0("font-size:22px;color:rgb(",rd0,",",gr0,",",bl0,"); line-height: 1.0;")
    file_font <- paste0("font-size:18px; color:rgb(",rd1,",",gr1,",",bl1,"); line-height: 0.7;")

    ####################################################################
    ##
    ##    walk_dir()
    ##
    ####################################################################

    walk_dir <- function(df, top_dir, level) {

      files <- list.files(top_dir, include.dirs = F)
      full_dirs <- list.dirs(top_dir,full.names = T, recursive = F)
      dirs <- basename(full_dirs)

      files <- files[!files%in%dirs]


      sapply (files, function(file) {

        is_dir <- dir.exists(file)
        fldr <- dirname(file)
        df <<- df %>%
          bind_rows(data.frame(file, fldr, base = basename(file), level, is_dir=F))
      })

      sapply (full_dirs, function(dir) {
        fldr <- dirname(dir)
        df <<- df %>%
          bind_rows(data.frame(file = dir, fldr, base = basename(dir), level, is_dir=T))

        df <<- walk_dir(df, dir, level = level + 1)
      })


      df

    }

    ####################################################################
    ##
    ##    tree()
    ##
    ####################################################################

    tree <- function(top_dir) {

      df <- data.frame()
      df <- walk_dir(df,top_dir, level = 0)

      df <- df %>% mutate(text = paste0(strrep("&emsp;",level*3),base))



      files <- mapply(function(file, yn, lvl){
        if(yn) {

          x <- paste0("<hr style='width:80%; height:2px; border-width:0px;",
                      "color:gray;background-color:gray'>",
                      "<p style='",dir_font,"'>",file,"</p>")
        }
        else {
          x <- paste0("<p style='",file_font,"'>",file,"</p>")
        }
        x
      },df$text,df$is_dir, df$level)

      paste0(files,collapse = " ")
    }


    ####################################################################
    ##
    ##     output reactives
    ##
    ####################################################################


    tree_htm <- reactive({

      dummy <- rvals$fs_changed
      top_dir <- paste0("./data/", input$year_id) %>% orrr::convert.dot()
      if(dir.exists(top_dir)) tree(top_dir)

    })

    tree_htm_raw <- reactive({
      dummy <- rvals$fs_changed
      top_dir <- paste0("./data_raw/", input$year_id) %>% orrr::convert.dot()
      if(dir.exists(top_dir)) tree(top_dir)

    })

    raw_hdr <- reactive({
      paste0("<p style=' border-radius: 10px; border: 1px solid #cccccc; ",
             "background: #eeeeee; padding: 5px; color:green; font-size:150%;",
             "'>./data_raw/",input$year_id,"</p>")
    })

    not_raw_hdr <- reactive({
      paste0("<p style=' border-radius: 10px; border: 1px solid #cccccc; ",
             "background: #eeeeee; padding: 5px; color:darkgreen;font-size:150%;",
             "'>./data/",input$year_id,"</p>")
    })

    output$raw_hdr <- renderUI(HTML(raw_hdr()))
    output$not_raw_hdr <- renderUI(HTML(not_raw_hdr()))

    output$map_raw_htm <- renderUI(HTML(tree_htm_raw()))

    output$map_htm <- renderUI(HTML(tree_htm()))

    ############################################################################
    ##
    ##      View tab panel

    observeEvent(input$sect_type_id,
                 {
                   if(nchar(input$sect_type_id)>0) {
                     updatePickerInput(session = session,
                                       inputId = "section_id",
                                       choices = df_layout %>%
                                         filter(sect_type == input$sect_type_id) %>%
                                         pull(section) %>% unique()
                     )
                   }
                 }
    )

    observeEvent(input$section_id,
                 {
                   if(nchar(input$section_id)>0) {

                     updatePickerInput(session = session,
                                       inputId = "column_id",
                                       choices = df_layout %>%
                                         filter(section == input$section_id) %>%
                                         mutate(text = paste0(label, " (",col_name , ")")) %>%
                                         pull(text)
                     )
                   }
                 }
    )

    observeEvent(input$column_id, {

      rvals$coi <- gsub(".*[(](.*)[)]", "\\1",input$column_id)

    })


    output$stats_id <- renderTable({
      column <-rvals$coi
      survey_stats(coi = column, subset = c("SEXVAR"), pct = TRUE, digits = 2) %>%
        select(-measure) %>%
        rename(Response = {{column}})

    }

    )

  }
}


