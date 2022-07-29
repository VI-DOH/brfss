#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


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

  require(shiny)
  require(shinyWidgets)
  require(shinyjs)
  require(lubridate)
  require(gt)


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

  df_layout <- get.layout()

  sub_choices  <-
    if(is.null(df_layout)) "" else df_layout %>% pull(col_name) %>% sort()


  df_mods <- modules_used()
  if(!is.null(df_layout) && !is.null(df_mods)) {
    df_layout <- df_layout %>%
      filter(sect_type != "DUMMY") %>%
      left_join(modules_used() %>% select("module","geog"),
                by= c("section" = "module")) %>%
      filter(sect_type != "Module" | !is.na(geog)) %>%
      select(-geog)


    sec_types <- df_layout  %>%
      pull(sect_type)  %>%
      unique() %>% sort()
  } else {
    sec_types =NULL
  }

  patterns <- get.patterns()%>% pull(name) %>% sort()
  geogs <- get_geogs_all()%>% pull(Abbrev) %>% sort()
  versions <- 0:highest_version()

  shinyApp(
    ui =
      fluidPage(
        useShinyjs(),

        #switchInput colors

        bs_clr(bs = "info", bg = "black", fg = "magenta"),
        bs_clr(bs = "source_ascii", bg = "white", fg = "green"),
        bs_clr(bs = "source_sas", bg = "green", fg = "white"),
        bs_clr(bs = "extent_local", bg = "#0000ff50", fg = "white"),
        bs_clr(bs = "extent_national", bg = "white", fg = "#0044ff"),
        bs_clr(bs = "brfss_on", bg = "#ffff44aa", fg = "white"),

        tags$head(tags$style(paste0(".shiny-notification ",
                                    "{position: fixed; top: calc(50%); left: calc(50%);",
                                    "color: #222222; background-color: #ffffff;",
                                    "border-radius: 25px;  text-align: center;",
                                    "font-size: 20px;"))),

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
                          pickerInput(
                            inputId = "primary_geog_id",
                            label = "Primary:",
                            choices = my_geogs, selected = my_geog

                          ),

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

        tabsetPanel(

          tabPanel("Processing",
                   fluidPage(


                     # Sidebar with a slider input for number of bins
                     sidebarLayout(
                       sidebarPanel(
                         width = 3,
                         prettySwitch(
                           inputId = "select_all_id",
                           label = "Select All",
                           status = "primary",
                           slim = TRUE
                         ),
                         hr(style=paste0("height:1px;border-width:1px;",
                                         "background-color:blue;")),
                         div(style = "font-size: 20px;",

                             materialSwitch(
                               inputId = "download_id",
                               label = "Download",
                               status = "success",
                               right = TRUE
                             ),
                             div(id = 'dl_div', style = "margin-left:25px;",
                                 materialSwitch(
                                   inputId = "dl_metadata_id",
                                   label = "Metadata",
                                   status = "success",
                                   right = TRUE
                                 ),
                                 materialSwitch(
                                   inputId = "dl_codebook_id",
                                   label = "Codebook",
                                   status = "success",
                                   right = TRUE
                                 ),
                                 materialSwitch(
                                   inputId = "dl_data_id",
                                   label = "Raw: XPT/ASCII",
                                   status = "success",
                                   right = TRUE
                                 )),
                             hr(style=paste0("height:1px;border-width:1px;",
                                             "background-color:blue;")),
                             materialSwitch(
                               inputId = "process_id",
                               label = "Process",
                               status = "success",
                               right = TRUE
                             ),
                             div(id = 'proc_div',
                                 style = "margin-left:25px;font-size: 20px;",
                                 materialSwitch(
                                   inputId = "codebook_id",
                                   label = "Codebook",
                                   status = "success",
                                   right = TRUE
                                 ),
                                 materialSwitch(
                                   inputId = "layout_id",
                                   label = "Layout",
                                   status = "success",
                                   right = TRUE
                                 ),
                                 materialSwitch(
                                   inputId = "convert_id",
                                   label = "Survey Data",
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
                             )),
                         actionBttn(
                           inputId = "process_cmd_id",
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

                     sidebarPanel(width=3,
                                  div(style = "border: 1px solid blue; border-radius:15px; padding: 7px;",
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

                                      ),

                                      pickerInput(
                                        inputId = "subsets_id",
                                        label = "Subsets:",
                                        multiple = TRUE,
                                        choices = sub_choices,
                                        options = pickerOptions(
                                          actionsBox = TRUE,
                                          title = "Subset",
                                          header = "This is a title",
                                          liveSearch = TRUE
                                        )


                                      )
                                  )
                     ),


                     mainPanel(
                       gt_output(outputId = "stats_id")
                     )
                   ))
          ),

          tabPanel("File Mgr",
                   fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         fluidRow(
                           div(style = "border: 1px solid blue; padding: 3px",
                               actionButton(inputId = "file_btn",label = "Select File ..."),
                               htmlOutput(outputId = "filename")
                           )),

                         div(style = "margin-top: 15px;",
                             textInput(inputId = "filter",label = "Filter")
                         ),
                         selectInput(inputId = "pattern",label = "Pattern Name",
                                     choices = patterns),
                         tags$b(p("Pattern")),
                         htmlOutput(outputId = "raw_pattern"),
                         tags$b(p("Pattern Applied")),
                         htmlOutput(outputId = "applied"),
                         actionButton(inputId = "copy_btn",label = "Copy")

                       ),


                       mainPanel(
                         fluidRow(
                           column(width = 6,
                                  htmlOutput(outputId = "raw_hdr_fm"),
                                  htmlOutput(outputId = "map_raw_htm_fm")
                           ),
                           column(width = 6,style = 'border-left: 1px solid blue;',
                                  htmlOutput(outputId = "not_raw_hdr_fm"),
                                  htmlOutput(outputId = "map_htm_fm")
                           )
                         )

                       )
                     )
                   )
          ),
          tabPanel("Downloads",
                   fluidPage(
                     selectInput(inputId = "other_downloads_id",label = "Download:",
                                 choices = LETTERS[1:13],size = 10, selectize = FALSE, width = "800px")
                   )
          ),
          tabPanel("Aliases",
                   fluidPage(

                   )
          )
        )
      ),


    server = function(input, output, session = session){

      geog0 <- character()

      geog_main <- reactive( {
        x <- rvals$geog_changed
        geog_name(brfss.param(geog))
      })



      #######################################################
      ##
      ##    reactive values

      rvals <- reactiveValues(dl_init = TRUE,
                              download_init = TRUE,
                              dl_changed = FALSE,
                              download_changed = FALSE,
                              proc_init = TRUE,
                              process_init = TRUE,
                              proc_changed = FALSE,
                              process_changed = FALSE,
                              reanalyze = FALSE,
                              geog_changed = TRUE,
                              fs_changed = TRUE,
                              saq_enabled = TRUE,
                              coi = "")

      ##########################################################
      ##
      ##    input events

      observeEvent(input$process_cmd_id,{

        progress <- shiny::Progress$new()

        process_year(dl_metadata = input$dl_metadata_id,
                     dl_codebook = input$dl_codebook_id,
                     dl_data = input$dl_data_id,
                     layout = input$layout_id ,
                     convert = input$convert_id,
                     codebook = input$codebook_id,
                     split = input$split_id,
                     saq = input$saq_id,
                     factorize = input$factorize_id,
                     responses = input$response_id,
                     progress = progress)

        progress$close()

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

        df<- brfss_web_files(year)

        updateSelectInput(session = session, inputId = "other_downloads_id", choices = df$txt)
      })



      #####################################################
      ##
      ##    deal with processes ...

      procChanged <- reactive({
        list(input$layout_id, input$convert_id, input$factorize_id,
             input$saq_id, input$split_id, input$codebook_id, input$response_id)
      })


      observeEvent(procChanged(), {

        if(rvals$proc_init) {
          rvals$proc_init <- FALSE
          return()
        }

        if(rvals$process_changed) {
          rvals$process_changed <- FALSE

          return()
        }

        proc_state <- all(input$layout_id, input$convert_id, input$factorize_id,
                          input$saq_id || !rvals$saq_enabled,
                          input$split_id, input$codebook_id,
                          input$response_id)

        rvals$proc_changed <- proc_state != input$process_id
        rvals$process_changed <- FALSE


        if(proc_state != input$process_id) {
          updateSwitchInput(session = session,                                                            "process_id", value = proc_state)
        }

      })

      observeEvent(input$process_id, {

        if(rvals$process_init) {
          rvals$process_init <- FALSE
          return()
        }

        rvals$process_changed <- !rvals$proc_changed

        if(!rvals$proc_changed ) {

          updateMaterialSwitch(session = session, "layout_id", value = input$process_id)
          updateMaterialSwitch(session = session, "convert_id", value = input$process_id)
          updateMaterialSwitch(session = session, "factorize_id", value = input$process_id)
          updateMaterialSwitch(session = session, "saq_id",
                               value = input$process_id && rvals$saq_enabled)
          updateMaterialSwitch(session = session, "split_id", value = input$process_id)
          updateMaterialSwitch(session = session, "codebook_id", value = input$process_id)
          updateMaterialSwitch(session = session, "response_id", value = input$process_id)
        }
        rvals$proc_changed <- FALSE
      })

      #####################################################
      ##
      ##    deal with downloads ...

      dlChanged <- reactive({
        list(input$dl_codebook_id,input$dl_metadata_id, input$dl_data_id)
      })


      observeEvent(dlChanged(), {

        cat("--------------------------------------\n",
            format(Sys.time(), "%H:%M:%S"),"\n",
            "dl_changed() ... \n",
            "\nIn:\n",
            "rvals$download_changed = ",rvals$download_changed,"\n",
            "rvals$dl_changed = ",rvals$dl_changed,"\n")


        if(rvals$dl_init) {
          rvals$dl_init <- FALSE
          return()
        }

        if(rvals$download_changed) {
          rvals$download_changed <- FALSE
          rvals$dl_changed <- FALSE

          cat("\nOut:\n",
              "rvals$download_changed = ",rvals$download_changed,"\n",
              "rvals$dl_changed = ",rvals$dl_changed,"\n")

          return()
        }

        dl_state <- all(input$dl_codebook_id,input$dl_metadata_id, input$dl_data_id)

        rvals$dl_changed <- dl_state != input$download_id
        rvals$download_changed <- FALSE

        if(dl_state != input$download_id) {
          updateSwitchInput(session = session,"download_id", value = dl_state)
        }

        cat("\nOut:\n",
            "rvals$download_changed = ",rvals$download_changed,"\n",
            "rvals$dl_changed = ",rvals$dl_changed,"\n")
      })

      observeEvent(input$download_id, {


        cat("//////////////////////////////////////////////////////// \n",
            format(Sys.time(), "%H:%M:%S"),"\n",
            "download_changed() ... ",
            ifelse(rvals$dl_changed," by other buttons","pressed"),"\n",
            "\nIn:\n",
            "rvals$download_changed = ",rvals$download_changed,"\n",
            "rvals$dl_changed = ",rvals$dl_changed,"\n")

        if(rvals$download_init) {
          rvals$download_init <- FALSE
          return()
        }

        rvals$download_changed <- !rvals$dl_changed

        if(!rvals$dl_changed ) {
          updateSwitchInput(session = session, "dl_metadata_id", value = input$download_id)
          updateSwitchInput(session = session, "dl_codebook_id", value = input$download_id)
          updateSwitchInput(session = session, "dl_data_id", value = input$download_id)
          cat("   ... All are now ", ifelse(input$download_id," ON","OFF"), "\n")
        }
        rvals$dl_changed <- FALSE
        cat("\nOut:\n",
            "rvals$download_changed = ",rvals$download_changed,"\n",
            "rvals$dl_changed = ",rvals$dl_changed,"\n")
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

        update_geog_picker()

        rvals$geog_changed <- !rvals$geog_changed


      }, ignoreNULL = FALSE)

      update_geog_picker <- function() {

        selected <- unlist(brfss.param(geog))
        if(is.null(selected)) selected <- ""

        choices <- unlist(c(brfss.param(geog),brfss.param(geogs_other)))
        if(is.null(choices)) choices <- ""


        updatePickerInput(session = session,
                          inputId = "primary_geog_id",
                          choices = choices,
                          selected = selected
        )

      }
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

      observeEvent(input$other_downloads_id, {

        cat(" ... " , input$other_downloads_id, "\n")

      })

      disable_me <- function(input_id) {
        shinyjs::disable(input_id)
        #updateSwitchInput(session = session, input_id = input_id)
      }

      enable_me <- function(input_id) {
        shinyjs::enable(input_id)
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
          #hide_me("download_id")
          disable_me("download_id")


        } else {
          #show_me("download_id")
          enable_me("download_id")

        }
      })

      observeEvent(input$source_id, {

        src <- ifelse(input$source_id, "sas", "ascii")

        brfss.param(source = src)

        if(src == "sas") {
          updateSwitchInput(session = session, "saq_id", value = FALSE)
          disable_me("saq_id")
          rvals$saq_enabled <- FALSE
        } else {
          enable_me("saq_id")
          rvals$saq_enabled <- TRUE

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

      observeEvent(input$primary_geog_id, {
        geog <- geog_abb(input$primary_geog_id)
        geogs <- c(brfss.param(geog),brfss.param(geogs_other))

        geogs_other <- geogs[geogs!=geog]

        brfss.params(geog = geog, geogs_other = geogs_other)

        reanalyze()
      })

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

      observeEvent(input$section_id, {
        if(nchar(input$section_id)>0) {

          updatePickerInput(session = session,
                            inputId = "column_id",
                            choices = df_layout %>%
                              filter(section == input$section_id) %>%
                              mutate(text = paste0(label, " (",col_name , ")")) %>%
                              pull(text)
          )
        }
      })

      observeEvent(input$column_id, {

        rvals$coi <- gsub(".*[(](.*)[)]", "\\1",input$column_id)

      })

      ########   analysis ... stats ####################

      reanalyze <- function() {
        rvals$reanalyze <- !rvals$reanalyze
      }

      output$stats_id <- render_gt({

        dummy <- rvals$reanalyze
        column <-rvals$coi

        #subvar  subset  response  num   mean   se  CI_lower
        df <- survey_stats(coi = column, subsets = input$subsets_id,
                           pct = TRUE, digits = 1) %>%
          stats_wide()
      })

      ###################################################################
      ##
      ##    server-side ... file_mgr tab

      file_name_htm <- function(file_name) {
        css <- paste0("color:blue;",
                      "font-size:120%; ",
                      "margin: 5px; ",
                      "background-color: white; ",
                      "border: 1px solid orange"
        )

        txt <- paste0("<p style ='", css,"'>", file_name, "</p>")
      }

      file_in <- NULL
      file_out <- NULL

      rd0 <- 0
      gr0 <- 0
      bl0 <- 255

      rd1 <- 0
      gr1 <- 155
      bl1 <- 0


      dir_font <- paste0("font-size:18px;color:rgb(",rd0,",",
                         gr0,",",bl0,"); line-height: 1.1;")
      file_font <- paste0("font-size:15px; color:rgb(",rd1,",",
                          gr1,",",bl1,"); line-height: 0.7;")



      tree_htm_fm <- reactive({

        dummy <- rvals$fs_changed
        top_dir <- paste0("./data/", input$year_id) %>% orrr::convert.dot()
        tree(top_dir)

      })

      tree_htm_raw_fm <- reactive({
        dummy <- rvals$fs_changed
        top_dir <- paste0("./data_raw/", input$year_id) %>% orrr::convert.dot()
        tree(top_dir)

      })

      observeEvent( input$filter, {

        choices_flt <- grep(input$filter,patterns, value = TRUE)

        updateSelectInput(session = session, inputId = "pattern",
                          choices = choices_flt)

      })

      observeEvent(input$copy_btn, {


        from <- orrr::convert.dot(file_in)
        to <- orrr::convert.dot(file_out)
        to_dir <- dirname(to)

        if(!dir.exists(to_dir)) dir.create(to_dir, recursive = T)
        success <- file.copy(from = from, to = to )
        rvals$fs_changed <- !rvals$fs_changed
      })

      observeEvent(input$file_btn, {
        file_name <- choose.files(default = paste0(orrr::convert.dot("."),"/*.*"),
                                  multi = FALSE)
        file_in <<- file_name

        base <- orrr::convert.dot(".")

        file_name <- gsub("\\","/",file_name, fixed = TRUE)
        file_name <- gsub(base,".",file_name, fixed = TRUE)

        if(length(file_name)>0) {
          output$filename <- renderUI(HTML(file_name_htm(file_name)))
        }

      })

      output$raw_pattern <- renderText(HTML({
        css <- paste0("color:blue;",
                      "font-size:150%; ",
                      "background-color: white; ",
                      "border: 2px solid green;",
                      "padding: 2px;"
        )

        txt <- get.pattern(name = input$pattern, expand = TRUE)
        txt <- paste0("<p style ='", css,"'>", txt, "</p>")


        txt
      }))

      output$applied <- renderText(HTML({
        css <- paste0("color:blue;",
                      "font-size:150%; ",
                      "background-color: white; ",
                      "border: 2px solid green;",
                      "padding: 2px;"
        )

        params <- my.brfss.patterns()

        txt <- apply.pattern(name = input$pattern, params, expand = FALSE)

        file_out <<- txt
        txt <- paste0("<p style ='", css,"'>", txt, "</p>")
      })
      )


      output$raw_hdr_fm <- renderUI(HTML(raw_hdr()))
      output$not_raw_hdr_fm <- renderUI(HTML(not_raw_hdr()))

      output$map_raw_htm_fm <- renderUI(HTML(tree_htm_raw()))

      output$map_htm_fm <- renderUI(HTML(tree_htm()))





    }
  )
}




