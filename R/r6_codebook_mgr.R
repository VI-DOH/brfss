
#' @export
CodebookMgr <-
  R6::R6Class(
    classname = "CodebookMgr",

    private = list(

      dataset_mgr_pvt = NULL,
      file_mgr_pvt = NULL,


      table_question_params = function(table) {

        thead <- xml2::xml_find_all(table,".//thead")

        tr_head <- xml2::xml_find_all(thead,".//tr")[1]

        tr_head_txt <- as.character(tr_head)
        label <-  tr_head_txt %>% gsub(".*(Label:.*?)[<].*", "\\1", .)


        brs <- xml2::xml_find_all(tr_head, ".//br")

        xml2::xml_set_text(brs, "\n")

        txt <- xml2::xml_text(tr_head)

        stringr::str_split(txt, pattern = "\n")[[1]] %>% gsub("\r","",.)

      }


    ),

    public = list(

      initialize = function(dataset_mgr = NULL) {

        if(!is.null(dataset_mgr) && inherits(dataset_mgr, "DataSetMgr")) {

          private$dataset_mgr_pvt <- dataset_mgr
        } else {
          private$dataset_mgr_pvt <- LocalDataSetMgr$new()
        }

        private$file_mgr_pvt <- FileMgr$new(dataset_mgr = private$dataset_mgr_pvt)


      },

      download_codebook = function(fldrout = NULL, year = NULL) {

        file_mgr <- private$file_mgr_pvt
        dataset_mgr <- private$dataset_mgr_pvt

        dataset <- dataset_mgr$as.list()

        if(!is.null(year)) dataset_mgr$set(year = year)

        if(dataset$extent != "public") {

          stop(paste0("extent must be 'public'. local codebooks are not ",
                      "downloaded from the CDC BRFSS website."))

        }

        ##  URL patterns are stored under the group  "codebook_downloads"

        pats <- file_mgr$pattern_group("codebook_downloads") %>% pull(name)

        ## get storage location for the codebook
        ##    GEOG is not needed unless the user changes the folder pattern
        ##    but is included here in case

        if(is.null(fldrout)) fldrout <- file_mgr$apply("codebook_folder")

        fldrout <- gsub("([^[/])$","\\1/",fldrout)

        ext <- file_mgr$apply("codebook_ext")

        ## create the folder/dir if it does not exist

        if(!dir.exists(fldrout)) dir.create(fldrout, recursive = TRUE)

        ## for each pattern possibility (only one will succeed)
        ##  still looking for a clean way to suppress the fails but
        ##    report on the success


        invisible(
          sapply(pats,function(pat) {

            url <- file_mgr$apply(pat)

            ext <- gsub(".*([.].*)", "\\1", url)

            fileout <- file_mgr$apply("codebook_file")
            fileout <- paste0(fileout,ext)
            destfile <- paste0(fldrout,fileout)

            status <- httr::HEAD(url)$status

            if(status==200) {

              to <- getOption("timeout")
              options(timeout = 180)

              download.file(url = url,destfile = destfile,
                            method = "libcurl",quiet = F, mode = "wb")

              options(timeout = to)

              if(ext == ".zip") {
                dir <- dirname(destfile)
                unzip(destfile, exdir = dir)


                file.remove(destfile)
              }

              #rename the new codebook (the newest file in the dir) to a standard name

              files <- list.files(fldrout, full.names = T)
              newest_file <- file.info(files) %>% arrange(mtime) %>% tail(1) %>% rownames()

              pre <- gsub("(.*/).*", "\\1", newest_file)
              fname <- gsub("(.*/)(.*)[.].*", "\\2", newest_file)
              ext <- gsub("(.*/)(.*)[.](.*)", "\\3", newest_file)

              year <- private$dataset_mgr_pvt$get(year)%% 100
              fname <- paste0(pre,"CODEBOOK", year, "_LLCP.", ext)

              file.rename(from = newest_file, to = fname)


              # this may be necessary
              # set.pattern("codebook_ext", gsub("[.]","",ext))
            }
          })
        )
      },

      process_codebook = function(file = NULL) {

        # self$save_codebook()
        # self$save_codebook_values()
        self$process_layout(file)

      },

      process_layout = function(file = NULL) {

        ext <- self$codebook_ext()

        if(is.null(ext)) return(NULL)

        if(ext == "rtf"){

          df_layout <- self$rtf_layout(file)
          self$save_layout(df_layout)

          df_values <- self$rtf_values(file)
          self$save_values(df_values)


        } else if(ext == "html"){

          df_layout <- self$html_layout(file)
          self$save_layout(df_layout)

          df_values <- self$html_values(file)
          self$save_values(df_values)


        }

      },

      read_codebook = function(file=NULL, ...) {


        args <- list(...)
        #
        #   params <- my.brfss.patterns()

        if(is.null(file)) {

          file <- self$codebook_file()
        }

        if(!file.exists(file)) {
          message(paste0("File not found ... \n",file))
          return(NULL)
        }

        if(grepl("[.]txt$", file, ignore.case = TRUE)) {

          lines <- readLines(file)

        } else if(grepl("[.]rtf$", file,ignore.case = TRUE)) {


          lines <- striprtf::read_rtf(file)

          lines <- gsub("^[*][|] {0,1}","",lines)
          lines <- gsub("[|]","  ",lines)

          lines <- lines %>%
            strsplit(split = "\n") %>%
            unlist(recursive = TRUE)

          lines <- gsub("^[|]","",lines)

        } else if(grepl("[.]pdf$", file,ignore.case = TRUE)) {

          lines <- pdftools::pdf_text(file) %>%
            strsplit(split = "\n") %>%
            unlist(recursive = TRUE)

        } else if(grepl("[.]html$", file,ignore.case = TRUE)) {

          lines <- readLines(
            file,
            encoding = "UTF-8",
            warn = FALSE
          ) %>%
            iconv(from = "UTF-8", to = "UTF-8", sub = "")

          lines <- lines %>%
            htm2txt::htm2txt(file) %>%
            gsub(" "," ",.) %>%
            grep("^$", ., invert = T, value = T) %>%
            grep("^ *$", ., invert = T, value = T)

          comments <- rev(grep("<!--",lines))
          comment_ends <- rev(grep("-->",lines))

          mapply(function(s,e) {
            lines <<- lines[-(s:e)]
          }, comments, comment_ends)

          lines <- lines %>%
            strsplit(split = "\n") %>%
            unlist(recursive = TRUE)

        } else {
          lines <- NULL
        }

        lines %>% stringi::stri_trans_general("Latin-ASCII")

      },

      # get the name of the file after testing for different extensions

      codebook_folder = function() {

        #  get the folder

        private$file_mgr_pvt$apply("codebook_folder")

      },

      # get the name of the file after testing for different extensions

      codebook_file = function(ext = NULL) {

        exts <- ext %||% c("pdf", "txt", "rtf","html")

        #  get the folder

        fldr <- private$file_mgr_pvt$apply("codebook_folder")

        # get all filenames

        files <- list.files(fldr)

        fil_ex <- files %>% gsub("[.].*","",.)

        fil <- private$file_mgr_pvt$apply("codebook_file")

        if(!fil %in% fil_ex) fil <- fil_ex[1]

        ret <- NULL


        found <- sapply(exts, function(ext) {
          file <- paste0(fldr,fil,".",ext)
          if(file.exists(file)) ret <<-  file
        })

        return(ret)

      },

      codebook_ext = function() {

        file <- self$codebook_file()

        if(is.null(file)) return(NULL)

        if(!grepl(".*[.](.{3,4})$",file)) return(NULL)

        return(file %>% gsub(".*[.](.{3,4})$", "\\1", .))

      },

      codebook_exists = function(ext = NULL) {

        exts <- ext %||% c("pdf", "txt", "rtf","html")

        params <- my.brfss.patterns()

        fldr <- private$file_mgr_pvt$apply("codebook_folder")
        fil <- private$file_mgr_pvt$apply("codebook_file")

        found <- purrr::map_lgl(exts, function(ext) {

          file <- paste0(fldr,fil,".",ext)

          file.exists(file)
        })

        return(any(found))

      },


      rtf_values = function(file = NULL) {

        if(is.null(file)) {
          file <- self$codebook_file("rtf")

          if(is.null(file)) return(NULL)
        }

        lines <- striprtf::read_rtf(file)

        label_lns <- grep("Label:", lines)
        value_lns <- grep("Value.*Percentage", lines)

        df <- data.frame(lbl = label_lns, val = value_lns) %>%
          mutate(end = lead(lbl) - 1) %>%
          mutate(end = replace(end, is.na(end), length(lines)))


        df_values <- purrr::pmap(df, \(lbl, val, end) {

          x <- lines[lbl]%>%
            gsub("[[:space:]\u00A0\u200B\u200C\u200D\uFEFF]+", " ", ., perl = TRUE)

          col_name <- x %>%
            gsub(".*SAS Variable Name:[[:space:]](.*?)[[:space:]].*", "\\1", .) %>%
            stringr::str_trim()


          val_lns <- lines[(val +1):end] %>%
            grep("^$", ., invert = TRUE, value = TRUE) %>%
            gsub("^[*][|] ", "", .) %>%
            gsub("[|][[:space:]]+$","",.)

          purrr::map(val_lns, \(ln) {

            x <- ln %>% stringr::str_split_1( "[|]")

            value <- gsub("(.*)-.*", "\\1", x[1])
            maxval <- gsub("(.*)-(.*)", "\\2", x[1])

            data.frame(col_name, value, maxval, text = x[2], count = x[3] )
          }) %>%
            bind_rows()

        })%>%
          bind_rows()%>%
          mutate(text = gsub("Notes:.*", "", text)) %>%
          mutate(text = gsub("\n", "", text)) %>%
          mutate(text = gsub(".Go.to.*", "", text)) %>%
          mutate(text = gsub("Don.t know", "Don't know", text)) %>%
          mutate(text = gsub(".Code=.*", "", text)) %>%
          mutate(text = gsub("\\p{Pd}If[[:space:]].*", "", text, perl = TRUE)) %>%
          mutate(text = stringr::str_trim(text))


        df_values
      },

      rtf_layout = function(file = NULL) {

        if(is.null(file)) {
          file <- self$codebook_file("rtf")

          if(is.null(file)) return(NULL)
        }

        lines <- striprtf::read_rtf(file)


        label_lns <- grep("Label:", lines)
        value_lns <- grep("Value.*Percentage", lines)

        df <- data.frame(lbl = label_lns, val = value_lns) %>%
          mutate(end = lead(lbl) - 1) %>%
          mutate(end = replace(end, is.na(end), length(lines)))


        df_lo <- purrr::map(label_lns, \(lbl) {

          x <- lines[lbl]

          lns <- x %>%
            gsub("[[:space:]\u00A0\u200B\u200C\u200D\uFEFF]+", " ", ., perl = TRUE) %>%
            gsub(".Core Section Number:", "\nCore Section Number:", .) %>%
            gsub(".Module Number:", "\nSection Number:", .) %>%
            gsub(".Section Number:", "\nSection Number:", .) %>%
            gsub("Core\nSection", "Core Section", .) %>%
            gsub(".Section Name:", "\nSection Name:", .) %>%
            gsub(".Column:", "\nColumn:", .) %>%
            gsub(".Question", "\nQuestion",.) %>%
            gsub(".Type of Variable:", "\nType of Variable:", .) %>%
            gsub(".SAS Variable Name:", "\nSAS Variable Name:", .) %>%
            gsub(".*Label:", "Label:", .) %>%
            gsub("[|][[:space:]]+","",.) %>%
            stringr::str_split_1( "\n") %>%
            stringr::str_trim()

          var <- parse_item(lns, "SAS.Var.*:")
          label <- parse_item(lns, "Label:")

          sect_info <- parse_sect_info(lns)

          question <-  parse_item(lns, "Question:")
          question_num <-  parse_item(lns, "Quest.*Number:")

          columns <- parse_item(lns, "Column:")
          cols <- split_cb_line(columns, "-")

          col_start <- cols$left %>% as.integer()
          col_end <- cols$right %>% as.integer()

          var_type <- parse_item(lns, "Type.*Var.*:")



          data.frame(
            field_size = col_end - col_start + 1,
            col_start = col_start,
            col_end = col_end,
            col_name = var,
            var_type = var_type,
            label = label,
            sect_type = sect_info$sect_type,
            section = sect_info$sect_name,
            sect_num = sect_info$sect_num,
            question = question,
            question_num = question_num
          )

        }) %>% bind_rows()

        df_lo
      },

      html_values = function(file = NULL) {

        if(is.null(file)) {
          file <- self$codebook_file("html")

          if(is.null(file)) return(NULL)
        }

        html <- xml2::read_html(file)

        tables <- xml2::xml_find_all(
          html,
          "//table[contains(., 'Label:')]"
        )


        df_values <- purrr::map(tables, \(table) {

          params <- private$table_question_params(table)
          col_name <- grep("SAS.Var", params, value = T) %>% gsub(".*:.(.*)", "\\1", .)

          tbody <- xml2::xml_find_all(table,".//tbody")
          tr_body <- xml2::xml_find_all(tbody,".//tr")

          val_txt <- tr_body %>% xml2::xml_text()

          df_vals <- map(val_txt, \(txt) {

            x <- stringr::str_split(txt, pattern = "\r\n")[[1]]

            vals <-  x[1]
            text  <-  x[2] %>%
              gsub("Notes:.*", "", .) %>%
              gsub(".Go.to.*", "", .) %>%
              gsub(".Code=.*", "", .)  %>%
              iconv(., to = "ASCII", sub = "")

            count = x[3]

            val <- gsub("(.*)-.*", "\\1", vals)
            maxval <- gsub("(.*)-(.*)", "\\2", vals)
            data.frame(value = val, maxval = maxval, text = text, count = count)
          }) %>%
            bind_rows()

          df_vals %>% mutate(col_name = col_name)

        } ) %>%
          bind_rows() %>%
          relocate(col_name) %>%
          filter(!grepl("HIDDEN|BLANK",value))

        df_values
      },


      html_layout = function(file = NULL) {

        if(is.null(file)) {
          file <- self$codebook_file("html")

          if(is.null(file)) return(NULL)
        }

        html <- xml2::read_html(file) #"../brfss_data/data_raw/2019/public/codebook/CODEBOOK19_LLCP.HTML")


        tables <- xml2::xml_find_all(
          html,
          "//table[contains(., 'Label:')]"
        )


        df_lo <- purrr::map(tables, \(table) {

          params <- private$table_question_params(table)


          col_name <- parse_item(params, "SAS.Var.*:")
          label <- parse_item(params, "Label:")

          sect_info <- parse_sect_info(params)

          question <-  parse_item(params, "Question:")
          question_num <-  parse_item(params, "Quest.*Number:")

          columns <- parse_item(params, "Column:")
          cols <- split_cb_line(columns, "-")

          start <- cols$left %>% as.integer()
          end <- cols$right %>% as.integer()

          field_size = end - start + 1

          var_type <- parse_item(params, "Type.*Var.*:")

          sect_type <-  sect_info$sect_type
          section <- sect_info$sect_name
          sect_num <- sect_info$sect_num


          data.frame(field_size, start, end, col_name, sect_type, sect_num, section, label,
                     question_num, var_type, question, calculated = NA, saq = NA)
        } ) %>%
          bind_rows() %>%
          relocate(col_name)

        df_lo
      },


      parse_codebook = function() {

        lines <- self$read_codebook()

        parse_codebook_layout(lines)
      },

      save_codebook = function() {

        df_layout_cb <- self$parse_codebook()

        fldr <- private$file_mgr_pvt$apply("codebook_layout_folder")
        fil <- private$file_mgr_pvt$apply("codebook_layout_file")

        file <- paste0(fldr,fil)

        if(!dir.exists(fldr)) dir.create(fldr, recursive = TRUE)


        saveRDS(df_layout_cb, file = file)


      },

      save_layout = function(df_layout) {

        fldr <- private$file_mgr_pvt$apply("codebook_layout_folder")
        fil <- private$file_mgr_pvt$apply("codebook_layout_file")

        file <- paste0(fldr,fil)

        if(!dir.exists(fldr)) dir.create(fldr, recursive = TRUE)


        saveRDS(df_layout, file = file)


      },

      save_values = function(df_values) {


        fname <- private$file_mgr_pvt$apply("codebook_values_path")

        saveRDS(df_values, file = fname)


      },

      get_layout = function() {

        file <-  private$file_mgr_pvt$apply("codebook_layout_path")

        if(!file.exists(file)) {

          dataset_mgr <- private$file_mgr_pvt$dataset_mgr

          ext_save <- dataset_mgr$get(extent)
          gflag_save <- dataset_mgr$get(geog_flag)

          dataset_mgr$set(extent = "public")
          dataset_mgr$set(geog_flag = "off")


          file <-
            private$file_mgr_pvt$apply("codebook_layout_path")

          dataset_mgr$set(extent = ext_save)
          dataset_mgr$set(geog_flag = gflag_save)


          if(!file.exists(file)) return(NULL)
        }

        readRDS(file = file)

      },

      get_values = function() {

        file <-  private$file_mgr_pvt$apply("codebook_values_path")

        if(!file.exists(file)) {

          dataset_mgr <- private$file_mgr_pvt$dataset_mgr

          ext_save <- dataset_mgr$get(extent)
          gflag_save <- dataset_mgr$get(geog_flag)

          dataset_mgr$set(extent = "public")
          dataset_mgr$set(geog_flag = "off")


          file <-
            private$file_mgr_pvt$apply("codebook_values_path")

          dataset_mgr$set(extent = ext_save)
          dataset_mgr$set(geog_flag = gflag_save)


          if(!file.exists(file)) return(NULL)
        }

        readRDS(file = file)

      },

      parse_codebook_values = function() {

        file <- self$codebook_file()

        if(grepl("[.]pdf$", file)) {
          df_values_cb <- parse_codebook_values_pdf(file = file)

        } else {

          #browser()
          lines <- self$read_codebook()
          df_values_cb <- parse_codebook_values(lines = lines)
        }

        df_values_cb

      },

      save_codebook_values = function() {


        df_values_cb <- self$parse_codebook_values()

        fname <- private$file_mgr_pvt$apply("codebook_values_path")

        saveRDS(df_values_cb, file = fname)
      },

      get_codebook_values = function() {

        fname <- private$file_mgr_pvt$apply("codebook_values_path")

        readRDS( file = fname)
      }




    ),

    active = list(

      dataset_mgr = function(value) {

        if(!missing(value)) {
          if(inherits(value, "DataSetMgr")) {
            private$dataset_mgr_pvt <-  value
            private$patterns_mgr_pvt <- FileMgr$new(dataset_mgr = value)
          }
        }
        private$dataset_mgr_pvt

      }

    )



  )
