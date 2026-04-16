
#' @export
CodebookMgr <-
  R6::R6Class(
    classname = "CodebookMgr",

    private = list(

      dataset_mgr_pvt = NULL,
      file_mgr_pvt = NULL
    ),

    public = list(

      initialize = function(dataset_mgr = NULL) {

        if(!is.null(dataset_mgr) && inherits(dataset_mgr, "DataSetMgr")) {

          private$dataset_mgr_pvt <- dataset_mgr
        } else {
          private$dataset_mgr_pvt <- LocalDataSetMgr$new()
        }

        private$file_mgr_pvt <- FileMgr$new(dataset_mgr = dataset_mgr)


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

              # this may be necessary
              # set.pattern("codebook_ext", gsub("[.]","",ext))
            }
          })
        )
      },

      process_codebook = function() {

        save_codebook()

        save_codebook_values()

      },

      read_codebook = function(file=NULL, ...) {


        args <- list(...)
        #
        #   params <- my.brfss.patterns()

        if(is.null(file)) {

          file <- codebook_file()
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

          lines <- readLines(file) %>%
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


      codebook_file = function() {

        exts <- c("pdf", "txt", "rtf", "html")

        fldr <- private$file_mgr_pvt$apply("codebook_folder")

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

      codebook_exists = function() {

        exts <- c("pdf", "txt", "rtf","html")

        params <- my.brfss.patterns()

        fldr <- private$file_mgr_pvt$apply("codebook_folder")
        fil <- private$file_mgr_pvt$apply("codebook_file")

        found <- purrr::map_lgl(exts, function(ext) {

          file <- paste0(fldr,fil,".",ext)

          file.exists(file)
        })

        return(any(found))

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

      parse_codebook_values = function() {

        file <- self$codebook_file()

        if(grepl("[.]pdf$", file)) {
          df_values_cb <- parse_codebook_values_pdf(file = file)

        } else {

          #browser()
          df_values_cb <- parse_codebook_values(file = file)
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
            private$patterns_mgr_pvt <- FileMgr$new(value)
          }
        }
        private$dataset_mgr_pvt

      }

    )



  )
