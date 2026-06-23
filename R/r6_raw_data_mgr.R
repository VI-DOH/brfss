
#' RawDataMgr R6 Class
#'
#' @export
RawDataMgr <- R6::R6Class(
  classname = "RawDataMgr",

  private = list(

    file_mgr_pvt = NULL,
    dataset_mgr_pvt = NULL,
    layout_mgr = NULL,
    verbose_pvt = FALSE

  ),

  public = list(

    initialize = function(file_mgr = NULL, dataset_mgr = NULL, verbose = FALSE) {


      p <- private

      if(!is.null(file_mgr) && inherits(file_mgr,"FileMgr"))
        p$file_mgr_pvt <- file_mgr
      else
        p$file_mgr_pvt <- FileMgr$new()

      if(!is.null(dataset_mgr) && inherits(dataset_mgr,"DataSetMgr")) {
        p$file_mgr_pvt$dataset_mgr <- dataset_mgr
        p$dataset_mgr_pvt <- dataset_mgr
      } else {

        p$dataset_mgr_pvt <- p$file_mgr_pvt$dataset_mgr
      }
      p$verbose_pvt <- verbose

    },

    dl_metadata = function(fldrout = NULL, quietly = TRUE) {

      file_mgr <- private$file_mgr_pvt
      dataset_mgr <- private$dataset_mgr_pvt
      dataset <- dataset_mgr$as.list()

      if(private$verbose_pvt) cat(" ... downloading ... metadata ... ")

      pttrns <- file_mgr$pattern_group("sas_downloads") %>% pull(pattern)

      urlfiles <- file_mgr$apply("brfss_url_files")

      folderout <- file_mgr$apply("annual_raw_metadata_folder")

      folderout <- gsub("([^[/])$","\\1/",folderout)

      if(!dir.exists(folderout)) dir.create(folderout,recursive = T)

      to <- getOption("timeout")
      options(timeout = 300)


      sapply(pttrns,function(pttrn) {

        if(!quietly) cat(" ... trying", pttrn, "\n")

        has_vers <- grepl("^VERS^", pttrn, fixed = TRUE)
        cont <- TRUE
        version <- 0
        dataset_mgr$set(version = version)

        #        file <- file_mgr$patternize(strIn = file_pttrn)


        while(cont) {

          filename <- file_mgr$patternize(strIn = pttrn)
          url <- paste0(urlfiles,filename)
          fileout <- paste0(folderout,filename)

          #
          # download will fail on larger files if time to download is > 60 secs
          #   set time to 3 minutes and then restore when done

          if(!quietly) cat(" ... checking", url, "\n")
          x <- httr::GET(url = url)

          if(x$status_code  > 399) {
            url <- gsub("SAS$", "zip", url, ignore.case = TRUE)
            fileout <- gsub("SAS$", "zip", fileout, ignore.case = TRUE)
            x <- httr::GET(url = url)

          }

          cont <- x$status_code == 200
          if(!quietly) cat(" ... status_code: ", x$status_code, "\n")

          if(cont) {

            if(!quietly) cat(" ... downloading to", fileout, "\n")

            download.file(url = url,destfile = fileout,
                          method = "libcurl", quiet = quietly)

            if(grepl("[.]zip$",fileout)) {
              unzip(fileout,exdir = normalizePath(folderout))
              file.remove(fileout)
            }

            version <- version + 1
            dataset_mgr$set(version = version)
          }
          cont <- has_vers && cont
        }
      })

      options(timeout = to)

      return(invisible(NULL))

    },

    save_sas_layout = function() {

      file_mgr <- private$file_mgr_pvt
      dataset_mgr <- private$dataset_mgr_pvt
      dataset <- dataset_mgr$as.list()

      year <- dataset$year

      # get the filename for the data
      #  name format based on year
      if (year < 2011) {
        return(NULL)
      }

      version <- 0
      dataset_mgr$set(version = version)

      file <- file_mgr$apply("sas_sasout_path")

      browser()
      while (file.exists(file)) {
        #  read the sasout file

        lines <- readLines(file, warn = F, encoding = "latin1")

        lines <- lines %>% brfss:::convert_raw_chars()
        lines <- gsub("—", "-", lines) %>%
          iconv(to = "ASCII//TRANSLIT")

        # get the start point of interest and remove everything before

        start <- as.integer(grep("^Label$", lines)) + 1
        if (length(start) == 0)
          start <- grep("\\* ASSIGN ", lines)
        lines <- lines[start:length(lines)]

        # get the end point of interest (; by itself on a line) and remove everything after
        #   including that end line

        lines <- lines[1:(grep("^;$", lines) - 1)]

        #####################################
        ##
        ##  put together broken lines

        varlines <- grep(" = '", lines)
        vline2 <- c(varlines[2:length(varlines)] - 1, length(lines))

        browser()

        ulines <- purrr::map2_chr(varlines, vline2, \(l0, l1) {
          paste(lines[l0:l1], collapse = "")

        })


        vars <- gsub("(.*) =.*", "\\1", ulines)
        vars <- stringr::str_trim(vars)
        #vars <- gsub("^_","X_",vars)

        question <- gsub("(.*) = '(.*)'$", "\\2", ulines)


        df_ranges <- self$read_sas_field_ranges()

        df <- dplyr::left_join(df_ranges,
                               data.frame(
                                 var = vars,
                                 label = question,
                                 stringsAsFactors = F
                               ),
                               by = "var")

        df <- df %>% brfss:::deduped_layout()

        colnames(df)[colnames(df) == "var"] <- "col_name"
        df$field_size <- as.integer(df$end) - as.integer(df$start) + 1

        df_layout_sas <- df %>% select(field_size,
                                       start,
                                       end,
                                       col_name,
                                       sect_type,
                                       sect_num,
                                       section,
                                       label) %>%
          group_by(section) %>%
          mutate(question_num = row_number()) %>%
          relocate(question_num, .before = label) %>%
          as.data.frame() %>%
          brfss:::fill_dummies()


        fldr <- file_mgr$apply("layout_folder")
        if (!dir.exists(fldr))
          dir.create(fldr, recursive = TRUE)

        saveRDS(df_layout_sas, file = file_mgr$apply("sas_layout_path"))

        version <- version + 1
        dataset_mgr$set(version = version)

        file <- file_mgr$apply("sas_sasout_path")

      }

      invisible()
    },

    dl_codebook = function(fldrout = NULL) {

      if(private$verbose_pvt) cat(" ... downloading ... codebook ... ")

      cb_mgr <- CodebookMgr$new(dataset_mgr = self$dataset_mgr)
      cb_mgr$download_codebook(fldrout = fldrout)

    },

    read_sas_field_ranges = function() {

      file_mgr <- private$file_mgr_pvt
      dataset_mgr <- private$dataset_mgr_pvt
      dataset <- dataset_mgr$as.list()

      # get the filename for the data
      #  name format based on year
      year <- dataset$year

      if(year>2010) {
        file <- file_mgr$apply("sas_sasout_path")

      } else {
        return(NULL)
        #file <- paste("sasout",sprintf("%02d",year%%100),".sas",sep="")
      }

      lines <- readLines(file, warn=F) %>%
        iconv(to = "UTF-8")

      lines <- gsub("—", "-",lines)

      # get the start point of interest and remove everything before


      start <- grep("^(INPUT[[:space:]]|INPUT$)",lines)

      if(nchar(lines[start]) > 10) {
        lines[start] <- gsub("^INPUT[[:space:]]","",lines[start])
      } else {
        start <- start+1
      }
      lines <- lines[start:length(lines)]

      # get the end point of interest (; by itself on a line, the first one) and
      #   remove everything after ... including that end line

      end <- min(grep("^;$",lines))-1
      lines <- lines[1:end ]

      ## remove known unwanted line, if they exist

      lines <- lines[grep("STATE ADDED ",lines,invert=T)]
      lines <- lines[grep("^\\*",lines,invert=T)]

      ##
      ##  get the sections/modules
      ##

      # annotated lines
      lines_secmod <- grep("[/][*]",lines)

      lines_save <- lines

      # blank unannotated lines
      lines_save[-lines_secmod] <-""

      # blank unannotated lines
      lines_save <- stringr::str_trim(gsub(".*[/][*] (.*)","\\1",lines_save))
      lines_save <- stringr::str_trim(gsub("[*][/]","",lines_save))

      #####################################################################
      ##
      ##    get the section
      type <- ""

      section <- purrr::map_chr(lines_save, \(typ) {

        if(typ!="") type<<- typ
        return(type)

      })

      section <- stringr::str_trim(unname(gsub(".*:","",section)))

      # lines of interest ... beginning of Section or Module
      lines_secmod2<- grep("^(Sec|Mod).*:",lines_save)

      # put random word ('STOP') at postion where not a question
      lines_save[lines_secmod[!lines_secmod%in%lines_secmod2]] <-"STOP"
      lines_save[grep("[Vv]ariable",lines_save)] <-"STOP"
      lines_save[grep("Questionnaire",lines_save)] <-"STOP"

      lines_stop <- grep("^STOP$",lines_save)

      #beginning (1st question) of Section
      sect_start <- grep("^Section ",lines_save)

      sect_end <- sapply(sect_start,function(x) min(c(lines_stop[which(lines_stop>x)],lines_secmod2[which(lines_secmod2>x)]))-1)

      #beginning (1st question) of Module
      mod_start <- grep("^Module ",lines_save)
      mod_end <- sapply(mod_start,function(x) min(c(lines_stop[which(lines_stop>x)],lines_secmod2[which(lines_secmod2>x)]))-1)

      section_text <- character(length(lines_save))
      section_index <- integer(length(lines_save))

      mapply(function(s0,s1) {
        section_text[s0:s1]<<- stringr::str_trim(gsub("Section (.*):(.*)[*][/]","\\2",lines_save[s0]))
        section_index[s0:s1]<<-(s0:s1)-s0+1

      },sect_start,sect_end)

      module_text <- character(length(lines_save))
      module_index <- integer(length(lines_save))

      mapply(function(m0,m1) {
        module_text[m0:m1]<<- stringr::str_trim(gsub("Module (.*):(.*)[*][/]","\\2",lines_save[m0]))
        module_index[m0:m1]<<-(m0:m1)-m0+1

      },mod_start,mod_end)

      df_sectmod <- data.frame(section_text,section_index,module_text,module_index)

      #########

      lines <- stringr::str_trim(gsub("[/][*].*","",lines))

      keep <-!grepl("[*][/]",lines)
      lines <- lines[keep]
      df_sectmod <- df_sectmod[keep,]

      vars <- gsub("(.*)[[:space:]](.*)","\\1",lines)
      #vars <- stringr::str_trim(gsub("^_","X_",vars))
      vars <- stringr::str_trim(vars)

      range <- gsub("(.*)[[:space:]](.*)","\\2",lines)
      range <- gsub("[$]","",range)
      start <- as.integer(gsub("(.*)-.*","\\1",range))
      end <- as.integer(gsub("(.*)-(.*)","\\2",range))

      df <- data.frame(var=vars,start=start,end=end,stringsAsFactors = F)

      df <- cbind(df,df_sectmod) %>%
        mutate(sect_type = ifelse(section_index >0,"Core",""))  %>%
        mutate(sect_type = ifelse(module_index >0,"Module",sect_type)) %>%
        mutate(sect_type = ifelse(sect_type == "" , section, sect_type)) %>%
        mutate(sect_num = ifelse(section_index>module_index,section_index,module_index)) %>%
        mutate(section = section)

      df <- df[!is.na(df$start),]


      df
    },


    get_geog = function(geog = NULL, main=TRUE, versions=TRUE, col_atts = TRUE,
                        factorize = TRUE, verbose=TRUE) {

      # make sure some version will be split

      file_mgr <- private$file_mgr_pvt
      dataset_mgr <- private$dataset_mgr_pvt
      dataset <- dataset_mgr$as.list()

      geog_save <- dataset$geog

      geog_mgr <- GeogMgr$new()

      if(!is.null(geog))
        geog_mgr$abbrev <- geog
      else
        geog_mgr$abbrev <- dataset$geog

      geog <- geog_mgr$abbrev

      if(dataset$extent != "public") {

        warning("extent must be 'public' to split geographies")
        return(NULL)
      }

      if(!(main || versions)) return(NULL)

      ver <- 0:highest_version()
      if(!main) ver <- tail(ver,-1)
      if(!versions) ver = head(ver,1)


      purrr::walk(ver,function(version) {

        dataset_mgr$set(version = version)
        dataset_mgr$set(geog_flag = 'off')

        rdata_file <- file_mgr$apply("annual_data_path")

        if(verbose) cat(paste0("Splitting ... trying version [", version, "]\n"))

        df_brfss <- readRDS(file = rdata_file)

        dataset_mgr$set(geog_flag = 'on')

        df_state <- df_brfss %>% filter(`_STATE` == geog)

        # get data for the state of interest and make sure there is data

        if(!is.null(df_state) && nrow(df_state) > 0) {
          fname <- brfss_data_path( rw = 'w')

          # brfss.param(extent = ext)

          if(verbose) cat("Going to save :", fname, "\n")

          saveRDS(df_state,file = fname)
        }


        dataset_mgr$save(geog = geog_save)

      })

      brfss.param(version = 0)

      invisible()
    },

    factorize = function(main=TRUE, versions=TRUE, verbose=FALSE) {

      file_mgr <- private$file_mgr_pvt
      dataset_mgr <- private$dataset_mgr_pvt
      dataset <- dataset_mgr$as.list()

      geog_mgr <- GeogMgr$new()
      geog_mgr$abbrev <- dataset$geog

      if(!(main || versions)) return(NULL)
      ver <- integer(0)
      if(main) ver <-0

      vermax <- highest_version()
      if(vermax == 0) versions = FALSE

      if(versions) ver <- c(ver,1:vermax)

      # df_geogs <- get_geogs_all()

      # factorize each version

      sapply(ver,function(version) {

        browser()
        dataset_mgr$set(version = version)

        params <- file_mgr$patterns()

        ext <- brfss.param(extent)
        #brfss.param(extent = "local")

        if(brfss.param(geog) == '*') {
          geogs <- unique(df_brfss$`_STATE`)

        } else {

          geogs <- c(brfss.param(geog),brfss.param(geogs_other))
          if(is.character(geogs)) {
            geogs <- sapply(geogs,function(state) {
              df_geogs[df_geogs$Abbrev==state,"Id"]
            })

            geogs <- unlist(unname(geogs))
          }
        }

        geog_save <- brfss.param(geog)

        if(brfss.param(geog_flag) == "off") df_geogs <- data.frame(Id = 0,Abbrev = "US")


        mapply(function(id,nm) {
          if(id%in%geogs || id == 0) {

            brfss.param(geog = nm)
            params <- my.brfss.patterns()

            # get data for the state of interest and make sure there is data

            # make sure, even if temporarily, extent param is set to local
            #     to make sure we are saving the data under the geog folder
            fname <- brfss_data_path( rw = 'w')
            df_state <- data.frame()

            tryCatch(expr = {df_state <- readRDS(file = fname)},
                     error =  function(e) e)

            if(nrow(df_state)>0) {
              if(verbose) cat("Factorizing ",nm,"V",version,"\n")

              show_progress(progress,
                            message = paste0("Factorizing ... ", nm, "V", version))

              ##   for now, have to save and (re-)attach the attributes for the columns

              df_state <- df_state %>% make_factors(verbose = verbose)
            }

            # make sure, even if temporarily, extent param is set to local
            #     to make sure we are saving the data under the geog folder


            fname <- brfss_data_path( rw = 'w')

            if(verbose) cat("Going to save :", fname, "\n")

            show_progress(progress,
                          message = paste0("Factorizing ... saving ", fname))

            saveRDS(df_state,file = fname)

          }
        },df_geogs$Id,df_geogs$Abbrev)

        brfss.param(geog = geog_save)
        #brfss.param(extent = ext)


      })


      invisible()
    },

    highest_version = function() {

      file_mgr <- private$file_mgr_pvt
      dataset_mgr <- private$dataset_mgr_pvt
      dataset <- dataset_mgr$as.list()

      if(dataset$source == "sas") {
        fldr <- file_mgr$apply("sas_data_folder")
      } else {

        fldr <- file_mgr$apply("annual_data_folder")
      }

      files <- list.files(fldr)

      if(length(files) == 0) {
        if(brfss.param(source) == "sas") {
          fldr <- file_mgr$apply("sas_raw_data_folder")
        } else {
          fldr <- file_mgr$apply("ascii_path")
        }
        files <- list.files(fldr)

      }

      vers <- 0
      if(length(files) > 0) {

        files <- files[grep("V[0-9][.]",files)]
        vers <- as.integer(gsub(".*V([0-9]*)[.].*","\\1",files))
        if(length(vers)==0) vers <- 0
      }

      max(vers)
    }



  ),

  active = list(

    verbose = function(value) {

      if(missing(value)) {
        return(private$verbose_pvt)
      } else {
        if(!class(value) == "logical") {
          warning("This property requires a logical value")
          return(invisible())
        }

        private$verbose_pvt <- value

        return(invisible(NULL))
      }


    },

    has_raw_data = function(value) {

      if(!missing(value)) {
        message("this property is read-only")
        return(NULL)
      }

      p <- private

      file <- p$file_mgr_pvt$apply("annual_raw_data_path")

      file.exists(file)

    },
    dataset_mgr = function(value) {

      if(missing(value)) return(private$dataset_mgr_pvt)

      if(!inherits(value, "DataSetMgr")) {
        message("value must be a DataSetMgr object")
        return(NULL)
      }

      file_mgr$dataset_mgr <- value
      private$dataset_mgr_pvt <- file_mgr$dataset_mgr
    }
  )
)

#' @export
AsciiRawDataMgr <- R6::R6Class(
  classname = "AsciiRawDataMgr",
  inherit = RawDataMgr,

  private = list(

  ),

  public = list(

    initialize = function(...) {

      super$initialize(...)

      private$dataset_mgr_pvt$set(source = "ascii")

    },

    ascii_data_url = function(year) {

      cur_year <-  private$file_mgr_pvt$dataset_mgr$get(year)
      if(missing(year))
        year <-  private$file_mgr_pvt$dataset_mgr$get(year)
      else
        private$file_mgr_pvt$dataset_mgr$set(year = year)

      url <- private$file_mgr_pvt$apply("ascii_downloads_url")
      #paste0("https://www.cdc.gov/brfss/annual_data/", year,"/files/LLCP",year,"ASC.zip")

      private$file_mgr_pvt$dataset_mgr$set(year = cur_year)
      url
    },

    download = function(destpath = NULL, unzip=TRUE, rmzip=TRUE, progress = NULL) {

      p <- private

      #if this object is used for download then it must be public

      if(p$dataset_mgr$get(extent) != "public") {

        stop(paste0("extent must be 'public'. ",
                    "local data are not downloaded from the CDC BRFSS website."))

      }

      file_mgr <- p$file_mgr_pvt
      file_mgr$dataset_mgr$set(geog_flag = "off")

      if(is.null(destpath)) destpath <- file_mgr$apply("ascii_raw_data_folder")
      destpath <- normalizePath(destpath,winslash = "/",mustWork = FALSE)
      #

      if(!dir.exists(destpath)) dir.create(destpath,recursive = TRUE)

      version <- 0

      cont <- TRUE
      ctr <- 0
      while(cont && ctr<6) {
        ctr <- ctr + 1

        file_mgr$dataset_mgr$set(version = version)

        url <- file_mgr$apply("ascii_downloads_url")

        destfile <- file_mgr$apply("ascii_zip_path")

        status <- httr::HEAD(url)$status
        if(status==200) {

          path <- dirname(destfile)

          if(!dir.exists(path)) dir.create(path,recursive = TRUE)

          ## the larger BRFSS files began to take more than 60 seconds to download
          ##  so a larger timeout option is necessary

          to <- getOption("timeout")
          options(timeout = 180)

          download.file(url = url,destfile = destfile,
                        method = "libcurl",quiet = TRUE, mode = "wb")

          options(timeout=to)

          if(unzip) {
            exdir <- gsub("/$","", path)
            unzip(destfile,exdir = exdir)
            if(rmzip) file.remove(destfile)
          }

        } else {

          cont <- FALSE
        }
        version <- version + 1
      }

    },

    convert = function(layout = NULL, completes = T, main = TRUE,
                       versions = TRUE, verbose = FALSE) {


      file_mgr <- private$file_mgr_pvt
      file_mgr$dataset_mgr$set(geog_flag = "off")

      if(is.null(layout)) {
        layout <- private$layout_mgr$layout
      }

      if(is.null(layout)) {
        return (NULL)
      }

      if(main) version <- 0 else {
        if(versions) version = 1 else return()
      }

      file_mgr$dataset_mgr$set(version = version)


      file_raw <- file_mgr$apply("ascii_filename_raw")
      path_raw <- file_mgr$apply("ascii_path_raw")

      while (file.exists(path_raw)) {

        if(verbose) cat("... reading version [", version, "] : ", path_raw, "\n")

        df = self$read(filename = path_raw, layout = layout, verbose = verbose)

        #    df <- add_col_attributes(df)

        path <- file_mgr$apply("annual_data_path")
        #    path_unf <-  file_mgr$apply("annual_data_unfactored_path")


        if(!dir.exists(dirname(path))) dir.create(dirname(path))


        if(verbose) cat("... writing ", path,"\n")

        saveRDS(df, file = path)
        #    saveRDS(df, file = path_unf)

        version <- version + 1
        file_mgr$dataset_mgr$set(version = version)

        path_raw <- file_mgr$apply("ascii_path_raw")

      }

      file_mgr$dataset_mgr$set(version = 0)

      invisible()
    }

    ,

    read = function(filename=NULL,layout = NULL, verbose = FALSE) {

      #  env <- get.brffs.env()
      # state <- match.arg(state)


      if(is.null(filename) || is.null(layout)) {
        return (NA)

      }


      ################################
      ##
      ##  get the layout data
      ##
      if (class(layout) == "character") {
        df_fields_yy <- read.brfss.layout(layout)
      } else {
        df_fields_yy <- layout
      }

      ############################################################
      ##
      ##  remove col_names of negative width columns
      ##
      widths  <-  as.integer(df_fields_yy$field_size)

      col.names  <-  df_fields_yy$col_name[widths>0]

      ################################
      ##
      ##  read the file based on the layout
      ##

      col_types <- layout$var_type
      names(col_types) <- layout$col_name
      col_types[grepl("^DUMMY",names(col_types))] <- 'NULL'

      ## this is a kludge ... the weight fields should be numeric

      col_types[grepl("_.*WT.*",names(col_types))] <- 'numeric'
      col_types[is.na(col_types)] <- 'character'

      # end kludge

      widths <- layout$field_size

      x <- readLines(filename)


      df <- iotools::dstrfw(x = x,col_types = col_types, widths = widths)


      #######################################################
      ##
      ##  if completes only then make sure DISPCODE in completes range (<2000)
      ##
      # if (completes) df <- df %>% filter(DISPCODE<2000)

      ##########################################################
      ##
      ##  return the data frame
      ##

      df
    },

    national_pcts = function(year, col, roi = NULL) {

      if(missing(col)) {
        message("col must be supplied")
        return(NULL)
      }

      if(missing(year)) year <- private$dataset_mgr_pvt$get(year)

      cb_mgr <- CodebookMgr$new(dataset_mgr = private$dataset_mgr_pvt)

      df_lo <- cb_mgr$get_layout() %>%
        filter(col_name == .env$col)

      bad_col <- df_lo %>%
        {nrow(.) != 1}

      if(bad_col) {
        message(paste0("col (", col, ") does not exist"))
        return(NULL)
      }


      df <- self$get_one_column(year = year, col = col)

      # df2 <- df %>% mutate(COI = if_else(grepl(roi, {{col}}),TRUE, FALSE))
      #
      df_each <- df %>%
        group_by(`_STATE`, .data[[col]]) %>%
        summarise(n = sum(`_LLCPWT`), .groups = "drop")

      df_each_den <- df_each %>%
        group_by(`_STATE`) %>%
        summarise(den = sum(n), .groups = "drop")



      df <- df_each %>% left_join(df_each_den) %>%
        mutate(pct = round(n/den*100,1))  %>%
        filter(grepl(roi, .data[[col]]))

      response <- df %>% pull(.data[[col]]) %>% as.character() %>% unique()

      df <- df %>%
        left_join(GeogMgr$geogs(), join_by(`_STATE` == fips)) %>%
        select(name, abbr, region, division, pct) %>%
        as.data.frame() %>%
        arrange(desc(pct))

      structure(df,
                year = year,
                col_name = df_lo$col_name,
                label = df_lo$label,
                response = response)


    },

    get_one_column = function(year, col, geog = NULL, inc_wt = TRUE, rm_inv = TRUE) {

      if(missing(col)) {
        message("col (column to read) must be supplied")
        return(NULL)
      }

      if(missing(year)) year <- DataSetMgr$new()$get(year)

      ds_mgr <-DataSetMgr$new(year = year, geog = geog, geog_flag = "off",
                                      extent = "public", source = "ascii")

      file_mgr <- FileMgr$new(dataset_mgr = ds_mgr)

      #file_mgr$get("ascii_raw_data_folder")
      asc_file <- file_mgr$apply("ascii_path_raw")


      cb_mgr <- CodebookMgr$new(dataset_mgr = ds_mgr)

      df_lo <- cb_mgr$get_layout() %>%
        dplyr::filter(col_name %in% c("_STATE", "_LLCPWT", col))


      col_names  <-  df_lo %>% pull(col_name)

      cols <- readr::fwf_positions(
        start = df_lo %>% pull(start),
        end   = df_lo %>% pull(end),
        col_names = df_lo %>% pull(col_name)
      )

      col_types <- df_lo %>% pull(var_type) %>% tolower() %>% substring(1,1)


      suppressMessages(
        df <- readr::read_fwf(file = asc_file, col_positions = cols,
                              readr::cols(.default = "c") ) %>%
          readr::type_convert()
      )

      geog_mgr <- GeogMgr$new()

      if(!missing(geog)) {

        geog_mgr$geog <- geog
        fip <- ggeog_mgr$fips
        df <- df %>% filter(`_STATE` == fips)
      }

      # apply column values if available

      vals <- cb_mgr$get_values() %>% filter(col_name == .env$col)

      if(!is.null(vals)) {

        lvls <- vals$value
        lbls <- vals$text
        vals <- df[[col]]

        df <- df %>% mutate({{col}} := factor(vals, levels = lvls, labels = lbls))

        if(rm_inv) {
          df <- df %>%
            filter(!is.na(.data[[col]])) %>%
            filter(!grepl("refuse|not sure",.data[[col]], ignore.case = T))
        }
      }

      df
    },

    national_plot = function(year, col, roi) {

      if(missing(year)) {
        year <- private$dataset_mgr$get(year)
      }
      df_natl <- self$national_pcts(year = year, col = col, roi = roi)


      attrs <- df_natl %>% attributes()

      labels<-sprintf("%0.1f",df_natl[["pct"]])

      title <- attrs$label
      subtitle <- attrs$year
      caption <- attrs$label

      lvls <- df_natl %>% pull(name)

      df_gpl <- df_natl %>%
        mutate(name = factor(name, levels = lvls)) %>%
        mutate(me = abbr == {{my_geog}})

      gpl<- df_gpl %>%

        ggplot(aes(x = name,y=pct,fill=me)) +
        geom_bar(stat = "identity") +

        geom_text(size=3,label=labels,
                  show.legend=F, hjust=-0.1, vjust=0.5) + #,
        # position=position_dodge(width=dodge_width), vjust=vjust) +

        # scale_fill_gradient(limits=c(fmin,fmax), low = "#13FF00", high = "#FF1311", space = "Lab",
        #                     na.value = "grey50", guide = "colourbar") +
        scale_fill_manual(values = clrs) +

        coord_flip() +
        #      geom_errorbar(aes_string(ymin="CI_lower",ymax="CI_upper"),
        #                    width=error_bar_width,color="blue") +
        labs(title=title, subtitle=subtitle,caption=caption) +
        #  scale_y_continuous(limits = c(0,national_rate_max)) +
        #      scale_x_discrete(labels=labels) +
        theme(panel.grid.major.x = element_line(colour = "#999999", linewidth = 0, linetype=1, lineend="butt"),
              panel.background = element_blank(),
              panel.grid.major.y = element_blank(),
              legend.position = "none")

      gpl

    }



  ),

  active = list(

    file_mgr = function(value) {

      if(missing(value)) return(private$file_mgr_pvt)

      private$file_mgr_pvt <- value
    }
  )
)

#' @export
SasRawDataMgr <- R6::R6Class(
  classname = "SasRawDataMgr",
  inherit = RawDataMgr,

  private = list(

  ),

  public = list(

    initialize = function(...) {

      p <- private

      super$initialize(...)

      p$dataset_mgr_pvt$set(source = "sas")

    },

    download = function(folderout = NULL) {

      p <- private


      if(p$dataset_pvt$get(extent) != "public") {

        stop(paste0("extent must be 'public'. ",
                    "local data are not downloaded from the CDC BRFSS website."))

      }

      file_mgr <- p$file_mgr_pvt
      dataset_mgr <- file_mgr$dataset_mgr
      dataset_mgr$set(geog_flag = "off")


      params <- dataset_mgr$patterns

      #files <- sas.url.pattern.downloads.versions()
      file_pttrn <- file_mgr$get("xpt_download_zip_file")

      if(is.null(folderout)) folderout <- file_mgr$apply("sas_raw_data_folder")

      folderout <- gsub("([^[/])$","\\1/",folderout)

      if(!dir.exists(folderout)) dir.create(folderout,recursive = T)

      urlfiles <- file_mgr$apply("brfss_url_files")


      to <- getOption("timeout")
      options(timeout = 180)

      ok <- TRUE
      version <- 0

      while(ok) {

        dataset_mgr$set(version = version)

        file <- file_mgr$patternize(strIn = file_pttrn)

        url <- paste0(urlfiles,file)
        status <- httr::HEAD(url)$status
        if(status==200) {

          fileout <- paste0(folderout,file)

          download.file(url = url,destfile = fileout,
                        method = "libcurl", quiet = TRUE)

          if(grepl("[.]zip$",fileout)) {
            unzip(fileout,exdir = normalizePath(folderout))
            file.remove(fileout)
          } # end if(grepl()


        } else {
          ok <- FALSE
        }
        version <- version + 1
      }
      options(timeout = to)
      invisible()
    },

    convert = function(verbose = FALSE) {

      p <- private

      file_mgr <- p$file_mgr_pvt
      dataset_mgr <- file_mgr$dataset_mgr

      dataset_mgr$set(geog_flag = "off")

      self$read(version = 0)

      cont <- TRUE

      ivers <-1

      if(verbose) cat(" ... trying versions\n ")

      while (cont) {

        if(verbose) cat("Converting version=",ivers,"\n")

        df_xpt <- read.xpt(version=ivers,verbose=TRUE)

        cont <- !is.null(df_xpt)
        if(cont) {
          ##
          ##  get save file (.rds) location
          ##

          save_file <- file_mgr$apply("annual_data_path")

          #df_xpt <- df_xpt %>% add_col_attributes()

          if(!dir.exists(dirname(save_file))) dir.create(dirname(save_file),recursive = T)

          saveRDS(df_xpt,file = save_file)
        }

        ivers <- ivers + 1
      }

      dataset_mgr$set(version = 0)
    },

    read = function(version = 0, verbose = F) {

      p <- private

      file_mgr <- p$file_mgr_pvt
      dataset_mgr <- file_mgr$dataset_mgr

      dataset_mgr$set(geog_flag = "off")
      old_vers <- dataset_mgr$get(version)
      dataset_mgr$set(version = version)

      ########################################################################%%%%%%%%%
      ##
      ##    If file and folder names not supplied, create them from the file patterns

      ##
      ##    get sasout location
      ##
      sasout_file <- file_mgr$apply("sas_sasout_path")

      ##
      ##    get xpt raw data location
      ##

      xpt_file <- file_mgr$apply("xpt_path")

      dataset_mgr$set(version = old_vers)

      ##
      ##    read the xpt files
      ##

      if(version>0) {
        if(verbose) cat("Trying version ",version,"\n")
      } else {
        if(verbose) cat("Trying main file \n")
      }



      if(file.exists(xpt_file)) {
        if(verbose) cat("Reading ",xpt_file,"\n")
        df_xpt <- structure(
          haven::read_xpt(xpt_file),
          class = c( "brfss_sas_raw", "data.frame"),
          year = dataset_mgr$get(year)
        )

        # st <- fips::state_fips(geog)
        # df_xpt <- df_xpt %>% filter(`_STATE` == st)

        cat("Getting sasout\n")

      } else {
        if(verbose) cat(xpt_file," doesn't exist\n")
        df_xpt <- NULL
      }

      df_xpt
    },

    get_one_column = function(year, col, geog = NULL, version = 0, inc_wt = TRUE, rm_inv = TRUE) {

      if(missing(col)) {
        message("col (column to read) must be supplied")
        return(NULL)
      }

      file_mgr <- private$file_mgr_pvt$clone(deep = TRUE)
      dataset_mgr <- file_mgr$dataset_mgr


      if(!missing(year)) dataset_mgr$set(year = year)

      year <- dataset_mgr$get(year)

      dataset_mgr$set(geog = geog, geog_flag = "off", extent = "public", source = "sas")


      cb_mgr <- brfss::CodebookMgr$new(dataset_mgr = dataset_mgr)

      suppressMessages(
        df <- self$read(version = version) %>%
          dplyr::select(`_STATE`, `_LLCPWT`, all_of(col))
      )

      browser()
      geog_mgr <- GeogMgr$new()

      if(!missing(geog)) {

        geog_mgr$geog <- geog
        fip <- ggeog_mgr$fips
        df <- df %>% filter(`_STATE` == fips)
      }

      # apply column values if available

      vals <- cb_mgr$get_values() %>% filter(col_name == .env$col)

      if(!is.null(vals)) {

        lvls <- vals$value
        lbls <- vals$text
        vals <- df[[col]]

        df <- df %>% mutate({{col}} := factor(vals, levels = lvls, labels = lbls))

        if(rm_inv) {
          df <- df %>%
            filter(!is.na(.data[[col]])) %>%
            filter(!grepl("refuse|not sure",.data[[col]], ignore.case = T))
        }
      }

      df
    }


  ),

  active = list(
  )

)
