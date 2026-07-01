
#' RawDataMgr R6 Class
#'
#' @export
RawDataMgr <- R6::R6Class(
  classname = "RawDataMgr",

  private = list(

    ..file_mgr = NULL,
    ..dataset_mgr = NULL,
    ..codebook_mgr = NULL,
    ..verbose = FALSE,
    ..values = NULL,
    ..layout = NULL,

    ..data_path = function(rw = c("r","w")) {

      p <- private

      read <- rw == 'r'
      write <- rw == 'w'

      fldr <- self$file_mgr$apply("annual_data_folder")

      if(!dir.exists(fldr) && write) {
        dir.create(fldr, recursive = TRUE)
      }

      file <- self$file_mgr$apply("annual_data_file")

      path <- paste0(fldr,file)

      if(read && !file.exists(path)) path <- NULL

      path
    },

    ..add_column_attributes = function(df_brfss = NULL, layout = NULL,
                                       verbose = FALSE, progress = NULL) {

      p <- private

      if(is.null(layout)) {
        layout <- p$..layout
      }

      if(is.null(layout)) {
        message("Adding column attributes requires a layout.")
        return (NULL)
      }

      df_brfss <- df_brfss %>%
        p$..add_col_attributes()

      df_brfss
    },

    ..prep_atts = function(x) {

      if(is.na(x) || is.null(x)) x <- ""

      x
    },

    ..add_col_attributes = function(df_in) {

      p <- private
      df_layout <- p$..layout

      if(!"saq" %in% colnames(df_layout)) {
        df_layout$saq <- NA
      }

      purrr::pwalk(df_layout, \(...) {

        row <- list(...)

        col_name <- row$col_name
        if(!is.null(df_in[[col_name]])) {

          row$sect_type  <- row$sect_type |> p$..prep_atts()
          row$sect_num  <- row$sect_num |> p$..prep_atts()
          row$section  <- row$section |> p$..prep_atts()
          row$label  <- row$label |> p$..prep_atts()
          row$question   <- row$question  |> p$..prep_atts()
          row$question_num   <- row$question_num  |> p$..prep_atts()



          atts <-  c(
            "section_type" = row$sect_type,
            "section_num" = row$sect_num,
            "section_index" = row$question_num,
            "section_name" = stringr::str_trim(row$section),
            "label" = row$label,
            "question" = row$question,
            "variable" = col_name,
            "is_calculated" = as.logical(row$calculated),
            "is_custom" = FALSE,
            "is_saq" = as.logical(row$saq)
          )

          df_in <<- df_in %>% p$..add_attributes({{col_name}}, atts = atts)
          class(df_in[[col_name]]) <<- c("brfss", class(df_in[[col_name]]))

        } else {

        }
      })

      df_in

    },

    ..add_attributes = function(df, ... , atts) {

      col <- as.character(quosures(...))[1]

      mapply(function(att, nm) {
        #browser()

        tryCatch(
          {
            if(is.null(att)) att <- ""
            attr(df[[col]], nm) <<- att
          },
          error = function(e) {

            cat("\n", e$message,"\n")
            cat("trying to set column [",col, "] attribute [", nm, "] to value [", att,"]\n", sep = "")
            if(is.null(df[[col]]))  cat(" ... column ", col, " = NULL", "\n\n", sep = "")
          },
          warning = function(w) {

            cat("\n", w$message,"\n")
            cat("trying to set column [",col, "] attribute [", nm, "] to value [", att,"]\n", sep = "")
            if(is.null(df[[col]]))  cat(" ... column ", col, " = NULL", "\n\n", sep = "")
          }
        )
      }, atts, names(atts) )
      df
    }




  ),

  public = list(

    initialize = function(file_mgr = NULL, dataset_mgr = NULL, verbose = FALSE) {

      p <- private

      if(!is.null(file_mgr) && inherits(file_mgr,"FileMgr"))
        p$..file_mgr <- file_mgr
      else
        p$..file_mgr <- FileMgr$new()

      if(!is.null(dataset_mgr) && inherits(dataset_mgr,"DataSetMgr")) {
        p$..file_mgr$dataset_mgr <- dataset_mgr
        p$..dataset_mgr <- dataset_mgr
      } else {

        p$..dataset_mgr <- p$..file_mgr$dataset_mgr
      }

      p$..codebook_mgr <- CodebookMgr$new(dataset_mgr = p$..dataset_mgr)

      p$..values <- p$..codebook_mgr$get_values()

      p$..layout <- p$..codebook_mgr$get_layout()

      p$..verbose <- verbose

    },

    dl_metadata = function(fldrout = NULL, quietly = TRUE) {

      file_mgr <- private$..file_mgr
      dataset_mgr <- private$..dataset_mgr
      dataset <- dataset_mgr$as.list()

      if(private$..verbose) cat(" ... downloading ... metadata ... ")

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

      file_mgr <- private$..file_mgr
      dataset_mgr <- private$..dataset_mgr
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

      if(private$..verbose) cat(" ... downloading ... codebook ... ")

      p$..codebook_mgr$download_codebook(fldrout = fldrout)

    },

    read_sas_field_ranges = function() {

      file_mgr <- private$..file_mgr
      dataset_mgr <- private$..dataset_mgr
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

      sect_end <- sapply(sect_start,function(x) {
        min(c(lines_stop[which(lines_stop>x)],lines_secmod2[which(lines_secmod2>x)]))-1
      })

      #beginning (1st question) of Module
      mod_start <- grep("^Module ",lines_save)
      mod_end <- sapply(mod_start,function(x) {
        min(c(lines_stop[which(lines_stop>x)],lines_secmod2[which(lines_secmod2>x)]))-1
      })

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

    convert = function(f_read = NULL, layout = NULL, completes = T, main = TRUE,
                       versions = TRUE, verbose = FALSE) {


      p <- private

      p$..file_mgr$dataset_mgr$set(geog_flag = "off")

      if(is.null(layout)) {

        layout <- p$..layout
      }

      if(is.null(layout)) {

        return (NULL)
      }

      if(main) version <- 0 else {
        if(versions) version = 1 else return()
      }

      p$..file_mgr$dataset_mgr$set(version = version)

      path_raw <- p$..file_mgr$apply("path_raw")

      while (file.exists(path_raw)) {

        if(verbose) cat("... reading version [", version, "] : ", path_raw, "\n")

        browser()
        df  <-  do.call(what = f_read,
                        args = list(filename = path_raw, layout = layout, verbose = verbose)) %>%
          self$factorize(verbose = verbose)

        #    df <- add_col_attributes(df)

        path <- p$..file_mgr$apply("annual_data_path")
        #    path_unf <-  file_mgr$apply("annual_data_unfactored_path")


        if(!dir.exists(dirname(path))) dir.create(dirname(path))


        if(verbose) cat("... writing ", path,"\n")

        saveRDS(df, file = path)
        #    saveRDS(df, file = path_unf)

        version <- version + 1
        p$..file_mgr$dataset_mgr$set(version = version)

        path_raw <- p$..file_mgr$apply("path_raw")

      }

      p$..file_mgr$dataset_mgr$set(version = 0)

      invisible()

    },

    get_geog = function(geog = NULL, main=TRUE, versions=TRUE, col_atts = TRUE,
                        factorize = TRUE, verbose=TRUE) {

      # make sure some version will be split

      file_mgr <- private$..file_mgr
      dataset_mgr <- private$..dataset_mgr
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

      ver <- 0:self$highest_version()
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
          fname <- private$..data_path( rw = 'w')

          # brfss.param(extent = ext)

          if(verbose) cat("Going to save :", fname, "\n")

          saveRDS(df_state,file = fname)
        }


        dataset_mgr$save(geog = geog_save)

      })

      brfss.param(version = 0)

      invisible()
    },

    factorize = function(df_brfss = NULL, verbose=FALSE) {

      file_mgr <- private$..file_mgr
      dataset_mgr <- private$..dataset_mgr
      dataset <- dataset_mgr$as.list()


      # get data for the state of interest and make sure there is data

      # make sure, even if temporarily, extent param is set to local
      #     to make sure we are saving the data under the geog folder

      df_brfss |>
        private$..add_column_attributes() |>
        self$make_factors(verbose = verbose)

    },

    make_factors = function(df_brfss = NULL, df_layout = NULL, df_vals = NULL,
                            cols = NULL, overwrite = TRUE, verbose = FALSE) {


      p <- private

      if(is.null(df_brfss)) return(NULL)

      if(is.null(df_vals)) df_vals <- p$..values

      if(is.null(df_layout)) df_layout <- p$..layout

      df_not_factors <- self$quest_types(df_layout, df_vals) %>%
        filter(type!= "factor") %>%
        {row.names(.)<-NULL;.}

      df_factors <- self$quest_types(df_layout, df_vals) %>%
        filter(type == "factor")  %>%
        {row.names(.)<-NULL;.}

      if(verbose) cat("factorizing ... \n")

      if(!is.null(cols)) {

        df_factors <- df_factors %>% filter(col_name %in% cols)
      }

      # make the 'not factors that are all NA compatible with factors)

      purrr::walk(df_not_factors$col_name, function(col) {

        x <- df_brfss[[col]]

        x_test <- x %>% unique()

        if(length(x_test) == 1 && is.na(x_test)) {

          df_brfss[[col]] <- factor(NA)
        }



      })


      purrr::walk(df_factors$col_name, function(col) {
        #

        if(verbose) cat("  .. ", col)

        tryCatch({

          if(!(is.factor(df_brfss[[col]])) || overwrite) {

            a<- attributes(df_brfss[[col]])

            df_my_vals <- df_vals %>% filter(col_name==col)

            levels <- df_my_vals %>% pull(value)
            labels <- df_my_vals %>% pull(text)

            x <- df_brfss[[col]]
            x_test <- x %>% unique() %>% {.[!is.na(.)]}
            if(all(gsub("[0-9]","",levels) == "")) levels <- as.integer(levels)
            if(all(gsub("[0-9]","",x_test) == "")) x <- as.integer(x)

            atts_mgr <- Attributes_Mgr$new(a)
            x <- atts_mgr$modify(x)
            x <- atts_mgr$factorize(x, levels = levels, labels = labels)


            #          f <- x %>% add_prep_attrs(a) %>% factor_brfss()

            #attributes(f) <- c(attributes(f),a)
            df_brfss[col] <<- x
            if(verbose) cat(" OK","\n")
          } else {
            if(verbose) cat(" ... already factorized","\n")
          }

        }, error = function(e) {

          if(verbose) cat(" Failed","\n")


        },
        warning = function(w) {

          message(paste0(
            w$message,"\n",
            " attempting to make factors for column ",
            cli::style_bold(cli::col_blue(paste0("[",col, "]"))),"\n")
          )
        })
      })

      if(verbose) cat(" ... DONE factorizing  \n")


      df_brfss
    },

    quest_types = function(df_layout = NULL, df_vals = NULL, ...) {

      if(is.null(df_vals)) return(NULL)

      if(is.null(df_layout))return(NULL)

      df_cnt <- df_vals %>% group_by(col_name) %>% summarise(n=n())

      is.calc <- grepl("^Calc",df_layout$section)
      is.wt <- grepl("[Ww]eighting.V",df_layout$section)

      has.mult <- df_layout %>%
        left_join(df_cnt, by = "col_name")%>%
        replace(is.na(.), 0) %>%
        pull(n) %>% .[] > 1

      col_names <- df_layout %>% pull(col_name)
      #  df <- data.frame(col_name = col_names)

      types <- mapply(function(col, calc, wt) {
        #cat(" trying ... [", col, "] is.calc: ", calc," | is.wt (range): ", wt, "\n")
        df_lo <- df_layout %>% filter(col_name == col)

        df0 <- df_vals %>%
          filter(col_name == col)

        if(nrow(df0)>0) {
          maxv <- df0 %>% pull(maxval)
          txt <- df0 %>% pull(text)

          dk <- any(grepl("^[Dd]on.{0,1}t [Kk]now",txt))
          ref <-any(grepl("^[Rr]efused$",txt))
          yes1 <- tolower(txt[1]) == "yes"
          fm1 <- grepl("male",tolower(txt[1]))


          #      if(any(!is.na(maxv)) && !calc && !wt) {
          if(any(!is.na(maxv)) && !wt) {
            return("range")
            #      } else if((fm1 || dk || ref || yes1) && !wt){ #&& !calc && !wt){
          } else if(!wt){ #&& !calc && !wt){
            return ("factor")
          }else {
            return ("unknown")
          }
        } else {
          return("unknown")
        }
      }, col_names, is.calc, is.wt & !has.mult)

      types[grepl("_STSTR",col_names)] <- "stratum"

      df <- data.frame(col_name = col_names, type = types,
                       calc = is.calc, wt = is.wt,
                       row.names = NULL)
      df
    },

    highest_version = function() {

      file_mgr <- private$..file_mgr
      dataset_mgr <- private$..dataset_mgr
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
        return(private$..verbose)
      } else {
        if(!class(value) == "logical") {
          warning("This property requires a logical value")
          return(invisible())
        }

        private$..verbose <- value

        return(invisible(NULL))
      }


    },

    has_raw_data = function(value) {

      if(!missing(value)) {
        message("this property is read-only")
        return(NULL)
      }

      p <- private

      file <- p$..file_mgr$apply("annual_raw_data_path")

      file.exists(file)

    },

    dataset_mgr = function(value) {

      if(missing(value)) return(private$..dataset_mgr)

      if(!inherits(value, "DataSetMgr")) {
        message("value must be a DataSetMgr object")
        return(NULL)
      }

      file_mgr$dataset_mgr <- value
      private$..dataset_mgr <- file_mgr$dataset_mgr
    },

    file_mgr = function(value) {

      if(missing(value)) return(private$..file_mgr)

      if(!inherits(value, "FileMgr")) {
        message("value must be a FileMgr object")
        return(NULL)
      }


      private$..file_mgr <- value
    }
  )
)

#=========================================================================================

#   ASCII Raw Data Manager

#' @export
AsciiRawDataMgr <- R6::R6Class(
  classname = "AsciiRawDataMgr",
  inherit = RawDataMgr,

  private = list(

  ),

  public = list(

    initialize = function(...) {

      super$initialize(...)

      private$..dataset_mgr$set(source = "ascii")

    },

    ascii_data_url = function(year) {

      cur_year <-  private$..file_mgr$dataset_mgr$get(year)
      if(missing(year))
        year <-  private$..file_mgr$dataset_mgr$get(year)
      else
        private$..file_mgr$dataset_mgr$set(year = year)

      url <- private$..file_mgr$apply("ascii_downloads_url")
      #paste0("https://www.cdc.gov/brfss/annual_data/", year,"/files/LLCP",year,"ASC.zip")

      private$..file_mgr$dataset_mgr$set(year = cur_year)
      url
    },

    download = function(destpath = NULL, unzip=TRUE, rmzip=TRUE, progress = NULL) {

      p <- private

      #if this object is used for download then it must be public

      if(p$dataset_mgr$get(extent) != "public") {

        stop(paste0("extent must be 'public'. ",
                    "local data are not downloaded from the CDC BRFSS website."))

      }

      file_mgr <- p$..file_mgr
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


      super$convert(f_read = self$read, layout = layout, completes = completes, main = main,
                    versions = versions, verbose = verbose)


      return()

      ##  this is the original below


      p <- private

      p$..file_mgr$dataset_mgr$set(geog_flag = "off")

      if(is.null(layout)) {

        layout <- p$..layout
      }

      if(is.null(layout)) {

        return (NULL)
      }

      if(main) version <- 0 else {
        if(versions) version = 1 else return()
      }

      p$..file_mgr$dataset_mgr$set(version = version)


      file_raw <- p$..file_mgr$apply("ascii_filename_raw")
      path_raw <- p$..file_mgr$apply("ascii_path_raw")

      while (file.exists(path_raw)) {

        if(verbose) cat("... reading version [", version, "] : ", path_raw, "\n")

        df  <-  self$read(filename = path_raw, layout = layout, verbose = verbose) %>%
          self$factorize(verbose = verbose)

        #    df <- add_col_attributes(df)

        path <- p$..file_mgr$apply("annual_data_path")
        #    path_unf <-  file_mgr$apply("annual_data_unfactored_path")


        if(!dir.exists(dirname(path))) dir.create(dirname(path))


        if(verbose) cat("... writing ", path,"\n")

        saveRDS(df, file = path)
        #    saveRDS(df, file = path_unf)

        version <- version + 1
        p$..file_mgr$dataset_mgr$set(version = version)

        path_raw <- p$..file_mgr$apply("ascii_path_raw")

      }

      p$..file_mgr$dataset_mgr$set(version = 0)

      invisible()

    },

    read = function(filename=NULL,layout = NULL, verbose = FALSE) {

      #  env <- get.brffs.env()
      # state <- match.arg(state)


      if(is.null(filename) || is.null(layout)) {
        return (NA)

      }


      # ################################
      # ##
      # ##  get the layout data
      # ##
      # if (class(layout) == "character") {
      #   df_fields_yy <- read.brfss.layout(layout)
      # } else {
      #   df_fields_yy <- layout
      # }
      #
      # ############################################################
      # ##
      # ##  remove col_names of negative width columns
      # ##
      # widths  <-  as.integer(df_fields_yy$field_size)
      #
      # col.names  <-  df_fields_yy$col_name[widths>0]

      ################################
      ##
      ##  read the file based on the layout
      ##

      layout <- layout %>% filter(!grepl("DUMMY", col_name))


      col_types <- layout$var_type
      names(col_types) <- layout$col_name


      ## this is a kludge ... the weight fields should be numeric

      # col_types[grepl("_.*WT.*",names(col_types))] <- 'numeric'
      # col_types[is.na(col_types)] <- 'character'

      start <- layout$start
      end  <-  layout$end
      col_names <- layout$col_name

      df <- readr::read_fwf(
        file = filename,
        col_positions  = readr::fwf_positions(
          start = start,
          end   = end,
          col_names = col_names),
        show_col_types = FALSE
      ) %>%
        as.data.frame()


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

      if(missing(year)) year <- private$..dataset_mgr$get(year)

      cb_mgr <- CodebookMgr$new(dataset_mgr = private$..dataset_mgr)

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

      if(missing(value)) return(private$..file_mgr)

      private$..file_mgr <- value
    }
  )
)


#=========================================================================================

#   SAS Raw Data Manager

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

      p$..dataset_mgr$set(source = "sas")

    },

    download = function(folderout = NULL) {

      p <- private


      if(p$dataset_pvt$get(extent) != "public") {

        stop(paste0("extent must be 'public'. ",
                    "local data are not downloaded from the CDC BRFSS website."))

      }

      file_mgr <- p$..file_mgr
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

    convert = function(layout = NULL, completes = T, main = TRUE,
                       versions = TRUE, verbose = FALSE) {


      super$convert(f_read = self$read, layout = layout, completes = completes, main = main,
                    versions = versions, verbose = verbose)


      return()


      p <- private

      file_mgr <- p$..file_mgr
      dataset_mgr <- file_mgr$dataset_mgr

      dataset_mgr$set(geog_flag = "off")

      cont <- TRUE

      ivers <-0

      if(verbose) cat(" ... trying versions\n ")

      while (cont) {

        if(verbose) cat("Converting version=",ivers,"\n")

        self$read(version = ivers)


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

    read = function(filename = NULL, layout = NULL, verbose = F) {

      p <- private

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

      xpt_file <- filename


      ##
      ##    read the xpt files
      ##

      # if(version>0) {
      #   if(verbose) cat("Trying version ",version,"\n")
      # } else {
      #   if(verbose) cat("Trying main file \n")
      # }
      #

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

      file_mgr <- private$..file_mgr$clone(deep = TRUE)
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
