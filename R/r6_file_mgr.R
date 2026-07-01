
#' @export
FileMgr <-
  R6::R6Class(
    classname = "FileMgr",

    # ===================================================================
    #
    #     PRIVATE
    #
    private = list(

      # -----------  properties  -------------

      ..dataset_mgr = NULL,
      ..filename = NULL,
      ..patterns = NULL,

      # -----------  methods  -------------

      ..next_condition = function(expr) {
        start <- max(gregexpr("{",expr, fixed  = T)[[1]])
        ends <- gregexpr("}",expr, fixed  = T)[[1]]
        tryCatch(
          end <- min(ends[ends>start]),
          warning = function(w){
            "Mismatched brackets"
          }
        )
        list(start = start, end = end)
      },

      ..has_conditions = function(expr) {
        grepl(".*[{](.*)[}].*",expr)
      },

      ..eval_pattern_cond = function(expr_in) {

        expr <- gsub(".*[{](.*)[}].*","\\1",expr_in)

        if(grepl(";",expr_in)) {
          expr <-  gsub("(.*);(.*)","\\1",expr)


          if(grepl(" *[=!]= *'",expr)) {
            expr <- gsub(" *([A-Za-z]*) *([=!]=)","'\\1' \\2",expr)
          }
          ok <- eval(parse(text = expr))

          if (ok) {
            ret <- gsub("(.*)[{].*;(.*)[}](.*)","\\1\\2\\3",expr_in)
          } else {
            ret <- gsub("(.*)[{].*;(.*)[}](.*)","\\1\\3",expr_in)
          }
        } else {
          ret <- eval(parse(text = expr))
        }
        ret
      }

    ),

    # ===================================================================
    #
    #     PUBLIC
    #

    public = list(

      initialize = function(init = FALSE, dataset_mgr = NULL, simple = FALSE,
                            root = NULL, use_excel = TRUE) {


        private$..filename <- here::here("data/naming_patterns.rds")

        if(!simple) {
          if(!is.null(dataset_mgr) && inherits(dataset_mgr, "DataSetMgr")) {

            private$..dataset_mgr <-  dataset_mgr

          } else {

            private$..dataset_mgr <-  DataSetMgr$new()
          }

        }
        if(init) {

          df_patterns <- init.patterns(root)
          df_patterns %>% saveRDS("./data/naming_patterns.rds")
        }

        if(use_excel) {

          private$..patterns <- xlsx::read.xlsx("./data/patterns.xlsx", sheetIndex = 1)

        } else if(file.exists(private$..filename)) {
          private$..patterns <- readRDS(private$..filename)
        } else {
          self$refresh()
        }



      },

      find = function(name) {


        private$..patterns %>% filter(grepl(.env$name, name))
      },

      get = function(name, expand = FALSE) {

        pat_name <- paste0("^",name,"$")

        df <- self$find(pat_name)

        if(nrow(df) == 0) return(NULL)

        pttrn <- df %>%
          pull(pattern)

        if(expand) {
          pttrn <- self$expand(pttrn)
        }
        pttrn
      },

      expand = function(pattern) {

        ctr <- 0
        while(grepl("\\$", pattern) && ctr < 100) {

          pat0 <- gsub(".*?\\$(.*?)\\$.*","\\1",pattern)
          pat1<- self$get(pat0)
          pattern <- gsub(paste0("(\\$",pat0,"\\$)"),pat1,pattern)
          ctr <- ctr + 1

        }

        # get rid of double slashes (usually from substitution of other patterns)

        pattern <- gsub("([^:])//","\\1/",pattern)

        pattern

      },


      apply = function(name) {

        pat <- self$get(name = name)


        pat_exp <- self$expand(pat) %>% unname()


        pat_exp <- unname(pat_exp)

        pat_filled <- self$patternize(pat_exp)

        pat_filled

      },

      patternize = function(strIn, expand = TRUE) {

        if(is.null(private$..dataset_mgr)) return(strIn)

        args <- self$dataset_mgr$patterns

        ret <- strIn

        # ===  substitute the dataset parameters like ^SRC^ and ^EXT^ with their values

        purrr::iwalk(args,\(val, nm) {
          nm <- paste0("^",nm,"^")
          ret<<-gsub(nm,val,ret,fixed = T)

        })


        #######################################################################
        ##
        ##    check for logical expressions
        ##    e.g. "LLCP(^VERS^==0;^YEAR^)(^VERS^>0;^YR^V^VERS^)_XPT.zip"

        while(private$..has_conditions(ret)) {

          # there is at least one condition .. get the text
          # cat("\n==========================================\n\n")
          # cat("ret (in)=",ret,"\n")

          next_cond <- private$..next_condition(ret)

          # cat("next_cond$start=",next_cond$start,"\n")
          # cat("next_cond$end=",next_cond$end,"\n")

          # parse the expression part of the condition

          expr <- substr(ret, next_cond$start, next_cond$end)
          # cat("expr=",expr,"\n")

          # are there any embedded params ... ^XXXX^ ...
          if(grepl("^",expr, fixed = TRUE)) {

            expr <- ""
          } else {

            # condition is clean ... evaluate
            expr <- private$..eval_pattern_cond(expr)
          }
          # cat("expr (new)=",expr,"\n")
          ret  <- paste0(substring(ret, 1,next_cond$start-1),
                         expr,
                         substring(ret, next_cond$end+1))

          # cat("ret=",ret,"\n")
        }

        # no more conditions

        if(expand && grepl("./",ret, fixed = T)) ret <- gsub("^.",here::here(),ret, fixed = T)
        #orrr::convert.dot(ret)

        # cat("ret (final)=",ret,"\n")

        ret
      },


      refresh = function() {

        e <- new.env()
        data("naming_patterns", envir = e, package = "brfss")

        private$..patterns <- e$naming_patterns

      },

      requirements = function(names = ".*") {

        df_pats<- self$find(names)

        df <- purrr::map2(df_pats$name, df_pats$pattern, \(nm,pat) {

          pat <- self$expand(pat)

          pat0 <- unname(pat)
          pat0 <- gsub("^^","^ ^",pat0,fixed=TRUE) # helps sub work right

          params <- character(0)

          while(grepl("\\^", pat0)) {
            x <- gsub(".*?\\^(.*?)\\^.*","\\1",pat0)
            params <- c(params,x)
            pat0 <- sub("?\\^(.*?)\\^","", pat0)
          }
          params <- params %>% stringr::str_replace("YR","YEAR") %>% unique()

          data.frame(name = nm, params = paste0(params, collapse = ", "))
        }) %>%
          bind_rows()

        df
      },

      pattern_group = function(group) {

        self$patterns %>%
          filter(grepl(.env$group, group))

      },

      try = function(names = ".*") {
        #
        pat_names <- self$find(names) %>% pull(name)

        df <- map(pat_names, function(nm) {

          result <- self$apply(nm)

          data.frame(name = nm, result)

        }) %>%
          bind_rows()

        df

      }
    ),

    # ===================================================================
    #
    #     ACTIVE BINDINGS
    #

    active = list(

      patterns = function(value) {

        if(!missing(value)) {
          message("<patterns> property is read-only")
          return(invisible())
        }

        private$..patterns
      },

      pattern_names = function(value) {

        if(!missing(value)) {
          message("<pattern_names> property is read-only")
          return(invisible())
        }

        private$..patterns$name
      },

      dataset_mgr = function(value) {
        if(!missing(value)) {
          if(inherits(value, "DataSetMgr")) {
            private$..dataset_mgr <- value
          }
        }

        return(private$..dataset_mgr)

      }
    )

  )
