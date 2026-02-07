
#' @export
BRFSS_FileMgr <-
  R6::R6Class(
    classname = "BRFSS_FileMgr",

    private = list(
      params_mgr_pvt = NULL,
      filename_pvt = NULL,
      patterns_pvt = NULL,

      next_condition = function(expr) {
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

      has_conditions = function(expr) {
        grepl(".*[{](.*)[}].*",expr)
      },

      eval_pattern_cond = function(expr_in) {

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

    public = list(

      initialize = function(params_mgr = NULL) {

        if(!is.null(params_mgr)) {
          if(inherits(params_mgr, "DataSetMgr")) {
            private$params_mgr_pvt <-  params_mgr
          }
        } else {

          private$params_mgr_pvt <-  DataSetMgr$new()
        }

        private$filename_pvt <- here::here("data/naming_patterns.rds")

        if(file.exists(private$filename_pvt)) {
          private$patterns_pvt <- readRDS(private$filename_pvt)
        } else {
          self$refresh()
        }

      },

      find = function(name) {


        private$patterns_pvt %>% filter(grepl({{name}}, name))
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
        while(grepl("\\$", pattern) && ctr < 10) {

          pat0 <- gsub(".*?\\$(.*?)\\$.*","\\1",pattern)
          pat1<- self$get(pat0)
          pattern <- gsub(paste0("(\\$",pat0,"\\$)"),pat1,pattern)
          ctr <- ctr + 1

        }

        # get rid of double slashes (usually from substitution of other patterns)

        pattern <- gsub("([^:])//","\\1/",pattern)

        pattern

      },

      # apply = function(name,  ...) {
      #
      #   pats <- self$get(name = name)
      #
      #   pats <- sapply(pats, function(pat) {
      #     #pat <- get.pattern(name)
      #     browser()
      #     self$expand(pat)
      #   })
      #
      #   pats <- unname(pats)
      #
      #   pats <- self$patternize(pats)
      #
      #   pats
      #
      # },

      apply = function(name) {

        pat <- self$get(name = name)


        pat_exp <- self$expand(pat) %>% unname()


        pat_exp <- unname(pat_exp)

        pat_filled <- self$patternize(pat_exp)

        pat_filled

      },

      patternize = function(strIn, expand = TRUE) {

        if(is.null(private$params_mgr_pvt)) return(strIn)

        #expand <- as.logical(args["expand"])
        ##  remove args with NULL value

        args <- self$params_mgr$patterns

        nms <- names(args)
        vals <- unlist(unname(args))

        ret <- strIn

        purrr::walk2( nms, vals,function(nm,val) {
          nm <- paste0("^",nm,"^")
          ret<<-gsub(nm,val,ret,fixed = T)

        })

        #######################################################################
        ##
        ##    check for logical expressions
        ##    e.g. "LLCP(^VERS^==0;^YEAR^)(^VERS^>0;^YR^V^VERS^)_XPT.zip"

        while(private$has_conditions(ret)) {

          # there is at least one condition .. get the text
          # cat("\n==========================================\n\n")
          # cat("ret (in)=",ret,"\n")

          next_cond <- private$next_condition(ret)

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
            expr <- private$eval_pattern_cond(expr)
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

        private$patterns_pvt <- e$naming_patterns

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
      }

    ),

    active = list(

      patterns = function(value) {

        if(!missing(value)) {
          message("<patterns> property is read-only")
          return(invisible())
        }

        private$patterns_pvt
      },

      pattern_names = function(value) {

        if(!missing(value)) {
          message("<patterns> property is read-only")
          return(invisible())
        }

        private$patterns_pvt$name
      },

      params_mgr = function(value) {
        if(!missing(value)) {
          if(inherits(value, "DataSetMgr")) {
            private$params_mgr_pvt <- value
          }
        }

        return(private$params_mgr_pvt)

      }
    )

  )
