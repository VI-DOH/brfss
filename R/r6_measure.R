
Measure <-
  R6::R6Class(
    classname = "Measure",

    private = list(

      ..dataset_mgr = NULL,
      ..col_name = NULL,
      ..attribs = NULL
    ),

    public = list(

        initialize = function(dataset_mgr = NULL, col_name = NULL) {

          p <- private

          p$..dataset_mgr <- dataset_mgr
          p$..col_name <- col_name

          if(!is.null(dataset_mgr) && !is.null(col_name)){

            browser()

          }

        }


    ),

    active = list(




    )

  )
