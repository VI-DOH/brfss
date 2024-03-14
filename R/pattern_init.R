

init.patterns <- function() {
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(tibble)

  fname <- "./data/naming_patterns.rds"
  if(file.exists(fname)) file.remove(fname)


  naming_patterns <- data.frame(name = character(0) ,
                                pattern = character(0) ,
                                type = character(0),
                                group = character(0),
                                desc = character(0))

  naming_patterns  <- naming_patterns %>%

    ################################################################################
  ##    base folder locations

  append.pattern("data_folder","./data/",
                 type = "folder",
                 desc ="Standard location of user created data") %>%

    append.pattern("raw_data_folder","./data_raw/",
                   type = "folder",
                   desc ="Standard location of data from another source") %>%

    append.pattern("output_folder","./output/^YEAR^/",
                   type = "folder",
                   desc ="Standard location of data, files, or reports for sharing") %>%

    append.pattern("reports_folder","./reports/",
                   type = "folder",
                   desc ="Standard location of user created data") %>%


    ################################################################################
  ##
  ##    for downloads

  append.pattern("brfss_url_files","https://www.cdc.gov/brfss/annual_data/^YEAR^/files/",
                 type = "url",
                 desc ="Base URL of BRFSS data") %>%

    append.pattern("sasout_download_file","SASOUT^YR^_LLCP{^VERS^>0;_V^VERS^}.SAS",
                   type = "file",
                   group ="sas_downloads",
                   desc ="URL filename of BRFSS SASOUT data") %>%

    append.pattern("sas_format_file", "FORMAT^YR^.sas",
                   type = "file",
                   group ="sas_downloads",
                   desc = "SAS Format library file") %>%

    append.pattern("sas_formas_file", "FORMAS^YR^.sas",
                   type = "file",
                   group ="sas_downloads",
                   desc = "SAS Format assignment file") %>%

    append.pattern("xpt_download_zip_file",
                   "LLCP{^VERS^ == 0;^YEAR^}{^VERS^ > 0;^YR^V^VERS^_}XPT.zip",
                   type = "file",
                   group ="xpt_downloads",
                   desc ="URL filename of BRFSS XPT data") %>%

    append.pattern("brfss_url_documentation",
                   "https://www.cdc.gov/brfss/annual_data/^YEAR^/pdf/",
                   type = "url") %>%

    ################################################################################
  ##
  ##    brfss raw data

  append.pattern("brfss_raw_data_folder","$raw_data_folder$",
                 type = "folder",
                 desc = "Folder to store raw (imported/downloaded from CDC) data") %>%


    append.pattern("brfss_annual_raw_data_folder_base",
                   paste0("$brfss_raw_data_folder$^YEAR^/",

                          #if it's a local file (NOT public)
                          "{^EXT^ == 'local' || ^EXT^ == 'monthly' ;local/^GEOG^/}",

                          # if it's public
                          "{^EXT^ == 'national';public/",
                          "{^GFLAG^ == 'on';state/^GEOG^/}}"),
                   type = "folder",
                   desc = "Folder to store raw BRFSS data") %>%

    append.pattern("brfss_annual_raw_data_folder",
                   paste0("$brfss_raw_data_folder$^YEAR^/",
                          "{^EXT^ == 'local';local/^GEOG^/}",
                          "{^EXT^ == 'national';public/}"),
                   type = "folder",

                   desc = paste0("Folder to store the annual raw ",
                                 "(imported/downloaded from CDC) data")) %>%

    #####################################################################################
  ##
  ##    brfss processed data

  append.pattern("brfss_data_folder","$data_folder$",
                 type = "folder",
                 desc = "Folder to store processed BRFSS data") %>%

    append.pattern("brfss_annual_data_folder_base",
                   paste0("$brfss_data_folder$^YEAR^/",

                          #if it's a local file (NOT public)
                          "{^EXT^ == 'local' || ^EXT^ == 'monthly' ;local/^GEOG^/}",

                          # if it's public
                          "{^EXT^ == 'national';public/",
                          "{^GFLAG^ == 'on';state/^GEOG^/}}"),
                   type = "folder",
                   desc = "Folder to store processed BRFSS data") %>%

    append.pattern("brfss_annual_data_folder",
                   paste0("$brfss_data_folder$^YEAR^/",

                          #if it's a local file (NOT public)
                          "{^EXT^ == 'local';local/^GEOG^/",
                          "{^SRC^ == 'sas';sas/}",
                          "{^SRC^ == 'ascii';ascii/}}",

                          #if it's a public file (downloaded from CDC public site)
                          # we must distinguish between the entire public data set and
                          #  the local data set using GFLAG ... kind of a kludge to handle
                          #  getting the entire public data set before the split into
                          # individual states

                          "{^EXT^ == 'national';public/",
                          "{^GFLAG^ == 'on';state/^GEOG^/}",
                          "{^SRC^ == 'sas';sas/}",
                          "{^SRC^ == 'ascii';ascii/}}"),

                   type = "folder",
                   desc = "Folder to store the annual processed BRFSS data")

  annual_data_pattern <- paste0("{^EXT^ == 'local';^GEOG^_^YEAR^}",
                                "{^EXT^ == 'national';",
                                "{^GFLAG^ == 'on';^GEOG^_^YEAR^}",
                                "{^GFLAG^ == 'off';",
                                "{^SRC^ == 'sas';sas_^YEAR^}",
                                "{^SRC^ == 'ascii';ascii_^YEAR^}}}",
                                "{^VERS^ > 0;_V^VERS^}")

  naming_patterns  <- naming_patterns %>%

    append.pattern("brfss_annual_data_file",
                   paste0(annual_data_pattern, ".rds"),
                   type = "file",
                   desc = paste0("File name for annual processed BRFSS data (main survey),
                   from specific geographies")) %>%

    append.pattern("brfss_annual_data_unfactored_file",
                   paste0(annual_data_pattern, "_unf.rds"),
                   type = "file",
                   desc = paste0("File name for annual processed BRFSS data (main survey),
                   from specific geographies")) %>%

    append.pattern("brfss_annual_data_path",
                   "$brfss_annual_data_folder$$brfss_annual_data_file$",
                   type = "path",
                   desc = "Full path for annual processed BRFSS data
                   (main survey) from specific geographies") %>%

    append.pattern("brfss_annual_data_unfactored_path",
                   "$brfss_annual_data_folder$$brfss_annual_data_unfactored_file$",
                   type = "path",
                   desc = "Full path for annual processed BRFSS data
                   (main survey) from specific geographies") %>%

    append.pattern("brfss_geog_folder","$brfss_data_folder$^YEAR^/local/",
                   type = "folder",
                   desc = "Folder holding data for specific geographies") %>%

    append.pattern("brfss_data_df","df_^GEOG^{^VERS^ > 0;_V^VERS^}_^YEAR^",
                   type = "data",
                   desc = "Consistent name for BRFSS data object (data.frame) stored and retrieved") %>%

    #####################################################################
  ##
  ##    metadata

  append.pattern("brfss_annual_raw_metadata_folder",
                 "$brfss_annual_raw_data_folder_base$metadata/",

                 type = "folder",
                 group = "metadata",
                 desc = "Folder to store the metadata ... modules and responses") %>%

    append.pattern("brfss_annual_metadata_folder",
                   paste0("$brfss_annual_data_folder_base$metadata/"),
                   type = "folder",
                   group = "metadata",
                   desc = "Folder to store the metadata ... modules and responses") %>%

    #############################################################################
  ##
  ##  codebook patterns

  append.pattern("brfss_url_codebook",
                 "https://www.cdc.gov/brfss/annual_data/^YEAR^/pdf/",
                 type = "url",
                 desc ="Base URL of BRFSS codebook") %>%

    append.pattern("brfss_codebook_file1",
                   "$brfss_url_codebook$codebook^YR^_llcp.pdf",
                   type = "file",
                   group ="codebook_downloads",
                   desc ="Pre-2017 codebook name") %>%

    append.pattern("brfss_codebook_file2",
                   "$brfss_url_codebook$codebook^YR^_llcp-v2-508.pdf",
                   type = "file",
                   group ="codebook_downloads",
                   desc ="Post-2016 codebook name") %>%

    append.pattern("brfss_codebook_file3",
                   "$brfss_url_codebook$codebook^YR^_llcp-v2-508.HTML",
                   type = "file",
                   group ="codebook_downloads",
                   desc ="Post-2016 codebook name in html") %>%

    append.pattern("codebook_folder","$brfss_annual_raw_data_folder$codebook/",
                   type = "folder",
                   group ="codebook",
                   desc = "location of the annual codebook file") %>%

    append.pattern("codebook_file",
                   paste0("{^EXT^ == 'local';^GEOG^^YR^CODE_LLCP}",
                          "{^EXT^ != 'local';CODEBOOK^YR^_LLCP}"),
                   type = "file",
                   group ="codebook",
                   desc = "file name of the annual codebook file") %>%

    append.pattern("codebook_ext","pdf",
                   type = "extension",
                   group ="codebook",
                   desc = "ext of the annual codebook file (txt, rtf, html, or pdf") %>%

    append.pattern("codebook_path","$codebook_folder$$codebook_file$.$codebook_ext$",
                   type = "path",
                   group ="codebook",
                   desc = "ext of the annual codebook file (.txt, .rtf, or .pdf") %>%

    append.pattern("codebook_layout_folder","$layout_folder$",
                   type = "folder",
                   group ="codebook",
                   desc = "location of the annual codebook layout file") %>%

    append.pattern("codebook_layout_file","layout^YR^_CB.rds",
                   type = "file",
                   group ="codebook",
                   desc = "file name of the annual codebook layout file") %>%

    append.pattern("codebook_layout_path","$codebook_layout_folder$$codebook_layout_file$",
                   type = "path",
                   group ="codebook",
                   desc = "path to the annual codebook layout file") %>%

    append.pattern("codebook_values_file","values^YR^_CB.rds",
                   type = "file",
                   group ="codebook",
                   desc = "file name of the annual codebook values file") %>%

    append.pattern("codebook_values_path","$codebook_layout_folder$$codebook_values_file$",
                   type = "path",
                   group ="codebook",
                   desc = "path to the annual codebook layout file") %>%


    #########################################################################################
  ##
  ##      layout patterns
  ##

  append.pattern("layout_folder",
                 paste0("$brfss_annual_metadata_folder$layout/"),
                 group ="layout",
                 type = "folder") %>%

    append.pattern("sas_layout_file","layout^YR^{^VERS^ > 0;_V^VERS^}_sas.rds",
                   group ="layout",
                   type = "file") %>%

    append.pattern("sas_layout_path","$layout_folder$$sas_layout_file$",
                   group ="layout",
                   type = "path") %>%




    #####################
  ##  ascii data

  append.pattern("ascii_zip_file",
                 "LLCP{^VERS^ == 0;^YEAR^}{^VERS^ > 0;^YR^V^VERS^_}ASC.zip",
                 type = "file") %>%
    #
    append.pattern("ascii_downloads_url","$brfss_url_files$$ascii_zip_file$",
                   type = "file",
                   group = "ascii_downloads") %>%

    append.pattern("ascii_raw_data_folder",
                   paste0("$brfss_annual_raw_data_folder$",
                          "{^EXT^ == 'local';ascii/}",
                          "{^EXT^ == 'national';ascii/}"),
                   type = "folder") %>%


    append.pattern("ascii_filename_raw",
                   paste0(
                     "{^EXT^ == 'national';LLCP{^VERS^ == 0;^YEAR^}{^VERS^ > 0;^YR^V^VERS^}.ASC}",
                     "{^EXT^ == 'local';^GEOG^^YR^COMP{^VERS^ > 0;_V^VERS^}.DAT}"),
                   type = "file") %>%

    append.pattern("ascii_zip_path","$ascii_raw_data_folder$$ascii_zip_file$",
                   type = "path") %>%

    append.pattern("ascii_path_raw","$ascii_raw_data_folder$$ascii_filename_raw$",
                   type = "path") %>%

    append.pattern("ascii_data_folder",
                   paste0("$brfss_data_folder$^YEAR^/",
                          "{^EXT^ == 'local';local/^GEOG^/ascii/}",
                          "{^EXT^ == 'national';public/ascii/}"),
                   type = "folder") %>%

    append.pattern("ascii_filename",
                   "{^EXT^ == 'local';^GEOG^_}^YEAR^{^VERS^ > 0;_V^VERS^}.rds",
                   type = "file") %>%

    append.pattern("ascii_path","$ascii_data_folder$$ascii_filename$",
                   type = "path") %>%

    append.pattern("ascii_df","df_ascii_^YEAR^{^VERS^ > 0;_V^VERS^}",
                   type = "data") %>%

    #####################
  ##  sas data


  append.pattern("sas_raw_data_folder",
                 paste0("$brfss_annual_raw_data_folder$",
                        "{^EXT^ == 'local';sas/}",
                        "{^EXT^ == 'national';sas/}"),
                 type = "folder") %>%


    append.pattern("sas_data_folder","$brfss_annual_data_folder$",
                   type = "folder") %>%

    append.pattern("xpt_folder", "$sas_raw_data_folder$",
                   type = "folder") %>%


    append.pattern("xpt_file",
                   paste0(
                     "{^EXT^ == 'local';^GEOG^^YR^FINL{^VERS^ > 0;_V^VERS^}}",
                     "{^EXT^ == 'national';LLCP{^VERS^ == 0;^YEAR^}",
                     "{^VERS^ > 0;^YR^V^VERS^}}.XPT"),
                   type = "file") %>%

    append.pattern("xpt_path", "$xpt_folder$$xpt_file$",
                   type = "path") %>%

    append.pattern("xpt_df","df_xpt_^YEAR^{^VERS^ > 0;_V^VERS^}",
                   type = "data") %>%

    append.pattern("sas_data_file","sas_^YEAR^{^VERS^ > 0;_V^VERS^}.rds",
                   type = "file") %>%

    append.pattern("sas_data_path","$sas_data_folder$$sas_data_file$",
                   type = "path") %>%


    append.pattern("sas_sasout_file",
                   "SASOUT^YR^_{^EXT^ == 'local';STATES}{^EXT^ == 'national';LLCP}{^VERS^>0;_V^VERS^}.SAS",
                   type = "file") %>%

    # append.pattern("sas_sasout_path", "$sas_raw_data_folder$$sas_sasout_file$",
    #                type = "path") %>%
    #
    append.pattern("sas_sasout_path", "$brfss_annual_raw_metadata_folder$$sas_sasout_file$",
                   type = "path") %>%


    #########################################################B############################
  ##
  ##    State-Added Questions (SAQ)brfss_annual_raw_data_folder

  append.pattern("saq_raw_folder","$brfss_annual_raw_data_folder_base$saq/",
                 type = "folder") %>%

    append.pattern("saq_raw_file","^GEOG^^YR^_layout_SAQ.csv",
                   type = "file")  %>%

    append.pattern("saq_raw_values_file","^GEOG^^YR^_values_SAQ.csv",
                   type = "file")  %>%

    append.pattern("saq_raw_path","$saq_raw_folder$$saq_raw_file$",
                   type = "path")%>%

    append.pattern("saq_raw_values_path","$saq_raw_folder$$saq_raw_values_file$",
                   type = "path")%>%

    append.pattern("saq_layout_folder", "$layout_folder$",
                   # paste0("$brfss_data_folder$^YEAR^/",
                   #        "{^EXT^ == 'local';local/^GEOG^/saq/}{^EXT^ == 'national';ERROR}"),
                   type = "folder",
                   desc = "Folder to store the annual processed BRFSS data") %>%

    append.pattern("saq_layout_file","layout^YR^_SAQ.rds",
                   type = "file") %>%

    append.pattern("saq_values_file","values^YR^_SAQ.rds",
                   type = "file") %>%

    append.pattern("saq_layout_path","$saq_layout_folder$$saq_layout_file$",
                   type = "path") %>%

    append.pattern("saq_values_path","$saq_layout_folder$$saq_values_file$",
                   type = "path") %>%


    ##  files from merging the saq data with the national data

    append.pattern("merged_layout_folder",paste0("$layout_folder$"),
                   type = "folder") %>%

    append.pattern("merged_layout_file","layout^YR^_mrg.rds",
                   type = "file") %>%

    append.pattern("merged_layout_path","$merged_layout_folder$$merged_layout_file$",
                   type = "path") %>%

    append.pattern("merged_values_file","values^YR^_mrg.rds",
                   type = "file") %>%

    append.pattern("merged_values_path","$merged_layout_folder$$merged_values_file$",
                   type = "path")  %>%


    ############################################################
  ##
  ##    data file of response totals

  append.pattern("brfss_responses_folder",
                 paste0("$brfss_annual_metadata_folder$"),
                 type = "fmolder") %>%

    append.pattern("brfss_responses_file","responses_^YEAR^.rds",
                   type = "file") %>%
    append.pattern("brfss_responses_path","$brfss_responses_folder$$brfss_responses_file$",
                   type = "path") %>%

    append.pattern("brfss_responses_df","df_responses_^YEAR^",
                   type = "data") %>%


    #####################################################################
  ##
  ##    data file of modules

  append.pattern("brfss_modules_folder","$brfss_annual_metadata_folder$",
                 type = "folder",
                 desc = "location of the calculated modules file") %>%


    append.pattern("brfss_modules_file","modules_^YEAR^.rds",
                   type = "file") %>%

    append.pattern("brfss_modules_path","$brfss_modules_folder$$brfss_modules_file$",
                   type = "path") %>%

    append.pattern("brfss_modules_df","df_modules_^YEAR^",
                   type = "data") %>%

    ####################################################################
  ##
  ## monthly data
  ##

  append.pattern("monthly_raw_data_folder_base",
                 paste0("$brfss_annual_raw_data_folder_base$monthly/"),
                 type = "folder") %>%

    append.pattern("monthly_raw_data_folder",
                   paste0("$monthly_raw_data_folder_base$",
                          "{sprintf('%02d',^MONTH^)}_",
                          "{toupper(month.abb[as.integer(^MONTH^)])}/"),
                   type = "folder") %>%

    append.pattern("monthly_data_folder",
                   "$brfss_annual_data_folder_base$monthly/",
                   type = "folder") %>%

    append.pattern("monthly_cell_file_raw",
                   paste0("$monthly_raw_data_folder$",
                          "CEL_^GEOG^",
                          "{toupper(month.abb[as.integer(^MONTH^)])}^YR^1.Dat"),
                   type = "file") %>%

    append.pattern("monthly_ll_file_raw",
                   paste0("$monthly_raw_data_folder$",
                          "LL_^GEOG^",
                          "{toupper(month.abb[as.integer(^MONTH^)])}^YR^1.Dat"),
                   type = "file") %>%

    append.pattern("monthly_file_raw",
                   paste0("$monthly_raw_data_folder$",
                          "{^PHON^ == 'cell';cel}{^PHON^ == 'land';ll}_^GEOG^",
                          "{toupper(month.abb[as.integer(^MONTH^)])}^YR^1.Dat"),
                   type = "file") %>%

    append.pattern("monthly_data_file_month",
                   paste0("$monthly_data_folder$",
                          "^GEOG^_^YEAR^_",
                          "{sprintf('%02d',^MONTH^)}.rds"),
                   type = "file") %>%

    append.pattern("monthly_data_file",
                   paste0("$monthly_data_folder$",
                          "^GEOG^_^YEAR^.rds"),
                   type = "file") %>%

    append.pattern("monthly_reports_folder",
                   "$reports_folder$^YEAR^/monthly/",

                   type = "folder",
                   group = "reports",
                   desc = "Folder to store the monthly reports") %>%

    append.pattern("monthly_reports_file",
                   paste0("monthly_report_^GEOG^_^YEAR^",
                          "{^MONTH^ > 0;_{sprintf('%02d',^MONTH^)}_{month.abb[^MONTH^]}}",
                          "{^YTD^ == 'on';_YTD}",
                          ".pdf"),
                   type = "folder",
                   group = "reports",
                   desc = "Folder to store the monthly reports") %>%


    ## Miscellaneous


    append.pattern("aliases_path","$data_folder$aliases.rds",
                   type = "path") %>%

    append.pattern("geogs_path","$data_folder$geogs.rds",
                   type = "path") %>%

    append.pattern("weight_col","{^VERS^==0;_LLCPWT}{^VERS^!=0;_LCPWTV^VERS^}",
                   type = "column") %>%

    append.pattern("stratum_col","_STSTR",
                   type = "column")




  #########################################################################
  ##
  ##  end of patterns ... save data

  usethis::use_data(naming_patterns, overwrite = TRUE)

}
