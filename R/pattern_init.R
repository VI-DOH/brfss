




init.patterns <- function() {
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(tibble)

  fname <- "./data/naming_patterns.rda"
  if(file.exists(fname)) file.remove(fname)


  naming_patterns <- data.frame(name = character(0) , pattern = character(0) , group = character(0),
                                desc = character(0))

  naming_patterns  <- naming_patterns %>%

    ################################################################################
  ##    base folder locations

  append.pattern("data_folder","./data/",
                 desc ="Standard location of user created data") %>%

    append.pattern("raw_data_folder","./data_raw/",
                   desc ="Standard location of data from another source") %>%

    append.pattern("output_folder","./output/[YEAR]/",
                   desc ="Standard location of data, files, or reports for sharing") %>%

    ################################################################################
  ##
  ##    for downloads

  append.pattern("brfss_url_files","https://www.cdc.gov/brfss/annual_data/[YEAR]/files/",
                 desc ="Base URL of BRFSS data") %>%

    append.pattern("sasout_download_file","SASOUT[YR]_LLCP.SAS",
                   group ="sas_downloads",
                   desc ="URL filename of BRFSS SASOUT data") %>%

    append.pattern("sas_format_file", "FORMAT[YR].sas",
                   group ="sas_downloads",
                   desc = "SAS Format library file") %>%

    append.pattern("sas_formas_file", "FORMAS[YR].sas",
                   group ="sas_downloads",
                   desc = "SAS Format assignment file") %>%

    append.pattern("xpt_download_zip_file","LLCP([VERS] == 0;[YEAR])([VERS] > 0;[YR]V[VERS]_)XPT.zip",
                   group ="xpt_downloads",
                   desc ="URL filename of BRFSS XPT data") %>%

    append.pattern("brfss_url_documentation", "https://www.cdc.gov/brfss/annual_data/[YEAR]/pdf/") %>%

    ################################################################################
  ##
  ##    brfss raw data

  append.pattern("brfss_raw_data_folder","{raw_data_folder}",
                 desc = "Folder to store raw (imported/downloaded from CDC) data") %>%

    append.pattern("brfss_annual_raw_data_folder",
                   "{brfss_raw_data_folder}[YEAR]/([EXT] == 'local';geog/[GEOG]/)",
                   desc = "Folder to store the annual raw (imported/downloaded from CDC) data") %>%

    #####################################################################################
  ##
  ##    brfss processed data

  append.pattern("brfss_data_folder","{data_folder}",
                 desc = "Folder to store processed BRFSS data") %>%

    append.pattern("brfss_annual_data_folder",
                   paste0("{brfss_data_folder}[YEAR]/",
                          "([EXT] == 'local';geog/[GEOG]/([SRC] == 'sas';sas/)([SRC] == 'ascii';ascii/))",
                          "([EXT] == 'national';([SRC] == 'sas';sas/)([SRC] == 'ascii';ascii/))"),
                   desc = "Folder to store the annual processed BRFSS data") %>%

    append.pattern("brfss_annual_data_file",
                   paste0("([EXT] == 'local';[GEOG]_[YEAR])",
                          "([EXT] == 'national';([SRC] == 'sas';sas_[YEAR])([SRC] == 'ascii';ascii_[YEAR]))",
                          "([VERS] > 0;_V[VERS])",
                          ".rda"),
                   desc = paste0("File name for annual processed BRFSS data (main survey),
                   from specific geographies")) %>%

    append.pattern("brfss_annual_data_path",
                   "{brfss_annual_data_folder}{brfss_annual_data_file}",
                   desc = "Full path for annual processed BRFSS data
                   (main survey) from specific geographies") %>%

    append.pattern("annual_metadata_folder","{brfss_annual_data_folder}metadata/",
                   desc ="Standard location of user created metadata") %>%

    append.pattern("brfss_geog_folder","{brfss_data_folder}[YEAR]/geog/",
                   desc = "Folder holding data for specific geographies") %>%

    append.pattern("brfss_data_df","df_[GEOG]([VERS] > 0;_V[VERS])_[YEAR]",
                   desc = "Consistent name for BRFSS data object (data.frame) stored and retrieved") %>%


  #############################################################################
  ##
  ##  codebook patterns

  append.pattern("brfss_url_codebook",
                 "https://www.cdc.gov/brfss/annual_data/[YEAR]/pdf/",
                 desc ="Base URL of BRFSS codebook") %>%

    append.pattern("brfss_codebook_file1",
                   "{brfss_url_codebook}codebook[YR]_llcp.pdf",
                   group ="codebook_downloads",
                   desc ="Pre-2017 codebook name") %>%

    append.pattern("brfss_codebook_file2",
                   "{brfss_url_codebook}codebook[YR]_llcp-v2-508.pdf",
                   group ="codebook_downloads",
                   desc ="Post-2016 codebook name") %>%

    append.pattern("brfss_codebook_file3",
                   "{brfss_url_codebook}codebook[YR]_llcp-v2-508.HTML",
                   group ="codebook_downloads",
                   desc ="Post-2016 codebook name in html") %>%

    append.pattern("codebook_folder","{brfss_annual_raw_data_folder}codebook/",
                   desc = "location of the annual codebook file") %>%

    append.pattern("codebook_file","([EXT] == 'local';[GEOG][YR]CODE_LLCP)([EXT] != 'local';CODEBOOK[YR]_LLCP)",
                   desc = "file name of the annual codebook file") %>%

    append.pattern("codebook_ext","pdf",
                   desc = "ext of the annual codebook file (txt, rtf, html, or pdf") %>%

    append.pattern("codebook_path","{codebook_folder}{codebook_file}.{codebook_ext}",
                   desc = "ext of the annual codebook file (.txt, .rtf, or .pdf") %>%

    append.pattern("codebook_layout_folder","{sas_layout_folder}",
                   desc = "location of the annual codebook layout file") %>%

    append.pattern("codebook_layout_file","layout[YR]_CB.rda",
                   desc = "file name of the annual codebook layout file") %>%

    append.pattern("codebook_layout_path","{codebook_layout_folder}{codebook_layout_file}",
                   desc = "path to the annual codebook layout file") %>%

    append.pattern("codebook_values_file","values[YR]_CB.rda",
                   desc = "file name of the annual codebook values file") %>%

    append.pattern("codebook_values_path","{codebook_layout_folder}{codebook_values_file}",
                   desc = "path to the annual codebook layout file") %>%


    #########################################################################################

  append.pattern("sas_layout_folder","{brfss_annual_data_folder}layout/") %>%
    append.pattern("sas_layout_file","layout[YR]_sas.rda") %>%
    append.pattern("sas_layout_path","{sas_layout_folder}{sas_layout_file}") %>%


    ##    data file of response totals

    append.pattern("brfss_responses_folder","{brfss_annual_data_folder}") %>%
    append.pattern("brfss_responses_file","responses_[YEAR].rda") %>%
    append.pattern("brfss_responses_path","{brfss_responses_folder}{brfss_responses_file}") %>%

    ##    data file of modules

    append.pattern("brfss_modules_folder","{brfss_annual_data_folder}") %>%
    append.pattern("brfss_modules_file","modules_[YEAR].rda") %>%
    append.pattern("brfss_modules_path","{brfss_modules_folder}{brfss_modules_file}") %>%


    #####################################################################################################
  ##  ascii data

  append.pattern("ascii_filename_zip","LLCP([VERS] == 0;[YEAR])([VERS] > 0;[YR]V[VERS]_)ASC.zip") %>%
    #
    append.pattern("ascii_downloads_url","{brfss_url_files}{ascii_filename_zip}",
                   group = "ascii_downloads") %>%

    append.pattern("ascii_raw_data_folder","{brfss_annual_raw_data_folder}ascii/") %>%

    append.pattern("ascii_filename_raw",
                   paste0(
                     "([EXT] == 'national';LLCP([VERS] == 0;[YEAR])([VERS] > 0;[YR]V[VERS]).ASC)",
                     "([EXT] == 'local';[GEOG][YR]COMP.DAT)")) %>%

    append.pattern("ascii_path_zip","{ascii_raw_data_folder}{ascii_filename_zip}") %>%

    append.pattern("ascii_path_raw","{ascii_raw_data_folder}{ascii_filename_raw}") %>%

    append.pattern("ascii_data_folder","{brfss_data_folder}[YEAR]/ascii/") %>%
    append.pattern("ascii_filename","ascii_[YEAR]([VERS] > 0;_V[VERS]).rda") %>%
    append.pattern("ascii_path","{ascii_data_folder}{ascii_filename}") %>%

    append.pattern("ascii_df","df_ascii_[YEAR]([VERS] > 0;_V[VERS])") %>%

    ###################################################################################

  append.pattern("sas_raw_folder","{brfss_raw_data_folder}[YEAR]/([EXT] == 'local';geog/[GEOG]/)sas/") %>%

    append.pattern("sas_data_folder","{brfss_annual_data_folder}sas/") %>%
    append.pattern("xpt_folder", "{sas_raw_folder}") %>%

    #   set.pattern("xpt_file", "LLCP([VERS] == 0;[YEAR])([VERS] > 0;[YR]V[VERS]).XPT")
    #   set.pattern("xpt_file","[GEOG][YR]FINL([VERS] > 0;_V[VERS]).XPT")
    #

    append.pattern("xpt_file",
                   paste0(
                     "([EXT] == 'local';[GEOG][YR]FINL([VERS] > 0;_V[VERS]))",
                     "([EXT] == 'national';LLCP([VERS] == 0;[YEAR])([VERS] > 0;[YR]V[VERS])).XPT")) %>%

    append.pattern("xpt_path", "{xpt_folder}{xpt_file}") %>%
    append.pattern("xpt_df","df_xpt_[YEAR]([VERS] > 0;_V[VERS])") %>%

    append.pattern("sas_data_file","sas_[YEAR]([VERS] > 0;_V[VERS]).rda") %>%
    append.pattern("sas_data_path","{sas_data_folder}{sas_data_file}") %>%

    append.pattern("sas_sasout", "SASOUT[YR]_([EXT] == 'local';STATES)([EXT] == 'national';LLCP).SAS") %>%
    append.pattern("sas_sasout_path", "{sas_raw_folder}{sas_sasout}") %>%

    #####################################################################################
  ##
  ##    State-Added Questions (SAQ)

  append.pattern("saq_raw_folder","{brfss_annual_raw_data_folder}saq/") %>%
    append.pattern("saq_raw_file","[GEOG][YR]_layout_SAQ.csv")  %>%
    append.pattern("saq_raw_values_file","[GEOG][YR]_values_SAQ.csv")  %>%
    append.pattern("saq_raw_path","{saq_raw_folder}{saq_raw_file}")%>%
    append.pattern("saq_raw_values_path","{saq_raw_folder}{saq_raw_values_file}")%>%

    append.pattern("saq_layout_folder","{sas_layout_folder}") %>%
    append.pattern("saq_layout_file","layout[YR]_SAQ.rda") %>%
    append.pattern("saq_values_file","values[YR]_SAQ.rda") %>%
    append.pattern("saq_layout_path","{saq_layout_folder}{saq_layout_file}") %>%
    append.pattern("saq_values_path","{saq_layout_folder}{saq_values_file}") %>%


    ##  files from merging the saq data with the national data

    append.pattern("merged_layout_file","layout[YR]_mrg.rda") %>%
    append.pattern("merged_layout_path","{codebook_layout_folder}{merged_layout_file}") %>%
    append.pattern("merged_values_file","values[YR]_mrg.rda") %>%
    append.pattern("merged_values_path","{codebook_layout_folder}{merged_values_file}")


  #########################################################################
  ##
  ##  end of patterns ... save data

  usethis::use_data(naming_patterns, overwrite = TRUE)

}
