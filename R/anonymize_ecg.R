#!/usr/bin/env Rscript

args = commandArgs(trailingOnly = TRUE)

# args[1] = input_dir
# args[2] = output_dir

# Setup directories

if (!dir.exists(args[2])) {
    dir.create(args[2])
}
if (!dir.exists(file.path(args[2], "log"))) {
    dir.create(file.path(args[2], "log"))
}

# Create logfile

zz <- file(file.path(args[2], "log", "logfile.txt"), open = "wt")

# Open connection to log file

sink(zz, type = "message")

# Require libraries

require(purrr)
require(xml2)


###### Main function ###########################

#' Anonymize Philips XML ECG
#' 
#' Removes certain identifiers from Philipx xml ECGs
#'
#' @param input_dir \code{character} with directory to search for .xml files
#' @param file_name \code{character} with filename to process
#' @param output_dir \code{character} with directory for output data
#'
#' @return
#' @export
#'
anonymize_ecg <- function(input_dir, file_name, output_dir){
    
    # Read file
    xml_file <- xml2::read_xml(file.path(input_dir, file_name))
    
    # Function to remove text from nodes without attrs
    .set_blank_node <- function(node_path){
        path <- paste0("//d1:", node_path)
        
        xml_file %>%
            xml_find_all(path) %>% 
            xml_set_text("")
    }
    
    # Function to set generic datetime attrs, since often required
    .set_generic_datetime_attr <- function(node_path){
        path <- paste0("//d1:", node_path)
        
        xml_file %>%
            xml_find_all(path) %>% 
            xml_set_attrs(list(c("date" = "1970-01-01", "time" = "12:00:00")))
    }
    
    # Generic function to set node attrs
    .set_blank_attrs <- function(node_path, attrs){
        path <- paste0("//d1:", node_path)
        
        xml_file %>%
            xml_find_all(path) %>% 
            xml_set_attrs(list(attrs))
    }
    
    # Function to set dateofbirth to 1969-01-01
    .set_generic_dateofbirth <- function(node_path){
        path <- paste0("//d1:", node_path)
        
        xml_file %>%
            xml_find_all(path) %>%
            xml_set_text("1969-01-01")
    }

    # Simple text nodes without attrs to blank
    nodes_to_blank <- list(
        "ordernumber",
        "uniqueorderid",
        "orderbillingcode",
        "orderremarks",
        "departmentid",
        "departmentname",
        "institutionid",
        "institutionname",
        "facilityid",
        "facilityname",
        "patientid",
        "uniquepatientid",
        "MRN",
        "lastname",
        "firstname",
        "middlename",
        "sex"
        
    )
    
    # Nodes with date and time attrs
    node_datetime_to_blank <- list(
        "reportinfo"
        
    )
    
    # Nods for which attrs needs to be specified
    node_with_attrs <- list(
        # The node path
        node_path = list(
            "operator"
            
        ),
        # Corresponding attrs in named character vector
        attrs = list(
            c("id" = "")
            
        )
    )
    
    purrr::walk(nodes_to_blank, .set_blank_node)
    purrr::walk(node_datetime_to_blank, .set_generic_datetime_attr)
    purrr::pwalk(node_with_attrs, .set_blank_attrs)
    .set_generic_dateofbirth("dateofbirth")
    
    # Update progressbar
    setTxtProgressBar(pb, value = (pb$getVal() + 1))
    
    # Write anonymized file
    write_xml(xml_file, file.path(output_dir, file_name))
    
}

# Create a list of files to anonymize

files <- list.files(path = args[1], 
                    pattern = "*.xml", 
                    full.names = TRUE, 
                    recursive = TRUE)

# Create a progressbar object/connection

pb <- txtProgressBar(max = length(files), style = 3)

# Run main function

purrr::walk(files, ~anonymize_ecg(input_dir = dirname(.x),
                                  file_name = basename(.x),
                                  output_dir = args[2]))
close(pb)
sink(type = "message")
close(zz)