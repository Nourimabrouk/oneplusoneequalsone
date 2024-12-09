#' Collate All R Code in the Current Repository
#' This script scans the current working directory for all `.R` files
#' and combines their contents into a single `.txt` or `.md` file.

# Load necessary libraries
library(tidyverse)

#' Function to collate all R files into one text or markdown file
#' @param output_file Path to the output file (e.g., "collated_code.txt" or "collated_code.md")
collate_R_files <- function(output_file = "collated_code.txt") {
  # Get all R files in the current directory and subdirectories
  r_files <- list.files(pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  
  # Check if there are any R files
  if (length(r_files) == 0) {
    stop("No R files found in the current repository!")
  }
  
  # Create an empty string to hold the consolidated content
  consolidated_content <- ""
  
  # Iterate over each R file and append its content
  for (file in r_files) {
    # Read the content of the file
    file_content <- readLines(file, warn = FALSE)
    
    # Add a header with the filename for context
    header <- paste0("\n\n# File: ", file, "\n", strrep("-", 80), "\n\n")
    
    # Append the header and file content to the consolidated content
    consolidated_content <- paste0(consolidated_content, header, paste(file_content, collapse = "\n"))
  }
  
  # Write the consolidated content to the output file
  writeLines(consolidated_content, output_file)
  
  # Notify the user
  message("Collated R files saved to: ", output_file)
}

# Run the function to collate all R files
# Change the output file name and format as desired
collate_R_files("collated_code.md")  # Save as a markdown file
# collate_R_files("collated_code.txt")  # Alternatively, save as a plain text file
