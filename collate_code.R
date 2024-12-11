# Unified Code Collation System
# A single source that manifests as dual outputs
setwd("C:/Users/Nouri/Documents/GitHub/oneplusoneequalsone")

#' Collate R Code with Dual Output
#' This system scans the repository for R files and creates two balanced output files
#' to manage size limitations while maintaining coherence.
#'
#' @param output_base Base name for output files (will be appended with _part1 and _part2)
#' @param format Output format ("txt" or "md")
collate_R_files <- function(output_base = "collated_code", format = c("txt", "md"), max_lines = 5000) {
  format <- match.arg(format)
  file_ext <- paste0(".", format)
  
  r_files <- list.files(pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  if (length(r_files) == 0) stop("No R files found in the current repository!")
  
  process_file <- function(file) {
    content <- readLines(file, warn = FALSE)
    content <- content[!grepl("^\\s*(#|$)", content)]  # Remove comments and blank lines
    content
  }
  
  all_content <- lapply(r_files, function(file) {
    content <- process_file(file)
    header <- sprintf("\n\n# File: %s\n%s\n\n", file, strrep("-", 80))
    list(
      text = paste0(header, paste(content, collapse = "\n")),
      size = length(content)
    )
  })
  
  part1 <- c()
  part2 <- c()
  current_lines <- 0
  
  for (content in all_content) {
    if (current_lines + content$size <= max_lines) {
      part1 <- c(part1, content$text)
      current_lines <- current_lines + content$size
    } else {
      part2 <- c(part2, content$text)
    }
  }
  
  output_file1 <- paste0(output_base, "_part1", file_ext)
  output_file2 <- paste0(output_base, "_part2", file_ext)
  
  writeLines(paste(part1, collapse = "\n"), output_file1)
  writeLines(paste(part2, collapse = "\n"), output_file2)
  
  message("Code collation complete:")
  message("Part 1 saved to: ", output_file1)
  message("Part 2 saved to: ", output_file2)
  
  invisible(list(part1 = output_file1, part2 = output_file2))
}

# Example usage:
collate_R_files("collated_code", format = "md")  # Creates markdown files
# collate_R_files("collated_code", format = "txt")  # Creates text files