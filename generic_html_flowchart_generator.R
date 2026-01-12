#' Generate Flowchart HTML from R or SAS Script Documentation Blocks
#'
#' Reads an R or SAS script, extracts documentation blocks marked by
#' specific tags (starts with # for R, * for SAS), reads corresponding 
#' patient counts from a text file, and creates an HTML file visualizing 
#' these blocks and counts as a vertical flowchart.
#'
#' @param script_path Path to the input script file (.R or .sas).
#' @param pats_num_path Path to the text file containing comma-separated
#'   patient numbers corresponding to each documentation block/step.
#' @param output_html_path Path where the output HTML file will be saved.
#'
#' @details
#' The function supports two comment styles for documentation tags:
#' \itemize{
#'   \item \strong{R Style:} Uses \code{#} (e.g., \code{#@DOCU-START})
#'   \item \strong{SAS Style:} Uses \code{*} (e.g., \code{*@DOCU-START;})
#' }
#' Trailing semicolons in SAS comments are automatically trimmed.
#'
#' @examples
#' \dontrun{
#'   # --- Example 1: R Script ---
#'   # 1. Create 'analysis.R' with tags:
#'   #    #@DOCU-START
#'   #    #@STEP:001
#'   #    # Load Data
#'   #    #@DOCU-END
#'   #    df <- read.csv("data.csv")
#'   
#'   # 2. Create 'pats_num.txt' (e.g., "100, 95")
#'   
#'   # 3. Run:
#'   generate_flowchart_html("analysis.R", "pats_nums.txt", "flowchart_r.html")
#'
#'   # --- Example 2: SAS Script ---
#'   # 1. Create 'analysis.sas' with tags:
#'   #    *@DOCU-START;
#'   #    *@STEP:001;
#'   #    * Load Data;
#'   #    *@DOCU-END;
#'   #    data test; set raw; run;
#'   
#'   # 2. Run:
#'   generate_flowchart_html("analysis.sas", "pats_num.txt", "flowchart_sas.html")
#' }
#'
#' @export
generate_flowchart_html <- function(script_path, 
                                    pats_num_path,
                                    output_html_path) {
    
    if (!requireNamespace("htmltools", quietly = TRUE)) {
        stop("Package 'htmltools' is required. Please install it.", call. = FALSE)
    }
    
    lines <- readLines(script_path, warn = FALSE)
    
    description_lines <- c()
    in_description_block <- FALSE
    description_html <- "" 
    
    for (line in lines) {
        trimmed_line_check <- trimws(line)
        # MODIFIED: Look for * or # at the start
        if (grepl("^[*#]\\s*@DESCRIPTION-START", trimmed_line_check, ignore.case = TRUE)) {
            in_description_block <- TRUE
        } else if (grepl("^[*#]\\s*@DESCRIPTION-END", trimmed_line_check, ignore.case = TRUE)) {
            in_description_block <- FALSE
        } else if (in_description_block) {
            # MODIFIED: Remove leading * or # and optional whitespace
            processed_line <- sub("^[*#]\\s?", "", line)
            # OPTIONAL: Remove trailing semicolon for SAS comments
            processed_line <- sub(";\\s*$", "", processed_line) 
            description_lines <- c(description_lines, processed_line)
        }
    }
    
    if (length(description_lines) > 0) {
        description_text <- paste(description_lines, collapse = "\n")
        description_html <- paste0("<p class='flowchart-description'>", description_text, "</p>")
    }
    
    docu_blocks <- list()
    in_block <- FALSE
    current_block_lines <- c()
    for (line in lines) {
        # MODIFIED: Look for * or #
        if (grepl("^[*#]@DOCU-START", trimws(line))) {
            in_block <- TRUE; current_block_lines <- c()
        } else if (grepl("^[*#]@DOCU-END", trimws(line))) {
            if (in_block) { 
                if (length(current_block_lines) > 0) { 
                    docu_blocks[[length(docu_blocks) + 1]] <- current_block_lines 
                } 
                in_block <- FALSE }
        } else if (in_block) {
            current_block_lines <- c(current_block_lines, line)
        }
    }
    
    if (!file.exists(pats_num_path)) {
        stop("Patient number file not found: ", pats_num_path, call. = FALSE)
    }
    
    patient_counts <- tryCatch({
        scan(pats_num_path, what = numeric(), sep = ",", quiet = TRUE, strip.white = TRUE)
    }, error = function(e) {
        warning("Could not read or parse patient numbers. Error: ", e$message, call. = FALSE)
        numeric(0)
    })
    
    if (length(docu_blocks) > length(patient_counts)) {
        warning("Blocks (", length(docu_blocks),") > Counts (", length(patient_counts), "). Padding with NA.")
        patient_counts <- c(patient_counts, rep(NA, length(docu_blocks) - length(patient_counts)))
    } else if (length(docu_blocks) < length(patient_counts)) {
        warning("Counts (", length(patient_counts),") > Blocks (", length(docu_blocks), "). Extra counts ignored.")
    }
    
    box_html_list <- list()
    # MODIFIED: Allow * or # in patterns for images and tables
    image_pattern <- "^\\s*[*#]\\s*@IMAGE:\\s*\"([^\"]+)\""
    html_table_pattern <- "^\\s*[*#]\\s*@HTML_TABLE:\\s*\"([^\"]+)\"" 
    
    for (i in seq_along(docu_blocks)) {
        block_content_original <- docu_blocks[[i]]
        has_table <- FALSE 
        
        count_html <- ""
        current_count <- patient_counts[i]
        if (!is.na(current_count)) {
            count_html <- paste0("<span class='patient-count'>n = ", current_count, "</span><br>")
        } else {
            count_html <- "<span class='patient-count na'>n = NA</span><br>"
        }
        
        inner_html_parts <- list()
        step_line_processed <- FALSE
        embedded_content_added <- FALSE
        
        for(original_line in block_content_original){
            
            is_image_line <- grepl(image_pattern, original_line, perl = TRUE)
            is_html_table_line <- grepl(html_table_pattern, original_line, perl = TRUE)
            
            if (is_image_line) {
                image_path <- sub(image_pattern, "\\1", original_line, perl = TRUE)
                embed_html <- paste0("<img src=\"", image_path, "\" alt=\"Image\" class=\"flowchart-image\"><br>")
                inner_html_parts[[length(inner_html_parts) + 1]] <- embed_html
                embedded_content_added <- TRUE
                next 
            } else if (is_html_table_line) {
                html_table_path <- trimws(sub(html_table_pattern, "\\1", original_line, perl = TRUE))
                if (file.exists(html_table_path)) {
                    html_table_content <- paste(readLines(html_table_path, warn = FALSE), collapse = "\n")
                    embed_html <- paste0("<div class='embedded-html-table'>", html_table_content, "</div><br>")
                    inner_html_parts[[length(inner_html_parts) + 1]] <- embed_html
                    embedded_content_added <- TRUE
                    has_table <- TRUE
                } else {
                    embed_html <- paste0("<p class='error'>Error: Table not found '", htmltools::htmlEscape(html_table_path), "'</p><br>")
                    inner_html_parts[[length(inner_html_parts) + 1]] <- embed_html
                }
                next 
            }
            
            # MODIFIED: Remove leading comment markers (* or #)
            line_no_marker <- sub("^[*#]@\\s?", "", original_line)
            line_no_comment <- sub("^[*#]\\s?", "", line_no_marker)
            
            # OPTIONAL: Remove trailing semicolon often found in SAS comments
            line_no_comment <- sub(";\\s*$", "", line_no_comment)
            
            trimmed_line <- trimws(line_no_comment)
            
            if (nchar(trimmed_line) > 0) {
                escaped_line <- htmltools::htmlEscape(trimmed_line)
                is_step_line <- grepl("^STEP:\\d+", escaped_line, ignore.case = TRUE)
                
                if (is_step_line && !step_line_processed){
                    inner_html_parts[[length(inner_html_parts) + 1]] <- paste0("<strong>", escaped_line, "</strong><br>")
                    inner_html_parts[[length(inner_html_parts) + 1]] <- count_html 
                    step_line_processed <- TRUE
                } else {
                    inner_html_parts[[length(inner_html_parts) + 1]] <- paste0(escaped_line, "<br>")
                }
            }
        } 
        
        if (!step_line_processed && !embedded_content_added && nzchar(count_html) && 
            count_html != "<span class='patient-count na'>n = NA</span><br>") {
            inner_html_parts[[length(inner_html_parts) + 1]] <- count_html
        }
        
        inner_html <- paste(unlist(inner_html_parts), collapse = "")
        
        box_classes <- "flowchart-box"
        if (has_table) {
            box_classes <- paste(box_classes, "has-embedded-table")
        }
        
        box_html_list[[i]] <- paste0(
            "<div class='", box_classes, "' id='box-", i, "'>\n",
            inner_html, "\n",
            "</div>"
        )
    } 
    
    all_boxes_html <- paste(box_html_list, collapse = "\n\n")
    
    # ... (CSS definitions remain unchanged) ...
    css <- "
  body { font-family: Arial, Helvetica, sans-serif; background-color: #f0f0f0; display: flex; justify-content: center; padding-top: 20px; padding-bottom: 20px; }
  #flowchart-container { display: flex; flex-direction: column; align-items: center; width: 90%; max-width: 700px; }
  h1 { color: #333; border-bottom: 2px solid #ccc; padding-bottom: 10px; margin-bottom: 25px; text-align: center; width: 100%; }
  .flowchart-description { background-color: #f9f9f9; border: 1px solid #cccccc; border-radius: 4px; padding: 15px; margin-bottom: 30px; text-align: justify; color: #555; width: 100%; box-sizing: border-box; line-height: 1.6; }
  .flowchart-box { background-color: #ffffff; border: 1px solid #ccc; border-radius: 5px; padding: 20px; margin-bottom: 50px; width: 100%; box-sizing: border-box; text-align: left; position: relative; box-shadow: 0 2px 4px rgba(0,0,0,0.1); line-height: 1.5; }
  .flowchart-box.has-embedded-table { min-width: 1000px; }
  .flowchart-box strong { font-weight: bold; color: #0066cc; display: inline; margin-right: 5px; }
  .patient-count { font-style: italic; color: #28a745; margin-left: 0px; } 
  .patient-count.na { color: #dc3545; }
  .flowchart-image { display: block; max-width: 95%; height: auto; margin: 10px auto 5px auto; border: 1px solid #ddd; }
  .embedded-html-table { margin-top: 10px; margin-bottom: 5px; max-width: 100%; overflow-x: auto; border: 1px solid #eee; }
  .embedded-html-table table { width: 1200px; min-width: 100%; border-collapse: collapse; font-size: 0.85em; margin: 0; }
  .embedded-html-table th, .embedded-html-table td { border: 1px solid #ddd !important; padding: 4px 6px !important; white-space: nowrap !important; text-align: left !important; }
  .embedded-html-table th { background-color: #f8f8f8 !important; font-weight: bold !important; }
  .error { color: red; font-weight: bold; }
  .flowchart-box:not(:last-child)::after { content: ''; position: absolute; bottom: -35px; left: 50%; transform: translateX(-50%); width: 2px; height: 25px; background-color: #555; z-index: -1; }
  .flowchart-box:not(:last-child)::before { content: ''; position: absolute; bottom: -40px; left: 50%; transform: translateX(-50%); width: 0; height: 0; border-left: 7px solid transparent; border-right: 7px solid transparent; border-top: 10px solid #555; z-index: -1; }
  "
    
    html_content <- paste(
        "<!DOCTYPE html>", "<html lang='en'>", "<head>", "<meta charset='UTF-8'>",
        "<meta name='viewport' content='width=device-width, initial-scale=1.0'>",
        "<title>R Script Flowchart with Counts and Content</title>",
        "<style>", css, "</style>", "</head>", "<body>",
        "<div id='flowchart-container'>",
        "<h1>Flowchart of the Pre-Processing Steps</h1>",
        description_html, 
        all_boxes_html,
        "</div>", "</body>", "</html>",
        sep = "\n"
    )
    
    tryCatch({
        output_dir <- dirname(output_html_path)
        if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
        }
        writeLines(html_content, output_html_path)
        message("Flowchart HTML generated successfully: ", output_html_path)
    }, error = function(e) {
        stop("Failed to write HTML file: ", e$message, call. = FALSE)
    })
    
    invisible(TRUE) 
}