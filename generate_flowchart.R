source("generic_html_flowchart_generator.R")

path <- "./output/"
html_table <- tryCatch({
  paste(readLines(paste0(path,"data_table.html"), warn = FALSE), collapse = "\n")
}, error = function(e) {
  stop(paste("Error reading HTML file:", e$message), call. = FALSE)
})

generate_flowchart_html(script_path = "preprocessing_test_data.R", 
                        pats_num_path = paste0(path,"pats_num.txt"),
                        output_html_path = paste0(path,"test_flowchart.html"))
