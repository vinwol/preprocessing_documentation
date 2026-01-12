source("generic_html_flowchart_generator.R")

# Generate flow chart for R script.
generate_flowchart_html(script_path = "r_code_snippet.R", 
                        pats_num_path = "pats_num.txt",
                        output_html_path = "r_flowchart.html")
                        
# Generate flow chart for SAS script.
generate_flowchart_html(script_path = "sas_code_snippet.sas", 
                        pats_num_path = "pats_num.txt",
                        output_html_path = "sas_flowchart.html")                        
