# Preprocessing Documentation and Automated Flowchart Generator

This repository provides a tool to automatically generate HTML flowcharts describing data preprocessing pipelines directly from **R** or **SAS** source code. 
By treating "Code as Documentation," it ensures your visual diagrams always match the actual analysis logic.

### Key Features

* **Multi-Language Support:** Works seamlessly with both R (`#`) and SAS (`*`) comments.
* **Automated Metrics:** Automatically extracts and displays patient counts () for each step alongside the description.
* **Rich Media:** Supports embedding plots and HTML tables directly into flowchart nodes.

### How It Works

1. **Annotate:** Add specific tags (e.g., `#@DOCU-START`, `#@STEP:001`) to your code comments.
2. **Log Counts:** Capture patient counts during script execution into a text file (`pats_num.txt`).
3. **Generate:** Run the provided R function to build a standalone HTML flowchart combining your comments and the logged counts.