# PDF OCR Shiny App

This R Shiny App is build to allow users upload pdf file, display the selected page of the pdf and use Tesseract OCR to read the text on the full page or read the text in the user cropped area.  

The OCR in use is a [Tesseract R binding](https://github.com/ropensci/tesseract), and the App itself is inspired by this [shiny-tesseract](https://github.com/JesseVent/shiny-tesseract).

# Setup
Below are the packages in use,   
```R
install.packages(shiny)  
install.packages(tidyverse)  
install.packages(magick)  
install.packages(pdftools)  
install.packages(tesseract)
```

Alternatively, if you use `renv`, there is a `renv.lock` file attached, you can use `renv::restore()` to restore the packages.

# Performance
The Tesseract setup is by default English language engine, it can read most pdf scans in plain text pages. However, the "Read full page" function doesn't recognise any column or table layout, the extracted text need further processing if the upload pdf page is a form or a table. 