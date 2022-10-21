library(shiny)
library(tidyverse)
library(magick)
library(pdftools)
library(tesseract)

# setup English language engine
english_engine <- tesseract('eng', datapath = '.')

# Define UI for data upload app ----
ui <- fluidPage(
  
  
  # App title ----
  titlePanel("Uploading Files"),
  
  fluidRow(
    
    column(4, 
           
           # Input: Select a file ----
           fileInput("file1", "Choose PDF File",
                     multiple = FALSE,
                     accept = c(".pdf")),
           
           # Horizontal line ----
           tags$hr(),
           
           # use a slider to adjust the pdf page width
           sliderInput("w_pix", "Select width",
                       600,
                       1400,
                       value = 800,
                       step = 8
           ),
           # rotate the pdf in case it is scanned on other directions
           numericInput('rotation', 'Rotate page',
                        0, 
                        min = 0,
                        max = 360
           ),
           # could use a slider input to replace the above rotation input
           # sliderInput("rotation1", "Rotate angle",
           #             0,
           #             360,
           #             value = 0,
           #             step = 1,
           #             animate = animationOptions(interval = 2000)
           # ),
           
           # show below UI if a PDF file is uploaded
           # the UI is to control the page of pdfs
           uiOutput("jump"),
           uiOutput("pagetext"),
           uiOutput("navbuttons"),
           checkboxInput("extract_full", "Extract full page", value = FALSE),
           
           hr(),    
           
           # the UI for the extracted text outputs
           uiOutput('crop_area_heading'),
           textOutput('ocr_crop_text'),
           tags$style(type="text/css", "#ocr_crop_text {white-space: pre-wrap;}"),
           uiOutput('extracted_text_full_page'),

           
           
    ),
    
    column(
           width = 8,
           # the loaded pdf page, with the cropping option (brush)
           imageOutput("pdf_page_image", inline = TRUE, 
                       click = 'image_click',
                       hover = hoverOpts(
                         id        = "image_hover",
                         delay     = 500,
                         delayType = "throttle"),
                       brush = brushOpts(
                         id        = "image_brush",
                         fill      = "#f5ad27",
                         stroke    = "#c2830e",
                         clip      = TRUE,
                         resetOnNew = TRUE),
           ),
       
           # the cropped image for debugging purposes
           # h2('Cropped image'),
           # imageOutput("croppedimage", width = '100%', inline =TRUE),
    )
  )
)



# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  # the demo pdf, i.e. the landing page
  demo_path <- ".\\data\\pdf\\sample.pdf"
  # how many pages in the pdf  
  pagecounter <- reactiveValues(pagevalue = 1) # Page counter initial value
  
  # the pdf file, use a reactivevalue for the new uploads
  infile <- reactiveValues(filepath = demo_path
  )
  
  # get the width from the slider input
  width_in <- reactive({
    input$w_pix
  }) 
  
  # pdf page controllers
  observeEvent(input$nex, {
    pagecounter$pagevalue <-
      pagecounter$pagevalue + 1     # Next - page counter
    
  })
  
  observeEvent(input$prv, {
    
    if(pagecounter$pagevalue > 1){
      pagecounter$pagevalue <- pagecounter$pagevalue - 1
    } else {
      pagecounter$pagevalue = 1
    }
      # Previous - page counter
  })
  
  observeEvent(input$nex2, {
    pagecounter$pagevalue <-
      pagecounter$pagevalue + 1     # Next - page counter
  })
  
  observeEvent(input$prv2, {
    pagecounter$pagevalue <-
      pagecounter$pagevalue - 1  # Previous - page counter
  })
  
  observeEvent(input$jump, {
    if (input$jump != pagecounter$pagevalue) {
      pagecounter$pagevalue <-
        as.integer(input$jump)  # Actual page - page counter
      
    }
  })
  
  observeEvent(input$file1, {
    pagecounter$pagevalue <- 1
  })
  
  
  
  totpages <- renderText({
    req(input$file1)
    
    pdf_info(input$file1$datapath)$pages
    
  })
  
  
  output$jump <- renderUI({
    req(input$file1)
    selectInput(
      "jump",
      "Page:",
      choices = seq(1:totpages()),
      selected = pagecounter$pagevalue
    )
  })
  
  output$pagetext <- renderUI({
    HTML(paste("out of", totpages()))
  })
  
  
  output$navbuttons <- renderUI({
    req(input$file1)
    
    tagList(actionButton("prv", "Previous page"),
            
            actionButton("nex", "Next page"))
  })
  
  # get the coords from the cropping
  coords <- reactive({
    w   <- round(input$image_brush$xmax - input$image_brush$xmin, digits = 2)
    h   <- round(input$image_brush$ymax - input$image_brush$ymin, digits = 2)
    dw  <- round(input$image_brush$xmin, digits = 2)
    dy  <- round(input$image_brush$ymin, digits = 2)
    coords <- paste0(w, "x", h, "+", dw, "+", dy)
    
    return(coords)
  })
  
  # the cropping coordinations
  # output$coordstext <- renderText({
  #   if (is.null(input$image_brush$xmin)) {
  #     "No Area Selected!"
  #   } else {
  #     coords()}
  # })
  
  # update the reactive value "infile" when a pdf is uploaded
  observeEvent(input$file1, {
    if (!is.null(input$file1)) {
      infile$filepath <- input$file1$datapath
      
    }
  })
  
  # function to load selected page of PDF into png, with resize and rotation options
  load_pdf_page <- reactive({
    
    page <- infile$filepath %>% image_read_pdf(., pages = pagecounter$pagevalue) %>% 
      image_resize(paste0(input$w_pix, 'x')) %>%
      image_rotate(input$rotation)
    
    page
    
  })
  
  # render the png of the selected pdf page
  output$pdf_page_image <- renderImage({
    # width   <- session$clientData$output_pdf_page_image_width
    # height  <- session$clientData$output_pdf_page_image_height
    
    imgs_out <- load_pdf_page() %>%
      image_write(tempfile(fileext = '.png'), format = 'png')
    
    list(src = imgs_out, contentType = "png") # width = width_in()
    
  }, 
  deleteFile=TRUE)
  
  # render the cropped image, this is more for debugging purpose, remove if not used
  output$croppedimage <- renderImage({
    req(input$image_brush)
    img_crop <- load_pdf_page() %>%
      image_crop(coords(), repage = FALSE) %>%
      image_write(tempfile(fileext = '.png'), format = 'png')
    
    list(src = img_crop, contentType = "png", width = (input$image_brush$xmax - input$image_brush$xmin)) 
  },
  deleteFile = TRUE
  )
  
  # render the UI if the "extract full page" is selected 
  output$extracted_text_full_page <- renderUI({
    req(input$extract_full)
    tagList(
      h3('Full text'),
      textOutput('ocr_text'),
      tags$style(type="text/css", "#ocr_text {white-space: pre-wrap;}")
      )
  })
  
  
  output$ocr_text <- renderText({
    
    if(input$extract_full){
      text <- load_pdf_page()  %>%
        tesseract::ocr(engine = english_engine)
    } else {
      text <- ''
    }

    text
  })

  output$crop_area_heading <- renderUI({
    req(input$image_brush)
    h3('Crop area text')
  })
  

  output$ocr_crop_text <- renderText({
    req(input$image_brush)
    text <- load_pdf_page() %>%
      image_crop(coords(), repage = FALSE) %>%
      image_ocr(datapath = '.')  #%>%
      #gsub(pattern = "\\n", replacement = "<br/>", .)
    
    text
    
  }
  )
  
  # observeEvent(input$extract_full,{
  #   
  #   output$ocr_text <- renderPrint({
  #     # req(input$file1)
  #     
  #     text <- infile$filepath %>% image_read_pdf(pages = pagecounter$pagevalue)  %>%
  #       tesseract::ocr(engine = english_engine) %>%
  #       gsub(pattern = "\\n", replacement = "<br/>", .)
  #     
  #     text
  #   })
  #   
  # })
  
  
}

# Create Shiny app ----
shinyApp(ui, server)
