library(shiny)
library(ggplot2)
library(plotly)

# UI
ui <- fluidPage(
  titlePanel("Interactive Plot with R Shiny"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", choices = c("mtcars", "iris")),
      uiOutput("xvar_ui"),
      uiOutput("yvar_ui")
    ),
    mainPanel(
      plotlyOutput("scatterPlot"),
      plotlyOutput("secondPlot"),
      verbatimTextOutput("clicked_info")
    )
  )
)

# Server
server <- function(input, output, session) {
  datasetInput <- reactive({
    switch(input$dataset,
           "mtcars" = df_plot1,
           "iris" = df_plot2)
  })
  
  all_clicks_df <- reactiveVal(data.frame(x = numeric(0), 
                                          y = numeric(0), 
                                          curveNumber=numeric(0),
                                          pointNumber=numeric(0),
                                          id=character(0),
                                          time = as.POSIXct(character(0))))
  
  
  
  output$xvar_ui <- renderUI({
    selectInput("xvar", "X-axis variable:", choices = names(datasetInput()))
  })
  
  output$yvar_ui <- renderUI({
    selectInput("yvar", "Y-axis variable:", choices = names(datasetInput()))
  })
  
  output$scatterPlot <- renderPlotly({
    #req(input$xvar, input$yvar)
    
    p <- ggplot(datasetInput())+#, aes(x = input$xvar, y = input$yvar)) +
      
      geom_segment(
        aes(x=start,
            xend=end,
            y=group,
            yend=group,
            color=pcol,
            
        ))+
      scale_color_gradientn(colors = c("blue", "yellow"))+
      
      #geom_point() +
      theme_minimal()
    
    ggplotly(p) %>% event_register("plotly_click")
  })
  
  output$secondPlot <- renderPlotly({
    
    click_info <- event_data("plotly_click")
    
    #req(input$xvar, input$yvar)
    data <- datasetInput()
   
    if (is.null(click_info)) {
      
      p2 <- ggplot(data,
                   aes(x = tm, y = rh_mm1, color=1)) +
        geom_point()+
        theme_minimal()
      
      ggplotly(p2)  
      
    } else {
      
      
      # click_info <- event
      # 
      clicked_point <- data %>%
        filter(start==click_info$x&group==click_info$y)
      
      # 
      # 
      # new_click <- data.frame(
      #   x = click_info$x,
      #   y = click_info$y,
      #   pointNumber = click_info$pointNumber,
      #   curveNumber = click_info$curveNumber,
      #   id=clicked_point$id,
      #   time = Sys.time()
      # )
      # # 
      # # # Append the new click data to the dataframe
      # updated_df <- bind_rows(all_clicks_df(), new_click)
      # # 
      # # # Update the reactive value with the new dataframe
      # all_clicks_df(updated_df)
      
      current_clicked_probes <- all_clicks_df()
      
      #clicked_point$id
      
      highlighted_data <- data %>% 
        mutate(hl=case_when(id %in% current_clicked_probes$id ~ "1", 
                            TRUE ~ "0"))
      
      
      # highlighted_data <- data %>% 
      #   mutate(hl=case_when(id== clicked_point$id~ "1", 
      #                       TRUE ~ "0"))
      
      p2 <- ggplot(highlighted_data,
                   aes(x = tm, y = rh_mm1, color=hl, size=as.character(hl))) +
        geom_point()+
        scale_color_manual(breaks=c(0,1), values = c("black", "red"))+
        scale_size_manual(breaks=c(0,1), values = c(1,3))+
        theme_minimal()
    
      ggplotly(p2)      
      
    }
    
    

  })
  
  output$clicked_info <- renderPrint({
    event <- event_data("plotly_click")
    if (is.null(event)) return("Click on a point to see details")
    
    data <- datasetInput()
    ## i think the + 1 is because r starts counting with 1 instead of zero
    clicked_point <- data[event$pointNumber + 1, ]
    
   # if ("Petal.Length" %in% colnames(data)) {
      paste("Petal Length:", clicked_point$id, "eventpointnumber: ", event$pointNumber)
  #  } else {
   #   "Petal Length not available in this dataset"
  #  }
  })
  
  
  observeEvent(event_data("plotly_click"), {
    #safe_info <- 0
   
    click_info <- event_data("plotly_click")
    
    data <- datasetInput()
    
    
    clicked_point <- data %>%
      filter(start==click_info$x&group==click_info$y)


    new_click <- data.frame(
      x = click_info$x,
      y = click_info$y,
      pointNumber = click_info$pointNumber,
      curveNumber = click_info$curveNumber,
      id=clicked_point$id,
      time = Sys.time()
    )
    
    ## check if clicked point was already clicked before... if so... remove it
    
    all_clicks <- all_clicks_df()
    
    if(clicked_point$id %in% all_clicks$id){
      updated_df <- all_clicks %>%
        filter(!id==clicked_point$id)
    } else {
      updated_df <- bind_rows(all_clicks, new_click)
    }
    
    # 
    # # Append the new click data to the dataframe
    
    # 
    # # Update the reactive value with the new dataframe
    all_clicks_df(updated_df)
    
    
    
    
    
    # Print event data in R console
    print(all_clicks_df())
    
   
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
