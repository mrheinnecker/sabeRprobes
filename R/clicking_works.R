library(shiny)
library(ggplot2)
library(plotly)

# UI
ui <- fluidPage(
  titlePanel("Interactive Plot with R Shiny"),
  sidebarLayout(
    # sidebarPanel(
    #   
    #   uiOutput("xvar_ui"),
    #   uiOutput("yvar_ui")
    # ),
    
    #############
    
      sidebarPanel(
        selectInput("dataset", "Choose a dataset:", choices = c("mtcars", "iris")),
        sliderInput("tm_range", "Tm Range:",
                    min = min(df$tm), max = max(df$tm),
                    value = range(df$tm), step = 0.1),
        sliderInput("hits_ref_range", "Hits Ref Range:",
                    min = min(df$rh_mm0), max = max(df$rh_mm0),
                    value = range(df$rh_mm0), step = 1),
        sliderInput("gc_content", "GC content:",
                    min = min(df$gc_content), max = max(df$gc_content),
                    value = range(df$gc_content), step = 0.01),
        sliderInput("access_range", "Class Range:",
                    min = min(df$access), max = max(df$access),
                    value = range(df$access), step = 1),
        checkboxGroupInput("target_filter", "Select Targets:",
                           choices = unique(df$target),
                           selected = unique(df$target)),
        actionButton("load_data", "Load & Plot Data"),
        downloadButton("downloadData", "Download Selected Rows as TSV")
      ),
    
    ###############
    
    mainPanel(  
      verbatimTextOutput("clicked_info"),
      plotlyOutput("scatterPlot"),
      fluidRow(
        column(6, plotlyOutput("secondPlot")),
        column(6, plotlyOutput("thirdPlot"))
      ),
   
    
    )
  )
)

# Server
server <- function(input, output, session) {
  
  current_data <- reactiveVal(NULL)
  
  # observe({
  #   if (is.null(current_data())) {
  #     current_data(df)
  #   }
  # })
  
  #filter_data <- function()
  
  observeEvent(input$load_data, {
    sel_data <- switch(input$dataset,
                   "mtcars" = df,
                   "iris" = df)
    
    
    sel_fill_data <- sel_data %>%
      filter(
        tm >= input$tm_range[1], tm <= input$tm_range[2],
        #rh_mm0 >= input$hits_ref_range[1], 
        rh_mm0 <= input$hits_ref_range[2],
    #    access >= input$access_range[1], access <= input$access_range[2],
       # target %in% input$target_filter
      )
    
    
    current_data(sel_fill_data)
  })
  
  all_clicks_df <- reactiveVal(data.frame(x = numeric(0), 
                                          y = numeric(0), 
                                          curveNumber=numeric(0),
                                          pointNumber=numeric(0),
                                          id=character(0),
                                          time = as.POSIXct(character(0)))
                               )
  
  
  
  # output$xvar_ui <- renderUI({
  #   selectInput("xvar", "X-axis variable:", choices = names(datasetInput()))
  # })
  # 
  # output$yvar_ui <- renderUI({
  #   selectInput("yvar", "Y-axis variable:", choices = names(datasetInput()))
  # })
  
  output$scatterPlot <- renderPlotly({
   
    
    
    ########################
    
     click_info <- event_data("plotly_click")
     req(current_data())
     data <- current_data()
    # 
    if (is.null(click_info)) {

      p <- ggplot(data)+#, aes(x = input$xvar, y = input$yvar)) +
        
        geom_segment(
          aes(x=start,
              xend=end,
              y=group,
              yend=group,
              color=pcol,
              
          ), linewidth=5)+
        scale_color_gradientn(colors = c("blue", "yellow"))+
        
        #geom_point() +
        theme_minimal()
      
      ggplotly(p) %>% event_register("plotly_click")

    } else {
      
        current_clicked_probes <- all_clicks_df()

        #clicked_point$id

        highlighted_data <- data %>%
          mutate(hl=case_when(id %in% current_clicked_probes$id ~ id,
                              TRUE ~ NA))
      
      
      p <- ggplot(highlighted_data)+#, aes(x = input$xvar, y = input$yvar)) +
        
        geom_segment(
          aes(x=start,
              xend=end,
              y=group,
              yend=group,
              color=hl,
              
          ), linewidth=5)+
      #  scale_color_gradientn(colors = c("blue", "yellow"))+
        
        #geom_point() +
        theme_minimal()
      
      ggplotly(p) %>% event_register("plotly_click")
      
      
    #   
    #   
    #   # click_info <- event
    #   # 
    #   clicked_point <- data %>%
    #     filter(start==click_info$x&group==click_info$y)
    #   
    #   
    #   
    #   current_clicked_probes <- all_clicks_df()
    #   
    #   #clicked_point$id
    #   
    #   highlighted_data <- data %>% 
    #     mutate(hl=case_when(id %in% current_clicked_probes$id ~ "1", 
    #                         TRUE ~ "0"))
    #   
    #   
    #   
    #   p2 <- ggplot(highlighted_data,
    #                aes(x = tm, y = rh_mm1, color=hl, size=as.character(hl))) +
    #     geom_point()+
    #     scale_color_manual(breaks=c(0,1), values = c("black", "red"))+
    #     scale_size_manual(breaks=c(0,1), values = c(1,3))+
    #     theme_minimal()
    #   
    #   ggplotly(p2)  
    
    }
    
    
    ########################
    
    
  })
  
  
  output$thirdPlot <- renderPlotly({
    
    click_info <- event_data("plotly_click")
    req(current_data())
    data <- current_data()
    
    if (is.null(click_info)) {
      
      p2 <- ggplot(data,
                   aes(x = rh_mm0, y = rh_mm1, color=1)) +
        geom_point()+
        theme_minimal()
      
      ggplotly(p2)   %>% event_register("plotly_click")
      
    } else {
      
      
      # click_info <- event
      # 
      clicked_point <- data %>%
        filter(start==click_info$x&group==click_info$y)
      
      
      
      current_clicked_probes <- all_clicks_df()
      
      #clicked_point$id
      
      highlighted_data <- data %>%
        mutate(hl=case_when(id %in% current_clicked_probes$id ~ id,
                            TRUE ~ NA),
               sz=case_when(id %in% current_clicked_probes$id ~ "1",
                            TRUE ~ "0"))
      
      
      
      p2 <- ggplot(highlighted_data,
                   aes(x = rh_mm0, y = rh_mm1, color=hl, size=sz)) +
        geom_point(shape=1)+
        #scale_color_manual(breaks=c(0,1), values = c("black", "red"))+
        scale_size_manual(breaks=c(0,1), values = c(1,3))+
        theme_minimal()
      
      ggplotly(p2)     %>% event_register("plotly_click")  
      
    }
    
    
    
  })
  
  
  
  output$secondPlot <- renderPlotly({
    
    click_info <- event_data("plotly_click")
    req(current_data())
    data <- current_data()
   
    if (is.null(click_info)) {
      
      p2 <- ggplot(data,
                   aes(x = tm, y = gc_content, color=1)) +
        geom_point()+
        theme_minimal()
      
      ggplotly(p2)   %>% event_register("plotly_click")
      
    } else {
      
      
      # click_info <- event
      # 
      clicked_point <- data %>%
        filter(start==click_info$x&group==click_info$y)
      
     
      
      current_clicked_probes <- all_clicks_df()
      
      #clicked_point$id
      
      highlighted_data <- data %>%
        mutate(hl=case_when(id %in% current_clicked_probes$id ~ id,
                            TRUE ~ NA),
               sz=case_when(id %in% current_clicked_probes$id ~ "1",
                            TRUE ~ "0"))
      
      
      
      p2 <- ggplot(highlighted_data,
                   aes(x = tm, y = gc_content, color=hl, size=sz)) +
        geom_point(shape=1)+
        #scale_color_manual(breaks=c(0,1), values = c("black", "red"))+
        scale_size_manual(breaks=c(0,1), values = c(1,3))+
        theme_minimal()
    
      ggplotly(p2)     %>% event_register("plotly_click")  
      
    }
    
    

  })
  
  
  output$clicked_info <- renderPrint({
   # req(current_data())  # make sure data is loaded
    
   
    
    
    if (is.null(current_data())) {
      data <- df
    } else {
       data <- current_data()
    }
    
    # Apply filters from UI
    filtered_data <- data %>%
      filter(
        tm >= input$tm_range[1], tm <= input$tm_range[2],
        #rh_mm0 >= input$hits_ref_range[1], 
        rh_mm0 <= input$hits_ref_range[2],
      )

    paste0("Filtered data has ", nrow(filtered_data))
    
    
    # Get click info
  #  event <- event_data("plotly_click")
    
    # Output summary
  #  if (is.null(event)) {
  #    paste("Filtered data has", nrow(filtered_data), "rows.\nClick on a point to see more info.")
  #  } else {
   #   clicked_point <- filtered_data[event$pointNumber + 1, ]
      # paste0("Filtered data has ", nrow(filtered_data), " rows.\n",
      #        "Clicked ID: ", clicked_point$id,
      #        "\nEvent point number: ", event$pointNumber)
   # }
  })
  
  # output$clicked_info <- renderPrint({
  #   event <- event_data("plotly_click")
  #   if (is.null(event)) return("Click on a point to see details")
  #   
  #   req(current_data())
  #   data <- current_data()
  #   
  #   ## i think the + 1 is because r starts counting with 1 instead of zero
  #   clicked_point <- data[event$pointNumber + 1, ]
  #   
  #  # if ("Petal.Length" %in% colnames(data)) {
  #     paste("Petal Length:", clicked_point$id, "eventpointnumber: ", event$pointNumber)
  # #  } else {
  #  #   "Petal Length not available in this dataset"
  # #  }
  # })
  
  
  observeEvent(event_data("plotly_click"), {
   
    click_info <- event_data("plotly_click")
    
    req(current_data())
    data <- current_data()
    
    
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
      print(new_click)
      Sys.sleep(5)
      updated_df <- bind_rows(all_clicks, new_click)
    }
    
    all_clicks_df(updated_df)
    
    # Print event data in R console
    print(all_clicks_df())
    
   
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
