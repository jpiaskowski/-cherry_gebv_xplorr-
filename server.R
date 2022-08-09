
shinyServer(function(input, output, session) {
  
  #create option to uncheck all boxes
  observe({
    if (input$unCheck > 0) {
      updateCheckboxGroupInput(session=session, inputId="show_vars",
                               choices = names(A1)[3:ncol(A1)],selected=NULL)
    }
  })
  
  
  #Display table of breeding values
  
  #choose the data set to use
  bv.df<-reactive({
    switch(input$gen.val,
                "a" = A1,
                "g" = G1,
                "p" = P1)
  })
  
  #write.csv(bv.df(),"test_bv.csv")

  #actual data table I need
  output$mytable = DT::renderDataTable(
    cbind(Individual = bv.df()[["Individual"]], bv.df()[, input$show_vars, drop=FALSE]),
    filter = 'top', selection = "none",rownames = F,
    options = list(aDatasort = TRUE, aLengthMenu = c(5,10,15), iDisplayLength = 5)
  )
  
  # access filtered data
  filtered_data <- reactive({
          rows <-input$mytable_rows_all 
          bv.df()[as.numeric(rows),c("Individual",input$show_vars)]
  })
  
  #download filtered data
  
  output$downloadData1 <- downloadHandler(
      filename = function() {
        dataset<-unique(as.character(bv.df()$values))
        paste("filtered_cherry_", dataset, "_values_",Sys.time(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(filtered_data(), file,row.names = F)
      }
      )
  
  # bivariate scatter plot
    
  output$bivar<-renderPlotly({

    plot_ly(data = bv.df(),y = ~get(input$trait1), x = ~get(input$trait2),type = "scatter",
            color = ~Germplasm,text = ~Individual,colors = myColors,hoverinfo="text",
            marker = list(opacity = 0.7,size = 10), source = "subset"
            ) %>%
          layout(
            dragmode = "select",
            xaxis = list(title = paste(input$trait2)),
            yaxis = list(title = paste(input$trait1)))
  })
  

  # render as a table in UI
  output$clickTable <-renderTable({
    
    
    #event.data.click <- event_data("plotly_click", source = "subset")
    event.data.select <- event_data("plotly_selected", source = "subset")

    if(is.null(event.data.select) == T) return("Choose individuals by selecting a region in the scatterplot") else { 

    # Get index from each germmplasm group
   cv.group <- subset(bv.df(), Germplasm == "cultivar & ancestor")[subset(event.data.select, curveNumber == 0)$pointNumber + 1,]
   sdlg.group <- subset(bv.df(), Germplasm == "unselected offspring")[subset(event.data.select, curveNumber == 1)$pointNumber + 1,]
   wild.group <- subset(bv.df(), Germplasm == "wild ancestor")[subset(event.data.select, curveNumber == 2)$pointNumber + 1,]

    # Combine and make table
    table.subset <- rbind(cv.group,sdlg.group,wild.group) %>% na.omit() %>% as.data.frame()
  } 

  })
  
  # Download data option

    #download function

  output$downloadData2 <- downloadHandler(
    filename = function() {
      dataset<-unique(as.character(bv.df()$values))
      paste("sweet_cherry_", dataset, "_values_", Sys.time(), ".csv", sep="")
    },
    content = function(file) {
      
      event.data.select <- event_data("plotly_selected", source = "subset")
      
      cv.group <- subset(bv.df(), Germplasm == "cultivar & ancestor")[subset(event.data.select, curveNumber == 0)$pointNumber + 1,]
      sdlg.group <- subset(bv.df(), Germplasm == "unselected offspring")[subset(event.data.select, curveNumber == 1)$pointNumber + 1,]
      wild.group <- subset(bv.df(), Germplasm == "wild ancestor")[subset(event.data.select, curveNumber == 2)$pointNumber + 1,]
      
      # Combine and make table
      dTable <- rbind(cv.group,sdlg.group,wild.group) %>% na.omit() %>% as.data.frame()

      write.csv(dTable, file,row.names = F)
    }
  )
  
})
  
  
  
  
  