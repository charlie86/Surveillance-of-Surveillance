require(shiny)
require(ggplot2)
require(ggthemes)
require(treemap)
require(ggvis)
require(dplyr)
require(googleVis)
# setwd("C:/Users/Charlie/Documents/Booz/Dashboard/Shiny/newdata")
# source("C:/Users/Charlie/Documents/Booz/Dashboard/Shiny/newdata/Data.R")
source ("./Data.R")

shinyServer(function(input, output, session) {
    
    ##### Categorical Plot Output #####
    
    output$cat.plot <- renderPlot({
        # The type of plot rendered is conditional on the selected input of cat.plot
        
        # Bar chart
        
        if (input$cat.chart == "Bar Chart") {
            
            if (input$cat.var == "updates") {
                freq_cat.updates <- freq_cat.updates %>% slice(match(update_order, Var1))
                p <- ggplot(data = get(paste0("freq_cat.", input$cat.var)),
                            aes(x = reorder(Var1, order), y = Freq))
            }
            
            else {
                p <- ggplot(data = get(paste0("freq_cat.", input$cat.var)),
                            aes(x = reorder(Var1, -Freq), y = Freq))
            }
            p + geom_bar(stat = "identity", fill = "#244A5D") + theme_economist() + 
                labs(x = "Category", y = "# of Systems") + 
                theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                      axis.text.y = element_text(size = 12),
                      axis.title.x = element_text(size = 16, face = "bold"),
                      axis.title.y = element_text(size = 16, face = "bold"))
        }
        
        # Pie chart 
        
        else if (input$cat.chart == "Pie Chart") {
            pie <- ggplot(get(paste0("freq_cat.", input$cat.var)),
                          aes(x = "", y = Freq, fill = Var1)) +
                geom_bar(width = 1, stat = "identity")
            pie + coord_polar("y", start = 0) + 
                #                 scale_fill_economist() + 
                blank_theme + theme(axis.text.x = element_blank()) + 
                guides(fill = guide_legend(title = NULL))
            #                 geom_text(aes(y = Freq/length(Freq) + c(0, cumsum(Freq)[-length(Freq)]),
            #                               label = Freq), size = 6)
        }
        
        # Treemap
        
        else if (input$cat.chart == "Treemap") {
            treemap(get(paste0("freq_cat.", input$cat.var)),
                    index = "label",
                    vSize = "Freq",
                    palette = c("#d5e4eb", "#244A5D"),
                    border.col = "white",
                    title = "")
        }
        
    })
    
    ##### Reactive Keyword Table #####
    # Renders/refreshes only when the "Go!" button is clicked
    # Table output depends on the number of keywords searched (1, 2, or 3)
    Keyword.Table <- eventReactive(input$goButton, {
        
        if (input$keynum == 1) {
            contains.tbl <- contains.table(keyword.text(), input$text.var)
            contains.tbl[, input$show_vars.keyword, drop = FALSE]
        }
        
        else if (input$keynum == 2) {
            contains.tbl <- contains.table2(keyword.text1(),
                                            keyword.text2(), input$text.var)
            contains.tbl[, input$show_vars.keyword2, drop = FALSE]   
        }
        
        else if (input$keynum == 3) {
            contains.tbl <- contains.table3(keyword.text1(),
                                            keyword.text2(),
                                            keyword.text3(), input$text.var)
            contains.tbl[, input$show_vars.keyword2, drop = FALSE]   
        }
        
    })
    # Displays only if the keyword input box contains text
    output$Keyword.Table <- renderDataTable({
        if (keyword.text() != "")
            Keyword.Table()
    }, 
    options = list(pageLength = 10)
    )
    
    ##### Generate keyword textbox input as text output #####
    
    output$keyword.var <- renderText({
        input$text.var
    })
    
    keyword.text <- eventReactive(input$goButton, {
        input$text.string
    })
    
    keyword.text1 <- eventReactive(input$goButton, {
        input$text.string1
    })
    
    keyword.text2 <- eventReactive(input$goButton, {
        input$text.string2
    })
    
    keyword.text3 <- eventReactive(input$goButton, {
        input$text.string3
    })
    
    output$keyword.text <- renderText({
        keyword.text()
    })
    
    output$keyword.tot <- renderText({
        contains.tbl <- contains.table(input$text.string, input$text.var)
        sum(contains.tbl[, "Keyword Count"])
    })
    
    keyword.sysnum <- eventReactive(input$goButton, {
        if (input$keynum == 1) {
            if (keyword.text() != "") {
                contains.tbl <- contains.table(keyword.text(), input$text.var)
                
                if (sum(contains.tbl[, "Keyword Count"]) > 1 & length(contains.tbl$System) > 1) {
                    paste0('Your chosen keyword, "', keyword.text(),
                           '," is mentioned ', sum(contains.tbl[, "Keyword Count"]),
                           ' times by ', length(contains.tbl$System), 
                           ' systems under the "', label.names[input$text.var], '" category.')
                }
                
                else if (sum(contains.tbl[, "Keyword Count"]) == 1 & length(contains.tbl$System) > 1) {
                    paste0('Your chosen keyword, "', keyword.text(),
                           '," is mentioned ', sum(contains.tbl[, "Keyword Count"]),
                           ' time by ', length(contains.tbl$System), 
                           ' systems under the "', label.names[input$text.var], '" category.')
                }
                
                else if (sum(contains.tbl[, "Keyword Count"]) == 1 & length(contains.tbl$System) == 1) {
                    paste0('Your chosen keyword, "', keyword.text(),
                           '," is mentioned ', sum(contains.tbl[, "Keyword Count"]),
                           ' time by ', length(contains.tbl$System), 
                           ' system under the "', label.names[input$text.var], '" category.')
                }
                
                else if (sum(contains.tbl[, "Keyword Count"]) > 1 & length(contains.tbl$System) == 1) {
                    paste0('Your chosen keyword, "', keyword.text(),
                           '," is mentioned ', sum(contains.tbl[, "Keyword Count"]),
                           ' times by ', length(contains.tbl$System), 
                           ' system under the "', label.names[input$text.var], '" category.')
                }
                
                else paste0('Your chosen keyword, "', keyword.text(),
                            '," is not mentioned by any systems under the "', 
                            label.names[input$text.var], '" category.')
            }
            else 'Please enter a keyword in the box to the left and click "Go!" to display a table.'
        }
        
        else if (input$keynum == 2) {
            
            contains.tbl <- contains.table2(keyword.text1(),
                                            keyword.text2(), input$text.var)
            
            if (keyword.text1() != "" & keyword.text2() != "") {
                
                if (length(contains.tbl$System) > 1) {
                    paste0('Your chosen key phrase, "', keyword.text1(), " ", keyword.text2(),
                           '," is mentioned by ', length(contains.tbl$System),
                           ' systems under the "', label.names[input$text.var], '" category.')
                }
                
                else if (length(contains.tbl$System) == 1) {
                    paste0('Your chosen key phrase, "', keyword.text1(), " ", keyword.text2(),
                           '," is mentioned by ', length(contains.tbl$System),
                           ' system under the "', label.names[input$text.var], '" category.')
                }
                
                else if (length(contains.tbl$System) == 0) {
                    paste0('Your chosen key phrase, "', keyword.text1(), " ", keyword.text2(),
                           '," is not mentioned by any systems under the "', 
                           label.names[input$text.var], '" category.')
                } 
            }
            
            else 'Please enter the two words in your key phrase in the boxes to the left 
            and click "Go!" to display a table.'
        }
        
        else if (input$keynum == 3) {
            
            contains.tbl <- contains.table3(keyword.text1(), keyword.text2(), 
                                            keyword.text3(), input$text.var)
            
            if (keyword.text1() != "" & keyword.text2() != "" & keyword.text3() != "") {
                
                if (length(contains.tbl$System) > 1) {
                    paste0('Your chosen key phrase, "', keyword.text1(), " ", keyword.text2(),
                           " ", keyword.text3(),
                           '," is mentioned by ', length(contains.tbl$System),
                           ' systems under the "', label.names[input$text.var], '" category.')
                }
                
                else if (length(contains.tbl$System) == 1) {
                    paste0('Your chosen key phrase, "', keyword.text1(), " ", keyword.text2(),
                           " ", keyword.text3(),
                           '," is mentioned by ', length(contains.tbl$System),
                           ' system under the "', label.names[input$text.var], '" category.')
                }
                
                else if (length(contains.tbl$System) == 0) {
                    paste0('Your chosen key phrase, "', keyword.text1(), " ", keyword.text2(),
                           " ", keyword.text3(),
                           '," is not mentioned by any systems under the "', 
                           label.names[input$text.var], '" category.')
                } 
            }
            
            else 'Please enter the three words in your key phrase in the boxes to the left 
            and click "Go!" to display a table.'
        }
        
    })
    
    output$keyword.sysnum <- renderText({
        keyword.sysnum()
    })
    
    
    ##### Titles for Keyword Frequency Plots #####
    
    output$key_freq.titles <- renderText({
        paste0('Top ', input$keyword.num, ' keywords in the "',
               label.names[input$freq.var], '" category')
    })
    
    ##### Titles for Categorical Plots #####
    
    output$cat.titles <- renderText({
        paste("Distribution of Systems by", label.names[input$cat.var])
    })
    
    
    output$FreqPlot <- renderPlot({
        key_dat <- unlist(data[, input$freq.var])
        key_dat.tbl <- as.data.frame(table(key_dat))
        key_dat.tbl <- key_dat.tbl[order(key_dat.tbl$Freq, decreasing = TRUE), ]
        names(key_dat.tbl) <- c("Keyword", "Frequency")
        
        if (input$keyword.chart == "Bar Chart") {
            
#             h <- my_hist(input$freq.var, input$keyword.num)
            
            h <- ggplot(key_dat.tbl[1:input$keyword.num, ], aes(x = reorder(Keyword, -Frequency), y = Frequency))
            
            h + geom_bar(stat = "identity", fill = "#244A5D") + theme_economist() +
                labs(x = "Frequency") +
                theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                      axis.text.y = element_text(size = 12),
                      axis.title.x = element_text(size = 16, face = "bold"),
                      axis.title.y = element_text(size = 16, face = "bold"))
        }
        
        else if (input$keyword.chart == "Treemap") {
            treemap(key_dat.tbl[1:input$keyword.num, ],
                    index = "Keyword",
                    vSize = "Frequency",
                    palette = c("#d5e4eb", "#244A5D"),
                    border.col = "white",
                    title = "")
        }
        
    })
    
    ##### Reactive UI #####
    
    output$catValues <- renderUI({
        catVals <- unique(data[, input$yr_cat.var])
        selectInput("yr_cat.vals",
                    "Value:",
                    catVals)
    })
    
    yr_dat <- reactive({
        Systems <- vector()
        Year <- vector()    
        
        for (i in input$yr.range[1]:input$yr.range[2]) {
            Year <- rbind(Year, i)
            Systems <- rbind(Systems, 
                             length(data$System[data[, input$yr_cat.var] == input$yr_cat.vals & 
                                                    data$est == i]))
        }
        
        rownames(Year) <- NULL
        
        yr_sys.dat <- data.frame(Year, Systems)
        
        yr_sys.dat
        
    })
    
    
    output$yrPlot <- renderPlot({
        
        g <- ggplot(data = yr_dat(), aes(x = Year, y = Systems))
        g + geom_bar(fill = "#244A5D", stat = "identity") + 
            theme_economist() +
            labs(x = "Year", y = "# of Systems") + 
            theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                  axis.text.y = element_text(size = 12),
                  axis.title.x = element_text(size = 16, face = "bold"),
                  axis.title.y = element_text(size = 16, face = "bold"))
        #     }
    })
    
    
    
    ##### Table #####
    
    output$ggvisTitle <- renderText({paste0('Number of Systems Established with "', 
                                            label.names[input$yr_cat.var], 
                                            ': ', 
                                            input$yr_cat.vals,
                                            '"', " - ", input$yr.range[1], " to ",
                                            input$yr.range[2])})
    
    yr_sys.tbldat <- reactive({
        tmptab <- as.data.frame(data$System[data[, input$yr_cat.var] == input$yr_cat.vals &
                                                data$est >= input$yr.range[1] &
                                                data$est <= input$yr.range[2]])
        yr_sys.names <- data.frame(YearEst = data$est[data[, input$yr_cat.var] == input$yr_cat.vals &
                                                          data$est >= input$yr.range[1] &
                                                                   data$est <= input$yr.range[2]], tmptab)
  
        
        colnames(yr_sys.names) <- c("Year Established", paste0('From ', input$yr.range[1], ' to ',
                                         input$yr.range[2],
                                         ', there were ', nrow(yr_sys.names),
                                         ' systems established with "', 
                                         label.names[input$yr_cat.var], ': ', 
                                         input$yr_cat.vals, '":'))
        
        yr_sys.names[order(yr_sys.names[, "Year Established"]), ]
        
    })
    
    output$yr_sys.tbl <- renderDataTable({
        yr_sys.tbldat()
    })
    
    
    output$unks.tbl <- renderDataTable({
        unks.df
    })
    
    output$unks.text <- renderText({
        paste0('There are missing values for ', length(unks("IT_system")), 
               ' systems under the "IT System" category and ',
               length(unks("analytics")), ' systems under the "Analytics" category.')
    })
    
    
    
})