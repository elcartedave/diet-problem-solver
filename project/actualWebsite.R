#Dave Andrie A. Elcarte

#Implementation of the actual website
#UI and Server are found here

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(ggplot2)
source("actualsimplex.r")
source("quadraticSpline.r")
source("ElcarteEx06.r")
main=function(data){ #function that returns the returned data of the simplexMethod as a 1 row matrix
  arg=simplexSet_up(data)
  arg1=simplex_Convert(arg)
  arg2=simplexMethod(arg1)
  
    matrix=matrix(arg2$data[nrow(arg2$data),],nrow=1, dimnames=list(1,colnames(arg2$data)))
    return(matrix)
}

bool=function(data){ #boolean function that returns 1 if the last row of the iteration has negative value in it, otherwise, 0
  arg=simplexSet_up(data)
  arg1=simplex_Convert(arg)
  arg2=simplexMethod(arg1)
  matrix=matrix(arg2$tableaus[nrow(arg2$tableaus),],nrow=1, dimnames=list(1,colnames(arg2$tableaus)))
  for(i in 2:ncol(matrix)){
    if(matrix[1,i]<0){
      return(1)
    }
  }
  return(0)
}
mainn=function(data){  
  arg=simplexSet_up(data)
  arg1=simplex_Convert(arg)
  arg2=simplexMethod(arg1)
  return(arg2$data) #function that returns the entire data returned by the simplexMethod
}
mainn1=function(data){  #function that returns the entire iterations returned by the simplexMethod
  arg=simplexSet_up(data)
  arg1=simplex_Convert(arg)
  arg2=simplexMethod(arg1)
  return(arg2$tableaus)
}
main2=function(data){  #function that returns the constraints of the simplexSet_up function
  args=simplexSet_up(data)
  return(args$constraints)
}
main3=function(data){ #function that returns the objectiveFXN of the simplexSet_up function
  args=simplexSet_up(data)
  return(args$objectiveFXN)
}
main4=function(data){ #function that returns the augmented Matrix of the simplexSet_up function
  args=simplexSet_up(data)
  return(args$augCoeffMatrix)
}
main5=function(data){ #function that returns the initial tableau before proceeding to the actual simplex method
  arg=simplexSet_up(data)
  arg1=simplex_Convert(arg)
  return(arg1)
}




ui <- dashboardPage(title="CMSC 150 Project", #dashboard main title
        skin="blue",
        dashboardHeader(title="CMSC 150 MENU"),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Quadratic Spline", tabName = "quadraticSpline", icon=icon("chart-line")),         #dashboard contents
            menuItem("Polynomial Regression", tabName="polynomialRegression", icon=icon("subscript")),
            menuItem("Diet Problem Solver", tabName = "dietProb", icon=icon("pizza-slice")),
            menuItem("User's Manual", tabName="manual", icon=icon("circle-exclamation"), selected=TRUE)
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(
              tabName = "dietProb",
              h1(div("Welcome to Diet Problem Solver!", style = "text-align: center;")),     #diet problem solver layout and interface
              
              sidebarLayout(
                sidebarPanel(
                    textInput("name", "Enter your name", ""),
                    numericInput("age", "Enter your age", ""),
                    radioButtons("gender", "Select the gender", list("Male", "Female"), ""),
                    selectizeInput("statenames", "Select foods", choices = nutritionalValues[,1], multiple = TRUE),
                    checkboxInput("all", "Select All", value = FALSE),
                    checkboxInput("none", "Select None", value = FALSE),
                    fluidRow(style="text-align: center;",
                      actionButton('submitBtn', "Submit!", icon=icon("apple-whole")), br(), br(),
                      downloadButton('downloadData', 'Download Foods Table', style = "max-width: 100%;")
                    )
                    
                ),
                mainPanel(
                  fluidRow(style="margin-bottom: -200px;",
                    box(title="PERSONAL INFORMATION", status="warning", solidHeader = T,
                        HTML("<b>Name:</b>"), textOutput("myName", inline = TRUE), br(),
                        HTML("<b>Age:</b>"), textOutput("myAge", inline = TRUE), br(),
                        HTML("<b>Gender:</b>"), textOutput("myGender", inline = TRUE), br(),
                        HTML("<b>Food Choices:</b>"), textOutput("state", inline = TRUE)
                    ),
                    infoBoxOutput("infoBox", width=6),
                    uiOutput("finalValueBox")
                  ),
                 
                  
                  tabsetPanel(type="tab", 
                              tabBox(width=13, 
                                     tabPanel("Foods",
                                              DTOutput("allFoods"),
                                              style = "overflow-x: auto;"
                                              ),
                                     tabPanel("Food Choices",
                                              dataTableOutput("data"),
                                              style = "overflow-x: auto;"
                                     ),
                                     tabPanel("Initial Set Up",
                                              fluidRow(
                                                column(6,
          
                                                       tableOutput("objectiveFunction"),
                                                       tableOutput("initialSetUp")
                                                       
                                                ),
                                                column(6,
                                                       uiOutput("title", style = "margin-top: 7px; margin-right: 15px;"),
                                                       fluidRow(
                                                         tableOutput("augcoeff"),
                                                         style = "overflow-x: auto; margin-right: 10px;"
                                                       )
                                                )
                                              )
                                     ),
                                     tabPanel("Initial Tableau",
                                              uiOutput("title1", style = "margin-top: 7px; margin-left: 15px;"),
                                              fluidRow(
                                                style = "overflow-x: auto; margin-right:10px; margin-left:10px;",
                                                tableOutput("transposed")
                                              ),
                                              
                                     ),
                                     tabPanel("Tableau Per Iteration",
                                              uiOutput("titleTableau", style = "margin-left: 15px;"),
                                              fluidRow(
                                                uiOutput("tableaus", inline=T),style="overflow-x: auto; margin-left:10px; margin-right:10px;"
                                              ),
                                              uiOutput("titleBasicSol", style="margin-top:10px; margin-left:15px;"),
                                              fluidRow(
                                                style = "overflow-x: auto; margin-right:10px; margin-left:10px;",
                                                uiOutput("basicSol")
                                              ),
                                              
                                              
                                             
                                       
                                     ),
                                     tabPanel(HTML("<b>ANSWER</b>"),
                                              fluidRow(style="margin-left: -10px;  margin-right:10px;",
                                                       column(6, 
                                                              uiOutput("finalAnswerTitle"),
                                                              uiOutput("data2"),
                                                       ),
                                                       column(6,
                                                              uiOutput("finalAnswerTitle2"),
                                                              uiOutput("data11")
                                                       ),
                                                       uiOutput("finalAnswerTitle3",style="margin-left:15px;"),
                                                       fluidRow(style="overflow-x: auto; margin-left:10px;",
                                                                uiOutput("data12")
                                                       )
                                              ),
                                              value = "default" 
                                     ),
                                     selected = "default"  # Set default selected tab
                              )
                              
                  ) 
                )
                              
                  ) 
                ),
            tabItem(tabName = "quadraticSpline",   #quadratic spline interpolation layout and interface
                    
                    fluidRow(
                      h1(div("Quadratic Spline Interpolation", style = "text-align: center;")),
                      box(title="Upload File", status="primary", solidHeader = T, width=12, height=160,
                          fileInput("file", "Upload your csv file"),
                          helpText("Default max. file size is 5MB", style = "margin-top: -23px;")),
                      ),
                    
                    fluidRow(
                      box(title="Data and Functions per Intervals", status="primary", solidHeader = T, width=6,
                          fluidRow( style="margin-left: 2px;",
                                    column(4,
                                           uiOutput("tb")
                                    ),
                                    column(8,
                                           fluidRow(style="margin-right: 50px;",
                                                    tableOutput("quadSpline")
                                           )
                                    )
                          )
                      ),
                      box(title="Enter Estimate", status="primary", solidHeader = T, width=6,
                          numericInput("estimate", "Please enter your estimate", value=NA),
                          sliderInput("slideEstimate",NULL,1,100,20,step=0.5),
                          fluidRow(style="margin-left: 47%;",
                                   actionButton('estimateBtn',"Enter!")
                          )
                      ),
                    ),
                      
                      fluidRow(
                                
                                 box(title="Correct f(x) and estimate", status="warning", solidHeader=T, width=12,
                                     fluidRow( style="width: 100%; margin-left:0px;",
                                               uiOutput("correctFXTitle"),
                                               verbatimTextOutput("correctEstimate"), 
                                               uiOutput("correctEstimateTitle"),
                                               verbatimTextOutput("finalEst")
                                     )
                                 )
                               
                               
                      ),
                      fluidRow(
                        box(title="Set-Up", status="primary", width=12,
                            fluidRow(style="width:100%; margin-left:0px;",
                                     uiOutput("quadTable1")
                            )
                        )
                      )
                    
                    
                    
                    
            ),
            tabItem(tabName="polynomialRegression",  #polynomial regression layout and interface
                    fluidPage(
                      h1(div("Polynomial Regression", style = "text-align: center;")),
                      column(6,
                             fluidRow(
                               box(title="Upload File", status="primary", solidHeader = T, width=12,
                                   fileInput("polyFile", "Upload your csv file"),
                                   helpText("Default max. file size is 5MB", style = "margin-top: -23px;")),
                             ),
                             fluidRow(
                               box(title="Enter Degree", status="primary", solidHeader = T, width=12,
                                   sliderInput("degreePoly", "Please enter degree",min=1, max=2, value=1, step=1),
                                   fluidRow(style="margin-left: 47%;",
                                            actionButton('estimatePoly',"Enter!")
                                   )
                                   
                               )
                             ),
                             fluidRow(
                               box(title="Enter Estimate", status="primary", solidHeader = T, width=12,
                                   numericInput("estimateInput", "Please enter your estimate", value=NA),
                                   fluidRow(style="margin-left: 47%;",
                                            actionButton("estimateBtnPoly", "Enter!")
                                   )
                               ),
                               
                             )
                      ),
                      column(6,
                             box(title="Data and Correct Function", width=12, status="warning",solidHeader=T,
                                 column(4,uiOutput("polyReg")),
                                 column(8,
                                        uiOutput("polyTitle"),
                                        textOutput("polyRegSol"),
                                        br(),
                                        uiOutput("finalEstTitle"),
                                        verbatimTextOutput("finalEstPoly")
                                        )
                                       
                             ),
                             box(title="Graph",width=12, status="primary", solidHeader=T,style="margin-left: 10px;",
                                   uiOutput("plot")
                             )
                      )
                    )
                    ),
            tabItem(tabName="manual",  #user's manual layout and interface
                    fluidPage(
                      box(width=12, status="primary",
                        h1(div(HTML("<b>WELCOME TO MY CMSC 150 PROJECT!</b>"), style="text-align: center;"),
                           column(12,
                                  br(),
                                  tags$p("This website contains all the functionalities needed such as the Quadratic Spline Interpolation, Polynomial
                               Regression, and the Diet Problem Solver. Feel free to use it as you like and make sure to read the user's manual
                               provided below in order to maximize your experience! ENJOY!", style="font-size: 18px;"), br(),
                                  tags$p(div(HTML("Submitted by: <b>Dave Andrie A. Elcarte</b>"), style="font-size: 18px; margin-top: -25px;")),
                                  tags$p("Feel free to reach out if you have any concerns!", style="font-size: 18px;")
                           )
                           
                        )
                      )
                      ,
                      box(width=12,
                          h2(div("User's Manual", style="text-align: center; margin-top: -20px;")),
                          verbatimTextOutput("manualText")
                      )
                    )
                    
                     
                    )
            
              )
          )
        )
            
  
  
  
  
  
  

server <- function(input, output, session) {
  output$manualText<-renderText({  #rendering of the user manual
    "On this interface, you can hover to the right side and click on the functionality you want to try (e.g. Quadratic Spline, etc.)

On Quadratic Spline interface:
     1. Before starting, make sure to upload a VALID CSV FILE first by clicking the 'Browse' button in the 'Upload File' box and 
        selecting the CSV file you want to upload.
     2. The 'Data and Functions per Intervals, and 'Set-Up' table will automatically show right after uploading the CSV file.
     3. To see the correct f(x) and estimate, you have to enter your estimate first (located in the 'Enter Estimate' box). You 
        can do so by typing in the input box or sliding through the numbers provided in the slider. After choosing your estimate, click
        'Enter!'
     4. Scroll down, and you can now see the correct function and estimate for your given value.

On Polynomial Regression Interface:
     1. Make sure to first upload a VALID CSV FILE (with no header row) in the 'Upload File' box.
     2. Upon uploading, the table inside the 'Data and Correct Function' will load automatically.
     3. In the 'Enter Degree' box, choose the degree from the slider input that you want to be shown, then click 'Enter'.
     4. The correct function should now display.
     5. If the 'Estimate' is NA, it is because you didn't enter an estimate in the 'Enter Estimate portion. Please enter an estimate
        first, then click 'Enter'. The correct estimate and its corresponding graph should now display.

On the Diet Problem Solver:
     1. (Optional) Before proceeding, you may enter your name, age, and gender.
     2. You can select foods by clicking on the white box in the 'Select Foods' portion. You can choose any of the foods you like. You
        can also tick the 'Select All' checkbox if you want to select all or tick the 'Select None' checkbox if you want to remove the
        foods you've currently selected. You can also remove your foods chosen by manually clicking the 'backspace' button on your
        keyboard.
     3. After selecting your foods. You can now click the 'Submit' button below.
     4. The 'Download Foods Table' button is an option if you want to download all the nutritional values of all the foods into a CSV 
        file.
     5. 'Personal Information' box contains all your inputs including all the foods you've chosen.
     6. The info box on the right side of the 'Personal Information' box, provides a brief summary of your choices (whether the diet
        is infeasible or not). It also displays a value box that will show the total cost of the diet, only if the diet is feasible.
     7. You can also hover through the tabs below by clicking the tab's name.
     8. Each tab contains different functionalities, tables, and the final solutions depending on the nutritional values and 
        requirements by your chosen diet. Feel free to go through them!
        
     Tabs and their functions:
        'Foods'- shows all the foods available and all their nutritional data.
        'Food Choices' - shows foods the user has selected and their corresponding nutritional data.
        'Initial Set-Up' - shows the objective function, nutritional requirements' constraints, and its corresponding augmented matrix.
        'Initial Tableau' - shows the initial tableau which will be invoked in the simplex method.
        'Tableaus per Iteration' - shows all the iterations done (only one or two iterations are displayed. Next iterations can be 
                                   hovered by clicking 'Next' into the data table) and their basic solutions as well.
        'Answer' - shows the final cost of the optimized menu (if applicable) and also shows the recommended servings of foods and 
                   their respective cost. It also shows the final basic solution for the last iteration of the simplex method."
     
    
    })
  
  
  observe({  #selects all choices when the "Select all" is checked
    updateSelectInput(
      session, "statenames", 
      choices = as.vector(nutritionalValues[, 1]),
      selected = if(input$all) as.vector(nutritionalValues[, 1])
    )
  })
  
  observe({ # dynamically updates the value of the slider input
    newMax=nrow(csvPoly())-1
    updateSliderInput(session,"degreePoly", max=newMax)
  })
  
  observe({ #resets choices when the "Select none" is checked
      updateSelectInput(
        session, "statenames",
        choices = as.vector(nutritionalValues[, 1]),
        selected = if(input$none) NA
      )
  })
  
  observe({
    if(length(selected_data1())<64 && length(selected_data1())>0){
      updateCheckboxInput(
        session, "all",
        value=FALSE
      )
      updateCheckboxInput(
        session, "none",
        value=FALSE
      )
    }

  })
  
  output$line_plot <- renderPlot({  #renders the graph of the polynomial regression function
    if(input$estimateBtnPoly==0) return()
    input$estimateBtnPoly
    input$estimatePoly
    isolate({
      fx<-polyRegReactive()
      fx <- gsub("^\\w+\\s*", "",fx)
      fx <- gsub("^\\([^)]+\\)", "", fx)
      x <- seq(input$estimateInput-3,(input$estimateInput+3), by=0.1)
      y <-eval(parse(text=fx))
      df <- data.frame(x = x, y = y)
      ggplot() +
        geom_line(aes(x,y),color = "blue") +
        labs(x = "X", y = "Y", title = "Polynomial Regression")
    })
  })
  
  output$plot<-renderUI({  #renders empty when the estimated number is NULL
    input$estimateBtnPoly
    isolate({
      if(input$estimateBtnPoly==0 || is.na(input$estimateInput)){
        return(tags$b("Data Empty"))
      } 
      plotOutput("line_plot")
    })
    
  })
  
  selected_data1=reactive({ #returns the nutritional values with only those who were selected by the user
    selectedFoods=input$statenames
    nutritionalValues[nutritionalValues[,1]%in% selectedFoods]
  })
  
  
  observe({ #dynamically updtes the slider and numeric input depending on the input by the user
    newMin=csv()[1,1]
    newMax=csv()[nrow(csv()),1]
    updateSliderInput(session, "slideEstimate", max=newMax, min=newMin) 
    updateNumericInput(session, "estimate", value=input$slideEstimate)
  })
  
  
  
  csv<-reactive({ #reads the csv file received in the quadratic spline interpolation
    file1=input$file
    if(is.null(file1)) return()
    fileee=read.table(file=file1$datapath, sep=',')
    colnames(fileee)=c("x","y")
    return(fileee)
  })
  
  
  csvPoly<-reactive({  #reads the csv file received in the polynomial regression
    file1=input$polyFile
    if(is.null(file1)) return()
    fileee=read.table(file=file1$datapath, sep=',')
    colnames(fileee)=c("x","y")
    return(fileee)
  })
  
  selected_data=eventReactive(input$submitBtn,{ #only runs when the submitBtn has been clicked by the user
    selectedFoods=input$statenames
    nutritionalValues[nutritionalValues[,1]%in% selectedFoods]
  })
  
  mat=reactive({ #created matrix that will return the selected data and their corresponding nutritional values
    matrix(as.vector(selected_data()),ncol=ncol(nutritionalValues), byrow=F,
           dimnames=list(NULL,c("Foods", "Price per serving","Calories", "Cholesterol", "Total Fat", "Sodium", "Carbohydrates", "Dietary Fiber",
                                "Protein", "Vitamin A", "Vitamin C", "Calcium", "Iron")))
  })
  
  try1=reactive({ #creates the table wi the foods needed in the diet and their corresponding servings and prices
    if(length(selected_data())!=0){
      matrix=matrix(main(mat()),nrow=1,dimnames=list(NULL,colnames(main(mat()))))
      last_index=ncol(matrix)-1
      xis=length(selected_data())/13
      theory=22+xis+1
      i=1
      count=0
      for(j in theory:last_index){
        if(round(matrix[1,j],2)!=0){
          count=count+1
        }
      }
      newMatrix=matrix(NA, ncol=3, nrow=count, dimnames=list(NULL,c("Food","Servings","Cost($)")))
      j=1
      while(i<=xis){
        if(round(matrix[1,theory],2)!=0){
          newMatrix[j,1]=mat()[i,1]
          newMatrix[j,2]=round(matrix[1,theory],2)
          newMatrix[j,3]=round((matrix[1,theory]*as.numeric(mat()[i,2])),2)
          j=j+1
        }
        theory=theory+1
        i=i+1
      }
    }
    return(newMatrix)
  })

  
  quadSplineReactive<-reactive({ #gets the function returned by quadratic spline and returns it as a matrix with proper column names
    if(is.null(csv())||ncol(csv())!=2) return()
    
    return(matrix(quadraticSpline(csv())$string,byrow=F, ncol=1, dimnames=list(NULL,"Functions per Interval")))
  })  
  
  quadTableReactive<-reactive({ #returns the matrix returned by the quadratic spline
    if(is.null(csv())||ncol(csv())!=2){
      return()
    } 
    
    return(quadraticSpline(csv())$matrix)
  })  
  
  quadSplineReactive1<-reactive({ #binds the two columns and returns the resulting table
    quad_spline1<-cbind(quadSplineReactive(),intervals())
    if(is.null(quad_spline1)) return()
    return(quad_spline1)
  })
  
  
  polyRegReactive<-reactive({ #returns the expression returned by the polynomial regression
    if(is.null(csvPoly())||ncol(csvPoly())!=2) return()
    a=csvPoly()[,1]
    b=csvPoly()[,2]
    pol=PolynomialRegression(input$degreePoly,list(a,b))
    return(pol$polynomial_string)
  })
  
  output$csvTablePoly<-renderTable({ #returns the table for the csv file input in the polynomial regression
    if(is.null(csvPoly())) return()
    csvPoly()
  })
  
  output$polyReg<-renderUI({ #prompts when the csv table is empty, returns otherwise
    if(!is.null(csvPoly())){
      if(ncol(csvPoly())!=2){
        state="File invalid!"
        return(tags$b(state))
      }
      tableOutput("csvTablePoly")
    } 
    else state=tags$b("File Empty!")
  })
  
  
  output$polyRegSol<-renderText({ #returns polyRegReactive if and only if the estimatePoly and estimateBtnPoly are pressed
    input$estimatePoly
    input$estimateBtnPoly
    isolate({
      if(is.null(polyRegReactive())) return()
      return(polyRegReactive())
    })
    
  })
  
  
  
  output$polyTitle<-renderUI({ #returns the title if the estimatePoly button is clicked
    input$estimatePoly
    isolate({
      if(is.null(polyRegReactive())) return()
      return(tags$b("The correct function is:"))
    })
   
  })
  
  output$finalEstPoly<-renderText({ #returns the final estimate for the polynomial regression if estimatePoly and estimateBtnPoly are pressed
    input$estimateBtnPoly
    input$estimatePoly
    isolate({
      if(!is.null(polyRegReactive())){
        if(is.na(input$estimateBtnPoly) || is.na(input$estimatePoly)) return()
        fx=eval(parse(text=polyRegReactive()))
        return(fx(input$estimateInput))
      }
    })
  })
  
  output$quadTable<-renderTable({ 
    return(quadTableReactive())
  })
  
  output$quadTable1<-renderUI({  #prompts when the quadratic spline table is empty, returns otherwise
      if(!is.null(quadTableReactive())){
        tableOutput("quadTable")
      }else return(tags$b("Please enter a valid CSV File first!!"))
  })
  
  output$finalEstTitle<-renderUI({ #returns the label/title if the corresponding buttons are clicked
    input$estimateBtnPoly
    input$estimatePoly
    isolate({
      if(!is.null(polyRegReactive())){
        if(is.na(input$estimateBtnPoly) || is.na(input$estimatePoly)) return()
        return(tags$b("Estimate"))
      }
    })
  })
  
  
  output$tableaus<-renderUI({ #returns the iterations tables
    if(input$submitBtn==0 || length(selected_data())==0) return(tags$b("Please enter foods first!"))
    else dataTableOutput("tableaus1")
  })
  
  output$tableaus1<-renderDataTable({ #creates a datatable for iterations
    index=nrow(mainn(mat()))+1
    index1=nrow(mainn1(mat()))/index
    return(datatable(mainn1(mat()),options=list(lengthMenu=c(index1,(index1+index1)),pageLength=nrow(mainn1(mat()))/index,searching=F,dom='lrtip')))
  },options=NULL, escape=F)
  
  
  output$titleTableau<-renderUI({ 
    if(length(selected_data())!=0){
      return(tags$b("Tableau for Each Iterations"))
    }
  })
  theory1<-reactive({
    count=0
    for(i in 1:nrow(mainn1(mat()))){
      if(mainn1(mat())[i,1]!=""){
        count=count+1
      }
    }
    return(count)
  })
  output$allFoods<-renderDT({
    return(datatable(nutritionalValues,options=list(searching=F),rownames=F))
  })
  
  output$correctEstimate<-renderText({
      input$estimateBtn
      if(input$estimateBtn==0) return("Please enter a valid CSV File first!")
      isolate({
        if(!is.null(quadSplineReactive1())){
          if(is.na(input$estimate)) return("Please enter a valid input")
          est=input$estimate
          for(i in 1:nrow(quadSplineReactive1())){
            if(csv()[i,1] <= est && csv()[i+1,1] >= est){
              return(quadSplineReactive1()[i,1])
            }
          }
          return("The interval doesnt bound the root")
        }else return("Please enter a valid CSV File first!")
      })
  })
  
  output$finalEst<-renderText({
    input$estimateBtn
    isolate({
      if(!is.null(quadSplineReactive1())){
        if(is.na(input$estimate)) return()
        est=input$estimate
        for(i in 1:nrow(quadSplineReactive1())){
          if(csv()[i,1] <= est && csv()[i+1,1] >= est){
            fx=eval(parse(text=quadSplineReactive1()[i,1]))
            return(fx(est))
          }
        }
      }
    })
  })

  output$correctFXTitle<-renderUI({
    input$estimateBtn
    isolate({
      if(!is.null(quadSplineReactive1())){
        if(!is.na(input$estimate)){
          est=input$estimate
          for(i in 1:nrow(quadSplineReactive1())){
            if(csv()[i,1] <= est && csv()[i+1,1] >= est){
              return(tags$b("CORRECT FUNCTION"))
            }
          }
        }
      }
    })
  })  
  
  output$correctEstimateTitle<-renderUI({
    input$estimateBtn
    isolate({
      if(!is.null(quadSplineReactive1())){
        if(!is.na(input$estimate)){
          est=input$estimate
          for(i in 1:nrow(quadSplineReactive1())){
            if(csv()[i,1] <= est && csv()[i+1,1] >= est){
              return(tags$b("ESTIMATE"))
            }
          }
        }
      }
    })
  })
  
  interval<-reactive({
    if(is.null(csv())) return()
    if(ncol(csv())==2){
      intervals=c()
      for(i in 1:(nrow(csv())-1)){
        intervals[i]=paste(csv()[i,1],"  ≤  ",csv()[i+1,1])
      }
      return(intervals)
    }
  })
  
  intervals<-reactive({
    if(is.null(csv())) return()
    if(ncol(csv())==2){
      return(matrix(interval(),ncol=1,dimnames=list(NULL,"Interval")))
    }
  })
  
    output$valueBox<-renderValueBox({
      if(length(selected_data())!=0){
        matrix=matrix(main(mat()),nrow=1,dimnames=list(NULL,colnames(main(mat()))))
        if(bool(mat())==0){
          cost=round(matrix[1,ncol(matrix)],2)
          return(valueBox(paste("$",cost,sep=""),"Optimal Cost", icon=icon("fish")))
        }
      }
    })
    
    output$finalValueBox<-renderUI({
      if(length(selected_data())==0) return()
      matrix=matrix(main(mat()),nrow=1,dimnames=list(NULL,colnames(main(mat()))))
      if(bool(mat())==1) return()
      valueBoxOutput("valueBox",width=6)
    })
    
    output$infoBox<-renderInfoBox({
      input$submitBtn
      if(input$submitBtn==0) return(infoBox(paste("Welcome!",sep=""), "Choose your foods to begin", icon=icon("face-smile")))
      else{if(length(selected_data())!=0){
        matrix=matrix(main(mat()),nrow=1,dimnames=list(NULL,colnames(main(mat()))))
        if(bool(mat())==0){
          return(isolate(infoBox(paste("Congrats ",input$name,"!",sep=""), "Your diet is now optimized!", icon=icon("bowl-food"))))
        }
        return(isolate(infoBox("Oops", "Diet is infeasible!", icon=icon("face-sad-tear"))))
      }
      return(isolate(infoBox(paste("Welcome ",input$name[1],"!",sep=""), "Choose your foods to begin", icon=icon("face-smile"))))}
    })
    output$data1<-renderTable({
      if(length(selected_data())!=0){
        matrix=matrix(main(mat()),nrow=1,dimnames=list(NULL,colnames(main(mat()))))
        if(ncol(matrix)!=1){
          try1()
        }
      }
    })
    output$data11<-renderUI({
      if(length(selected_data())!=0 && bool(mat())==0){
        tableOutput("data1")
      }
    })
    
    output$data12<-renderUI({
      if(length(selected_data())!=0 && bool(mat())==0){
        tableOutput("tableData")
      }
    })
    
    output$tableData<-renderTable({
      if(length(selected_data())!=0){
        matrix=matrix(main(mat()),nrow=1,dimnames=list(NULL,colnames(main(mat()))))
      }
    })
    
    output$finalAnswerTitle<-renderUI({
      if(length(selected_data())!=0){
        # if(ncol(matrix)!=1){
        tags$b("The Optimized Menu")
        # }
      }
      
    })
    output$finalAnswerTitle2<-renderUI({
      if(length(selected_data())!=0){
        # if(ncol(matrix)!=1){
        if(bool(mat())==1) return()
        tags$b("The Solution and Cost Breakdown by Food")
        # }
      }  
    })
    
    output$finalAnswerTitle3<-renderUI({
      if(length(selected_data())!=0){
        # if(ncol(matrix)!=1){
        if(bool(mat())==1) return()
        tags$b("Final Basic Solution")
        # }
      }
      
    })
    
    output$data2<-renderUI({
      if(input$submitBtn==0) return(tags$b("Foods Empty! Please choose foods first!"))
      if(length(selected_data())!=0){
        matrix=matrix(main(mat()),nrow=1,dimnames=list(NULL,colnames(main(mat()))))
        if(bool(mat())==0){
          cost=matrix[1,ncol(matrix)]
          statement=HTML(paste("The cost of this optimal diet is ", tags$b("$"),tags$b(round(cost,2)), sep=""))
        }else statement="There is no optimal solution for your chosen foods!"
      }else{
        state="Foods Empty! Please choose foods first!"
        return(tags$b(state))
      }
    })
    output$data<- renderDataTable({
      mat()
    },options=list(searching=F))
    output$augcoeff<-renderTable({
      if(length(selected_data())!=0){
        m=matrix(main4(mat()),ncol=ncol(main4(mat())),dimnames=list(NULL,colnames(main4(mat()))))
      }
      
    })
    output$title<-renderUI({
      if(length(selected_data())!=0){
        tags$b("Augmented Coefficient Matrix")
      }  
    })
    output$title1<-renderUI({
      if(length(selected_data())!=0){
        tags$b("Initial Tableau")
      }  
    })
    output$initialSetUp<- renderTable({
      if(length(selected_data())!=0){
        main2(mat())
      }
    })
    output$transposed<-renderTable({
      if(length(selected_data())!=0){
        main5(mat())
      }else{
        prompt=matrix("Please choose foods first!",ncol=1,dimnames=list(NULL,"Cannot show table!"))
      }
    })
    
    
    output$basicSol1<-renderTable({
      if(length(selected_data())!=0){
        mainn(mat())
      }
    },rownames=TRUE, bordered=T)
    
    output$basicSol<-renderUI({
      if(length(selected_data())!=0){
          return(tableOutput("basicSol1"))
      }
      # tags$b("Please choods foods first!")
    })
    
    output$titleBasicSol<-renderUI({
      if(length(selected_data())!=0){
        if(nrow(mainn(mat()))==1) return()
        tags$b("Basic Solutions for Each Iterations")
      }  
    })
    output$objectiveFunction<- renderTable({
      if(length(selected_data())!=0){
        mat=matrix(main3(mat()),dimnames=list(NULL,"Minimize the Cost"))
      }else prompt=matrix("Please choose foods first!",ncol=1,dimnames=list(NULL,"Cannot show table!"))
    })
    
    output$initialTableau<- renderTable({
        if(length(selected_data())!=0){
          matrix(main(mat()),nrow=1,dimnames=list(NULL,colnames(main(mat()))))
        }else prompt=matrix("Please choose foods first!",ncol=1,dimnames=list(NULL,"Cannot show solution"))
    })
    
    output$state<-renderText({
      input$submitBtn
      isolate(paste(input$statenames,","))
    })
    
    
    output$myChoice<-renderText("Food Choices:")
    output$myName <- renderText({
                      input$submitBtn
                      isolate(input$name)
                      })
    output$myAge <- renderText({
                      input$submitBtn
                      isolate({
                        if(!is.na(input$age)){
                          input$age
                        }
                        })
                    })
    output$myGender <- renderText({
                        input$submitBtn
                        isolate(input$gender)
                      })
    
    output$downloadData<-downloadHandler(
      filename = function(){
        paste("NutritionalValues","csv",sep=".")
      },
      content=function(file){
        write.table(nutritionalValues,file,sep=",",row.names=FALSE)
      }
    )
   
  
 
  
  
  output$quadSpline<-renderTable({
    quad_spline<-cbind(quadSplineReactive(),intervals())
    if(is.null(quad_spline)) return()
    return(quad_spline)
  })
  
  output$filedf<-renderTable({
    if(is.null(csv())) return()
    input$file
  })
  
  output$csvTable<-renderTable({
    if(is.null(csv())) return()
    csv()
  })
  
  output$tb<-renderUI({
    if(!is.null(csv())){
      if(ncol(csv())!=2){
        state="File invalid!"
        return(tags$b(state))
      }
      tableOutput("csvTable")
    } 
    else state=tags$b("File Empty!")
  })
  

  
}
shinyApp(ui = ui, server = server)
