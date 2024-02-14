



library(shiny)
library(tidyverse)
library(lpSolve)


ui <- fluidPage(
  fluidRow(
    column(3,
  
  "Input",
  
  "How many options are there?",
  numericInput("options", label = "options", 5, step = 1),
  textInput("time_x", label = "time as proportion of 1 hour"),
  textInput("var2", label = "var2"),
 
  textInput("optim_var1", label = "variable to maximise")
  ),
  column(3,
  "Constraints min or max",
  
   textInput("nr_constraints_min", label = "minimum number per option"),
  textInput("nr_constraints_max", label = "maximum number per option")),
  
  column(3,
         "Constraints time",
  
  selectInput("constraint1", label = "sum of", choices = c("time", "var2")),
  selectInput("constraint1_sign", label = "must be less than, add up to, or exceed", c("<=", "=", ">=")),
  textInput("constraint1_rhs", label = "this number")),
  column(3,
        "Constraints other" 
         
         )
  
  ),
  fluidRow(
  tableOutput("table"),
  verbatimTextOutput("optim_res"),
  verbatimTextOutput("optim_res2")
  
))

server <- function(input, output, session) {
  
  # make data frame from inputs
  # options <- reactive({unlist(str_split(input$options, pattern = ","))})
  options <- reactive({letters[1:input$options]})
  time_x <- reactive({as.numeric(unlist(str_split(input$time_x, pattern = ",")))})
  var2 <- reactive({unlist(str_split(input$var2, pattern = ","))})
  nr_constraints_min <- reactive({as.numeric(unlist(str_split(input$nr_constraints_min, 
                                                              pattern = ",")))})
  nr_constraints_max <- reactive({as.numeric(unlist(str_split(input$nr_constraints_max, 
                                                   pattern = ",")))})
  
  optim_var1 <- reactive({as.numeric(unlist(str_split(input$optim_var1, pattern = ",")))})
  
  
  df <- reactive({tibble(options = options(),
         time = time_x(),
         var2 = var2(),
         nr_constraints_min = nr_constraints_min(),
         nr_constraints_max = nr_constraints_max(),
         optim_var1 = optim_var1()) %>% 
      arrange(desc(optim_var1))
  })
  
  output$table <- renderTable({
    df()
    
    })
  
  # output$optim_res2 <- renderPrint({df()$time})
  
    obj_fn <- reactive({optim_var1()})
    
    constr_mx <- reactive({matrix(c(time_x(),nr_constraints_min(),
                                    c(1,0,0,0,0)),
                                    # nr_constraints_max()),
                        ncol = length(options()), byrow = TRUE)
      })
    
    
    constr_dir <- reactive({c(input$constraint1_sign, ">=", "<=")})
    
    constr_rhs <- reactive({c(input$constraint1_rhs, 
                              sum(nr_constraints_min()),
                              sum(nr_constraints_max()))})
    
    lp_res1 <- reactive({lp("max", obj_fn(), constr_mx(), constr_dir(), 
                           constr_rhs(), compute.sens = TRUE)$solution
      })
    
    lp_res2 <- reactive({lp("max", obj_fn(), constr_mx(), constr_dir(),
                            constr_rhs(), compute.sens = TRUE)
    })
    
    
    output$optim_res <- renderPrint({
      
      lp_res1()
      
          })
    
    output$optim_res2 <- renderPrint({lp_res2()})
    
  
  
  
  
  
}

shinyApp(ui, server)
