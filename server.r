library(shiny)
library(RMySQL)

#Set globals for database values
Host <- "absciex.clkcnndwb0cc.ap-southeast-1.rds.amazonaws.com"
Port <- 3306
Username <- "sciexadmin"
Password <- "5prime-3prime"
DBName <- "ABSciex"

#Set globals for login settings
CurSupplier <- NULL

#Function for refreshing data
CheckLoginCredentials <- function(LoginUsername, LoginPassword) {
  DBCon <- dbConnect(MySQL(), username = Username, password = Password, host = Host, port = Port, dbname = DBName)
  LoginRow <<- dbGetQuery(DBCon, paste("SELECT * FROM Suppliers WHERE Email='", LoginUsername, "' AND Password='", LoginPassword, "'", sep=""))
  dbDisconnect(DBCon)
  if(nrow(LoginRow) != 1) FALSE
  else LoginRow$Supplier_Name
}

shinyServer(function(input,output){
  CurSupplier <- NULL
  if(is.null(CurSupplier)){
    output$Display <- renderUI({
        list(
          fluidRow(
            column(12,
              h1("ABSciex"),
              wellPanel(
                h2("Login"),
                textInput("Login_Username", label="Email:", value=""),
                passwordInput("Login_Password", label="Password:", value=""),
                actionButton("Login", label="Login"),
                textOutput("Login_Message")
              )
            )
          ),
          fluidRow(
            column(8,
              wellPanel(
                h2("Register"),
                textInput("Register_Username", label="Email:", value=""),
                textInput("Register_Supplier_Name", label="Supplier Name:", value=""),
                passwordInput("Register_Password", label="Password:", value=""),
                passwordInput("Register_Password_Confirm", label="Confirm Password:", value=""),
                actionButton("Register", label="Register"),
                textOutput("Register_Message")
              )
            ),
            column(4, 
              wellPanel(
                h2("Reset Password"),
                textInput("Reset_Email", label="Email:", value=""),
                actionButton("Reset", label="Reset"),
                textOutput("Reset_Message")
              )
            )
        )
        )
    }
    )
    observeEvent(input$Login, {
      LoginResult <- CheckLoginCredentials(input$Login_Username, input$Login_Password)
      if(LoginResult == FALSE) output$Login_Message <- renderText({"Email and password combination not found"})
      else {
        CurSupplier <- LoginResult
        output$Display <- renderUI({
          list(
            fluidRow(12,
              h1(paste("Welcome ", CurSupplier, sep=""))
            ),
            fluidRow(12,
              wellPanel("Explanation")
            ),
            
          )
        })
      }
    })
    observeEvent(input$Register, {
      email_pat <- "^([a-z0-9_\\.-]+)@([\\da-z\\.-]+)\\.([a-z\\.]{2,6})$"
      Errmsg <- NULL
      if(nchar(input$Register_Username) == 0) Errmsg <- paste(Errmsg, "Email cannot be left blank,\t", sep="")
      if(!grepl(pattern = email_pat, x = input$Register_Username)) Errmsg <- paste(Errmsg, "Invalid email provided,\t", sep="")
      if(nchar(input$Register_Supplier_Name) == 0) Errmsg <- paste(Errmsg, "Supplier name cannot be left blank,\t", sep="")
      if(nchar(input$Register_Password) < 8) Errmsg <- paste(Errmsg, "Password must be at least 8 characters long,\t", sep="")
      if(!setequal(input$Register_Password, input$Register_Password_Confirm)) Errmsg <- paste(Errmsg, "Passwords do not match,\t", sep="")
      if(is.null(Errmsg)) {
        DBCon <- dbConnect(MySQL(), username = Username, password = Password, host = Host, port = Port, dbname = DBName)
        TestRow <- dbGetQuery(DBCon, paste("SELECT * FROM Suppliers WHERE Email='", input$Register_Username, "' OR Supplier_Name='", input$Register_Supplier_Name, "'", sep=""))
        if(nrow(TestRow) != 0) {
          if(setequal(TestRow$Email, input$Register_Username)) Errmsg <- paste(Errmsg, "Email already in use,\t", sep="")
          if(setequal(TestRow$Email, input$Register_Supplier_Name)) Errmsg <- paste(Errmsg, "Supplier Name already in use,\t", sep="")
        }
        else {
          dbSendQuery(DBCon, paste("INSERT INTO Suppliers (Email, Password, Supplier_Name) VALUES('", input$Register_Username, "','", input$Register_Password, "','", input$Register_Supplier_Name, "')", sep=""))
          Errmsg <- "Account successfully created, you can now proceed to login"
        }
      }
      output$Register_Message <- renderText(Errmsg)
    })
  }
})