# Import libraries 
library(shiny)
library(rsconnect)
library(shinythemes)

# Generate random dataset (will be deprecated in the future)
createRandomDataset <- function() {
    datasetSize <- 20 
    x <- data.frame("ID" = 1:20, "Age" = sample(1:50, datasetSize), 
                    "ER Status" = sample(c("Yes","No"), datasetSize, replace = TRUE),
                    "Subtype"= sample(c("A","B", "C"), datasetSize, replace = TRUE)
    )
    return(x)
}

# Define UI for application that draws a histogram
ui <- fluidPage( 
    theme = shinytheme("cosmo"),
    
    # Application title
    titlePanel("Complete Pathological Response Predictor"),
    
    # Navbar Panel
    navbarPage(
        "App",
        tabPanel("NavBar1",
            # Show a plot of the generated distribution
            sidebarPanel(
                tags$h5("Input:"),
                numericInput("num1", "Select your age", 0), #input1 for server
                selectInput("num2", "Select your ER Status", choices = c("Yes", "No")), #input2 for server
                selectInput("num3", "Select your Subtype", choices = c("A", "B", "C")), #input3 for server
                plotOutput("distPlot"),
                actionButton("submitbutton", "Submit", class = "btn btn-primary")
            ),
            mainPanel(
                h5("The result is:"),
                textOutput("txtout")
            )
        ),
        tabPanel("NavBar2", "This panel is blank"),
        tabPanel("NavBar3", "This panel is blank")
       
        )
 )
    

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$txtout <- renderText({
        switch(input$num3, #does something based on input from UI, and sends txtout back to the UI
               "A" = 5,
               "B" = 3,
               "C" = 4
               )
    })
}





# Main function -> Run the application 
test <- createRandomDataset() # to be replaced
shinyApp(ui = ui, server = server)

# If you want to remove functions: 
# createRandomDataset <- NULL