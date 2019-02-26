library(shiny)

# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("movement of vessel"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#        sliderInput("bins",
#                    "Number of bins:",
#                    min = 1,
#                    max = 50,
#                    value = 30)
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#        plotOutput("distPlot")
#     )
#   )
# ))

shinyUI(
  fluidPage(
    tags$video(id="videos", type = "videos/mp4",src = "SampleVideo_1280x720_20mb.mp4", controls = "controls")
  )
)