library(shiny)
library(websocket)
library(shinyjs)

# Modified from:
# https://github.com/rstudio/shiny-examples/blob/master/147-websocket/app.R

# deploy to shinyapps using the following commands:
# library(rsconnect)
# rsconnect::deployApp('<path-to-app>')

ui <- fluidPage(
  shinyjs::useShinyjs(),
  fluidRow(
    column(12, offset = 0,
           h1("WebSocket client", style = "text-align: center;"),
           tags$p(
             tags$strong("Status:"),
             textOutput("status", inline = TRUE)
           ),
           wellPanel(
             textInput("input", "Message to send:"),
             actionButton("send", "Send message"),
             actionButton("highlight", "Highlight sample sites AX2 & BN2"),
             actionButton("clear", "Clear highlighted sample sites"),
             actionButton("close", "Close"),
             actionButton("reconnect", "Connect")
           ),
           
           tags$strong("Messages received:"),
           tableOutput("output")
           
    )
  )
)

server <- function(input, output, session) {
  
  status <- reactiveVal("Waiting for input")
  history <- reactiveVal(
    data.frame(Date = NULL, Message = NULL)
  )
  
  setEnabled <- function(enable) {
    withReactiveDomain(session, {
      shinyjs::toggleState("input", enable)
      shinyjs::toggleState("send", enable)
      shinyjs::toggleState("highlight", enable)
      shinyjs::toggleState("clear", enable)
      shinyjs::toggleState("close", enable)
      shinyjs::toggleState("reconnect", !enable)
    })
  }
  setEnabled(FALSE)
  
  websocketUrl <- "<websocket-server-url>"
  sessionId <- "1"
  
  connect <- function(url) {
    ws <- WebSocket$new(websocketUrl)
    status(paste0("Connecting to ", url, ", please wait..."))
    
    ws$onError(function(event) {
      print(event$data)
      setEnabled(FALSE)
      status(paste0("Error: ", event$message))
    })
    
    ws$onMessage(function(event) {
      old <- isolate(history())
      new <- data.frame(
        Date = format(Sys.time()),
        Message = event$data,
        stringsAsFactors = FALSE)
      history(rbind(new, old))
    })
    
    ws$onOpen(function(event) {
      setEnabled(TRUE)
      status(paste0("Connected to ", isolate(input$url)))
      message <- paste0('{
        "action": "connect",
        "session_id": "', sessionId, '"
      }')
      print(message)
      ws$send(message)
    })
    
    ws$onClose(function(event) {
      print(event$data)
      setEnabled(FALSE)
      status(paste0("Closed connection: ", event$code, " - ", event$reason))
    })
    ws
  }
  
  ws <- NULL
  
  showModal(
    modalDialog(
      textInput("url", "WebSocket URL", websocketUrl),
      footer = actionButton("connect", "OK"),
      easyClose = FALSE,
      size = "s"
    )
  )
  
  observeEvent(input$connect, {
    removeModal()
    ws <<- connect(input$url)
  })
  
  observeEvent(input$reconnect, {
    ws <<- connect(input$url)
  })
  
  observeEvent(input$send, {
    msg <- input$input
    print(msg)
    
    formattedMsg <- paste0('{"action": "send_data", "session_id": "', sessionId,'", "data": "', msg, '"}')
    print(formattedMsg)
    
    ws$send(formattedMsg)
    updateTextInput(session, "input", value = "")
  })
  
  # An example of sending a list of sample sites, in this case sites AX2 and BN2.
  observeEvent(input$highlight, {
    sample_sites <- list("AX2", "BN2")
    
    # Send message as a JSON-formatted string.
    formattedMsg <- paste0('{"action": "highlight", "session_id": "', sessionId,'", "sample_sites": ["', paste( unlist(sample_sites), collapse='","'), '"]}')
    print(formattedMsg)
    
    ws$send(formattedMsg)
    updateTextInput(session, "input", value = "")
  })
  
  # Clear any highlighted sample sites in the model.
  observeEvent(input$clear, {
    
    # Send message as a JSON-formatted string. 
    formattedMsg <- paste0('{"action": "highlight", "session_id": "', sessionId,'", "sample_sites": []}')
    print(formattedMsg)
    
    ws$send(formattedMsg)
  })
  
  observeEvent(input$close, {
    ws$close()
  })
  
  output$output <- renderTable(width = "100%", {
    history()
  })
  
  output$status <- renderText({
    status()
  })
  
}

shinyApp(ui, server)
