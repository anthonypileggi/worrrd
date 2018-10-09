library(shiny)
library(gamer)

# placeholder words
word_list <- c("insert", "your", "words", "here", "like", "this", "printing", "is", "cool", "too")
word_list <- paste(word_list, collapse = ",\n")
word_list


# UI ------------------------------------------------
ui <- fluidPage(

  titlePanel("Gamer"),

  sidebarLayout(
    sidebarPanel(
      h3("Puzzle Setup:"),
      selectInput("game", label = "Choose a game:", choices = c("wordsearch", "crossword")),
      textAreaInput("words", "Word List:", value = word_list, height = 400, resize = "both"),
      hr(),
      h3("Summary:"),
      htmlOutput("words"),
      actionButton("generate", "Generate"),
      checkboxInput("solution", "Show Solution:"),
      downloadButton("pdf", "Save (PDF)")
    ),

    mainPanel(
      plotOutput("plot")
    )
  )
)


# SERVER ----------------------------------------
server <- function(input, output, session) {

  # parse input text
  words <- reactive({
    words <- stringr::str_split(toupper(input$words), ",")[[1]]
    stringr::str_replace_all(words, " |\n", "")
  })

  # generate puzzle
  puzzle <- eventReactive(input$generate, {
    f <- eval(parse(text = input$game))
    f(words())
  })

  puzzle_plot <- reactive({
    plot(puzzle(), solution = input$solution)
  })

  # word stats
  output$words <- renderText({
    paste0("Create a ", input$game, " puzzle with ", length(words()), " words.")
  })

  # plot puzzle
  output$plot <- renderPlot(
    puzzle_plot()
  )

  # Downloadable csv of selected dataset ----
  output$pdf <- downloadHandler(
    filename = "game.pdf",
    content = function(file) {
      printable(puzzle_plot(), file)
    }
  )

}

shinyApp(ui, server)