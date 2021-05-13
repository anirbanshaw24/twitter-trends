### MAIN SERVER ###

function(input, output, session) {
  router$server(input, output, session)
  
  w = Waiter$new(
    html = spin_heartbeat(),
    color = transparent(.75)
  )
  
  show <- reactiveVal(FALSE)

  output$number_of_tweets_output <- renderText({
    sprintf("Current Value: %s", input$number_of_tweets)
  })
  
  output$current_search_phrase <- renderText({
    sprintf("Current word/phrase: %s", input$search_phrase)
  })
  
  
  values <- reactiveValues(df_data = NULL)
  
  search_text <- reactiveVal({
    x = ''
  })
  
  search_click_tracker <- 0
  
  sentimet_plot <- 0
  
  observeEvent(input$search_click, {
    search_click_tracker <<- 1
    tmp <- input$search_phrase
    search_text(tmp)
  })
  
  data_reactive <- reactiveValues(df_data = NULL)
  
  output$overlay <- renderReact({
    if (show()) {
      Overlay(
        onClick = JS("function() { Shiny.setInputValue('toggleOverlay', Math.random()); }"),
        isDarkThemed = TRUE,
        div(
          style="background: white; width: 25vw; height: 15rem; margin: auto;",
          div(
            style = "padding: 2rem;",
            h1("No Data"),
            h5('There are no tweets for this search. Please enter a different search term or choose a different type of tweet from Popular, Mixed and Recent. '),
            p("Click anywhere to hide.")
          )
        )
      )
    }
  })
  
  observeEvent(input$toggleOverlay, show(!show()))
  
  observeEvent(input$search_click, {
    validate(
      need(nchar(search_text()) > 1, message = 'Please enter a search phrase.'),
      need(search_click_tracker == 1, message = 'Please click the button.'),
      need(input$search_phrase, 'Enter search phrase', 'Search')
    )
    w$show()
    if (search_click_tracker == 1) {
      values$df_data <- search_tweets(
        search_text(),
        n = input$number_of_tweets,
        include_rts = FALSE,
        token = token,
        type = input$type_of_tweets
      )
    }
    w$hide()
    if (length(values$df_data) == 0) {
      show(TRUE)
    }
  })
  
  output$sentiment <- renderPlot({
    validate(
      need(nchar(search_text()) > 1, message = 'Please enter a search phrase.'),
      need(search_click_tracker == 1, message = 'Please click the button.'),
      need(input$search_phrase, 'Enter search phrase'),
      need(length(values$df_data) > 0, 'No Data!')
    )
    if (search_click_tracker == 1) {
      
      rt <- values$df_data
      data <- rt[, c(1, 3:6)]
      data <- data %>%
        mutate(
          day = format(as.Date(created_at, format = "%Y-%m-%d"), format = "%d"),
          text_cleaned = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", text)
        ) %>%
        arrange(created_at) %>%
        mutate(
          linenumber = row_number(),
          date_created = as.Date(created_at,
                                 format = "%m/%d/%Y"),
          half_hr = trunc_hms(created_at, 60 * 60 * 1)
        )
      
      custom_stop_words <-
        tibble(word = c(gsub(
          "[^a-zA-Z]", "", tolower(search_text())
        )))
      
      data_tidy <- data %>%
        unnest_tokens(word, text_cleaned) %>%
        anti_join(stop_words) %>%
        anti_join(custom_stop_words)
      
      data_tidy %>%
        dplyr::count(word, sort = TRUE)
      
      data_tidy_save <<- data_tidy
      
      afinn_ungrouped <- data_tidy %>%
        inner_join(afinn_lex) %>%
        mutate(sentiment = value)
      afinn <- afinn_ungrouped %>%
        group_by(half_hr) %>%
        dplyr::summarise(
          sentiment = mean(value),
          date_created = mean(date_created),
          created_at = mean(created_at),
          half_hr = mean(half_hr),
          text = paste(text, collapse = "\n")
        ) %>%
        mutate(method = "AFINN")
      
      data_afinn_save <<- afinn
      
      data_reactive$df_data <- afinn_ungrouped
      
      sentimet_plot <<- afinn %>%
        ggplot(aes(as.character(half_hr), sentiment, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        scale_fill_gradient(
          low = "tomato",
          high = "limegreen",
          breaks = c(0),
          oob = squish
        ) +
        theme_clean() +
        theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
        labs(
          title = paste(
            'Sentiment of Tweets of',
            input$search_phrase  ,
            'over the last few days'
          ),
          x = 'Tweet Date and Time',
          y = 'Average Sentiment'
        )
      search_click_tracker <<- 0
    }
    
    sentimet_plot
  })
  
  
  
  observe({
    data_reactive_save <<- data_reactive$df_data
    validate(need(input$search_phrase == '', 'Enter search phrase', 'Search'))
    
    updateSelectizeInput(session,
                         'timestamp',
                         choices = '',
                         server = TRUE)
    updateSliderInput(session, 'sentiment_score', value = c(-5, 5))
    search_click_tracker <<- 0
  })
  
  observe({
    data_reactive_save <<- data_reactive$df_data
    validate(
      need(data_reactive$df_data, 'Please search first', 'Search'),
      need(input$search_phrase, 'Enter search phrase', 'Search')
    )
    
    updateSelectizeInput(
      session,
      'timestamp',
      choices = unique(data_reactive$df_data$date_created),
      server = TRUE,
      selected = unique(data_reactive$df_data$date_created)
    )
  })
  
  observeEvent(input$search_phrase, {
    data_reactive$df_data <- NULL
  })
  
  output$twitter_data_table <- DT::renderDataTable({
    validate(
      need(((length(values$df_data) != 0) & (length(input$search_phrase) != 0)), 'Please search first', 'Search')
    )

    data_table_display <<- data.table(data_reactive$df_data )
    DT::datatable(data_reactive$df_data,
                  options = list(
                    pageLength = 5,
                    lengthMenu = c(5, 10, 15, 20)
                  ))
  })
}