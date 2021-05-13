### GLOBAL INITIALIZATIONS ###

rm(list = ls())
options(shiny.autoreload = TRUE)

library(shiny.fluent)
library(shiny.react)
library(shiny.router)
library(shiny)
library(readr)
library(ggthemes)
library(sass)
library(rtweet)
library(scales)
library(purrr)
library(ggplot2)
library(hms)
library(data.table)
library(dplyr)
library(tidytext)
library(shinycssloaders)
library(waiter)

# Create twitter token
token <- create_token(
  app = "shiny_02",
  consumer_key = 'SopsfXfakxtTu2O2X7ybUPvN6',
  consumer_secret = 'O4A0UiVUsu7Cb8527nKq4U5of1aUWSIIhoUsaG8GsKY81V9S38',
  access_token = '200649932-9CZrzEDjm4E6CN4dINhvKl9ZVriXx1Vnd3NRzbXc',
  access_secret = 'h4mVB91NgGJzn8KBIL5tmhPSqxyzhIIzgWuFi9HtbEN5v',
  set_renv = TRUE
)

# Get sentiment dictionary
afinn_lex <- read_tsv(
  'AFINN-111.txt',
  col_types = cols(word = col_character(),
                   value = col_double()),
  col_names = c("word", "value")
)



### HEADER ###

logo <- img(src = "twitter_logo.png", class = "logo")
title <-
  div(Text(variant = "xLarge", "Twitter Trends"), class = "title")

header <- tagList(logo, title)


### NAVIGATION ###

navigationStyles <- list(
  root = list(
    height = '100%',
    boxSizing = 'border-box',
    border = '1px solid #eee',
    overflowY = 'auto'
  )
)


linkGroups <- function(examples) {
  examplesLinks <- imap(examples, function(example, name) {
    list(name = name,
         url = paste0('#!/', name),
         key = name)
  })
  names(examplesLinks) <- NULL
  
  list(list(links = list(
    list(
      name = 'Search & Tweets',
      url = '#!/',
      key = 'home',
      icon = 'search',
      isExpanded = FALSE
    ),
    list(
      name = 'Sentiments',
      url = '#!/sentiments',
      key = 'sentiments',
      icon = 'heart',
      iconProps = list(iconName = 'heart',
                       styles = list(root = list(
                         fontSize = 20,
                         color = '#106ebe'
                       )))
    ),
    list(
      name = 'About',
      url = '#!/about',
      key = 'about',
      icon = 'help',
      iconProps = list(iconName = 'help',
                       styles = list(root = list(
                         fontSize = 20,
                         color = '#106ebe'
                       )))
    )
  )))
  
}

navigation <- function(examples) {
  tagList(Nav(
    groups = linkGroups(examples),
    initialSelectedKey = 'home',
    styles = navigationStyles
  ))
}


examples <- list()


### UTILS ###
makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}

### HOME ###

tweet_types_options <- list(
  list(key = "recent", text = "Recent"),
  list(key = "mixed", text = "Mixed"),
  list(key = "popular", text = "Popular")
)

get_tweets_card <- div(class = "card ms-depth-8",
                       Stack(
                         tokens = list(childrenGap = 10),
                         Stack(
                           wrap = T,
                           tokens = list(childrenGap = 30),
                           horizontal = T,
                           horizontalAlign = 'center',
                           
                           Stack(
                             grow = '0.2',
                             Stack(
                               horizontalAlign = 'center',
                               Text(variant = "large",
                                    "Search Word or Phrase",
                                    block = F)
                             ),
                             tokens = list(childrenGap = 10),
                             TextField.shinyInput("search_phrase")
                           ),
                           Stack(
                             tokens = list(childrenGap = 10),
                             grow = '0.2',
                             Stack(
                               horizontalAlign = 'center',
                               Text(
                                 variant = "large",
                                 "No. of Tweets to Fetch (Takes Longer)",
                                 block = F
                               )
                             ),
                             Slider.shinyInput(
                               "number_of_tweets",
                               value = 1000,
                               min = 0,
                               max = 10000,
                               step = 1000,
                               showValue = T
                             ),
                             Stack(vertverticalAlign = 'center',
                                   horizontalAlign = 'center')
                           ),
                           Stack(
                             tokens = list(childrenGap = 10),
                             grow = '0.3',
                             Stack(
                               horizontalAlign = 'center',
                               Text(variant = "large",
                                    "Type of Tweets",
                                    block = F)
                             ),
                             Dropdown.shinyInput("type_of_tweets",
                                                 value = "popular",
                                                 options = tweet_types_options)
                           )
                         ),
                         PrimaryButton.shinyInput("search_click", text = "Search")
                       ))

tweets_dataframe_card <- div(class = "card ms-depth-8",
                             Stack(
                               tokens = list(childrenGap = 10),
                               
                               DT::DTOutput(outputId = 'twitter_data_table')
                             ))

homePage <-
  makePage(
    "Get Tweets",
    "Enter a word or a search phrase to get the related tweets",
    div(get_tweets_card, tweets_dataframe_card)
  )


### SENTIMENTS ###


sentiments_plot_card <- div(class = "card ms-depth-8",
                            Stack(
                              tokens = list(childrenGap = 10),
                              Stack(
                                tokens = list(childrenGap = 10),
                                plotOutput('sentiment') %>%
                                  withSpinner(color = 'blue', type = 6, size = 0.3)
                              )
                            ))

sentimentsPage <-
  makePage(
    "Sentiment Trends",
    "The below plot shows the sentiments of tweets for the searched term",
    div(sentiments_plot_card)
  )

### ABOUT ###


about_card_1 <- div(class = "card ms-depth-8",
                    Stack(
                      tokens = list(childrenGap = 10),
                      Stack(
                        tokens = list(childrenGap = 5),
                        Text(variant = "large",
                             "",
                             block = TRUE),
                        Text(
                          variant = 'mediumPlus',
                          "This app fetches the most recent Twitter feed based on the provided search term. Users can select the type of tweets to fetch which are 'Popular', 'Recent' and 'fixed'. Users can also choose the number of tweets to download. Once the 'Search' button is clicked, R downloads the data and plots out the aggregate sentiments of the tweets. The downloaded data can also be inspected from the data table displayed below the 'Search' button."
                        ),
                        span(
                          class = "ms-fontColor-sharedCyanBlue10",
                          style = "padding-top: 15px",
                          
                          "Please note that downloading large number of tweets will increase the download time."
                          
                        )
                      )
                    ))

about_card_2 <- div(class = "card ms-depth-8",
                    Stack(
                      tokens = list(childrenGap = 10),
                      span(class = "ms-font-su ms-fontColor-themePrimary", "shiny.react"),
                      Stack(
                        tokens = list(childrenGap = 5),
                        Text("shiny.react makes it easy to use React libraries in Shiny apps.")
                      ),
                      Stack(
                        tokens = list(childrenGap = 10),
                        Text(variant = "large", "Want to know more?", block =
                               TRUE),
                        Text(
                          "To make a React library convenient to use from Shiny, we need to write an R package that wraps it - for example, a shiny.fluent package for Microsoft's Fluent UI, or shiny.blueprint for Palantir's Blueprint.js. We try to make this as fast as possible, by providing a generator and automating whatever we can. Go to generator readme to learn how to generate an R wrapper package."
                        ),
                        Text(
                          "Communication and other issues in integrating Shiny and React are solved and standardized in shiny.react package. Generated packages depend on shiny.react."
                        ),
                        Text(
                          "shiny.react strives to do as much as possible automatically, but there's no free lunch here, so in all cases except trivial ones you'll need to do some amount of manual work. The more work you put into a wrapper package, the less work your users will have to do while using it."
                        )
                      )
                    ))

aboutPage <-
  makePage("About",
           "Description and Usage",
           div(about_card_1))

### FOOTER ###

footer <- Stack(
  tokens = list(childrenGap = 10),
  horizontal = T,
  horizontalAlign = 'center',
  Text(
    variant = "large",
    "Built with shiny.fluent",
    block = TRUE
  )
)

pages <-
  c(list(
    route("/home", homePage),
    route("about", aboutPage),
    route("sentiments", sentimentsPage)
  ))
router <- lift(make_router)(pages)

layout <- div(
  class = "grid-container",
  div(class = "header", header),
  div(class = "sidenav", navigation(examples)),
  div(class = "main", router$ui),
  div(class = "footer", footer)
)

# shiny.router dependencies do not get picked up because they're added in a non-standard way.
shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js <- file.path("shiny.router", "shiny.router.js")

sass(sass_file("style.scss"),
     output = "www/style.css")
