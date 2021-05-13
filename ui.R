### MAIN UI ###

fluidPage(
  use_waiter(),
  
  title=HTML('Twitter Trends'), 
  
  reactOutput("overlay"),
  
  suppressDependencies("bootstrap"),
  
  tags$head(
    tags$head(tags$link(rel = "twitter icon", href = "twitter_logo.png")),
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
    shiny::tags$script(type = "text/javascript", src = shiny_router_js),
    tags$script(src = "examples_scripts.js")
  ),
  
  htmltools::htmlDependency(
    "office-ui-fabric-core",
    "11.0.0",
    list(href="https://static2.sharepointonline.com/files/fabric/office-ui-fabric-core/11.0.0/css/"),
    stylesheet = "fabric.min.css"
  ),
  
  shiny::tags$body(
    class = "ms-Fabric",
    dir = "ltr",
    layout
  )
)