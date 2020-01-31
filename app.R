##
## This is a Shiny web application. You can run the application by clicking
## the 'Run App' button above.
##
## Find out more about building applications with Shiny here:
##
##    http://shiny.rstudio.com/
##

library(shiny)
library(DBI)
library(RSQLite)
## also needs: stringr and HTTR packages

push_notify <- function(sp_key,
                        msg = "Hello from shiny app!",
                        title = "Feedback Received") {
  httr::POST("https://api.pushover.net/1/messages.json",
             encode = "json",
             body = list(
               token = "aj6bto4ek76eyudb6eogahpnkjaf8k",
               user = sp_key,
               title= "Feedback Received",
               message = msg))
}

reconnect <- function() {
  if (!dbIsValid(con)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
    if (!dbIsValid(con)) {
      stop("couldn't connect to the database")
    }
  }
  con
}

generate_token <- function() {
  my_str <- sample(c(sample(LETTERS, 2), sample(letters, 5)))
  paste0(c(my_str[1], sample(c(my_str, sample(0:9, 1)))), collapse = "")
}

get_field <- function(field, my_token, student = TRUE) {
  where_field <- "student_token"
  if (!student) {
    where_field <- "staff_token"
  }
  sql <- paste0("SELECT ", field, " FROM courses WHERE ", where_field,
                " = ", dbQuoteString(con, my_token))
  ## print(sql)
  res <- dbGetQuery(con, sql)
  if (nrow(res)) {
    res[, field]
  } else {
    NULL
  }
}

## modekey_create <- "NNk3ayzDd"
## modekey_reader <- "DDtp3Qrjk"

## dbname <- "/srv/shiny-server/Dale/feedback/feedback.db"
dbname <- "feedback.db"
first_time <- FALSE

## get the tokens (or initialize if none found)
con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
sql <- paste0("SELECT name, token FROM tokens WHERE name = ",
              dbQuoteString(con, "creator"), " OR name = ",
              dbQuoteString(con, "reader"))
res <- dbGetQuery(con, sql)
if (nrow(res) == 0) {
  ## need to create new creator / reader tokens
  modekey_create <- generate_token()
  modekey_reader <- generate_token()
  first_time <- TRUE
  sql <- paste0("INSERT INTO tokens VALUES (",
                dbQuoteString(con, "creator"), ",",
                dbQuoteString(con, modekey_create),
                "), (",
                dbQuoteString(con, "reader"), ",",
                dbQuoteString(con, modekey_reader), ")")
  res <- dbSendQuery(con, sql)
} else if (nrow(res) == 2) {
  ## load them in
  modekey_create <- res[res$name == "creator", "token"]
  modekey_reader <- res[res$name == "reader", "token"]
} else {
  stop("problem with the database")
}

## Define UI for application
ui <- fluidPage(theme = "custom.css",
  
  ## Application title
  titlePanel("Course Feedback"),

  ## main
  fluidPage(
    uiOutput("main")
  )
)

## Define server logic 
server <- function(input, output, session) {
  tokens <- reactiveValues(post_result = "Your post was submitted successfully.")
  
  output$main <- renderUI({
    my_port <- if (session$clientData$url_port != "") {
                 paste0(":", session$clientData$url_port)
               } else {
                 ""
               }
    base_url <- paste0(session$clientData$url_protocol, "//",
                       session$clientData$url_hostname,
                       my_port,
                       session$clientData$url_pathname)
    query <- parseQueryString(session$clientData$url_search)
    ## print(paste(names(query), query, sep = "=", collapse=", "))

    con <- reconnect()

    if (first_time) {
      ## report creation token
      first_time <- FALSE
      fluidPage(
        h3("Startup successful!"),
        p("To create a course, use the following URL:"),
        a(href = paste0(base_url, "?mode=", modekey_create),
          paste0(base_url, "?mode=", modekey_create)),
        p("Please save this URL somewhere safe and then click exit."),
        br(),
        actionButton("exit", "Click here to exit")
      )
    } else {
      
      ## if we have a course string, get the course name (student token)
      tokens$coursename <- NULL
      if (!is.null(query[["course"]])) {
        tokens$student <- query[["course"]]
        tokens$staff <- get_field("staff_token", tokens$student)
        tokens$coursename <- get_field("description", tokens$student)
      }

      if (!is.null(query[["token"]])) {
        tokens$staff <- query[["token"]]
        tokens$student <- get_field("student_token", tokens$staff, FALSE)
        tokens$coursename <- get_field("description", tokens$student)
      }

      mode <- "post"
      if (!is.null(query[["mode"]])) {
        mode <- query[["mode"]]
        ## cat("mode is: ", mode, "\n")
      }

########################
### create a new course
      if (mode == modekey_create) {
        used_student <-
          dbGetQuery(con, "SELECT student_token FROM courses")[, "student_token"]
        while((tokens[["student"]] <- generate_token()) %in% used_student) {};
        
        used_staff <-
          dbGetQuery(con, "SELECT staff_token FROM courses")[, "staff_token"]
        while((tokens[["staff"]] <- generate_token()) %in% used_staff) {};
        
        fluidPage(
          conditionalPanel("input.submit == 0",
                           p("Use this page to create a new course for use in the fast feedback system."),
                           p("Once you have entered the information below, click Submit and you will receive links to distribute to students and instructors."),
                           textInput("course_name", "Friendly description of your course for students (e.g., \"L3 statistics 2018-2019\"):"),
                           div(class = "optional",
                               h3("OPTIONAL: Receive mobile notifications"),
                               textInput("simplepush_key", "Pushover mobile app user key (leave blank if you don't have one):"),
                               a(href="https://pushover.net", "Visit pushover.net to download the app and obtain a user key"),
                               br(),
                               actionButton("testpush", "Click here to send a test notification"),
                               ),
                           br(),
                           actionButton("submit", "Submit")
                           ),
          conditionalPanel("input.submit > 0",
                           h3("Course created."),
                           p("Now copy and distribute these links:"),
                           p("Link to distribute to STUDENTS:"),
                           a(href = paste0("?course=",
                                           tokens[["student"]]),
                             paste0(base_url, "?course=", tokens[["student"]])),
                           hr(),
                           p("Link to distribute to STAFF:"),
                           a(href = paste0("?mode=", modekey_reader,
                                           "&token=",
                                           tokens[["staff"]]),
                             paste0(base_url, "?mode=", modekey_reader,
                                    "&token=", tokens[["staff"]])),
                           br(),
                           p("Click exit to finish."),
                           actionButton("exit", "Exit")
                           )          
        )
        ##
        ## staff member reading page
      } else if ((mode == modekey_reader) && (!is.null(tokens$coursename))) {
        my_sql <- "SELECT posted_on, fbk_text FROM posts WHERE student_token = ?st ORDER BY post_id DESC"
        sql2 <- sqlInterpolate(con, my_sql, st = tokens[["student"]])
        res <- dbGetQuery(con, sql2)
        if (nrow(res)) {
          args <- mapply(function(.x, .y) {
            list(strong(.x), p(), pre(.y), hr())
          }, res[["posted_on"]], res[["fbk_text"]], SIMPLIFY = FALSE)
          do.call(shiny::fluidPage, purrr::flatten(args))
        } else {
          fluidPage(p("No comments yet."))
        }
        
        ##
        ## student posting to page
      } else if (!is.null(tokens$coursename)) {
        fluidPage(conditionalPanel("input.post == 0",
                                   h3(tokens$coursename),
                                   textAreaInput("comment",
                                                 "Enter your comment below (plain text only please!)",
                                                 rows = 10, cols = 80
                                                 ),
                                   actionButton("post", "Post this comment")),
                  conditionalPanel("input.post > 0",
                                   p(tokens$post_result)))
      } else {
        fluidPage(p("You have reached this page in error."))
      }
    }
  })

  observeEvent(input$post, {    
    my_sql <- "INSERT INTO posts VALUES (NULL, ?sd_tok, ?sf_tok, ?dt, ?tx)"
    sql2 <- sqlInterpolate(con, my_sql,
                           sd_tok = tokens$student,
                           sf_tok = tokens$staff,
                           dt = as.character(strptime(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                           tx = stringr::str_trim(input$comment))
    dbSendQuery(con, sql2)
    ## notify instructor
    if ((sp_key <- get_field("simplepush_key", tokens$student)) != "") {
      if ((input$sp_key != "") && (!is.null(input$sp_key))) {
        push_notify(input$sp_key, sprintf("You received feedback for %s.",
                                          tokens$coursename))
      }
    }
  })

  observeEvent(input$exit,
               if (input$exit > 0L)
                 stopApp())

  observeEvent(input$testpush, {
    push_notify(input$simplepush_key, "Hello from shiny app!")
  })
  
  observeEvent(input$submit, {
    desc = stringr::str_trim(input$course_name)
    if (desc != "") {
      my_sql <- paste0("INSERT INTO courses VALUES ('",
                       tokens[["student"]], "', '",
                       tokens[["staff"]], "', '",
                       strptime(Sys.time(), "%Y-%m-%d %H:%M:%S"), "', ",
                       "?dc, ",
                       "?sp)")
      sql2 <- sqlInterpolate(con, my_sql,
                             dc = desc,
                             sp = stringr::str_trim(input$simplepush_key))
      res <- dbSendQuery(con, sql2)
    }

    dbDisconnect(con)
  })
}

                                        # Run the application 
shinyApp(ui = ui, server = server)
