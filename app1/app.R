# Load packages ----------------------------------------------------------------

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggtext)

# Load data --------------------------------------------------------------------

bbnames <- read_csv("data/babynames.csv", show_col_types = FALSE)

#rename labels
bbnames$sex = ifelse(bbnames$sex=="F","Female","Male")


# Define UI --------------------------------------------------------------------


ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Popularity of names over time"),
                sidebarLayout(
                  sidebarPanel(

                    # Select names
                    textInput(inputId = "name", label = "Pick a baby name:",
                                #choices = unique(bbnames$name)[1:100],
                                #selected = "Karen"
                              value = "Janice"
                              ),

                    # Select names to compared
                    textInput(inputId = "name2", label = "Pick another baby name:",
                                #choices = unique(bbnames$name)[1:100],
                                #selected = "Anna"
                              value = "Eva"
                              ),

                    # Select year
                    selectInput(inputId = "year", label = "Pick a year:",
                                choices = unique(bbnames$year),
                                selected = "2000"),


                    )
                  ,

                  # Output: Description, lineplot, and reference
                  mainPanel(
                    #tableOutput("table"),
                    plotOutput(outputId = "barplot"),
                    plotOutput(outputId = "lineplot"),
                    plotOutput(outputId = "lineplot2"),

                  ))
                )


# Define server function --------------------------------------------
server <- function(input, output) {

  # Subset data
  bbnames_r = reactive({
    req(input$name)
    req(input$name2)
    #validate(need(!is.na(input$name)), "Error: Please provide a name!")
    #validate(need(!is.na(input$name2)), "Error: Please provide a name!")
    #validate(need((lower(bbnames$name)%in% c(lower(input$name),lower(input$name2)))),
    #         "Error: Please check for typo or provide a different name!")
    bbnames %>%
      filter(
        tolower(name)%in% c(tolower(input$name),tolower(input$name2))
        )
  })

  # #head data
  # output$table = renderTable({
  #   head(bbnames_r())
  #   })

  # Create barplot
  output$barplot <- renderPlot({
    #line plot for names
    #bbnames_r %>% filter(name %in% c(input$name,input$name2) & year %in% input$year) %>%
    bbnames_r() %>% filter(year %in% input$year) %>%
      ggplot(aes(y = name, x = n,fill = name,color=sex,group=sex)) +
      geom_col()+
      labs(
        title=paste("Popularity of baby names",input$name,"versus",input$name2,"in",input$year),
        x = "Number of births",
        y = "Name",
        fill = "",
        color = ""
      )+
      theme_bw()+
      theme(
        panel.border = element_blank(),
        plot.title = element_markdown()
      )+
      scale_x_continuous(labels = scales::comma)+
      scale_fill_manual(values = c("dodgerblue4","green4"))

  })


  # Create lineplot
  output$lineplot <- renderPlot({
    #line plot for names
    bbnames_r() %>% filter(tolower(name) %in% tolower(input$name)) %>%
      ggplot(aes(y = n, x = year,color = sex, group= sex)) +
      geom_line(lwd=1.25)+
      geom_vline(xintercept = as.numeric(input$year))+
      labs(
        title=paste("Popularity of baby names over time for",input$name),
        y = "Number of births",
        x = "Year",
        color = ""
      )+
      theme_bw()+
      theme(
        panel.border = element_blank(),
        plot.title = element_markdown()
      )+
      scale_y_continuous(labels = scales::comma)+
      scale_color_manual(values = c("dodgerblue4","green4"))

  })

  # Create lineplot for the second name
  output$lineplot2 <- renderPlot({
    #line plot for names
    bbnames_r() %>% filter(tolower(name) %in% tolower(input$name2)) %>%
      ggplot(aes(y = n, x = year,color = sex, group= sex)) +
      geom_line(lwd=1.25)+
      geom_vline(xintercept = as.numeric(input$year))+
      labs(
        title=paste("Popularity of baby names over time for",input$name2),
        y = "Number of births",
        x = "Year",
        color = ""
      )+
      theme_bw()+
      theme(
        panel.border = element_blank(),
        plot.title = element_markdown()
      )+
      scale_y_continuous(labels = scales::comma)+
      scale_color_manual(values = c("dodgerblue4","green4"))

  })

}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)
