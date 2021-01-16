## Attach packages
library(shiny)
library(tidyverse)
library(palmerpenguins)

# Create the user interface
ui = fluidPage(
    titlePanel("I am adding a TITLE!"), # Camelcase
    sidebarLayout(
        sidebarPanel("put my widgets here!",
                     radioButtons(inputId = "penguin_species",
                                  label = "Choose penguin species:",
                                  choices = c("Adelie", "COOL CHINSTRAP PENGUINS" =  "Chinstrap", "Gentoo"), # change the name by "name" = "string_in_data"
                                  ),

                    selectInput(inputId = "pt_color", label = "Select point color", choices = c("Awesome red!" = "red", "Pretty purple" = "purple", "ORAANGE" = "orange"))
                     ),
        mainPanel("Here's my graph!",
                  plotOutput(outputId = "penguin_plot"),# to call up plot from server
                  tableOutput(outputId = "penguin_table") # to call up table from server
                  )
    )
) # Need to keep track of ()

# Create the server function
server <- function(input, output) {

    penguin_select <- reactive({
        penguins %>%
            filter(species == input$penguin_species) # only keep input user selected from their radio button
    })

    penguin_table <- reactive({
        penguins %>%
            filter(species == input$penguin_species) %>%
            group_by(sex) %>%
            summarize(
                mean_flip = mean(flipper_length_mm),
                mean_mass = mean(body_mass_g)
            )
    })

    output$penguin_plot <- renderPlot({

        ggplot(data = penguin_select(), aes(x = flipper_length_mm, y = body_mass_g)) +
            geom_point(color = input$pt_color) # must have emtpy () after the df for reactive graph

    })

    output$penguin_table <- renderTable({
        penguin_table()
    })

}


# Combine into an app
shinyApp(ui = ui, server = server)

# For nicer tables, consider: Package DT, gt, renderDataTable, reactable










