library(shiny)
library(ggplot2)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(usdata)
library(gridExtra)
library(ggthemes)
library(packcircles)
library(treemap)
library(tidyr)
library(usmap)

# Question 2: Often you are asked whether particular States are improving their mortality rates (per cause)faster than, or slower than, 
# the national average. Create a visualization that lets your clients see this for themselves for one cause of death at the time. 
# Keep in mind that the national average should be weighted by the national population


url = 'https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv'
df <- read.csv(url)
nataves <- df %>%
    group_by( ICD.Chapter, Year ) %>%
    summarise( popsum = sum( Population ), deathsum = sum( Deaths ), n = n() ) %>%
    mutate( nat_crude_death_rate = deathsum/popsum*100000)
res <- left_join( df, nataves, by = c( "ICD.Chapter", 'Year')) %>%
    mutate( difference = Crude.Rate - nat_crude_death_rate )
ui <- fluidPage(
    headerPanel('Mortality Rate Explorer'),
    h4('Compare State-Level Crude Mortality Rates to the National Rates for a given International Classification of Disease (ICD)'),
    theme = shinythemes::shinytheme( 'journal' ),
    sidebarPanel(
        tags$head(tags$style("#text1{color: white;
                             font-size: 20px;
                         font-style: bold;
                         }"
        )
        ),
        tags$head(tags$style("#text1b{color: white;
                             font-size: 18px;
                         }"
        )
        ),    
        selectInput('icd', '(ICD)', unique(res$ICD.Chapter), selected='Certain infectious and parasitic diseases'),
        selectInput('state', 'State', unique(res$State), selected='AL')
    ),
    mainPanel( 
        tabsetPanel(
            tabPanel( 'Bar Chart per State', textOutput("text1"), textOutput("text1b") , plotOutput('plot1'))
        ))
)
server <- shinyServer(function(input, output, session) {
    output$text1 <- renderText({
        "Chose a ICD and State:"
    })
    
    output$text1b <- renderText({
        "observe how a given state's mortality rate compares to the national average across all years for which data is present"
    })
    

    
    selectedData <- reactive({
        dfSlice <- res %>%
            filter(ICD.Chapter == input$icd) %>%
            mutate( value = difference,
                    region = tolower( abbr2state( State ) ),
                    fips = fips( State ) )
    })

    
    
    output$plot1 <- renderPlot({
        dfSlice <- selectedData() %>%
            filter( State == input$state ) %>%
            select( Year, Crude.Rate, nat_crude_death_rate) %>%
            pivot_longer( !Year, names_to = 'group', values_to = 'rate' )
        
        ggplot(dfSlice, aes( x = Year, y= rate, fill = group)) +
            geom_bar(stat="identity", position=position_dodge()) +
            labs(
                title = paste0( input$state, ': ', input$icd  ),
                subtitle = "State-level vs National Crude Mortality",
                y = "Mortality Rate",
                x = 'Year'
            ) +
            theme_classic() +
            scale_fill_discrete(labels = c( input$state,'National Average' )) +
            theme(plot.title = element_text(hjust = 0.5, size = 18, family = 'serif'),
                  plot.subtitle = element_text(hjust = 0.5, size = 16, family = 'serif'))  +
            guides(fill=guide_legend(title=""))
    })
    
    
})
shinyApp(ui = ui, server = server)