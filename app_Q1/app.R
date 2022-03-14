#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(RColorBrewer)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(choroplethr)))
suppressMessages(suppressWarnings(library(choroplethrMaps)))
suppressMessages(suppressWarnings(library(usdata)))
suppressMessages(suppressWarnings(library(gridExtra)))
suppressMessages(suppressWarnings(library(ggthemes)))
suppressMessages(suppressWarnings(library(packcircles)))
suppressMessages(suppressWarnings(library(treemap)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(usmap)))



rsconnect::setAccountInfo(name='bharanimadhav', token='1C3F08712AEC485AB2ADDB433018A5E9', secret='VRbMrk1YxZdxzwbb9BFlvwn+UmMCD8kFW45+A/65')


# Question 1: As a researcher, you frequently compare mortality rates from particular causes across different States. 
#You need a visualization that will let you see (for 2010 only) the crude mortality rate, across all States, from one cause 
#(for example, Neoplasms, which are effectively cancers). Create a visualization that allows you to rank States by crude mortality for each cause of death.



url = 'https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv'
df <- read.csv(url)
dim( df )
head( df )
glimpse( df )





# preparing the data
# filter for 2010
df_2010 <- df %>%
    filter( Year == 2010 )
dim( df_2010 )
head( df_2010 )

hist(df_2010$Deaths)
hist(df_2010$Population)
hist(df_2010$Year)



# create an example plot
df_2010_exp <- df_2010 %>% 
    filter( ICD.Chapter == 'Certain infectious and parasitic diseases' ) 
#bar plot
y_lines = c( 20, 40, 60 )
plt1 <- df_2010_exp %>%
    ggplot( aes( x= reorder(State,Crude.Rate), y= Crude.Rate ) ) +
    geom_col( ) +
    coord_flip() +
    theme_classic() +
    geom_hline(yintercept=y_lines, color="white", size=1)
plt1
#choropleth
map_data <- df_2010_exp
map_data$value = map_data[, 6]
map_data$region = tolower( abbr2state( df_2010_exp$State ) )

plt2 <- state_choropleth(df = map_data,
                         title = colnames(map_data)[2], 
                         num_colors = 7) + 
    theme(legend.position=c(.9, .23)) +
    labs(title = 'Certain infectious and parasitic diseases',
         subtitle = "2010 US Census",
         fill = "Crude Mortality Rate") 
plt2



#packcircles
packing <- circleProgressiveLayout(map_data$Crude.Rate, sizetype='area')
map_data <- cbind(map_data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=25)
# Make the plot
ggplot() + 
    # Make the bubbles
    geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.4) +
    # Add text in the center of each bubble + control its size
    geom_text(data = map_data, aes(x, y, size=Crude.Rate, label = State)) +
    scale_size_continuous(range = c(1,4)) +
    # General theme:
    theme_void() + 
    theme(legend.position="none") +
    coord_equal()




ui <- fluidPage(
    headerPanel('Mortality Rate Explorer'),
    theme = shinythemes::shinytheme( 'cosmo' ),
    sidebarPanel(
        selectInput('icd', 'International Classification of Disease', unique(df_2010$ICD.Chapter), selected='Certain infectious and parasitic diseases'),
    ),
    mainPanel(
        h4('Compare Mortality Rates from Particular International Classification of Disease across Different States'),
        tabsetPanel(
            tabPanel( 'Map View', plotOutput('plot1'), verbatimTextOutput('stats')),
            tabPanel( 'Ranked View', plotOutput('plot2'))
        ))
)
server <- shinyServer(function(input, output, session) {
    
    selectedData <- reactive({
        dfSlice <- df_2010 %>%
            filter(ICD.Chapter == input$icd) %>%
            mutate( value = Crude.Rate,
                    region = tolower( abbr2state( State ) ))
    })
    
    output$plot1 <- renderPlot({
        
        dfSlice <- df_2010 %>%
            filter(ICD.Chapter == input$icd)
        
        state_choropleth(df = selectedData(),
                         num_colors = 7) + 
            theme(legend.position=c(.9, .22)) +
            labs(title = input$icd,
                 fill = "Crude Mortality Rate") + 
            theme(plot.title = element_text(hjust = 0.3, size = 20, family = 'serif'))
    })
    
    output$stats <- renderPrint({
        dfSlice <- selectedData() %>%
            filter(ICD.Chapter == input$icd)
        summary(dfSlice$Crude.Rate)
    })
    
    output$plot2 <- renderPlot({
        dfSlice <- selectedData() %>%
            filter(ICD.Chapter == input$icd)
        
        ggplot( selectedData(),
                aes( x= reorder(State,Crude.Rate), y= Crude.Rate ) ) +
            geom_col( ) +
            coord_flip() +
            labs( 
                title = 'Ranked Mortality Rate',
                subtitle = input$icd,
                y = "Crude Mortality Rate",
                x = 'State'
            ) + 
            theme_tufte(base_size = 15)
    },
    width = 600,
    height = 900) 
    
    
})

shinyApp(ui = ui, server = server)

