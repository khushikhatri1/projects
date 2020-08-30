#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Shiny App version  1: Investment Returns in Different Scenarios
# Inputs:
#   initial: initial investment
#   hyield_annual: annual average rate for high yield savings account
#   hyield_volatility: annual volatility for high yield savings account
#   years: number of years
#   contribution: annual contribution
#   fincome_annual: annual average rate for U.S. bonds
#   fincome_volatility: annual volatility for U.S. bonds
#   seed: random seed
#   growth: growth rate of annual contribution
#   usequity_annual: annual average rate for U.S. stocks
#   usequity_volatility: annual volatility for U.S. stocks
#   facet: whether or not to facet the graph
#   
# Outputs:
#   plot of value of investment in three different investing scenarios


library(shiny)
library(ggplot2)
library(tidyr)



# Define UI for application that makes the visual plot for investing scenarios ----
ui <- fluidPage(

    
    # Application title ----
    titlePanel("Investment Returns in Different Scenarios"),
    
    # Layout for input widgets ----
    fluidRow(
        column(3,
               
               #Input: Slider for initial investment ----
               sliderInput(inputId ="initial",
                          label = "Initial Amount",
                          min = 0,
                          max = 10000,
                          value = 1000,
                          pre="$",
                          step=100)
               ),
        column(3,
               
               #Input: Slider for annual average rate for high yield savings account ----
               sliderInput(inputId="hyield_annual",
                           label = "High Yield annual rate (in %)",
                           min = 0,
                           max = 20,
                           value = 2,
                           step=0.1)
               ),
        column(3,
               
               #Input: Slider for annual volatility for high yield savings account ----
               sliderInput(inputId="hyield_volatility",
                           label = "High Yield volatility (in %)",
                           min = 0,
                           max = 20,
                           value = 0.1,
                           step=0.1)
               ),
        column(3,
               
               #Input: Slider for number of years ----
               sliderInput(inputId="years",
                           label = "Years",
                           min = 0,
                           max = 50,
                           value = 20,
                           step=1)
               )
        ),
    
    fluidRow(
        column(3,
               
               #Input: Slider for annual contribution ----
               sliderInput(inputId ="contribution",
                           label = "Annual Contribution",
                           min = 0,
                           max = 5000,
                           value = 200,
                           step=100,
                           pre="$")
        ),
        column(3,
               
               #Input: Slider for annual average rate for U.S. bonds ----
               sliderInput(inputId="fincome_annual",
                           label = "Fixed income annual rate (in %)",
                           min = 0,
                           max = 20,
                           value = 5,
                           step=0.1)
        ),
        column(3,
               
               #Input: Slider for annual volatility for U.S. bonds ----
               sliderInput(inputId="fincome_volatility",
                           label = "Fixed Income volatility (in %)",
                           min = 0,
                           max = 20,
                           value = 4.5,
                           step=0.1)
        ),
        column(3,
               
               #Input: Number for random seed ----
               numericInput(inputId="seed",
                           label = "Random Seed",
                           value = 12345)
               )
        ),
    fluidRow(
        column(3,
               
               #Input: Slider for growth rate of annual contribution ----
               sliderInput(inputId ="growth",
                           label = "Annual Growth Rate (in %)",
                           min = 0,
                           max = 20,
                           value = 2,
                           step=0.1)
        ),
        column(3,
               
               #Input: Slider for annual average rate for U.S. stocks ----
               sliderInput(inputId="usequity_annual",
                           label = "US Equity annual rate (in %)",
                           min = 0,
                           max = 20,
                           value = 10,
                           step=0.1)
        ),
        column(3,
               
               #Input: Slider for annual volatility for U.S. stocks ----
               sliderInput(inputId="usequity_volatility",
                           label = "US Equity volatility (in %)",
                           min = 0,
                           max = 20,
                           value = 15,
                           step=0.1)
        ),
        column(3,
               
               #Input: Selection of whether or not to facet the graph ----
               selectInput(inputId="facet",
                             label = "Facet?",
                             choices=c("Yes","No"), selected="Yes")
        ),
        
        column(4, h4("Timelines")),


        # Output: plot of the generated graph of yearly balances----
            
           plotOutput(outputId="investmentPlot")
    )
)




# Define server logic required to make the visual plot for investing scenarios ----

server <- function(input, output) {
    
    output$investmentPlot <- renderPlot({
        
        
        hyield_values=c(input$initial)
        fincome_values=c(input$initial)
        usequity_values=c(input$initial)
        
        set.seed(input$seed)
        
        new_amount_hy=input$initial
        new_amount_fi=input$initial
        new_amount_use=input$initial
        
        
        for (i in 1:input$years){
            g_rate=input$growth/100
            
            hyield_rate_i=rnorm(1, input$hyield_annual/100, input$hyield_volatility/100)
            new_amount_hy=new_amount_hy*(1+hyield_rate_i)+(input$contribution)*(1+g_rate)^(i-1)
            hyield_values[i+1]=new_amount_hy
            
            fincome_rate_i=rnorm(1,input$fincome_annual/100, input$fincome_volatility/100)
            new_amount_fi=new_amount_fi*(1+fincome_rate_i)+(input$contribution)*(1+g_rate)^(i-1)
            fincome_values[i+1]=new_amount_fi
            
            usequity_rate_i=rnorm(1, input$usequity_annual/100, input$usequity_volatility/100)
            new_amount_use=new_amount_use*(1+usequity_rate_i)+(input$contribution)*(1+g_rate)^(i-1)
            usequity_values[i+1]=new_amount_use
            
        }
        
        # Generate a plot of the yearly balances ----
        # and only facet the graph if requested
        
        if (input$facet=="No"){
            investments=data.frame(year=0:input$years, high_yield=hyield_values, us_bonds=fincome_values, us_stocks=usequity_values)
            ggplot(investments, aes(x=year))+geom_line(aes(y=high_yield, color= "high_yield"), size=0.8)+geom_point(aes(y=high_yield, color= "high_yield"), size=1)+geom_line(aes(y=us_bonds,color="us_bonds"), size=0.8)+geom_point(aes(y=us_bonds,color="us_bonds"), size=1)+geom_line(aes(y=us_stocks,color="us_stocks"), size=0.8)+geom_point(aes(y=us_stocks,color="us_stocks"), size=1)+labs(x="year",y= "amount")+ggtitle("Three indices")+theme_bw()+scale_color_discrete(name="index")
        }
        
        else if (input$facet=="Yes"){
            investments_facet=data.frame(high_yield=hyield_values, us_bonds=fincome_values, us_stocks=usequity_values)
            long_investments_facet=gather(investments_facet, key=index)
            
            long_investments_facet["year"]=rep(0:input$years, 3)
            
            ggplot(long_investments_facet,aes(x=year))+geom_area(aes(y=value, color=index, fill=index), alpha=0.55, size=0.8)+geom_point(aes(y=value, color=index), size=0.9)+facet_wrap(~index)+theme_bw()+ggtitle("Three indices")+labs(x="year",y= "amount")
            
        }
    })
}


# Run the Shiny application ----
shinyApp(ui = ui, server = server)
