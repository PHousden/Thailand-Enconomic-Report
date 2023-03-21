library(shiny)
library(WDI)
library(tidyverse)
library(plotly)
library(bslib)
library(shinyWidgets)

# Retrieve population data for Thailand
pop_data <- WDI(indicator = "SP.POP.TOTL", country = "THA", start = 2010, end = 2020)

# Clean population data
pop_data_clean <- pop_data %>%
    select(year = year, population = SP.POP.TOTL) %>%
  filter(!is.na(population))



# Retrieve poverty data for Thailand
pov_data <- WDI(indicator = "SI.POV.DDAY", country = "THA", start = 2010, end = 2021)

# Clean poverty data for Thailand
pov_data_clean <- pov_data %>%
  select(year = year, poverty = SI.POV.DDAY) %>%
  filter(!is.na(poverty))


# Retrieve GDP data for Thailand
gdp_data <- WDI (indicator = "NY.GDP.MKTP.KD.ZG", country = "THA", start = 2010, end = 2021)

# Clean GDP data for Thailand 
gdp_data_clean <- gdp_data %>%
  select(year = year, gdp = NY.GDP.MKTP.KD.ZG) %>%
  filter(!is.na(gdp))



# Retrieve Import goods and services data for Thailand
import_data <- WDI (indicator = "NE.IMP.GNFS.KD.ZG", country = "THA", start = 2010, end = 2021)

# Clean Import data for Thailand
import_data_clean <- import_data %>% 
  select(year = year, imports = NE.IMP.GNFS.KD.ZG) %>%
  filter(!is.na(imports))

# Retrive Export goods and services data for Thailand 
export_data <- WDI(indicator = "NE.EXP.GNFS.KD.ZG", country = "THA", start = 2010, end = 2021)

# Clean Export data for Thailand 
export_data_clean <- export_data %>%
  select(year = year, export = NE.EXP.GNFS.KD.ZG) %>%
  filter(!is.na(export))


# Define UI ----
ui <- navbarPage(title = "Thai Economic",
                         
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
             ),
             tabPanel(
               "Home",
               id = "home_page",
               h1("Thailand Economic report"),
               p ("This code is a user interface (UI) code for a dashboard on the Thai economy. The dashboard consists of four tabs; Population, GDP, Poverty, and Import and Export Goods and Service. Each tab has a title, a description of the topic, and a chart to display the data. The charts are interactive, and the user can choose to view the data in a bar chart or line chart format by clicking on the corresponding button.

The Population tab displays the population growth of Thailand over the last decade, including a brief description of the ethnic groups, religions, and languages that make up the population.

The GDP tab displays the economic growth of Thailand over the last decade, including a brief description of the sectors that drive the growth, such as manufacturing, services, and agriculture. The impact of the COVID-19 pandemic on the economy is also discussed.

The Poverty tab displays the progress made by Thailand in reducing poverty over the last decade, including a brief description of the challenges still faced in certain areas of the country.

The Import and Export Goods and Service tab displays Thailand's role in international trade, including a brief description of the goods and services exported and imported, and the country's main trading partners. The tab includes two charts, one for imports and one for exports.
")
               
             ),
             # Global Economic Development Page.
             tabPanel("Population",
                      id = "population",
                       h1("Thailand Population"),
                       br(),
                       tags$div (class="border_chart" ,
                        plotlyOutput("thai_population_chart")
                      ),
                       br(),
                       fluidRow(
                         column(width = 2, actionButton("pop_line","Line Chart")),
                         column(width = 2, actionButton("button_id", "Bar Chart"))
                       ),
                       br(),
                       p("Over the last decade, Thailand's population has continued to grow steadily. 
                According to the World Bank, the population of Thailand in 2011 was approximately 68.3 million, 
                while the estimated population in 2020 was approximately 69.8 million. 
                This represents an increase of around 1.5 million people over the course of the decade. 
                Despite this growth, the rate of population increase in Thailand has been slowing down in recent years, 
                due in part to a declining birth rate and an aging population. Nonetheless, 
                Thailand remains one of the most populous countries in Southeast Asia, with a diverse population that includes a mix of ethnic groups, 
                religions, and languages.")
                      
             ),
             
             
             
             # Thailand GDP Page
             tabPanel("GDP",
                        h1("Thailand GDP"),
                        br(),
                        tags$div(
                          class= "border_chart",
                          plotlyOutput("thai_gdp_chart")
                        ),
                        br(),
                        fluidRow(
                          column(width = 2, actionButton("gdp_line", "Line Chart")),
                          column(width = 2, actionButton("gdp_bar", "Bar Chart"))
                        ),
                        br(),
                        p("Thailand's economy is one of the largest and most diverse in Southeast Asia, with a GDP of approximately $505 billion in 2020, according to the World Bank. Over the past decade, Thailand's economy has grown steadily, with an average annual growth rate of around 3.5%. The country's economic growth is driven by a range of sectors, including manufacturing, services, and agriculture, with tourism also playing a significant role. However, the COVID-19 pandemic has had a significant impact on Thailand's economy, leading to a contraction of around 6.1% in 2020. The Thai government has implemented a range of measures aimed at mitigating the impact of the pandemic and promoting economic recovery, including fiscal stimulus measures, support for small and medium-sized enterprises, and the promotion of domestic tourism. Despite the challenges posed by the pandemic, Thailand's economy is expected to recover in the coming years, supported by ongoing investments in infrastructure, digital technology, and sustainable development.")
                      
             ),
             
             # Poverty Page
             tabPanel("Poverty",
                        h1("Thailand Poverty"),
                        br(),
                        tags$div(class="border_chart",
                                 plotlyOutput("thai_poverty_chart")
                                 ),
                        br(),
                        fluidRow(
                          column(width = 2, actionButton("pov_line", "Line Chart")),
                          column(width = 2, actionButton("poverty_bar", "Bar Chart"))
                        ),
                        br(),
                        p("Over the past decade, Thailand has made significant progress in reducing poverty. 
                            According to the World Bank, the poverty headcount ratio in Thailand has decreased from 10.5% in 2012 to 2.7% in 2018, 
                            representing a significant reduction in poverty over this period. Despite this progress, 
                            however, poverty remains a significant challenge in certain areas of the country, particularly in rural and remote areas 
                            where access to basic services such as healthcare, education, and infrastructure is limited. 
                            The Thai government has implemented a range of policies and programs aimed at reducing poverty and promoting inclusive growth, 
                            including the provision of targeted social assistance programs, investments in education and health, and the promotion of sustainable and 
                            inclusive economic growth.")
                      
             ),
             
             # Import and Export goods and Service Page
             tabPanel("Import and Export goods and Service",
                        h1("Import and Export goods and Service"),
                        br(),
                        fluidRow(
                          column(width = 6, tags$div (class="border_chart",
                                 plotlyOutput("thai_imports_chart")
                          )),
                          column(width = 6, tags$div(class="border_chart", 
                                 plotlyOutput("thai_export_chart")               
                          ))
                        ),
                        br(),
                        fluidRow(
                          column(width = 3,
                                 actionButton("im_line", "Line Chart"),
                                 actionButton("im_bar", "Bar Chart")
                          ),
                          column(width = 3, offset = 6,
                                 actionButton("ex_line", "Line Chart"),
                                 actionButton("ex_bar", "Bar Chart")
                          ),
                        ),
                        br(),
                        p("Thailand is a major player in international trade and has a well-developed export-oriented economy. The country is known for its exports of electronics, automobiles, rubber, textiles, and agricultural products such as rice and seafood. In addition to goods, Thailand also exports a significant amount of services, including tourism, finance, and logistics. The country's main trading partners are China, Japan, and the United States, and Thailand is also a member of several regional trade agreements such as the ASEAN Free Trade Area (AFTA) and the Asia-Pacific Economic Cooperation (APEC) forum. Imports to Thailand consist of a wide range of goods, including machinery, chemicals, and petroleum products, and the country is also a significant importer of services such as transportation and telecommunications. Overall, international trade is a critical component of Thailand's economy and plays a vital role in the country's economic growth and development.")
                      
             ),
            
)



# Define server logic ----
server <- function(input, output) {
  
  output$thai_population_chart <- renderPlotly({
    plot_ly(pop_data_clean, x = ~year, y = ~population, type = 'scatter', mode = 'lines') %>%
      layout(title = "Thailand Population", xaxis = list(title = "Year"), yaxis = list(title = "Population (millions)"))
  })
  
  # Render Thailand population line chart
  observeEvent(input$pop_line, {
    output$thai_population_chart <- renderPlotly({
      plot_ly(pop_data_clean, x = ~year, y = ~population, type = 'scatter', mode = 'lines') %>%
        layout(title = "Thailand Population", xaxis = list(title = "Year"), yaxis = list(title = "Population (millions)"))
    })
  })
  
  observeEvent(input$button_id, {
    # Render Thailand population BarChart
    output$thai_population_chart <- renderPlotly({
      plot_ly(pop_data_clean, x = ~population, type = 'bar') %>%
        layout(title = "Thailand Population Distribution", xaxis = list(title = "Population (millions)"), yaxis = list(title = "Count"))
    })
  })
  
  output$thai_poverty_chart <- renderPlotly({
    plot_ly(pov_data_clean, x = ~year, y = ~poverty, type = 'scatter', mode = 'lines') %>%
      layout(title = "Thailand Poverty ratio", xaxis = list(title = "Year"), yaxis = list(title = "Poverty Ratio"))
  })
  
  # Render Thailand Poverty line chart
  observeEvent(input$pov_line, {
    output$thai_poverty_chart <- renderPlotly({
      plot_ly(pov_data_clean, x = ~year, y = ~poverty, type = 'scatter', mode = 'lines') %>%
        layout(title = "Thailand Poverty ratio", xaxis = list(title = "Year"), yaxis = list(title = "Poverty Ratio"))
    })
  })
  
  
  observeEvent(input$poverty_bar, {
    # Render Thailand poverty Bar Chart
    output$thai_poverty_chart <- renderPlotly({
      plot_ly(pov_data_clean, x = ~year, y = ~poverty, type = 'bar') %>%
        layout(title = "Thailand Poverty ratio", xaxis = list(title = "Year"), yaxis = list(title = "Poverty Ratio"))
    })
  })
  
  
  output$thai_gdp_chart <- renderPlotly({
    plot_ly(gdp_data_clean, x = ~year, y= ~gdp, type = 'scatter', mode = 'lines') %>%
      layout(title = "Thailand GDP", xaxis = list(title = "Year"), yaxis = list(title = "GDP(Gross capital formation)"))
  })
  
  # Render Thailand GDP line chart
  observeEvent(input$gdp_line, {
    output$thai_gdp_chart <- renderPlotly({
      plot_ly(gdp_data_clean, x = ~year, y= ~gdp, type = 'scatter', mode = 'lines') %>%
        layout(title = "Thailand GDP", xaxis = list(title = "Year"), yaxis = list(title = "GDP(Gross capital formation)"))
    })
  })
  
  
  observeEvent(input$gdp_bar, {
    # Render Thailand GPD Bar Chart
    output$thai_gdp_chart <- renderPlotly({
      plot_ly(gdp_data_clean, x = ~year, y = ~gdp, type = 'bar') %>%
        layout(title = "Thailand GDP", xaxis = list(title = "Year"), yaxis = list(title = "GDP(Gross capital formation)"))
    })
  })
  
  output$thai_imports_chart <- renderPlotly({
    plot_ly(import_data_clean, x = ~year, y = ~imports, type = 'scatter', mode = 'lines') %>%
      layout(title = "Import of goods and services (% GDP annual)")
  })
  
  # Render Thailand Import of goods and services 
  observeEvent(input$im_line, {
    output$thai_imports_chart <- renderPlotly({
      plot_ly(import_data_clean, x = ~year, y = ~imports, type = 'scatter', mode = 'lines') %>%
        layout(title = "Import of goods and services (% GDP annual)")
    })
  })
  
  output$thai_export_chart <- renderPlotly({
    plot_ly(export_data_clean, x = ~year, y = ~export, type = 'scatter', mode = 'lines') %>%
      layout(title = "Export of goods and services (% GDP annual)")
  })
  
  # Render Thailand Export of goods and services
  observeEvent(input$ex_line, {
    output$thai_export_chart <- renderPlotly({
      plot_ly(export_data_clean, x = ~year, y = ~export, type = 'scatter', mode = 'lines') %>%
        layout(title = "Export of goods and services (% GDP annual)")
    })
  })
  
  # Import bar chart
  observeEvent(input$im_bar, {
    output$thai_imports_chart <- renderPlotly({
      plot_ly(import_data_clean, x = ~year, y = ~imports, type = 'bar') %>%
        layout(title = "Import of goods and services (% GDP annual)")
    })
  })
  
  # Export Bar chart
  observeEvent(input$ex_bar, {
    output$thai_export_chart <- renderPlotly({
      plot_ly(export_data_clean, x = ~year, y = ~export, type = 'bar') %>%
        layout(title = "Export of goods and services (% GDP annual)")
    })
  })
}


# Run the app ----
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
