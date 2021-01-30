library(shiny)
library(tidyverse)
dispo_alim <- read.csv("dispo_alim.csv", header=TRUE)
# Define UI
ui <- fluidPage(
  # Application title
    titlePanel(title = h1("FAO",align="center")),
  
    sidebarLayout(
  
      # Sidebar panel
      sidebarPanel(
      
        # Select Input
        selectInput("country", 
                    label ="Choisir le pays", 
                    choices = as.vector(pays_sous_nutrition$Area),
                    selected = c("Viet Nam"), 
                    selectize = TRUE, 
                    multiple = TRUE),
        
        selectInput("year", 
                    label = "Choisir l'annee:",
                    choices = c(2014,2015,2016,2017),
                    selected = 2014)
      
      ),
  # Main Panel
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("Top produits export",plotOutput("Top_produits_export")),
                  tabPanel("Top countries import",plotOutput("Top_countries_import")),
                  tabPanel("Top import by produits", plotOutput("Import_by_produits")),
                  tabPanel("Text", textOutput("Text")),
                  tabPanel("Table", tableOutput("Table"))
      )
    )
  )
)

# Define server
server <- shinyServer( function(input, output) {
  
  list_country <- reactive({as.character(input$country)}) 
  output$Text <- renderText({
    paste(list_country())
  })
  
  year_chosen <- reactive({as.numeric(input$year)}) 
  
  output$Table <- renderTable({
    total_export_country <- dispo_alim %>% select(country, year, item, export_quantity) %>% filter(country %in% list_country()) %>% 
    group_by(year,item) %>% summarise(total_export=sum(export_quantity,na.rm=TRUE))
  })
  
  output$Top_produits_export <- renderPlot({
    total_export_country <- dispo_alim %>% select(country, year, item, export_quantity) %>% filter(country %in% list_country() & year==year_chosen()) %>% 
    group_by(item) %>% summarise(total_export=sum(export_quantity,na.rm=TRUE)) %>% arrange(desc(total_export)) %>% head(15)
    
    ggplot(main="Top_produits_export",data=total_export_country, aes(y=reorder(item, total_export), x=total_export,fill = total_export)) +
    geom_bar(stat="identity") + xlab("Total Export Value") + ylab("Item")
  })
  
  output$Top_countries_import <- renderPlot({
    total_export_country <- dispo_alim %>% select(country, year, item, export_quantity) %>% filter(country %in% list_country() & year==year_chosen()) %>% 
    group_by(item) %>% summarise(total_export=sum(export_quantity,na.rm=TRUE)) %>% arrange(desc(total_export)) %>% head(15)
    
    top_import_country <- dispo_alim %>% filter(year==year_chosen() & item %in% total_export_country$item) %>%
    arrange(desc(import_quantity)) %>% head(75) 
  
    ggplot(main="Top_countries_import",data=top_import_country, aes(y=reorder(country, import_quantity), x=import_quantity,fill = item)) +
    geom_bar(stat="identity") + xlab("Import Value") + ylab("Country")
  })
  
  output$Import_by_produits <- renderPlot({
    total_export_country <- dispo_alim %>% select(country, year, item, export_quantity) %>% filter(country %in% list_country() & year==year_chosen()) %>% 
    group_by(item) %>% summarise(total_export=sum(export_quantity,na.rm=TRUE)) %>% arrange(desc(total_export)) %>% head(15)
    
    top_import_country <- dispo_alim %>% filter(year==year_chosen() & item %in% total_export_country$item) %>%
    arrange(desc(import_quantity)) %>% head(75) 
    
    import_by_produits <- top_import_country %>% group_by(item) %>% summarise(import_quantity=sum(import_quantity,na.rm=TRUE))
  
    ggplot(main="Import_by_produits",data=import_by_produits, aes(y=reorder(item, import_quantity), x=import_quantity, fill = item)) +
    geom_bar(stat="identity") + xlab("Import Value") + ylab("Item")
  })

})

# Run the application 
shinyApp(ui = ui, server=server)

