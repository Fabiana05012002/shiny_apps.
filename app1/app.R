library(shiny)
library(shinydashboard)
library(ggplot2)
ui <- dashboardPage(
  dashboardHeader(title = "Demo Shiny Dashboard", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Datos", tabName = "datos", icon = icon("table")),
      menuItem("Gráficos", tabName = "grafico", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        title = "Contenido de la pestaña Datos",
        tabName = "datos",
        tabsetPanel(
          tabPanel("Nombre de país",
                   h3("Lista de países"),
                   selectInput("Pais", "Escoja el país", choices = NULL, selected = NULL),
                   selectInput("año", "Escoja el año", choices = NULL, selected = NULL),
                   actionButton("filtro_pais", "Filtrar"),
                   dataTableOutput("tabla_pais")
          ),
          tabPanel("Códigos",
                   h3("Lista de códigos"),
                   selectInput("Codigo", "Escoja el código del país", choices = NULL, selected = NULL),
                   actionButton("filtro_codigo", "Filtrar"),
                   dataTableOutput("tabla_codigo")
          ),
          tabPanel("Años",
                   h3("Lista de años"),
                   selectInput("Año", "Escoja el año", choices = NULL, selected = NULL),
                   actionButton("filtro_año", "Filtrar"),
                   dataTableOutput("tabla_año")
          )
        )
      ),
      tabItem(
        title = "Contenido de la pestaña Gráficos",
        tabName = "grafico",
        tabsetPanel(
          
          tabPanel("Variables de Hombre",
                   br(),
                   selectInput("columna_Hombre", "Selecciona la columna para Hombre", choices = NULL, selected = NULL),
                   plotOutput("plot_hombres")
          ),
          
          tabPanel("Variables de Mujer",
                   br(),
                   selectInput("columna_Mujer", "Selecciona la columna para Mujer", choices = NULL, selected = NULL),
                   plotOutput("plot_mujeres")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  datos_empleo_genero <- read.csv("C:/Users/Fabi Hidalgo/Desktop/CETAV/PROGRAMACIÓN II/shinyApp/app1/datos/datos_empleo_genero.csv")
  #Se debe corregir la dirección especifica
  
  observe({
    updateSelectInput(session, "Pais", 
                      choices = unique(datos_empleo_genero$pais_region))
    
    updateSelectInput(session, "año", 
                      choices = unique(datos_empleo_genero$anyo))
    
    updateSelectInput(session, "Codigo", 
                      choices = unique(datos_empleo_genero$codigo_pais_region))
    
    updateSelectInput(session, "Año", 
                      choices = unique(datos_empleo_genero$anyo))
    
    updateSelectInput(session, "columna_Mujer", 
                      choices = c(
                        "empleadoras_mujeres", "empleo_agricultura_mujeres",
                        "empleo_industria_mujeres", "empleo_servicios_mujeres",
                        "empleo_informal_mujeres", "autoempleo_mujeres",
                        "desempleo_educacion_mujeres", "desempleo_mujeres",
                        "trabajo_domestico_no_remunerado_mujeres"
                      ))
    updateSelectInput(session, "columna_Hombre", choices = c(
      "empleadores_hombres", "empleo_agricultura_hombres",
      "empleo_industria_hombres", "empleo_servicios_hombres",
      "empleo_informal_hombres", "autoempleo_hombres",
      "desempleo_educacion_hombres", "desempleo_hombres",
      "trabajo_domestico_no_remunerado_hombres"
    ))
  })
  
  observeEvent(input$filtro_pais, {
    output$tabla_pais <- renderDataTable({  
      datos_filtrados <- datos_empleo_genero[
        datos_empleo_genero$pais_region == input$Pais & datos_empleo_genero$anyo == input$año, ]
    }, options = list(scrollX = TRUE))  
  })
  
  observeEvent(input$filtro_codigo, {
    output$tabla_codigo <- renderDataTable({
      
      datos_filtrados <- datos_empleo_genero[
        datos_empleo_genero$codigo_pais_region == input$Codigo, ]
      
      
    }, options = list(scrollX = TRUE))
  })
  
  observeEvent(input$filtro_año, {
    output$tabla_año <- renderDataTable({
      
      datos_filtrados <- datos_empleo_genero[
        datos_empleo_genero$anyo == input$Año, ]
      
    }, options = list(scrollX = TRUE))
  })
  
  output$plot_hombres <- renderPlot({
    if (!is.null(input$columna_Hombre)) {
      
      datos_ordenados <- datos_empleo_genero[order(-datos_empleo_genero[[input$columna_Hombre]]), ]
      
      
      top_5_hombres <- head(datos_ordenados, 3)
      
      
      ggplot(top_5_hombres, aes(x = reorder(pais_region, -get(input$columna_Hombre)), y = get(input$columna_Hombre))) +
        geom_bar(stat = "identity", fill = "green", color = "black") +
        labs(title = "Top tres de Valores Más Altos para Hombres",
             y= "Cantidad",
             x= "Pais o Region")
    }
  })
  
  output$plot_mujeres <- renderPlot({
    if (!is.null(input$columna_Mujer)) {
      
      datos_ordenados <- datos_empleo_genero[order(-datos_empleo_genero[[input$columna_Mujer]]), ]
      
      
      top_5_mujeres <- head(datos_ordenados, 3)
      
      ggplot(top_5_mujeres, aes(x = reorder(pais_region, -get(input$columna_Mujer)), y = get(input$columna_Mujer))) +
        geom_bar(stat = "identity", fill = "pink", color = "black") +
        labs(title = "Top tres de Valores Más Altos para Mujeres",
             y= "Cantidad",
             x= "Pais o Region")
    }
  })
}

shinyApp(ui, server)
