#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyalert)
library(shinyBS)
library(DT)

# Define UI for application

ui <- tagList(
  navbarPage(theme = "cerulean", "ASOACCTASCO (v0.1)", 
             tabPanel("Introducción/Presentación"),
             tabPanel('Importar/Guardar Información',
                      icon = icon("file"), 
                      sidebarPanel(width = 4,
                                   h2("Importar Información"),
                                   fileInput("file", "Archivo Entrada:"), 
                                   checkboxInput('header', ' Header?', TRUE),
                                   checkboxInput('rownames', ' Row names?', FALSE),
                                   selectInput('sep', 'Separator:',
                                               c("Comma","Semicolon","Tab","Space"), 'Comma'),
                                   selectInput('quote', 'Quote:',
                                               c("None","Double Quote","Single Quote"),
                                               'Double Quote'),
                                   selectInput('dec', 'Decimal mark', c("Period", "Comma"),
                                               'Period'),
                                   numericInput('nrow.preview','Number of rows in the preview:',20),
                                   numericInput('ncol.preview', 'Number of columns in the preview:',
                                                10)),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Vista Previa"),
                          tabPanel("Resumen Estadístico")
                        )), 
                     ),                  
             tabPanel("Abastecimiento y Tratamiento",
                      icon = icon("chart-bar"),
                      sidebarPanel(
                        h2("Abastecimiento y Tratamiento")
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Manejo de Cuenca", 
                                   h2("Manejo de Cuenca"), 
                                   br(),
                                   DTOutput("manejo_cuenca"),
                                   # column(3, 
                                   #        textInput("poblacion_capacitada", label = "Población Capacitada", value = "0"),
                                   #        bsTooltip(id = "poblacion_capacitada", 
                                   #                  title = "Indica el número de personas que han sido capacitadas en 
                                   #                  talleres o cursos ambientales sobre la protección y manejo de la cuenca"),
                                   #        textInput("areas_proteccion", label = 'Areas dedicadas a protección', value = "0"),
                                   #        #bsTooltip(id = "areas_proteccion", 
                                   #        #          title = "Cantidad de áreas que han sido declaradas bajo alguna categoría de 
                                   #        #          manejo en la cuenca por parte de la corporación en un año"),
                                   #        textInput("riesgos_incendios", label = "Sitios de Riesgo Incendios Forestales", value = '0'),
                                   #        textInput("indice_deforestacion", label = "Indice Deforestación", value = '0')),
                                   # column(3, 
                                   #        textInput("areas_recuperacion", label = 'Areas destinadas Recuperación', value = '0'), 
                                   #        textInput("fuentes_agua", label = 'Fuentes de Agua', value = '0'), 
                                   #        textInput("arboles_planteados", label = "Arboles Planteados por Ha", value = '0'), 
                                   #        textInput("superficie_reforestacion", label = 'Superficie Reforestación', value = '0'))
                          ),
                          tabPanel("Captación", 
                                   icon = icon("faucet"), 
                                   h2("Captación"), 
                                   br(),
                                   DTOutput("captacion")),
                                   # textInput("caudal", label = "Caudal(l/s)", value = '0'), 
                                   # textInput("caudal_medio_mensual", label = 'Caudal Medio Mensual (l/s)', value = '0'), 
                                   # textInput("caudal_medio_anual", label = "Caudal Medio ANual (l/s)", value = '0'),
                                   # textInput("Agua Captada", label = "Agua Captada", value = '0')), 
                          tabPanel("Aducción", 
                                   h2("Aducción"), 
                                   br(),
                                   DTOutput("aduccion")),
                          tabPanel("Tratamiento", 
                                   h2("Tratamiento"), 
                                   br(), 
                                   DTOutput("tratamiento"))
                        )
                      )),
             tabPanel("Conducción",
                      icon = icon("list-alt"),
                      sidebarPanel(
                        h2("Conducción")
                      ), 
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Almacenamiento", 
                                   h2("Almacenamiento"), 
                                   br(),
                                   DTOutput("almacenamiento")),
                          tabPanel("Distribución", 
                                   h2("Distribución"), 
                                   br(),
                                   DTOutput('distribucion'))
                        )
                      )), 
             tabPanel("Prestación del Servicio", 
                      sidebarPanel(
                        h2("Prestación del Servicio")
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Facturación", 
                                   h2("Facturación"), 
                                   br(),
                                   DTOutput("facturacion")),
                          tabPanel("Recaudo", 
                                   h2("Recaudo"),
                                   br(),
                                   DTOutput("recaudo")),
                          tabPanel("Suspensión y Reinstalación", 
                                   h2("Suspensión y Reinstalación"),
                                   br(),
                                   DTOutput('suspension')),
                          tabPanel("Atención al Beneficiario",
                                   h2("Atención al Beneficiario"),
                                   br(),
                                   DTOutput("atencion")),
                          tabPanel("Gestión Talento Humano", 
                                   h2("Gestión del Talento Humano"),
                                   br(),
                                   DTOutput("gestion"))
                        )
                      )), 
             tabPanel("Contable", 
                      sidebarPanel(
                        h2("Contable")
                      ), 
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Gestión Financiera y Contable", 
                                   h2("Gestión Financiera y Contable"), 
                                   br(),
                                   DTOutput("contable"))
                        )))
))


# ui <- shinyUI(pageWithSidebar(
#   # Application title
#   headerPanel("PROGRAMA DE FORTALECIMIENTO ACUEDUCTOS COMUNITARIOS ASOACCTASCO (v0.1)"), 
#   sidebarPanel(width = 3,
#                useShinyalert(force = TRUE),
#                imageOutput("uptc.logo", inline=TRUE),
#                p(HTML("<h5>This is PumpingTest-GUI, the Shiny interface for analysis and
#             evaluation of pumping and slug test data in <strong>R</strong>.</h5>
#             This application can be used for the identification of the type of
#             aquifer, estimation of hydraulic parameters, diagnosis of the estimated
#             model, uncertainty quantification of the estimated parameters, and
#             evaluation of drawdown in space and time using the  <strong>R</strong>
#             package  <a href='http://www.github.com/khaors/pumpingtest'>pumpingtest</a>.
#             The analysis of a pumping/slug test is achieved in six simple steps using the
#             panels on the right."))
#   ), 
#   mainPanel(
#     tabsetPanel(
#       tabPanel("Abastecimiento y Tratamiento", 
#                h2("Manejo de Cuenca"), 
#                h2("Captación"), 
#                h2("Aducción"),
#                h2("Tratamiento")),
#       tabPanel("Conducción", 
#                h2("Almacenamiento"), 
#                h2("Distribución")),
#       tabPanel("Prestación del Servicio", 
#                h2("Facturación"), 
#                h2("Recaudo"), 
#                h2("SUSPENSIÓN Y REINSTALACIÓN"), 
#                h2("ATENCIÓN AL BENEFICIARIO"), 
#                h2("GESTIÓN TALENTO HUMANO"))
#       #tabPanel("Manejo de Cuenca"),
#       #tabPanel("Captación"), 
#       #tabPanel("Aducción"),
#       #tabPanel("Tratamiento")
#     )
#   )))
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
#
v1.names <- c("Población Capacitada", "Areas dedicadas a Protección", "Sitios Riesgo Incendios", 
              "Indice de Deforestación", "Areas Recuperación", "Fuentes Agua", "Arboles Plantados", 
              "Superficie Reforestación")
v1.values <- c(35,10,10,50,3,5,3,3)
seed <- 12345
set.seed(seed)
#
v2.names <- c("Caudal",  "Caudal Medio Mensual", 
"Caudal Medio Anual",
"Agua captada",
"Mantenimiento de estructuras", 
"Reparación Tuneles y Ductos")
# Aducion
v3.names <- c("Cantidad de Agua en Aducción", 
              "Indice de remoción y manejo de sólidos",
              "Mantenimiento de estructuras", 
              "Reparación Tuneles y Ductos")
# tratamiento
v4.names <- c("Indice de Tratamiento de Agua",
              "Indice de Riesgo de la Calidad del Agua",
              "Mantenimiento de estructuras", 
              "Capacidad Instalada Planta de Tratamiento",
              "Indice de eficiencia planta de tratamiento")
#almacenamiento
v5.names <- c("Agua tratada almacenada",
              "Mantenimiento de estructuras", 
              "Indice de almacenamiento")
# Distribution
v6.names <- c("Reparación y Matenimiento de Redes",
              "CUMPLIMIENTO DE COBERTURA",
              "Indice de Cierres", 
              "INDICE DE CONTINUIDAD", 
              "Indice de Rehabilitación de Redes",
              "Indice de Consumo")
# Facturacion
v7.names <- c("Tarifa promedio", 
              "Indice de Micromedición",
              "Indice de Efectividad de la Entrega",
              "Indice de Inconsistencias")
# Recaudo
v8.names <- c("EFICIENCIA DEL RECAUDO (ER)",
              "Cartera Vencida", 
              "Indice de Recuperación de Cartera",
              "Indice de Usuarios en Mora")
# Suspension
v9.names <- c("Suspenciones Aprobadas",
              "Indice de suspención",
              "Reinstalaciones Programadas",
              "Indice de Reinstalacion", 
              "Ingresos por Reinstalación")
# Atencion
v10.names <- c("PQRS",
               "PQRS Gestionadas",
               "Indice PQRS",
               "Indice de Eficiencia PQR",
               "Indice de Quejas por acometida",
               "Indice de Quejas por continuidad",
               "Indice de Quejas por facturación",
               "Eficacia de ACPM")
# Gestion
v11.names <- c("Indice de Formación y Capacitación (horas/empleado/año)",
               "Accidentes de trabajo",
               "Accidentes de trabajo Nuevos",
               "Tasa de Incidencia",
               "Tasa de Prevalencia")
# Contable
v12.names <- c("INDICE DE LIQUIDEZ",
               "COEFICIENTE DE CUBRIMIENTO DE COSTOS")
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$uptc.logo <- renderImage(list(src = "uptc_jpg.jpg"),
                                  deleteFile = FALSE)
  v <- reactiveValues(data = { 
    data.frame(variables = v1.names, 
               P1 = c(round(v1.values*runif(1))), 
               P2 = c(round(v1.values*runif(1))), 
               P3 = c(round(v1.values*runif(1))), 
               P4 = c(round(v1.values*runif(1))), 
               P5 = c(round(v1.values*runif(1))),
               P6 = c(round(v1.values*runif(1))),
               P7 = c(round(v1.values*runif(1))),
               P8 = c(round(v1.values*runif(1))),
               P9 = c(round(v1.values*runif(1))),
               P10 = c(round(v1.values*runif(1))),
               P11 = c(round(v1.values*runif(1))),
               P12 = c(round(v1.values*runif(1)))
               )})
  #
  v1 <- reactiveValues(data = { 
    data.frame(variables = v2.names, 
               P1 =rep(0,length(v2.names)), 
               P2 = rep(0,length(v2.names)), 
               P3 = rep(0,length(v2.names)), 
               P4 = rep(0,length(v2.names)), 
               P5 = rep(0,length(v2.names)),
               P6 = rep(0,length(v2.names)),
               P7 = rep(0,length(v2.names)),
               P8 = rep(0,length(v2.names)),
               P9 = rep(0,length(v2.names)),
               P10 = rep(0,length(v2.names)),
               P11 = rep(0,length(v2.names)),
               P12 = rep(0,length(v2.names))
               )})
  #
  v2 <- reactiveValues(data = {
    data.frame(variables = v3.names, 
               P1 =rep(0,length(v3.names)), 
               P2 = rep(0,length(v3.names)), 
               P3 = rep(0,length(v3.names)), 
               P4 = rep(0,length(v3.names)), 
               P5 = rep(0,length(v3.names)),
               P6 = rep(0,length(v3.names)),
               P7 = rep(0,length(v3.names)),
               P8 = rep(0,length(v3.names)),
               P9 = rep(0,length(v3.names)),
               P10 = rep(0,length(v3.names)),
               P11 = rep(0,length(v3.names)),
               P12 = rep(0,length(v3.names))
               
  )})
  #
  v3 <- reactiveValues(data = {
    data.frame(variables = v4.names, 
               P1 =rep(0,length(v4.names)), 
               P2 = rep(0,length(v4.names)), 
               P3 = rep(0,length(v4.names)), 
               P4 = rep(0,length(v4.names)), 
               P5 = rep(0,length(v4.names)),
               P6 = rep(0,length(v4.names)),
               P7 = rep(0,length(v4.names)),
               P8 = rep(0,length(v4.names)),
               P9 = rep(0,length(v4.names)),
               P10 = rep(0,length(v4.names)),
               P11 = rep(0,length(v4.names)),
               P12 = rep(0,length(v4.names))
               )})
  # Almacenamiento
  v4 <- reactiveValues(data = {
    data.frame(variables = v5.names,
               P1 =rep(0,length(v5.names)),
               P2 = rep(0,length(v5.names)),
               P3 = rep(0,length(v5.names)),
               P4 = rep(0,length(v5.names)),
               P5 = rep(0,length(v5.names)),
               P6 = rep(0,length(v5.names)),
               P7 = rep(0,length(v5.names)),
               P8 = rep(0,length(v5.names)),
               P9 = rep(0,length(v5.names)),
               P10 = rep(0,length(v5.names)),
               P11 = rep(0,length(v5.names)),
               P12 = rep(0,length(v5.names))
    )})
  # Distribution
  v5 <- reactiveValues(data = {
    data.frame(variables = v6.names,
               P1 =rep(0,length(v6.names)),
               P2 = rep(0,length(v6.names)),
               P3 = rep(0,length(v6.names)),
               P4 = rep(0,length(v6.names)),
               P5 = rep(0,length(v6.names)),
               P6 = rep(0,length(v6.names)),
               P7 = rep(0,length(v6.names)),
               P8 = rep(0,length(v6.names)),
               P9 = rep(0,length(v6.names)),
               P10 = rep(0,length(v6.names)),
               P11 = rep(0,length(v6.names)),
               P12 = rep(0,length(v6.names))
    )})
  # Facturacion
  v6 <- reactiveValues(data = {
    data.frame(variables = v7.names,
               P1 =rep(0,length(v7.names)),
               P2 = rep(0,length(v7.names)),
               P3 = rep(0,length(v7.names)),
               P4 = rep(0,length(v7.names)),
               P5 = rep(0,length(v7.names)),
               P6 = rep(0,length(v7.names)),
               P7 = rep(0,length(v7.names)),
               P8 = rep(0,length(v7.names)),
               P9 = rep(0,length(v7.names)),
               P10 = rep(0,length(v7.names)),
               P11 = rep(0,length(v7.names)),
               P12 = rep(0,length(v7.names))
    )})
  # Recaudo
  v7 <- reactiveValues(data = {
    data.frame(variables = v8.names,
               P1 =rep(0,length(v8.names)),
               P2 = rep(0,length(v8.names)),
               P3 = rep(0,length(v8.names)),
               P4 = rep(0,length(v8.names)),
               P5 = rep(0,length(v8.names)),
               P6 = rep(0,length(v8.names)),
               P7 = rep(0,length(v8.names)),
               P8 = rep(0,length(v8.names)),
               P9 = rep(0,length(v8.names)),
               P10 = rep(0,length(v8.names)),
               P11 = rep(0,length(v8.names)),
               P12 = rep(0,length(v8.names))
    )})
  # Suspension
  v8 <- reactiveValues(data = {
    data.frame(variables = v9.names,
               P1 =rep(0,length(v9.names)),
               P2 = rep(0,length(v9.names)),
               P3 = rep(0,length(v9.names)),
               P4 = rep(0,length(v9.names)),
               P5 = rep(0,length(v9.names)),
               P6 = rep(0,length(v9.names)),
               P7 = rep(0,length(v9.names)),
               P8 = rep(0,length(v9.names)),
               P9 = rep(0,length(v9.names)),
               P10 = rep(0,length(v9.names)),
               P11 = rep(0,length(v9.names)),
               P12 = rep(0,length(v9.names))
    )})
  # Atencion
  v9 <- reactiveValues(data = {
    data.frame(variables = v10.names,
               P1 =rep(0,length(v10.names)),
               P2 = rep(0,length(v10.names)),
               P3 = rep(0,length(v10.names)),
               P4 = rep(0,length(v10.names)),
               P5 = rep(0,length(v10.names)),
               P6 = rep(0,length(v10.names)),
               P7 = rep(0,length(v10.names)),
               P8 = rep(0,length(v10.names)),
               P9 = rep(0,length(v10.names)),
               P10 = rep(0,length(v10.names)),
               P11 = rep(0,length(v10.names)),
               P12 = rep(0,length(v10.names))
    )})
  # Gestion
  v10 <- reactiveValues(data = {
    data.frame(variables = v11.names,
               P1 =rep(0,length(v11.names)),
               P2 = rep(0,length(v11.names)),
               P3 = rep(0,length(v11.names)),
               P4 = rep(0,length(v11.names)),
               P5 = rep(0,length(v11.names)),
               P6 = rep(0,length(v11.names)),
               P7 = rep(0,length(v11.names)),
               P8 = rep(0,length(v11.names)),
               P9 = rep(0,length(v11.names)),
               P10 = rep(0,length(v11.names)),
               P11 = rep(0,length(v11.names)),
               P12 = rep(0,length(v11.names))
    )})
  # Contable
  v11 <- reactiveValues(data = {
    data.frame(variables = v12.names,
               P1 =rep(0,length(v12.names)),
               P2 = rep(0,length(v12.names)),
               P3 = rep(0,length(v12.names)),
               P4 = rep(0,length(v12.names)),
               P5 = rep(0,length(v12.names)),
               P6 = rep(0,length(v12.names)),
               P7 = rep(0,length(v12.names)),
               P8 = rep(0,length(v12.names)),
               P9 = rep(0,length(v12.names)),
               P10 = rep(0,length(v12.names)),
               P11 = rep(0,length(v12.names)),
               P12 = rep(0,length(v12.names))
    )})
  
  #output the datatable based on the dataframe (and make it editable)
  output$manejo_cuenca <- renderDT({
    DT::datatable(v$data, editable = TRUE)
  })
  #
  output$captacion <- renderDT({
    DT::datatable(v1$data, editable = TRUE)
  })
  #
  output$aduccion <- renderDT({
    DT::datatable(v2$data, editable = TRUE)
  })
  #
  output$tratamiento <- renderDT({
    DT::datatable(v3$data, editable = TRUE)
  })
  #
  output$almacenamiento <- renderDT({
   DT::datatable(v4$data, editable = TRUE)
  })
  #
  output$distribucion <- renderDT({
    DT::datatable(v5$data, editable = TRUE)
  })
  #
  output$facturacion <- renderDT({
    DT::datatable(v6$data, editable = TRUE)
  })
  #
  output$recaudo <- renderDT({
    DT::datatable(v7$data, editable = TRUE)
  })
  #
  output$suspension <- renderDT({
    DT::datatable(v8$data, editable = TRUE)
  })
  #
  output$atencion <- renderDT({
    DT::datatable(v9$data, editable = TRUE)
  })
  #
  output$gestion <- renderDT({
    DT::datatable(v10$data, editable = TRUE)
  })
  # Contable
  output$contable <- renderDT({
    DT::datatable(v11$data, editable = TRUE)
  })
  
  #when there is any edit to a cell, write that edit to the initial dataframe
  #check to make sure it's positive, if not convert
  observeEvent(input$manejo_cuenca_cell_edit, {
    #get values
    info = input$manejo_cuenca_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    v$data[i,j] <- k
    #cmm$data[i,j] <- k
  })
  #
  observeEvent(input$captacion_cell_edit, {
    #get values
    info = input$captacion_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    v1$data[i,j] <- k
    #cmm$data[i,j] <- k
  })
  #
  observeEvent(input$aduccion_cell_edit, {
    #get values
    info = input$aduccion_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    v2$data[i,j] <- k
    #cmm$data[i,j] <- k
  })
  #
  observeEvent(input$tratamiento_cell_edit, {
    #get values
    info = input$tratamiento_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    v3$data[i,j] <- k
    #cmm$data[i,j] <- k
  })
  
  

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
