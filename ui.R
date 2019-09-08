
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(markdown)
library(DT)

navbarPage("ANOVA BCA",
           
 tabPanel("Datos",
  column(3,
         wellPanel(
      fileInput('file1', 'Choose xlsx file',
                accept = c(".xlsx")
      ),
      tags$label("Tratamientos", style="color:#ceddfe"),
      uiOutput("tratamientos"),
      tags$label("Bloques/Rep", style="color:#91b3fe"),
      uiOutput("bloques"),
      tags$label("Variable Dependiente", style="color:#6393fd"),
      uiOutput("dependiente")
     
    
    ),
    wellPanel(
      
      numericInput("n4", "N° de decimales:",
                   min = 0, max = 20, value = 1, step = 1),
      numericInput("n6", "Valor Alfa %:",
                   min = 0, max = 20, value = 0.1, step = 0.05),
      textInput("n5", "Unidad Variable Dependiente",value = "Rendimiento (kg/ha)"),
      textInput("n7", "Tratamiento Testigo",value = "T1")),
      wellPanel(
        uiOutput("row"),
        uiOutput("col")
      )
      
    ),

    # Show a plot of the generated distribution
    column(9,
      dataTableOutput('contents'),
      plotOutput("plano")
    )),

 
 tabPanel("Análisis",
           column(6,
                  fluidRow(
                    column(12,tags$b("Tabla de Promedios"), dataTableOutput("table"))),
                  fluidRow(
                    column(12,tags$br(),tags$b("Coeficiente de variación"),  verbatimTextOutput("cv"))),
                  fluidRow(
                    column(12,tags$br(),tags$b("Tabla ANOVA"),  verbatimTextOutput("anova"))),
                  fluidRow(
                    column(12,tags$b("Tabla LSD Fisher "),  verbatimTextOutput("LSD")))
         
),
column(6,
  tabsetPanel(type = "tabs",
              tabPanel("Tratamientos",
                fluidRow(
                  column(12, tags$br(),plotOutput("plots"))),
                fluidRow(tags$b("BoxPlot"), verbatimTextOutput("boxplot"),tags$p(
                  "- La linea, dentro de la caja, es la mediana de los datos.",
                  tags$br("- Caja, representa el rango intercuartilico (del 25% al  75% de los datos)contiene el 50% de los datos."),
                  ("- Bigotes, las lineas salidas al final de las lineas de guiones que se extienden desde la caja. Definen los limites mas alla de los cuales consideramos los valores como atipicos. Existen diferentes modos de calcularlos."),  
                  tags$br("- Valores atipicos, aquellos puntos fuera de los bigotes. Se representan como puntos o circulos.")))
                ),
              tabPanel("Bloques",tags$br(), plotOutput("plotsblq")),
              tabPanel("LSD - Fisher", tags$br(),plotOutput("plotlsd")),
              tabPanel("Tabla de Datos", tags$br(),dataTableOutput("tabla"))
  )
)),
tabPanel("Comparaciones Multiples",
         column(3,
                wellPanel(
                  selectInput ("compMult", 
                               label = "Prueba",
                               choices = c('Pruebas...'='',
                                           'LSD'='LSD',
                                           'Tukey'='tukey',
                                           'Duncan'='duncan',
                                           'Student-Newman-Keuls'='SNK',
                                           'Scheffé'='scheffé',
                                           'Bonferroni'='bonferroni')),
                  tags$div( uiOutput('compMultDesc'))
                )
          ),
         column(9,
                tabsetPanel(type = "tabs",
                            tabPanel("Tabla", tags$br(),verbatimTextOutput("compMultTabla")
                                     
                                       ),
                            tabPanel("Bloques",tags$br())
                )
         )),
tabPanel("Supuestos Variancia",
         tabsetPanel(type = "tabs",
            tabPanel("Residuales Normalmente Distribuidos", 
              fluidRow(
                column(5, plotOutput("disres")),
              column(4,plotOutput("hista"),
                     tags$br(),tags$b("Prueba de Normalidad (shapiro-wilks modificado)"),
                     verbatimTextOutput("shap"),
                     tags$div("Ho: Los residuos siguen la distribución normal"),
                       
                        tags$div("Ha: Los residuos no siguen la distribución normal"),
                        tags$br(),
                        tags$div("p-value > 0,05 no puedo rechazar Ho, Residuos con distribucion Normal ")
                     )     
            )),
            tabPanel("Varianzas Homogeneas",
              fluidRow(
                column(6,tags$br(),tags$div(tags$b("Predichos vs Residuos"), align = "center"),plotOutput("homo")),
                column(6,tags$br(),tags$div(tags$b("Residuos vs Tratamientos"), align = "center"),plotOutput("resi"),
                       tags$b("Test de Barlett "),tags$br(),verbatimTextOutput("test"),tags$div("p-value > 0,05 Varianza Homogenea"))
            )),
            tabPanel("Aditividad Bloque - Tratamiento ",
                     column(6,tags$br(),tags$br(),plotOutput("aditi")
            )),
            tabPanel("Errores independientes -> Repeticiones",
                     fluidRow(
            column(6,tags$br(),tags$div(tags$b("Independencia"), align = "center"),plotOutput("inde")))

)
)),
navbarMenu("More",
           tabPanel("Exportar",
                  downloadButton("report", "Generate report")
                    
                    ),
           tabPanel("Contacto: pasquinelli@a-w-a.com.ar"))
)