library(shiny)
require(nlme)
library(Hmisc)
library(blme)
library(cluster)
library(knitr)
library(kml)
library(ggplot2)
library(dplyr)
library(rmarkdown)
library(knitr)
library(agricolae)
library(shiny)
library(rsconnect)
withMathJax()

##titulo de laaplicacion_______________________________________
shinyUI(fluidPage(
  titlePanel( "Análisis de datos estudiantiles", "ITM"),
      navbarPage("",
                 tabPanel("Introducción",
                          ### ingresar la base de datos
                              sidebarPanel(
                              conditionalPanel(
                              helpText("Herramienta para analisis de la caracterizacion y deserción estudiantil", align="center"),
                              fileInput(inputId="file",label=h5("Ingrese los datos en formato.txt"),
                                        accept=c('text/csv','text/comma-separated-values',
                                                 'text/plain','.csv','.txt')),  
                              
                              selectInput("facultad", "Escoja la facultad",c(Choose='')),
                              selectInput("Programa", "Escoja el programa"," "),
                              selectInput("tipo", "Escoja el tipo de programa", " "),
                              HTML('</br>'),
                              uiOutput('dv'),    
                              HTML('</br>'),
                              uiOutput('iv'),
                              HTML('</br>'),
                              
                              radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                           inline = TRUE),
                              downloadButton('downloadReport'), 
                              includeHTML('help.html'),
                              img(src="itm.png",height = 75, width =150, align="center")
                                )
                            ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Datos",
                                       HTML("</br>Select a data set from the 'Choose a dataset 
                                            menu' or enter your own data below </br> </br>"),
                                       fluidRow(column(4,selectInput("facultad", "Escoja la facultad", 
                                                                     c("All", unique(as.character("facultad"))),c(Choose=''))),  
                                                column(4,selectInput("Programa", "Escoja el programa",
                                                                     choices=textOutput("Programa"))),
                                                column(4,selectInput("tipo", "Escoja el tipo de programa", 
                                                                     c("All", unique(as.character("tipo")))))),
                                       fluidRow(dataTableOutput("table"))
                              )
                            )
                          )
                 ),
                
                 tabPanel("Tamaños de muestra", 
                            sidebarPanel(
                              
                              selectInput("Encu", "Escoja la variable encuestado","  "),
                            sliderInput("bins1","error admisible:",
                                        min = 0.02,max=0.2,value = 0.05, step=0.005),
                                      
                          sliderInput("bins2","Nivel de confianza:",
                                      min = 0.90,max=0.99,value = 0.95, step=0.01)
                 ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Muestreo 1",
                                        h4("Información del muestreo"),
                                        tableOutput("summary4"),
                                        withMathJax( helpText(
                                        h1(' $$ n = \\frac{N z^2 p q}{(N-1)e^2+z^2 p q} \
                                                $$'))),
                                        h4("Tamaño de muestra por facultad"),
                                         dataTableOutput("contents27"),
                                         h4("Tamaño de muestra por programa"),
                                         dataTableOutput("contents26")
                                          ),
                                
                                tabPanel("Muestreo 2", tableOutput("summary2"),
                                         uiOutput('ex1'),
                                h4("Tamaño de muestra por facultad"),
                                dataTableOutput("tabb"),
                                h4("Tamaño de muestra por facultad"),
                                dataTableOutput("cont26")
                                )
                                
                                   )
                                )
                            ),
                 tabPanel("Causas de deserción",
                          
                          sidebarPanel( 
                            conditionalPanel(
                              'input.dataset === "data1"',
                              
                              selectInput("Causa", "Escoja la causa","  "),
                              selectInput("Razn", "Escoja la razn", "  "),
                              selectInput("nivel", "Escoja el nivel:", "  "),
                              selectInput("sede", "Escoja la sede:", "  "),
                              selectInput("zona", "Escoja la zona:", "  "),
                              selectInput("gen", "Escoja el genero:", "  "),
                              selectInput("civil", "Escoja el estado civil:", "  "),
                              selectInput("estrato", "Escoja la variable estrato:", "  "),
                              selectInput("edad", "Escoja la edad:", "  "),
                             # selectInput("facu", "Escoja la facultad:",
                              #            c("All",unique(as.character("facultad")))),
                              numericInput(inputId = "obs",
                                           label = "Numero de intervalos:",
                                           value = 10)
                                    )),
                          mainPanel(
                                tabsetPanel(
                                  tabPanel("Descriptiva",
                                           h4("Variable cuantitativa (edad)"),
                                           plotOutput("pred_plot14", width="100%"),
                                           dataTableOutput("tabless7"),
                                           h4("Gráfico de zona de vivienda"),
                                           plotOutput("pred_plot6", width="100%"),
                                           dataTableOutput("tabless10"),
                                           h4("Genero"),
                                           plotOutput("total_plot7",height = "600px", width="100%"),
                                           dataTableOutput("tabless9"),
                                           h4("Estado civil"),
                                           plotOutput("total_plot8",height = "600px", width="100%"),
                                           dataTableOutput("tabless8"),
                                           h4("deserción segun el tipo de programa"),
                                           plotOutput("total_plot9",height = "600px", width="100%"),
                                           dataTableOutput("tabless11"),
                                           h4("Estrato de los desertores"),
                                           plotOutput("total_plot10",height = "600px", width="100%"),
                                           dataTableOutput("tabless12")
                                           
                                           
                                              ),
                                  tabPanel("Institucional",  
                                           h4("Gráfico de causas a nivel institucional"),
                                           plotOutput("pred_plot", width="100%"),
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           dataTableOutput("tabless"), 
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           h4("Gráfico de causas y razones a nivel institucional"),
                                           plotOutput("total_plot",height = "600px", width="100%"),
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           dataTableOutput("contents_dv"),
                                           h4("Gráfico de nivel de deserción"),
                                           plotOutput("total_plot6",height = "600px", width="100%"),
                                           h4("Tabla de nivel de deserción segun tipo de programa"),
                                           verbatimTextOutput("summar21"),
                                           h4("Comparativo de causas entre facultades"),verbatimTextOutput("summary3"),
                                           h4("Comparativo de razones entre facultades"),verbatimTextOutput("summar") 
                                           
                                           ),
                                  tabPanel("Artes",
                                           #selectInput("facu1", "Escoja la facultad:",
                                            #           c("All",unique(as.character("facultad")))),
                                           h4("Gráfico de causas en la facultad de artes"),
                                           plotOutput("pred_plot2", width="100%"),
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           dataTableOutput("tabless2"),
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           plotOutput("total_plot2",height = "600px", width="100%"),
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           dataTableOutput("tablel"),
                                           h4("Nivel de deserción en los programas de la facultad de artes"),
                                           plotOutput("total_plot66",height = "600px", width="100%"),
                                           verbatimTextOutput("tabless22"), 
                                           h4("Comparación entre causas de programas tecnológicos de la facultad de artes"),
                                           verbatimTextOutput("summar5"),
                                           h4("Comparación entre razones de programas tecnológicos de la facultad de artes"),
                                           verbatimTextOutput("summar7"),
                                           h4("Gráfico de causas y razones en Diseño Industrial"),
                                           plotOutput("pred_plot23"),
                                           plotOutput("total_plot13",height = "600px", width="100%"),
                                           h4("Gráfico de causas y razones en Informática Músical"),
                                           plotOutput("pred_plot24"),
                                           plotOutput("total_plot14",height = "600px", width="100%"),
                                           h4("Comparación entre causas de programas profesionales de la facultad de artes"),
                                           verbatimTextOutput("summar6"),
                                           h4("Comparación entre razones de programas profesionales de la facultad de artes"),
                                           verbatimTextOutput("summar8"),
                                           h4("Gráfico de causas en Artes de la grabación"),
                                           plotOutput("pred_plot21"),
                                           plotOutput("total_plot11",height = "600px", width="100%"),
                                           h4("Gráfico de causas en Artes visuales"),
                                           plotOutput("pred_plot22"),
                                           plotOutput("total_plot12",height = "600px", width="100%"),
                                           h4("Gráfico de causas y razones en Ingeniería en Diseño Industrial"),
                                           plotOutput("pred_plot25"),
                                           plotOutput("total_plot15",height = "600px", width="100%")
                                           ),
                                  
                                  tabPanel("Económicas",h4("Causas en la facultad de ciencias Económicas"),
                                           plotOutput("pred_plot3", width="100%"),
                                           
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           dataTableOutput("tabless3"),
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           
                                           
                                           h4("Razones en la facultad de ciencias Económicas"),
                                           plotOutput("total_plot3",height = "600px", width="100%"),
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           dataTableOutput("tablel3"),
                                           
                                           
                                           
                                           h4("Semestre de deserción en los programas de la facultad de Ciencias Económicas"),
                                           plotOutput("total_plot67",height = "600px", width="100%"),
                                           verbatimTextOutput("tabless23"), 
                                           h4("Comparación entre causas de programas tecnológicos de la facultad de Ciencias Económicas"),
                                           verbatimTextOutput("summar9"),
                                           h4("Comparación entre razones de programas tecnológicos de la facultad de Ciencias Económicas"),
                                           verbatimTextOutput("summar10"),
                                           
                                           h4("Gráfico de causas y razones en Calidad "),
                                           plotOutput("pred_plot30"),
                                           plotOutput("total_plot30",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Gestión administrativa "),
                                           plotOutput("pred_plot31"),
                                           plotOutput("total_plot31",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Producción"),
                                           plotOutput("pred_plot32"),
                                           plotOutput("total_plot32",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Análisis de costos y resupuestos "),
                                           plotOutput("pred_plot34"),
                                           plotOutput("total_plot34",height = "600px", width="100%"),
                                           
                                           h4("Comparación entre causas de programas profesionales de la facultad de Ciencias Económicass"),
                                           verbatimTextOutput("summar11"),
                                           h4("Comparación entre razones de programas profesionales de la facultad de Ciencias Económicass"),
                                           verbatimTextOutput("summar12"),
                                           
                                           h4("Gráfico de causas y razones en Administración tecnológica"),
                                           plotOutput("pred_plot33"),
                                           plotOutput("total_plot33",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Contaduría pública"),
                                           plotOutput("pred_plot35"),
                                           plotOutput("total_plot35",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Ingeniería financiera y de negocios"),
                                           plotOutput("pred_plot36"),
                                           plotOutput("total_plot36",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Ingeniería de producción"),
                                           plotOutput("pred_plot37"),
                                           plotOutput("total_plot37",height = "600px", width="100%")
                                           ),
                                  
                                  tabPanel("Ciencias Exactas",
                                           h4("Gráfico de causas en la facultad de ciencias exactas"),
                                           plotOutput("pred_plot4", width="100%"),
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           dataTableOutput("tabless4"),
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           plotOutput("total_plot4",height = "600px", width="100%"),
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           dataTableOutput("tablel4"),
                                           h4("Nivel de deserción en los programas de la facultad de CIENCIAS EXACTAS"),
                                           plotOutput("total_plot68",height = "600px", width="100%"),
                                           verbatimTextOutput("tabless24"), 
                                           h4("Comparación entre causas de programas tecnológicos de la facultad de Ciencias exactas"),
                                           verbatimTextOutput("summar13"),
                                           h4("Comparación entre razones de programas tecnológicos de la facultad de Ciencias exactas"),
                                           verbatimTextOutput("summar14"),
                                           
                                           h4("Gráfico de causas y razones en Contrucción de Acabados Arquitectónicos"),
                                           plotOutput("pred_plot26"),
                                           plotOutput("total_plot16",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Mantenimiento de equipo biomédico"),
                                           plotOutput("pred_plot27"), 
                                           plotOutput("total_plot17",height = "600px", width="100%"),
                                           
                                           h4("Comparación entre causas de programas profesionales de la facultad de Ciencias exactas"),
                                           verbatimTextOutput("summar15"),
                                           h4("Comparación entre razones de programas profesionales de la facultad de Ciencias exactas"),
                                           verbatimTextOutput("summar16"),
                                           
                                           h4("Gráfico de causas y razones Ingeniería Biomédica"),
                                           plotOutput("pred_plot28"),
                                           plotOutput("total_plot18",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Química Industrial "),
                                           plotOutput("pred_plot29"),
                                           plotOutput("total_plot19",height = "600px", width="100%")
                                                                          ),
                                  
                                  
                                  
                                  tabPanel("Ingenierías",
                                           h4("Gráfico de causas en la facultad de ingenierías"),
                                           plotOutput("pred_plot5", width="100%"),
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           dataTableOutput("tabless5"),
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           plotOutput("total_plot5",height = "600px", width="100%"),
                                           textInput("text_summary", label = "Interpretation", value = "Enter text..."),
                                           dataTableOutput("tablel5"),
                                           h4("Nivel de deserción en los programas de la facultad de ingenierías"),
                                           plotOutput("total_plot69",height = "600px", width="100%"),
                                           verbatimTextOutput("tabless25"), 
                                           h4("Comparación entre causas de programas tecnológicos de la facultad de ingenierías"),
                                           verbatimTextOutput("summar17"),
                                           h4("Comparación entre razones de programas tecnológicos de la facultad de ingenierías"),
                                           verbatimTextOutput("summar18"),
                                           
                                           h4("Gráfico de causas y razones en Electromecánica"),
                                           plotOutput("pred_plot38"),
                                           plotOutput("total_plot38",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Electrónica"),
                                           plotOutput("pred_plot39"),
                                           plotOutput("total_plot39",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Sistemas de información"),
                                           plotOutput("pred_plot45"),
                                           plotOutput("total_plot45",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Telecomunicaciones"),
                                           plotOutput("pred_plot46"),
                                           plotOutput("total_plot46",height = "600px", width="100%"),
                                           
                                           h4("Comparación entre causas de programas profesionales de la facultad de ingenierías"),
                                           verbatimTextOutput("summar19"),
                                           
                                           h4("Comparación entre razones de programas profesionales de la facultad de ingenierías"),
                                           verbatimTextOutput("summar20"),
                                           
                                           h4("Gráfico de causas y razones en Ingeniería de Sistemas"),
                                           plotOutput("pred_plot40"),
                                           plotOutput("total_plot40",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Ingeniería Electromecánica"),
                                           plotOutput("pred_plot41"),
                                           plotOutput("total_plot41",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Ingeniería Electrónica"),
                                           plotOutput("pred_plot42"),
                                           plotOutput("total_plot42",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Ingeniería de Telecomunicaciones"),
                                           plotOutput("pred_plot43"),
                                           plotOutput("total_plot43",height = "600px", width="100%"),
                                           
                                           h4("Gráfico de causas y razones en Ingeniería Mecatrónica"),
                                           plotOutput("pred_plot44"),
                                           plotOutput("total_plot44",height = "600px", width="100%")
                                           
                                           
                                  )
                                  
                                                                  )
                                                                )),
                          tabPanel("Índices de deserción",
                                   
                                   sidebarPanel( ),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("ITM",
                                                h4("Indice de deserción"),
                                                withMathJax( helpText(
                                                  h1(' $$ Indice \\quad de \\quad desercion = \\frac{Desertores \\quad del \\quad periodo \\quad t}
                                                           {Matriculados \\quad en \\quad el \\quad periodo \\quad t} \ $$'))),
                                                h4("Gráfico de barras"),plotOutput("pred_plot20"),
                                                h4("Índices de deserción por semestre en el ITM"),
                                                dataTableOutput("tabless6"),
                                                h4("Gráfico comparativo de índices de deserción por Facultad"),
                                                plotOutput("pred_plot15", width="100%"),
                                                h4("Índices de deserción por semestre y facultad"),
                                                dataTableOutput("tabless1"),
                                                h4("Índices de deserción por semestre y programa"),
                                                dataTableOutput("contents99")
                                                ),
                                       tabPanel("Artes",
                                                h4("Índice de deserción de la facultad de Artes y humanidades"),
                                                plotOutput("pred_plot19"),
                                                h4("Programas profesionales de Artes"),plotOutput("pred_plot1"),
                                                h4("Programas tecnológicos de Artes"),plotOutput("pred_plot7")
                                       ),
                                       tabPanel("Ciencias Económicas",
                                                h4("Índice de deserción de la facultad de ciencias económicas"),plotOutput("pred_plot18"),
                                                h4("Programas profesionales de ciencias Económicas "),plotOutput("pred_plot8"),
                                                h4("Programas tecnológicos de ciencias Económicas"),plotOutput("pred_plot9")
                                       ),
                                       tabPanel("Ciencias Exactas",
                                                h4("Índice de deserción de la facultad de ciencias exactas"),
                                                plotOutput("pred_plot17"),
                                                h4("Programas profesionales de ciencias exactas"),plotOutput("pred_plot10"),
                                                h4("Programas tecnológicos de ciencias exactas"),plotOutput("pred_plot11")
                                       ),
                                       tabPanel("Ingenierías",
                                                h4("Índice de deserción de la facultad de ingenierías"),plotOutput("pred_plot16"),
                                                h4("Programas profesionales de Ingenierías "),plotOutput("pred_plot12"),
                                                h4("Programas tecnológicos de Ingenierías"),plotOutput("pred_plot13")
                                       ),
                                       tabPanel("UEMB",
                                                h4("Índice de deserción de U en mi barrio "),
                                                plotOutput("pred_plot48"),
                                                h4("Gráfico comparativo de índices de deserción por sede"),
                                                plotOutput("pred_plot47", width="100%"),
                                                h4("Índices de deserción por semestre y sede"),
                                                dataTableOutput("tabless13"),
                                                h4("Gráfico índices de deserción en U en mi barrio por facultad"),
                                                plotOutput("pred_plot49"),
                                                h4("índices de deserción en U en mi barrio por facultad"),
                                                dataTableOutput("contents100"),
                                                h4("Gráfico índices de deserción en U en mi barrio en la  facultad de ciencias económicas"),
                                                plotOutput("pred_plot50"),
                                                h4("Gráfico índices de deserción en U en mi barrio en la  facultad de ciencias exactas"),
                                                plotOutput("pred_plot51"),
                                                h4("Gráfico índices de deserción en U en mi barrio en ingenierias"),
                                                plotOutput("pred_plot52"),
                                                h4("Índices de deserción por semestre y programa ofrecido en U en mi barrio"),
                                                dataTableOutput("tabless14")
                                                ),
                                       tabPanel("PP",
                                                h4("Índice de deserción con Presupuesto participativo "),
                                                plotOutput("pred_plot53"),
                                                h4("Gráfico comparativo de índices de deserción con y sin PP"),
                                                plotOutput("pred_plot54", width="100%"),
                                                h4("Gráfico comparativo de índices de deserción con y sin PP en artes"),
                                                plotOutput("pred_plot55", width="100%"),
                                                h4("Gráfico comparativo de índices de deserción con y sin PP en económicas"),
                                                plotOutput("pred_plot56", width="100%"),
                                                h4("Gráfico comparativo de índices de deserción con y sin PP en exactas"),
                                                plotOutput("pred_plot57", width="100%"),
                                                h4("Gráfico comparativo de índices de deserción con y sin PP en ingenierias"),
                                                plotOutput("pred_plot58", width="100%")
                                       )
                                                 )
                                   )
                          
                            
                            
                              
                          ),
                 tabPanel("Rendimiento Académico",
                          
                          sidebarPanel(
                            conditionalPanel(
                              'input.dataset === "data3"',
                              selectInput("asig", "Escoja la asignatura","  ")
                              )
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Matemática Básica",
                                       h4("Boxplot comparativo de las notas finales"),
                                       plotOutput("pred_plot63"),
                                       plotOutput("graf21"),
                                       h4("Estadísticas por semestre"),
                                       dataTableOutput("tabless18"),
                                       h4("Distribución de las notas aprobadas"),
                                       plotOutput("graf22"),
                                       plotOutput("graf23"),
                                       dataTableOutput("tabb14"),
                                       h4("Distribución de las notas reprobadas"),
                                       plotOutput("graf24"),
                                       plotOutput("graf25"),
                                       dataTableOutput("tabb15"),
                                       h4("Proporción de ganadores por facultad"),
                                       plotOutput("graf26"),
                                       dataTableOutput("tabb16"),
                                       h4("Proporción de ganadores por sede"),
                                       plotOutput("graf27"),
                                       dataTableOutput("tabb17")
                                       
                              ),
                              tabPanel("Estadística básica",
                                       h4("Boxplot comparativo de las notas finales"),
                                       plotOutput("pred_plot62"),
                                       plotOutput("graf14"),
                                       h4("Estadísticas por semestre"),
                                       dataTableOutput("tabless17"),
                                       h4("Distribución de las notas aprobadas"),
                                       plotOutput("graf15"),
                                       plotOutput("graf16"),
                                       dataTableOutput("tabb10"),
                                       h4("Distribución de las notas reprobadas"),
                                       plotOutput("graf17"),
                                       plotOutput("graf18"),
                                       dataTableOutput("tabb11"),
                                       h4("Proporción de ganadores por facultad"),
                                       plotOutput("graf19"),
                                       dataTableOutput("tabb12"),
                                       h4("Proporción de ganadores por sede"),
                                       plotOutput("graf20"),
                                       dataTableOutput("tabb13")
                                       
                              ),
                              tabPanel("Cálculo diferencial",
                                       h4("Boxplot comparativo de las notas finales"),
                                       plotOutput("pred_plot60"),
                                       plotOutput("graf"),
                                       h4("Estadísticas por semestre"),
                                       dataTableOutput("tabless15"),
                                       h4("Distribución de las notas aprobadas"),
                                       plotOutput("graf1"),
                                       plotOutput("graf2"),
                                       dataTableOutput("tabb2"),
                                       h4("Distribución de las notas reprobadas"),
                                       plotOutput("graf3"),
                                       plotOutput("graf4"),
                                       dataTableOutput("tabb3"),
                                       h4("Proporción de ganadores por facultad"),
                                       plotOutput("graf5"),
                                       dataTableOutput("tabb4"),
                                       h4("Proporción de ganadores por sede"),
                                       plotOutput("graf6"),
                                       dataTableOutput("tabb5")
                                       
                              ),
                              tabPanel("Cálculo integral",
                                       h4("Boxplot comparativo de las notas finales"),
                                       plotOutput("pred_plot61"),
                                       plotOutput("graf7"),
                                       h4("Estadísticas por semestre"),
                                       dataTableOutput("tabless16"),
                                       h4("Distribución de las notas aprobadas"),
                                       plotOutput("graf8"),
                                       plotOutput("graf9"),
                                       dataTableOutput("tabb6"),
                                       h4("Distribución de las notas reprobadas"),
                                       plotOutput("graf10"),
                                       plotOutput("graf11"),
                                       dataTableOutput("tabb7"),
                                       h4("Proporción de ganadores por facultad"),
                                       plotOutput("graf12"),
                                       dataTableOutput("tabb8"),
                                       h4("Proporción de ganadores por sede"),
                                       plotOutput("graf13"),
                                       dataTableOutput("tabb9")
                                         )
                            )
                          )
                       )
                                               
                                                          )
                                                          )
                                                        )


