library(shiny)

ui <- fluidPage(
    titlePanel("Análise Regional de Preços"),
    sidebarLayout(
        sidebarPanel(
            selectInput("regiao", "Selecione a Região:",
                        choices = c("NORTE", "SUL", "SUDESTE", "CENTRO-OESTE", "PREÇO UNITÁRIO")),
            downloadButton("downloadPNG", "Download PNG"),
            downloadButton("downloadEMF", "Download EMF")
        ),
        mainPanel(
           plotOutput("graficoLinhas", height = "500px")
        )
    )
)

server <- function(input, output) {
    
    dados_regioes <- list(
        "NORTE" = data.frame(
            periodo = 1:10,
            AP = c(190, 185, 180, 175, 165, 160, 158, 162, 165, 160),
            PA = c(195, 185, 180, 178, 165, 158, 155, 160, 165, 162),
            AM = c(175, 170, 165, 160, 155, 150, 148, 152, 155, 158),
            RO = c(170, 165, 162, 160, 155, 152, 150, 148, 145, 140),
            RR = c(115, 112, 110, 108, 105, 107, 108, 110, 108, 105),
            TO = c(80, 78, 75, 73, 70, 68, 65, 67, 68, 70),
            AC = c(75, 73, 70, 68, 65, 63, 60, 62, 63, 65)
        ),
        "SUL" = data.frame(
            periodo = 1:10,
            MG = c(160, 155, 150, 155, 158, 160, 162, 158, 155, 152),
            RS = c(140, 138, 135, 138, 140, 142, 145, 143, 140, 142),
            SC = c(135, 132, 130, 133, 135, 137, 140, 142, 145, 143),
            RJ = c(130, 128, 125, 128, 130, 132, 135, 137, 140, 142),
            SP = c(125, 123, 120, 122, 125, 127, 130, 132, 135, 137),
            ES = c(163, 155, 150, 145, 140, 135, 130, 128, 125, 120),
            PR = c(120, 118, 115, 117, 120, 122, 125, 127, 130, 128)
        ),
        "SUDESTE" = data.frame(
            periodo = 1:10,
            BA = c(155, 148, 145, 148, 150, 145, 140, 135, 130, 128),
            PE = c(145, 140, 135, 132, 130, 128, 125, 127, 130, 128),
            RN = c(135, 132, 130, 128, 125, 123, 120, 122, 125, 127),
            PB = c(130, 128, 125, 123, 120, 118, 115, 117, 120, 122),
            SE = c(125, 123, 120, 118, 115, 113, 110, 112, 115, 117),
            CE = c(150, 145, 140, 135, 130, 125, 120, 115, 110, 108),
            AL = c(128, 125, 122, 120, 118, 115, 112, 110, 112, 115),
            MA = c(110, 108, 105, 107, 110, 112, 115, 117, 120, 118),
            PI = c(115, 113, 110, 112, 115, 117, 120, 122, 125, 123)
        ),
        "CENTRO-OESTE" = data.frame(
            periodo = 1:10,
            GO = c(160, 158, 162, 165, 162, 160, 165, 162, 160, 162),
            DF = c(145, 142, 140, 145, 148, 145, 142, 140, 145, 148),
            MS = c(95, 97, 100, 103, 105, 107, 110, 112, 115, 113),
            MT = c(88, 90, 93, 95, 98, 100, 103, 105, 108, 110)
        ),
        "PREÇO UNITÁRIO" = data.frame(
            periodo = 1:10,
            MAX = c(195, 192, 190, 192, 190, 192, 190, 192, 190, 188),
            MED = c(142, 138, 135, 137, 135, 137, 135, 137, 135, 133),
            MIN = c(78, 75, 73, 70, 68, 65, 67, 68, 70, 72)
        )
    )
    
    plot_function <- function() {
        dados <- dados_regioes[[input$regiao]]
        
        cores <- c("#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd", 
                   "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
        simbolos <- c(16, 15, 17, 18, 8, 4, 3, 7, 9, 10)
        
        n_series <- ncol(dados) - 1
        
        par(mar = c(5, 4, 4, 8), bg = "white")
        
        y_min <- min(dados[,-1], na.rm = TRUE) - 5
        y_max <- max(dados[,-1], na.rm = TRUE) + 5
        
        plot(dados$periodo, dados[,2], type = "n", 
             ylim = c(y_min, y_max),
             xlab = "Período", ylab = "Preço (R$/ton)",
             main = paste("Gráfico -", input$regiao),
             panel.first = grid(col = "lightgray", lty = "dotted"))
        
        for(i in 2:ncol(dados)) {
            lines(dados$periodo, dados[,i], 
                  col = cores[i-1], lwd = 2, type = "b", 
                  pch = simbolos[i-1], cex = 1.2)
        }
        
        legend("right", inset = c(-0.18, 0), xpd = TRUE,
               legend = names(dados)[-1],
               col = cores[1:n_series],
               pch = simbolos[1:n_series],
               lty = 1, lwd = 2, cex = 0.8,
               bty = "n")
    }
    
    output$graficoLinhas <- renderPlot({
        plot_function()
    })
    
    output$downloadPNG <- downloadHandler(
        filename = function() {
            paste("grafico-", input$regiao, "-", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
            png(file, width = 1400, height = 700, res = 120)
            plot_function()
            dev.off()
        }
    )
    
    output$downloadEMF <- downloadHandler(
        filename = function() {
            paste("grafico-", input$regiao, "-", Sys.Date(), ".emf", sep = "")
        },
        content = function(file) {
            devEMF::emf(file, width = 12, height = 6)
            plot_function()
            dev.off()
        }
    )
}

shinyApp(ui = ui, server = server)
