library(shiny)
library(readxl)
library(writexl)  # Asegúrate de tener instalado writexl

# Leemos los datos y asignamos un ID único a cada registro
clientes_data <- read_excel("clientes.xlsx")
clientes_data$id <- seq_len(nrow(clientes_data))

# Creamos un reactiveVal para almacenar la data de clientes (editable)
data_reactive <- reactiveVal(clientes_data)

# ReactiveVal para almacenar el query de búsqueda (último término ingresado)
searchQuery <- reactiveVal("")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600&display=swap"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("
    $(document).ready(function () {

      // Capturar eventos de teclado
      $(document).on('keydown', function (e) {
        
        // F2: Editar el primer cliente
        if (e.keyCode === 113) { // 113 = F2
          e.preventDefault();
          if ($('.modal:visible').length === 0) {
            $('.edit-btn').first().click();
          }
        }

        // F1: Guardar cambios si el modal está abierto
        else if (e.keyCode === 112) { // 112 = F1
          e.preventDefault();
          if ($('.modal:visible').length > 0) {
            $('#guardar_edicion').click();
          }
        }

        // Enter: Búsqueda rápida
        else if (e.keyCode === 13) { // 13 = Enter
          if ($('.modal:visible').length === 0) { 
            e.preventDefault();
            $('#buscar').click();
            $('#empresa').val('').focus(); // Limpia y vuelve a enfocar sin delay
          }
        }

        // Tab: Solo dentro del modal y selecciona texto
        else if (e.keyCode === 9 && $('.modal:visible').length > 0) { // 9 = Tab
          e.preventDefault(); // Evita que Tab salga del modal
          
          var focusable = $('.modal:visible').find('input, select, textarea, button').filter(':visible');
          var currentIndex = focusable.index($(document.activeElement));

          // Si Shift+Tab y está en el primer elemento -> ir al último
          if (e.shiftKey && currentIndex === 0) {
            focusable.last().focus().select();s
          }
          // Si Tab normal y está en el último elemento -> ir al primero
          else if (!e.shiftKey && currentIndex === focusable.length - 1) {
            focusable.first().focus().select();
          }
          // Si no, moverse al siguiente campo y seleccionar texto
          else {
            focusable.eq(currentIndex + (e.shiftKey ? -1 : 1)).focus().select();
          }
        }
      });

    });
  "))
  )
  
  
  ,
  
  titlePanel("Búsqueda y Edición de Clientes"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("empresa", "Ingrese el nombre de la empresa:", value = ""),
      actionButton("buscar", "Buscar")
    ),
    mainPanel(
      uiOutput("detalle_cliente")
    )
  )
)

server <- function(input, output, session) {
  
  # Al presionar "Buscar", actualizamos el query de búsqueda
  observeEvent(input$buscar, {
    searchQuery(input$empresa)
    updateTextInput(session, "empresa", value = "")
  })
  
  # Reactive que filtra la data en base al query y a la data actual
  datos_filtrados <- reactive({
    df <- data_reactive()
    query <- searchQuery()
    if (query == "") {
      df
    } else {
      subset(df, grepl(query, Cliente, ignore.case = TRUE))
    }
  })
  
  # Renderizamos los paneles de detalle de cliente
  output$detalle_cliente <- renderUI({
    df <- datos_filtrados()
    
    if (nrow(df) == 0) {
      return(h3("No se encontró ningún cliente."))
    }
    
    panels <- lapply(1:nrow(df), function(i) {
      cliente <- df[i, ]
      
      # Botón de edición (ícono de lápiz) posicionado en la esquina superior derecha
      edit_button <- tags$button(
        class = "edit-btn",
        onclick = sprintf("Shiny.setInputValue('current_edit', %s, {priority: 'event'})", cliente$id),
        icon("pencil-alt")
      )
      
      wellPanel(
        div(style = "position: relative;",
            h3(cliente$Cliente),
            div(style = "position: absolute; top: 5px; right: 5px;", edit_button)
        ),
        fluidRow(
          column(6, p(strong("Fecha Ultimo Contacto:"), as.character(cliente$`Fecha de ultimo contacto`))),
          column(6, p(strong("Estado actual:"), cliente$`Estado actual`))
        ),
        fluidRow(
          column(6, p(strong("Accion siguiente:"), cliente$`accion siguiente`)),
          column(6, p(strong("Notas clave:"), cliente$`notas clave`))
        )
      )
    })
    do.call(tagList, panels)
  })
  
  # ReactiveVal para almacenar el id del registro que se está editando
  current_edit_id <- reactiveVal(NULL)
  
  # Cuando se hace clic en el botón de edición, mostramos el modal
  observeEvent(input$current_edit, {
    selected_id <- input$current_edit
    current_edit_id(selected_id)
    
    df <- data_reactive()
    record <- df[df$id == selected_id, ]
    
    showModal(modalDialog(
      title = paste("Editar", record$Cliente),
      textInput("edit_cliente", "Cliente", value = record$Cliente),
      dateInput("edit_fecha", "Fecha Ultimo Contacto", value = as.Date(record$`Fecha de ultimo contacto`)),
      textInput("edit_estado", "Estado actual", value = record$`Estado actual`),
      textInput("edit_accion", "Accion siguiente", value = record$`accion siguiente`),
      textInput("edit_notas", "Notas clave", value = record$`notas clave`),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("guardar_edicion", "Guardar")
      ),
      easyClose = TRUE
    ))
  })
  
  # Al guardar la edición, actualizamos el registro y escribimos en el Excel
  observeEvent(input$guardar_edicion, {
    selected_id <- current_edit_id()
    df <- data_reactive()
    
    new_fecha <- as.POSIXct(input$edit_fecha, tz = "UTC")
    
    df[df$id == selected_id, "Cliente"] <- input$edit_cliente
    df[df$id == selected_id, "Fecha de ultimo contacto"] <- new_fecha
    df[df$id == selected_id, "Estado actual"] <- input$edit_estado
    df[df$id == selected_id, "accion siguiente"] <- input$edit_accion
    df[df$id == selected_id, "notas clave"] <- input$edit_notas
    
    data_reactive(df)
    
    # Escribimos la data actualizada al archivo Excel
    writexl::write_xlsx(df, "clientes.xlsx")
    
    removeModal()
  })
}

shinyApp(ui = ui, server = server)
