# server

function(input, output,session,img_url) {


  # reactiva
  df_shape_y_t <- reactive({
    df_shape_y_t <- salmed %>% filter(year %in% input$year_m, sex==input$sex_m, educ %in% input$educ_m) 
    
  })
  
  output$gr2_plotly <- renderPlotly({
    
    gr2 <- ggplot(df_shape_y_t(), aes(x=educ,y=salmed, fill=as.factor(year))) + 
      geom_bar(position="dodge" ,stat="identity")  + theme_bw() +xlab("Schooling") +
      ylab("Quarterly Wage, 2022 Constant pesos") + scale_fill_manual(values=cbPalette)
    
    gr2 <- ggplotly(gr2,tooltip = "text") %>% 
      layout(showlegend = F) #
    gr2 
  }) 
  
  
  
  
  # Inicia glosario_pob interactivo
  output$glosario_pob_var <- renderText({ 
    selec_conc_pob <- input$select_conceptos_pob
    glosario_pob_display <- glosario_pob%>%filter(concepto==selec_conc_pob)
    paste(glosario_pob_display$def[1])
  })
  # Termina glosario_pob interactivo
  # Inicia glosario_nat interactivo
  output$glosario_nat_var <- renderText({ 
    selec_conc_nat <- input$select_conceptos_nat
    glosario_nat_display <- glosario_nat%>%filter(concepto==selec_conc_nat)
    paste(glosario_nat_display$def[1])
  })
  # Termina glosario_nat interactivo
  # Inicia glosario_mor interactivo
  output$glosario_mor_var <- renderText({ 
    selec_conc_mor <- input$select_conceptos_mor
    glosario_mor_display <- glosario_mor%>%filter(concepto==selec_conc_mor)
    paste(glosario_mor_display$def[1])
  })
  # Termina glosario_mor interactivo
  # Inicia glosario_mat interactivo
  output$glosario_mat_var <- renderText({ 
    selec_conc_mat <- input$select_conceptos_mat
    glosario_mat_display <- glosario_mat%>%filter(concepto==selec_conc_mat)
    paste(glosario_mat_display$def[1])
  })
  # Termina glosario_mat interactivo
  # Inicia glosario_div interactivo
  output$glosario_div_var <- renderText({ 
    selec_conc_div <- input$select_conceptos_div
    glosario_div_display <- glosario_div%>%filter(concepto==selec_conc_div)
    paste(glosario_div_display$def[1])
  })
  # Termina glosario_div interactivo
  # Inicia glosario_int interactivo
  output$glosario_intern_var <- renderText({ 
    selec_conc_int <- input$var1_inter
    glosario_int_display <- glosario_intern%>%filter(concepto==selec_conc_int)
    paste(glosario_int_display$def[1])
  })
  # Termina glosario_int interactivo
  
  # Inicia glosario_int_eco interactivo
  output$glosario_intern_var_eco <- renderText({ 
    selec_conc_int_eco <- input$var2_inter
    glosario_int_eco_display <- glosario_intern_eco%>%filter(concepto==selec_conc_int_eco)
    paste(glosario_int_eco_display$def[1])
  })
  # Termina glosario_int_eco interactivo
  
################################################################################
  df_sal <- reactive({
    df_sal <- enoe_sal_gen_trim %>% filter(sex %in% input$sex) 
    
  })
  
  output$gr1_plotly <- renderPlotly({
    
    
    
    gr1 <- ggplot(df_sal(), aes(x = date, y=saltrim, group=sex, color=sex)) + geom_line() +
      geom_point() +  theme_bw() +xlab("Date") +
      ylab("Quarterly Wage, 2022 Constant pesos")  + scale_colour_manual(values=cbPalette)
    
    
    gr1 <- ggplotly(gr1)
    
    gr1
    })
  

##### MAPA muni
  
  # reactiva mapa muni
  df_map <- reactive({
    df_map <- mex_map %>% filter(year == input$year_m2, ent== input$est_m) 
  })
  
  output$mp1_plotly <- renderPlotly({
  
  mapa2 <-    ggplot(data=df_map(),aes(fill = nivel, label=paste(NOM_MUN,":",gap, sep=""))) +
    geom_sf(colour = "black",  size = 0.1) +
    scale_fill_viridis_d() +
    labs(title = "",
         caption = "",
         fill = "") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.key.size = unit(0.4, 'cm'),
          legend.text = element_text(size=12),
          legend.title = element_text(size = 12),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(size=0.05, color='black'),
          panel.background = element_rect(fill='white'),
          plot.title = element_text(size=16))
  
  mapa2 <- ggplotly(mapa2)
  mapa2
  })
  
  ##### MAPA est 
  # reactiva mapa est
  df_map_est <- reactive({
    df_map_est <- mex_map2 %>% filter(year == input$year_m2_est, var== input$var_m) 
  })
  
  output$mp2_plotly <- renderPlotly({
    
    mapa2 <-    ggplot(data=df_map_est(),aes(fill = cuartil, label= paste(ent,":",value, sep=""))) +
      geom_sf(colour = "black",  size = 0.1) +
      scale_fill_viridis_d() +
      labs(title = "",
           caption = "",
           fill = "") +
      theme_bw() +
      theme(legend.position = "bottom",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.key.size = unit(0.4, 'cm'),
            legend.text = element_text(size=12),
            legend.title = element_text(size = 12),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            panel.grid.major.y = element_line(size=0.05, color='black'),
            panel.background = element_rect(fill='white'),
            plot.title = element_text(size=16))
    
    mapa2 <- ggplotly(mapa2)
    mapa2
  })
  
  ###tabla reactiva
  datasetInput <- reactive({
    datasetInput <- baseest %>% filter(year == input$year_m2) %>% select(ent, value) %>% arrange(desc(value))
     })
  
  output$mytable <- renderTable({
    datasetInput()[1:5,]
  })
  
  ### working tab
  employInput <- reactive({
    employInput <- employ %>% filter(year %in% input$year_w, var == input$var_w)
  })
  
  output$w1_plotly <- renderPlotly({
  
    
    w1 <-  ggplot(employInput(), aes(x=var,y=value, fill= as.factor(year))) + 
      geom_bar(position="dodge",stat="identity") + theme_bw() +xlab("Variable") +
      ylab("Percentage")  + scale_fill_manual(values=cbPalette)  
    
    w1 <- ggplotly(w1)
    w1
  })
  
  ### hour wage
  
  hourInput <- reactive({
    
    hourInput <- hours %>% filter(year %in% input$year_h, var == input$var_h, categoria %in% input$cat_h)
    
  })
  
  output$h1_plotly <- renderPlotly({
    
    
    h1 <-  ggplot(hourInput(), aes(x=year,y=hrs, group=as.factor(categoria), color=as.factor(categoria))) + 
      geom_line()+ geom_point() + theme_bw() +xlab("Years") + scale_color_viridis(discrete = TRUE) +
      ylab("Weekly Hours")
    
    h1 <- ggplotly(h1)
    h1
  })
  
  
  ### Econometria
  
  qregInput <- reactive({
    
    qregInput <- qreg %>% filter(year == input$year_q)
    
  })
  
  output$q1_plotly <- renderPlotly({
    
    
    q1 <-  ggplot(qregInput())+
      geom_line(aes(x=n,y=genero,color="Qreg coefficient"),size=.5)+
      geom_line(aes(x=n,y=genero_mco,color="OLS coefficient"),size=.75)+
      #geom_line(aes(x=n,y=ll_g,color="Qreg CI"),linetype="dashed",size=.5)+
      #geom_line(aes(x=n,y=ul_g,color="Qreg CI"),linetype="dashed",size=.5)+ 
      geom_line(aes(x=n,y=ll_g_mco,color="OLS CI"),linetype="dashed",size=.5)+
      geom_line(aes(x=n,y=ul_g_mco,color="OLS CI"),linetype="dashed",size=.5)+
      scale_color_manual(values = c(
        "Qreg coefficient" = "blue",
        "OLS coefficient" = "black",
        #"Qreg CI" = "grey",
        "OLS CI" = "grey")) +
      labs(y="Percent wage gap", x="Percentile")+
      theme_bw()
    
    q1 <- ggplotly(q1)
    q1
  })
  
  
  
    
}