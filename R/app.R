library(DT)
library(shiny)
library(shinyjs)
library(tidyverse)
`%notin%` <- Negate(`%in%`)
library(shinycssloaders)
library(Kendall)
library(viridis)
library(ggpubr)
library(reshape2)

source("JQM_Function.R")
source("fun_volcano.R")

# Define UI for data upload app ----
ui <- fluidPage(
  useShinyjs(),
  # App title ----
  titlePanel("Individual Reference Intervals"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(width=3,
                 # conditionalPanel("input.tabs1=='Description'",
                 #                  fileInput("file1", "Choose CSV file", multiple = FALSE,
                 #                            accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
                 #                            width = NULL, buttonLabel = "Browse...",
                 #                            placeholder = "No file selected"),
                 #                  # Horizontal line ----
                 #                  tags$hr(),
                 #                  
                 #                  # Input: Checkbox if file has header ----
                 #                  checkboxInput("header", "Header", TRUE),
                 #                  # Input: Select separator ----
                 #                  radioButtons("sep", "Separator",
                 #                               choices = c(Comma = ",",
                 #                                           Semicolon = ";",
                 #                                           Tab = "\t"),
                 #                               selected = ","),
                 #                  # Input: Select quotes ----
                 #                  radioButtons("quote", "Quote",
                 #                               choices = c(None = "",
                 #                                           "Double Quote" = '"',
                 #                                           "Single Quote" = "'"),
                 #                               selected = '"'),
                 #                  # Horizontal line ----
                 #                  tags$hr(),
                 #                  # Input: Checkbox if the sample data will be used ----
                 #                  checkboxInput("sample_data", "Use sample data", TRUE)
                 #                  
                 # ),
                 # 
                 conditionalPanel(condition="input.tabs1=='Data Upload'",
                                  fileInput("file1", "Choose CSV file", multiple = FALSE,
                                            accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
                                            width = NULL, buttonLabel = "Browse...",
                                            placeholder = "No file selected"),
                                  # Horizontal line ----
                                  tags$hr(),
                                  
                                  # Input: Checkbox if file has header ----
                                  checkboxInput("header", "Header", TRUE),
                                  # Input: Select separator ----
                                  radioButtons("sep", "Separator",
                                               choices = c(Comma = ",",
                                                           Semicolon = ";",
                                                           Tab = "\t"),
                                               selected = ","),
                                  # Input: Select quotes ----
                                  radioButtons("quote", "Quote",
                                               choices = c(None = "",
                                                           "Double Quote" = '"',
                                                           "Single Quote" = "'"),
                                               selected = '"'),
                                  # Horizontal line ----
                                  tags$hr(),
                                  # Input: Checkbox if the sample data will be used ----
                                  checkboxInput("sample_data", "Use sample data", FALSE)
                                  
                 ),
                 conditionalPanel(condition="input.tabs1=='Trend & Time Analysis'",
                                  #shinyjs::useShinyjs(),
                                  id = "trend1",
                                  selectInput("series1", "Choose a variable:", choices=c()),

                                  # Horizontal line ----
                                  tags$hr(),
                                  
                                  selectInput("mad", "Choose an outlier threshold:", choices=c("\n", "2","2.3")),
                                  
                                  actionButton(inputId = "run",  label = "Run Analysis", icon = icon("play", lib = "glyphicon")),
                                  actionButton("reset_trend", "Reset")
                                  
                 ),
                 
                 conditionalPanel(condition="input.tabs1=='RI Estimation'",
                                  id = "iri1",
                                  selectInput("series2", "Choose a variable:", choices=c()),
                                  
                                  # Horizontal line ----
                                  tags$hr(),
                                  
                                  # numeric input for alpha
                                  selectInput("empcov",
                                              label = "True empirical coverage",
                                              choices = c("\n", "85%", "90%",
                                                          "95%")),
                                  actionButton("iri", "Compute IRI"),
                                  actionButton("reset_iri", "Reset")
                  ),
    ),
      

    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Description"
        ),
        
        tabPanel("Data Upload",
        navbarPage(
          title = '',
          tabPanel('Data', DT::dataTableOutput('RawData')),
          tabPanel('Volcano Plot', 
                   selectInput("pct", "Percentage threshold of total subjects with trends and high correlations:", choices=c("10%","15%","20%")),
                   plotOutput("volcano")%>% withSpinner(color="#0dc5c1"),
                   h3(textOutput(outputId = "var.result")),
                   DT::dataTableOutput("excvar"))
          )
        ),
        
        tabPanel("Trend & Time Analysis",
                 navbarPage(
                   title = '',
                   tabPanel('Data', 
                            DT::dataTableOutput("contents"),
                            plotOutput("dataplot")
                            ),
                   tabPanel("Outlier",
                            h3(textOutput(outputId = "cnt.out")),
                               plotOutput("plot.out", height=800, 
                                          click = clickOpts(id = "plot_click"))%>% withSpinner(color="#0dc5c1"),
                            column(width = 6,
                                   verbatimTextOutput("click_info")
                            )
                   ),
                   tabPanel("Trend test",
                            h3(textOutput(outputId = "cnt.tr")),
                               DT::dataTableOutput("excsub"),
                               plotOutput("plot.trend")%>% withSpinner(color="#0dc5c1")
                   ),
                   tabPanel("Variance checking",
                            h3(textOutput(outputId = "cnt.var")),
                            DT::dataTableOutput("excsubvar"),
                            plotOutput("plot.var")%>% withSpinner(color="#0dc5c1")
                 ))),
        tabPanel("RI Estimation",
                 navbarPage(
                   title='',
                   tabPanel('Compute IRI',
                            h3(textOutput(outputId = "sub.out")),
                            DT::dataTableOutput("excsub_all"),
                            h4(textOutput(outputId = "choose.sub")),
                            plotOutput("plot") %>% withSpinner(color="#0dc5c1"),
                            actionButton("submit", "Submit report"),
                            actionButton("download", "Download report")
                            
                   )
                 )
                 ),
        
        id="tabs1",
        tabPanel("Report"
        )
      )
    )
  )
)

# Define server logic to read selected file ----
server <- function(session, input, output) {
  # read uploaded data
  data1 <- reactive({
    validate(need(input$file1,""))
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath,na.strings = c("", "NA", "#N/A"),
                   header = input$header,sep = input$sep,quote = input$quote)
    df    
  })
  
  # display data
  output$RawData <- DT::renderDataTable(
    data1(), options = list(iDisplayLength = 10)
  )
  
  # read input volcano
  td <- eventReactive(input$pct, {
    db<-data1()
    if(input$pct=="10%"){
      volcano(db=db, pct=10)
    }else if(input$pct=="15%"){
      volcano(db=db, pct=15)
    }else{
      volcano(db=db, pct=20)
    }
  })
  
  # volcano plot
  output$volcano<-renderPlot({
    trend<-td()
    res<-trend$res
    res.long1<-gather(res[,c(1:2,8,11)], type1, log.p.val, log_p_mk,log_p_cor, factor_key = T)
    res.long2<-gather(res[,c(1:2,4,6)], type2, coeff, MK_tau, spearman_rho, factor_key = T)
    res.long<-cbind(res.long1, res.long2[,-c(1:2)])
    res.long$type<-ifelse(res.long$type1=="log_p_mk" & res.long$type2=="MK_tau", "Mann-Kendall test", "Spearman correlation")
    
    ggplot(res.long, aes(x=coeff, y=log.p.val, group=as.factor(subject), color=as.factor(subject)))+
      geom_point()+
      geom_hline(linetype="dashed", yintercept = -log10(0.05))+
      geom_vline(xintercept = 0.7, linetype="dotted")+
      geom_vline(xintercept = -0.7, linetype="dotted")+
      scale_color_viridis(discrete = T, name="subject")+
      labs(y="-log(P.value)", x="Coefficient")+
      theme_bw()+
      facet_wrap(~type)+
      theme(strip.background =element_blank(),
            strip.text = element_text(size=16),
            title = element_text(size=14),
            text = element_text(size=12)) 
    
  })
  
  # text output - trends
  output$var.result <- renderText({
    trend <- td()
    paste0(length(trend$exc_var$variable), 
           " variable(s) have more than ", input$pct, " subjects with trends and high correlations. It is recommended to exclude them from the IRI estimation.")
  })
  
  # table output - trends and correlations
  output$excvar<-DT::renderDataTable({
    trend <- td()
    evar<-trend$exc_var
    evar[,-1]<-round(evar[,-1], digits = 4)
    evar
  })
  
  # read input variable choice 1
  data2 <- reactive({
    df3 <- data1()[,-c(1:2)]
    updateSelectInput(session,"series1",choices=colnames(df3))
    updateSelectInput(session,"series2",choices=colnames(df3))
    return(df3)    
  })
  
  observeEvent(input$series1, {
    updateSelectInput(session, 'series2', selected = input$series1)
  })
  
  observeEvent(input$series2, {
    updateSelectInput(session, 'series1', selected = input$series2)
  })
  
  # show data variable choice 1
  output$contents <- DT::renderDataTable({
      df.show<-cbind(data1()[,c(1:2)], data2()[,as.character(input$series1)])
      colnames(df.show)<-c(names(data1())[1:2],as.character(input$series1))
      df.show
  })
  
  output$dataplot <- renderPlot({
    if(input$run){
      db<-cbind(data1()[,c(1:2)], data2()[,as.character(input$series1)])
      colnames(db)<-c("subject","time","y")
      db$time<-as.factor(db$time)
      
      ggplot(db, aes(x=as.factor(subject), y=y, color=time))+
        geom_point(size=2)+
        scale_colour_viridis(discrete = T)+
        labs(y="Measurement", x="Subject")+
        theme_bw()
    }
  })
  
  # read input mad - outlier data
  trend <- eventReactive(input$mad, {
    if(input$run){
      db<-cbind(data1()[,c(1:2)], data2()[,as.character(input$series1)])
      
      if(input$mad=="2"){
        trendsub(db=db, lim=2)
      }else{
        trendsub(db=db, lim=2.3)
      }
    }
  })
  
  # text output outlier
  output$cnt.out <- renderText({
    if(input$run){
      d <- trend()
      paste0("There are ", d$cnt, " subjects with outliers:")
    }
  })
  
  # plot outlier
  output$plot.out<-renderPlot({
    if(input$run){
      d <- trend()
      d$df2$time<-as.factor(d$df2$time)
      ggplot(d$df2, aes(x=time, color=time))+
        geom_point(aes(y=y), size=2)+
        scale_colour_viridis(discrete = T)+
        geom_hline(data=d$d_mad, aes(yintercept = mad_up), color="red")+
        geom_hline(data=d$d_mad, aes(yintercept = mad_low), color="red")+
        labs(y="Measurement", x="Time")+
        theme_bw()+
        facet_wrap(~subject, scales = "free_x")+
        theme(strip.text = element_text(size=15),
              title = element_text(size=14),
              text = element_text(size=12),
              legend.text = element_text(size=14))
    }
  })
  output$click_info <- renderPrint({
    inp<-input$plot_click
    subj<-parse_number(as.character(inp$panelvar1))
    tm<-round(inp$x)
    y<-inp$y
    d<-trend()
    d2<-d$df2
    trend<-td()
    d.out<-trend$d.out
    d2<-d2 %>% left_join(., d.out[,c(1:2,ncol(d.out),ncol(d.out)-1)], by=c("subject","time"))
    #find logical increment
    m<-NULL
    for (i in 1:nrow(d2)) {
      m[i]<-abs(d2$y[i]-d2$y[i+1])
    }
    inc<-min(m[m!=0], na.rm = T)
    d2<-d2 %>% filter(., res==1)
    out.oth<-d2[d2$subject==subj & d2$time==tm & d2$y>=y-inc & d2$y<=y+inc,]$count.id
    paste0("Subject ",subj, ": There are ",out.oth," other parameters (out of 92) with outliers")
    #str(input$plot_click)
  })
  
  # text output trends
  output$cnt.tr <- renderText({
    if(input$run){
      d <- trend()
      paste0("There are ", length(unique(d$exc_sub)), " subjects with trends and/or high correlations:")
    }
  })
  
  # table output - subject trends and correlations
  output$excsub<-DT::renderDataTable({
    if(input$run){
      d <- trend()
      evar<-d$exc_dat[,1:5]
      evar[,-1]<-round(evar[,-1], digits = 4)
      evar
    }
  })
  
  # plot trends
  output$plot.trend<-renderPlot({
    if(input$run){
      d <- trend()      
      d$df3$time<-as.factor(d$df3$time)
      ggplot(d$df3, aes(x=time, color=time))+
        geom_point(aes(y=y), size=2)+
        scale_colour_viridis(discrete = T)+
        geom_hline(data=d$d_mad3, aes(yintercept = mad_up), color="red")+
        geom_hline(data=d$d_mad3, aes(yintercept = mad_low), color="red")+
        labs(y="Measurement", x="Time")+
        theme_bw()+
        facet_wrap(~subject, scales = "free")+
        theme(strip.text = element_text(size=15),
              title = element_text(size=14),
              text = element_text(size=12),
              legend.text = element_text(size=14))
    }
  })

  # read input mad - outlier data
  varcheck <- eventReactive(input$mad, {
    if(input$run){
      db<-cbind(data1()[,c(1:2)], data2()[,as.character(input$series1)])
      
      if(input$mad=="2"){
        varboot(db=db, lim=2)
      }else{
        varboot(db=db, lim=2.3)
      }
    }
  })
  
  # text output variance
  output$cnt.var <- renderText({
    if(input$run){
      d <- varcheck()
      paste0("There are ", length(d$out.mad), " subjects with high variances:")
    }
  })
  
  # table output - subject variances
  output$excsubvar<-DT::renderDataTable({
    if(input$run){
      d <- varcheck()
      evar<-d$exc_sub
      evar[,-1]<-round(evar[,-1], digits = 4)
      evar
    }
  })
  
  # plot data vs variances
  output$plot.var<-renderPlot({
    if(input$run){
      db<-cbind(data1()[,c(1:2)], data2()[,as.character(input$series1)])
      colnames(db)<-c("subject","time","y")
      db$time<-as.factor(db$time)
      g1<-ggplot(db)+
        geom_point(aes(x=as.factor(subject), y=y, color=time), size=2)+
        scale_colour_viridis(discrete = T)+
        labs(y="Measurement", x="Subject")+
        theme_bw()
      
      d <- varcheck()
      g2<-ggplot(d$varmat.long)+
        geom_point(aes(x=as.factor(subject), y=var.boot), shape=1, size=1, color="darkgrey")+
        geom_point(aes(x=as.factor(subject), y=mean.var), color="blue", size=2)+
        geom_hline(yintercept = d$mad.up, color="red")+
        geom_hline(yintercept = d$mad.low, color="red")+
        labs(y="Bootstrapped Variances", x="Subject")+
        theme_bw()
      ggarrange(g1,g2)
    }
  })
  
  # text output outlying variances
  output$sub.out <- renderText({
    d1 <- trend()
    d2 <- varcheck()
    exc<-unique(c(d1$exc_sub, d2$out.mad))

    paste0(length(exc), " subjects with trends and high variances:")
  })
  
  output$choose.sub <- renderText({
    # d1 <- trend()
    # d2 <- varcheck()
    # exc<-unique(c(d1$exc_sub, d2$out.mad))
    # 
    paste0("Select subjects to exclude from the IRI estimation:")
  })
  
  # table output - final subject with trends and correlations and high variance
  output$excsub_all<-DT::renderDataTable({
    d1 <- trend()
    d2 <- varcheck()
    Subject<-unique(c(d1$exc_sub, d2$out.mad))
    Remark<-c(rep("Trend/correlation is present", length(unique(d1$exc_sub))),
              rep("High variance", length(unique(d2$out.mad))))
    data.frame(Subject,Remark)
  })
  
  observeEvent(input$reset_trend, {
    shinyjs::reset("trend1")
  })
  observeEvent(input$reset_iri, {
    shinyjs::reset("iri1")
  })
  
  
  # read input IRI- JQM
  showplot<-eventReactive(input$iri, {
    df<-cbind(data1()[,c(1:2)], data2()[,as.character(input$series2)])
    colnames(df)<-c(names(data1())[1:2],as.character(input$series2))
    df2<-df
    
    d1 <- trend()
    d2 <- varcheck()
    Subject<-unique(c(d1$exc_sub, d2$out.mad))
    df2<-df2[df2$subject %notin% Subject,]
    colnames(df2)<-c("subject","time","y")
    
    exmad<-d1$d_mad
    inc<-exmad[exmad$subject %notin% Subject,]
    df2<-df2 %>% left_join(., inc, by="subject")
    df2$outlier<-ifelse(df2$y<df2$mad_low | df2$y>df2$mad_up,1,0)
    df2$outlier<-ifelse(is.na(df2$outlier)==T,0,df2$outlier)

    alpha<- switch(input$empcov,
                   "85%" = 0.15,
                   "90%" = 0.1,
                   "95%" = 0.05)
    res<-jqm(db=df2,
             alpha=alpha,
             lambda.u.seq = seq(0.02,0.1,0.02),
             lambda.z.seq = seq(0.5,5,0.5))
    uz <- cbind.data.frame(res$u, res$z)
    uz$low <- res$beta0 + res$u + res$z*res$beta1
    uz$up  <- res$beta0 + res$u + res$z*res$beta2
    uz$id <- unique(df2$subject)
    colnames(uz) <- c("u", "z", "low", "up","id")
    return(list(df=df,
           df2=df2,
           res=res,
           uz=uz))

  })

  # plot IRI - JQM
  output$plot<-renderPlot({
    .tmp <- showplot()
    df<- .tmp$df
    df2<- .tmp$df2
    res<- .tmp$res
    uz<- .tmp$uz
    
    ggplot(uz, aes(x=as.factor(id))) +
      geom_errorbar(aes(ymin = low, ymax = up), color="darkblue", size=1) +
      geom_point(data = df2, aes(x = as.factor(subject), y = y, color=as.factor(outlier)),
                 position = position_dodge(width = 0.9),size=2.5) +
      geom_vline(xintercept=seq(1.5, length(unique(df2$subject))-0.5, 1),
                 lwd=0.5, colour="grey") +
      scale_color_manual(name="Outlying observation", labels=c("No","Yes"), values = c("darkgrey","red"))+
      labs(x="Participants",y="Measurement",
           title=paste0("IRI of ",names(df)[3]),
           subtitle = paste0("Empirical coverage=",round(res$cov.tot, digits=4))) +
      theme_classic()+
      theme(legend.position = "right",
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            axis.ticks = element_blank(),
            panel.border = element_rect(NA))

  })

}

# Create Shiny app ----
shinyApp(ui, server)