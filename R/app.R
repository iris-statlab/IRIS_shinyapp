library(DT)
library(shiny)
library(shinyjs)
library(tidyverse)
`%notin%` <- Negate(`%in%`)
library(shinycssloaders)
library(Kendall)
library(viridis)
library(patchwork)
library(reshape2)
library(plotly)
library(htmltools)
library(knitr)

source("fun_volcano.R")
source("icons.R")

link_publications <- "https://publicationslist.org/m.pusparum"
link_repo <- "https://github.com/iris-statlab/IRIS_shinyapp"

jscode <- paste0(
  "var linkPublications = \"", link_publications, "\";",
  "var linkRepo = \"", link_repo, "\";",
  "$(document).ready(function(){",
  "$('a[data-value=\"link_publications\"]').click(function(){window.location.href = linkPublications});", 
  "$('a[data-value=\"link_repo\"]').click(function(){window.location.href = linkRepo});", 
  "});"
)

home <- tabPanel(
  title = "home",
  value = "home",
  div(
    class = "hero",
    HTML("<h1 class='hero-title'>Individual Reference Intervals App</h1>"),
    HTML("<p class='hero-desc'>An easy tool to build individual reference intervals based on biomarker's longitudinal data.</p>"),
  ),
  div(
    class = "home-menu-container", 
      div(
        class = "home-menu",
        actionLink(
          "menu_user_manual",
          HTML(
            paste0(
              "<span class='home-menu-item home-menu-item--manual'>",
              icon.file,
              "<span>User Manual</span>",
              "</span>"
            )
          )
        ),
        actionLink(
          "menu_analysis",
          HTML(
            paste0(
              "<span class='home-menu-item home-menu-item--analysis'>",             
              icon.activity,
              "<span>Analysis</span>",
              "</span>"
            )
          )
        )
      )
  ),
  div(
    class = "highlights",
    HTML(paste0(
      "<article class='highlight'>",
      "<h2>Personalised</h2>",
      "<p>A novel instrument for interpreting lab test results following personal biomarker signatures.</p>",
      "</article>"
    )),
    HTML(paste0(
      "<article class='highlight'>",
      "<h2>Precise</h2>",
      "<p>Provides essential assistance in early disease detection by uncovering small deviations of test results.</p>",
      "</article>"
    )),
    HTML(paste0(
      "<article class='highlight'>",
      "<h2>Versatile</h2>",
      "<p>Acommodates small subject sample sizes in small time series data.</p>",
      "</article>"
    ))
  ),
  div(
    class = "sponsors-container",
    HTML("<h2>Affilated with</h2>"),
    div(
      class = "sponsors",
      HTML(paste0(
        "<a class='sponsor' href='https://www.uhasselt.be/en/instituten-en/dsi' target='_blank' rel='noopener noreferrer'>",
        "<img src='image/dsi_logo.png' alt='Data Science Institute UHasselt Logo'>",
        "</a>" 
      )),
      HTML(paste0(
        "<a class='sponsor' href='https://vito.be/en' target='_blank' rel='noopener noreferrer'>",
        "<img src='image/vito_logo.png' alt='Vito Logo'>",
        "</a>"      
      ))
    )
  )
)

user_manual <- tabPanel(
  title = "user manual",
  value = "user_manual",
  column(width = 12,
         wellPanel(
           HTML("<h1>Individual Reference Intervals (IRIs) estimation app</h1>"),
           HTML('<br/>'),
           HTML('<center><img src="image/iri_do.svg" width = "85%"></center>'),
           HTML('<h3> This tool can be used to perform an IRI estimation for a particular biomarker/clinical test. 
           Instead of giving one reference interval of one biomarker, like the conventional methods, 
           <strong>IRIs provide subject-specific reference intervals, more precise than PRI.</strong></h3>'),
           HTML('<hr/>'),
           
           HTML("<h1>What you need:</h1>"),
           HTML('<center><img src="image/iri_need.svg" width = "50%"></center>'),
           HTML('<hr/>'),
           
           HTML("<h2>A pipeline will be run before estimating the IRI</h2>"),
           HTML('<center><img src="image/iris_pipeline.svg" width = "85%"></center>'),
           
           HTML('<hr/>'),
           HTML("<h2>Data upload procedures:</h2>"),
           HTML('<h3> In order to use this tool, load your data set using <em>Analysis > Data Upload</em> tab. </h3>'),
           HTML('<h3> The data must be in a <em>wide format</em> and the first two columns should indicate the <code>subject</code> and <code>time</code> indices. As an example:</h3>'),
           HTML('<center><img src="image/data_upload_example.PNG" width = "85%"></center>'),
           HTML('<h3> An overview of trends and correlations for all biomarkers is presented in the <em>Volcano Plot</em> tab.</h3>'),
           HTML('<hr/>'),
           HTML('<h3> You can continue the analysis by selecting a feature/biomarker in the <em> Trend & Time Analysis</em>.
                This feature will be carried out in the next steps until finally the IRIs are computed for eligible subjects.</h3>'),
           HTML('<h3> An example of estimated IRIs after leaving the pipeline:</h3>'),
           HTML('<center><img src="image/iri_hdl.svg" width = "85%"></center>'),
           HTML('<h3> For each individual, the IRIs are indicated by the <font color="darkblue"><b> blue error bars</b></font> . 
                They were estimated from the previous/historical measurements indicated by the full circles. The 
                <font color="red"> red circles </font> refer to outlying observations (included in the estimation).</h3>'),
           HTML('<h3> These IRIs are designed <b> to interpret the new test results or the future measurements </b> of each subject. </h3>'),
           HTML('<h3> The data quality check results as well as the IRI estimates can be retrieved by clicking the <code>Download report</code> button. </h3>'),
           HTML('<br/>')
        )
    ) 
)

analysis <- tabPanel(
  title = "analysis",
  value = "analysis",
  sidebarLayout(
  # Sidebar panel for inputs ----
  sidebarPanel(width=3,
               conditionalPanel(condition="input.tabs1=='Data Upload'",
                                id="data_input",
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
                                checkboxInput("sample_data", "Use sample data", FALSE),
                                actionButton("reset_data", "Reset Data")
                                  
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
                                  # select confounders
                                  checkboxGroupInput("confound", 
                                                     label = "Choose traits to be included in the estimation:",
                                                     choices = c("Age" = "age",
                                                                 "Sex" = "sex")),
                                  actionButton("iri", "Compute IRI"),
                                  actionButton("reset_iri", "Reset"),
                                  
                  ),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Data Upload",
        navbarPage(
          title = '',
          tabPanel('Data', DT::dataTableOutput('RawData')),
          tabPanel('Volcano Plot', 
                   selectInput("pct", "Percentage threshold of total subjects with trends and high correlations:", choices=c("10%","15%","20%")),
                   plotlyOutput("volcano")%>% withSpinner(color="#0dc5c1"),
                   h3(textOutput(outputId = "var.result")),
                   DT::dataTableOutput("excvar"))
          )
        ),
        
        tabPanel("Trend & Time Analysis",
                 navbarPage(
                   title = '',
                   tabPanel('Data', 
                            DT::dataTableOutput("contents"),
                            plotlyOutput("dataplot", height = 600)
                            ),
                   tabPanel("Outlier",
                            h3(textOutput(outputId = "cnt.out")),
                            plotlyOutput("plot.out", height = 800)%>% withSpinner(color="#0dc5c1")
                            # click = clickOpts(id = "plot_click"),
                            # column(width = 6,
                            #        verbatimTextOutput("click_info")
                            #)
                   ),
                   tabPanel("Trend test",
                            h3(textOutput(outputId = "cnt.tr")),
                               DT::dataTableOutput("excsub"),
                            plotlyOutput("plot.trend")%>% withSpinner(color="#0dc5c1")
                   ),
                   tabPanel("Variance checking",
                            h3(textOutput(outputId = "cnt.var")),
                            DT::dataTableOutput("excsubvar"),
                            plotlyOutput("plot.var")%>% withSpinner(color="#0dc5c1")
                 ))),
        tabPanel("RI Estimation",
                 navbarPage(
                   title='',
                   tabPanel('Compute IRI',
                            h3(textOutput(outputId = "sub.out")),
                            DT::dataTableOutput("excsub_all"),
                            h4(textOutput(outputId = "covariates")),
                            h4(textOutput(outputId = "choose.sub")),
                            plotlyOutput("plot") %>% withSpinner(color="#0dc5c1"),
                            # actionButton("submit", "Submit report"),
                            downloadButton("downloadReport", "Download report"),
                            radioButtons('format', 'Document format', c('HTML')),
                                         inline = TRUE)
                 )
                 ),
        
        id="tabs1"
        # tabPanel("Report")
      )
    )
  )
)

# Define UI for data upload app ----
ui <- fluidPage(
  useShinyjs(),
  # App title ----
  # headerPanel("Individual Reference Intervals estimation workflow v.1.0"),
  tags$head(
    tags$link(rel="stylesheet", type = "text/css",
              href = "https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@400;700;900&display=swap"),
    tags$script(jscode)
  ),
  div(
    class = "main",
    navbarPage(
      title = "IRI App v.1.0",
      id = "main_navbar",
      theme = "style.css",
      fluid = T,
      home,
      user_manual,
      analysis,
      tabPanel(title = "Publications", value = "link_publications", icon = icon("external-link", class = "fa-pull-right")),
      tabPanel(title = "", value = "link_repo", icon = icon("github"))
    )
  )
)

# Define server logic to read selected file ----
server <- function(session, input, output) {
  # For handling click on menu homepage
  observeEvent(input$menu_user_manual, {
    updateTabsetPanel(session, "main_navbar", selected = "user_manual")
  })
  observeEvent(input$menu_analysis,{
    updateTabsetPanel(session, "main_navbar", selected = "analysis")
  })
  
  # read uploaded data
  data1 <- reactive({
    if(input$sample_data==TRUE){
      df<-read.csv("./www/data/iam_clinical_sample.csv", sep=";")
      df
    }else{
      validate(need(input$file1,""))
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      df <- read.csv(inFile$datapath,na.strings = c("", "NA", "#N/A"),
                     header = input$header,sep = input$sep,quote = input$quote)
      df    
      
    }

  })
  
  observeEvent(input$reset_data, {
    shinyjs::reset("data_input")
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
  output$volcano<-renderPlotly({
    trend<-td()
    res<-trend$res
    res.long1<-gather(res[,c(1:2,8,11)], type1, log.p.val, log_p_mk,log_p_cor, factor_key = T)
    res.long2<-gather(res[,c(1:2,4,6)], type2, coeff, MK_tau, spearman_rho, factor_key = T)
    res.long<-cbind(res.long1, res.long2[,-c(1:2)])
    res.long$type<-ifelse(res.long$type1=="log_p_mk" & res.long$type2=="MK_tau", "Mann-Kendall test", "Spearman correlation")
    
    p<-ggplot(res.long, aes(x=coeff, y=log.p.val, group=as.factor(subject), color=as.factor(subject)))+
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
    fig<-ggplotly(p)
    
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
    dat<-data1()
    conf<-c("subject","time","age","sex")
    p<-sum(colnames(dat)%in%conf)
    df3 <- dat[,-c(1:p)]
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
      conf<-c("subject","time","age","sex")
      p<-sum(colnames(data1())%in%conf)
      df.show<-cbind(data1()[,c(1:p)], data2()[,as.character(input$series1)])
      colnames(df.show)<-c(names(data1())[1:p],as.character(input$series1))
      df.show
  })
  
  output$dataplot <- renderPlotly({
    if(input$run){
      conf<-c("subject","time")
      p<-which(colnames(data1())%in%conf)
      
      db<-cbind(data1()[,p], data2()[,as.character(input$series1)])
      colnames(db)<-c("subject","Time","y")
      db$Time<-as.factor(db$Time)
      
      p<-ggplot(db)+
        geom_point(aes(x=as.factor(subject), y=y, color=Time),size=2.5)+
        scale_colour_viridis(discrete = T)+
        labs(y="Measurement", x="Subject")+
        theme_bw()+
        theme(
          legend.title = element_text(size=15),
          legend.text = element_text(size = 14),
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size=15),
          axis.title.y = element_text(size=15),
          axis.ticks = element_blank())
      fig<-ggplotly(p, tooltip = c("y","color"))
      
      
      
    }
  })
  
  # read input mad - outlier data
  trend <- eventReactive(input$mad, {
    if(input$run){
      conf<-c("subject","time")
      p<-which(colnames(data1())%in%conf)
      
      db<-cbind(data1()[,p], data2()[,as.character(input$series1)])
      
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
  output$plot.out<-renderPlotly({
    if(input$run){
      d<-trend()
      d2<-d$df2
      trend<-td()
      d.out<-trend$d.out
      d2<-d2 %>% left_join(., d.out[,c(1:2,ncol(d.out),ncol(d.out)-1)], by=c("subject","time2"="time"))
      d2$pct<-round(d2$pct*100, digits = 2)
      d2<-rename(d2, Percentage_vars_with_outliers=pct)
      
      d2$time2<-as.factor(d2$time2)
      p<-ggplot(d2, aes(x=time2, color=time2))+
        geom_point(aes(y=y,size=Percentage_vars_with_outliers))+
        scale_colour_viridis(discrete = T)+
        geom_hline(data=d$d_mad, aes(yintercept = mad_up), color="red")+
        geom_hline(data=d$d_mad, aes(yintercept = mad_low), color="red")+
        labs(y="Measurement", x="Time")+
        theme_bw()+
        facet_wrap(~subject, scales = "free_x")+
        theme(strip.text = element_text(size=15),
              title = element_text(size=14),
              text = element_text(size=12),
              legend.position = "none")
      fig<-ggplotly(p, tooltip = c("size"))
      fig
      }
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
  output$plot.trend<-renderPlotly({
    if(input$run){
      d <- trend()      
      d$df3$time2<-as.factor(d$df3$time2)
      p<-ggplot(d$df3, aes(x=time2, color=time2))+
        geom_point(aes(y=y), size=4)+
        scale_colour_viridis(discrete = T)+
        geom_hline(data=d$d_mad3, aes(yintercept = mad_up), color="red")+
        geom_hline(data=d$d_mad3, aes(yintercept = mad_low), color="red")+
        labs(y="Measurement", x="Time")+
        theme_bw()+
        facet_wrap(~subject, scales = "free")+
        theme(strip.text = element_text(size=15),
              title = element_text(size=14),
              text = element_text(size=12),
              legend.text = element_text(size=14),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size=15),
              axis.title.y = element_text(size=15),
              axis.ticks = element_blank())
      fig<-ggplotly(p)
      fig
    }
  })

  # read input mad - outlier data
  varcheck <- eventReactive(input$mad, {
    if(input$run){
      conf<-c("subject","time")
      p<-which(colnames(data1())%in%conf)
      
      db<-cbind(data1()[,p], data2()[,as.character(input$series1)])
      
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
  output$plot.var<-renderPlotly({
    if(input$run){
      conf<-c("subject","time")
      p<-which(colnames(data1())%in%conf)
      
      db<-cbind(data1()[,p], data2()[,as.character(input$series1)])
      colnames(db)<-c("subject","time","y")
      db$time<-as.factor(db$time)
      g1<-ggplot(db)+
        geom_point(aes(x=as.factor(subject), y=y, color=time), size=2)+
        scale_colour_viridis(discrete = T)+
        labs(y="Measurement", x="Subject")+
        theme_bw()+
        theme(
          axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size=15),
          axis.title.y = element_text(size=15),
          axis.ticks = element_blank())
      fig1<-ggplotly(g1, tooltip = c("y","color"))
      
      
      d <- varcheck()
      g2<-ggplot(d$varmat.long)+
        geom_point(aes(x=as.factor(subject), y=var.boot), shape=1, size=1, color="darkgrey")+
        geom_point(aes(x=as.factor(subject), y=mean.var), color="blue", size=2)+
        geom_hline(yintercept = d$mad.up, color="red")+
        geom_hline(yintercept = d$mad.low, color="red")+
        labs(y="Bootstrapped Variances", x="Subject")+
        theme_bw()+
        theme(
          axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size=15),
          axis.title.y = element_text(size=15),
          axis.ticks = element_blank())
      fig2<-ggplotly(g2, tooltip = c("y"))
      subplot(fig2, fig1, nrows=1, shareX=TRUE, titleX=TRUE, shareY=FALSE, titleY=TRUE)
#      g1 + g2
    }
  })
  
  # text output outlying variances
  output$sub.out <- renderText({
    d1 <- trend()
    d2 <- varcheck()
    exc<-unique(c(d1$exc_sub, d2$out.mad))

    #paste0(length(exc), " subjects with trends and high variances:")
    paste0(length(exc), " subjects will be excluded from the IRI estimation:")
    
  })
  
  # output$choose.sub <- renderText({
  #   # d1 <- trend()
  #   # d2 <- varcheck()
  #   # exc<-unique(c(d1$exc_sub, d2$out.mad))
  #   # 
  #   paste0("Select subjects to exclude from the IRI estimation:")
  # })
  
  # table output - final subject with trends and correlations and high variance
  output$excsub_all<-DT::renderDataTable({
    d1 <- trend()
    d2 <- varcheck()
    #Subject<-unique(c(d1$exc_sub, d2$out.mad))
    Subject<-c(unique(d1$exc_sub), unique(d2$out.mad))
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
  
  # text output variance
  output$covariates <- renderText({
    inc_conf<-colnames(data1())
    if(("sex" %in% inc_conf) & ("age" %in% inc_conf)){
      paste0("Age and Sex variables are included in the data")
    }else if(("sex" %in% inc_conf) & (!"age" %in% inc_conf)){
      paste0("Only Sex variable is included in the data")
    }else if((!"sex" %in% inc_conf) & ("age" %in% inc_conf)){
      paste0("Only Age variable is included in the data")
    }else{paste0("Both Age and Sex variables are NOT included in the data")}
  })
  
  # read input IRI- JQM
  showplot<-eventReactive(input$iri, {
    conf<-c("subject","time")
    p<-which(colnames(data1())%in%conf)
    df<-cbind(data1()[,p], data2()[,as.character(input$series2)])
    colnames(df)<-c(names(data1())[p],as.character(input$series2))
    df2<-df
    df2<-sort_data_by_time(df2, "time")
    df2<-df2 %>%
      group_by(subject) %>%
      mutate(time = seq(1, n()))
    df2<-as.data.frame(df2)
    
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
#    df2$outlier<-
      
    db<-df2
    subjects<-unique(db$subject)
    cnt<-1; db2<-0; db3<-0
    for(s in subjects) {
      db.y<-db[(db$subject==s),]
      #db.y$outlier<-ifelse(db.y$time==max(db.y$time),2,db.y$outlier) # uncoment this if we want to exclude the last observation
      dba<-db.y
      db.y<-db.y[(db.y$time!=max(db.y$time)),] # exclude the last observation in the IRI estimation
      
      db2<-rbind(db2,dba)
      db3<-rbind(db3,db.y)
    }
    df2<-db2[-1,]; df3<-db3[-1,]

    alpha<- switch(input$empcov,
                   "85%" = 0.15,
                   "90%" = 0.1,
                   "95%" = 0.05)
    
    if((!"sex" %in% input$confound) & (!"age" %in% input$confound)){
      df2a=df2
      source("JQM_Function.R")
    }else if(("age" %in% input$confound) & ("sex" %in% input$confound)){
      df2a<-df2 %>% left_join(., data1()[,c("subject","time","age","sex")], by=c("subject","time"))
      source("JQM_Function_160522_FE.R")
    }else if(("age" %in% input$confound) & (!"sex" %in% input$confound)){
      df2a<-df2 %>% left_join(., data1()[,c("subject","time","age")], by=c("subject","time"))
      source("JQM_Function_160522_age.R")
    }else if(("sex" %in% input$confound) & (!"age" %in% input$confound)){
      df2a<-NULL
    }

    #db=df3, if we want to exclude the last obs
    res<-jqm(db=df2a,
             alpha=alpha,
             lambda.u.seq = seq(0.02,0.1,0.02),
             lambda.z.seq = seq(0.5,5,0.5))
    age<-NULL;sex<-NULL;cnt<-1
    for (s in subjects) {
      df2b<-df2a[df2a$subject==s,]
      age[cnt]<-df2b$age[1]
      sex[cnt]<-df2b$sex[1]
      cnt<-cnt+1
    }
    
    
    uz <- cbind.data.frame(res$u, res$z)
    if(length(age)==0 & length(sex)==0){
      uz$low <- res$beta0 + res$u + res$z*res$beta1
      uz$up  <- res$beta0 + res$u + res$z*res$beta2
    }else if(length(age)!=0 & length(sex)==0){
      uz$low <- res$beta0 + res$u + res$z*res$beta1 + res$beta3*age
      uz$up  <- res$beta0 + res$u + res$z*res$beta2 + res$beta3*age
    }else if(length(age)!=0 & length(sex)!=0){
      uz$low <- res$beta0 + res$u + res$z*res$beta1 + res$beta3*age + res$beta4*sex
      uz$up  <- res$beta0 + res$u + res$z*res$beta2 + res$beta3*age + res$beta4*sex
    }
    uz$id <- unique(df2$subject)
    colnames(uz) <- c("u", "z", "low", "up","id")
    return(list(df=df,
           df2=df2,
           res=res,
           uz=uz,
           inp=input$confound))

  })

  # plot IRI - JQM
  output$plot<-renderPlotly({
    .tmp <- showplot()
    df<- .tmp$df
    df2<- .tmp$df2
    res<- .tmp$res
    uz<- .tmp$uz
    inp<- .tmp$inp
    
    g1<-ggplot(uz, aes(x=as.factor(id))) +
      geom_errorbar(aes(ymin = low, ymax = up), color="darkblue", size=1) +
      geom_point(data = df2, aes(x = as.factor(subject), y = y, color=as.factor(outlier)),
                 position = position_dodge(width = 0.9),size=2) +
      geom_vline(xintercept=seq(1.5, length(unique(df2$subject))-0.5, 1),
                 lwd=0.5, colour="grey") +
      scale_color_manual(name="Outlying observation", labels=c("No","Yes","New measurement"), values = c("darkgrey","red", "darkgreen"))+
      labs(x="Participants",y="Measurement",
           title=paste0("IRI of ",names(df)[3]),
           subtitle = paste0("Empirical coverage=",round(res$cov.tot, digits=4),", confounder: ",inp)) +
      theme_classic()+
      theme(legend.position = "right",
            axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust=1),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            axis.ticks = element_blank(),
            panel.border = element_rect(NA))
    
     fig<-ggplotly(g1, tooltip = c("y"))

  })
  
  
  ###### Report ######
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('IRI_report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('Report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'Report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('Report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)
