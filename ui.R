library(shiny)
library(shinythemes)
data=read.csv("https://github.com/kang20006/divorce-data-project/raw/main/divorce.csv",sep=";")
fluidPage(theme = shinytheme("cerulean"),
              navbarPage(
                "Divorce Predictor",
                tabPanel("Prediction of probability of divorce",
                         headerPanel('Divorce Predictor'),
                         sidebarPanel(
                           HTML("<h3>Complete the survey</h4>"),
                           sliderInput("Atr1", label = "If one of us apologizes when our discussion deteriorates, the discussion ends.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr2", label = "I know we can ignore our differences, even if things get hard sometimes.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr3", label = "When we need it, we can take our discussions with my spouse from the beginning and correct it.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr4", label = "When I discuss with my spouse, contacting him will eventually work.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr5", label = "The time I spent with my wife is special for us.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr6", label = "We don't have time at home as partners.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr7", label = "We are like two strangers who share the same environment at home rather than family.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           
                           sliderInput("Atr8", label = "I enjoy our holidays with my wife.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr9", label = "I enjoy traveling with my wife.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr10", label = "Most of our goals are common to my spouse.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr11", label = "I think that one day in the future, when I look back, I see that my spouse and I have been in harmony with each other.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr12", label = "My spouse and I have similar values in terms of personal freedom.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr13", label = "My spouse and I have a similar sense of entertainment.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr14", label = "Most of our goals for people (children, friends, etc.) are the same.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr15", label = "Our dreams with my spouse are similar and harmonious.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr16", label = "We're compatible with my spouse about what love should be.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr17", label = "We share the same views about being happy in our life with my spouse.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr18", label = "My spouse and I have similar ideas about how marriage should be.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr19", label = "My spouse and I have similar ideas about how roles should be in marriage.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr20", label = "My spouse and I have similar values in trust.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr21", label = "I know exactly what my wife likes.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr22", label = "I know how my spouse wants to be taken care of when she/he is sick.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr23", label = "I know my spouse's favourite food.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr24", label = "I can tell you what kind of stress my spouse is facing in her/his life.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr25", label = " I have knowledge of my spouse's inner world.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr26", label = "I know my spouse's basic anxieties.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr27", label = "I know what my spouse's current sources of stress are.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr28", label = "I know my spouse's hopes and wishes.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr29", label = "I know my spouse very well.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr30", label = "I know my spouse's friends and their social relationships.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr31", label = "I feel aggressive when I argue with my spouse.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr32", label = "When discussing with my spouse, I usually use expressions such as ‘you always’ or ‘you never’.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr33", label = " I can use negative statements about my spouse's personality during our discussions.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr34", label = "I can use offensive expressions during our discussions.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr35", label = "I can insult my spouse during our discussions.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr36", label = "I can be humiliating when we discuss.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           
                           sliderInput("Atr37", label = "My discussion with my spouse is not calm.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr38", label = "I hate my spouse's way of opening a subject.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr39", label = "Our discussions often occur suddenly.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr40", label = "We're just starting a discussion before I know what's going on.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr41", label = "When I talk to my spouse about something, my calm suddenly breaks.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr42", label = "When I argue with my spouse, ı only go out and I don't say a word.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr43", label = "I mostly stay silent to calm the environment a little bit.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr44", label = "Sometimes I think it's good for me to leave home for a while.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr45", label = "I'd rather stay silent than discuss with my spouse.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr46", label = "Even if I'm right in the discussion, I stay silent to hurt my spouse.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr47", label = "When I discuss with my spouse, I stay silent because I am afraid of not being able to control my anger.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr48", label = "I feel right in our discussions.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr49", label = "I have nothing to do with what I've been accused of.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr50", label = "I'm not actually the one who's guilty about what I'm accused of.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr51", label = "I'm not the one who's wrong about problems at home.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr52", label = "I wouldn't hesitate to tell my spouse about her/his inadequacy.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr53", label = "When I discuss, I remind my spouse of her/his inadequacy.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           sliderInput("Atr54", label = "I'm not afraid to tell my spouse about her/his incompetence.",
                                       value = 0,
                                       min = 0,
                                       max = 4
                           ),
                           
                           actionButton("submitbutton", "Submit", class = "btn btn-primary")
                         ),#side panel
                         
                         mainPanel(
                           tags$label(h3('Status/Output')), # Status/Output Text Box
                           verbatimTextOutput('contents'),
                           tableOutput('tabledata') # Prediction results table
                           
                         )#main page
                ), # Navbar 1, tabPanel
                tabPanel("Data used for training", 
                         headerPanel('Divorce predictor dataset'),
                         fluidRow(
                           column(12,
                                  dataTableOutput('table')
                           )
                         )
                ), #Navbar 2, tabPanel
                tabPanel("Prediction through uploading file", 
                         sidebarLayout(
                           
                           # Sidebar panel for inputs ----
                           sidebarPanel(
                             
                             # Input: Select a file ----
                             fileInput("file1", "Choose CSV File, must contain Atr1 to Atr54",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")),
                             
                             # Horizontal line ----
                             tags$hr(),
                             
                             # Input: Checkbox if file has header ----
                             checkboxInput("header", "Header", TRUE)
                             
                           ),#sidepanel  
                           
                           mainPanel(
                             
                             # Output: Data file ----
                             tableOutput("pred")
                             
                           )#main page
                         )#Navbar3,tabpanel
                         
                ),
                tabPanel("Info about model LDA",
                         headerPanel('Accuracy of LDA model'),
                         HTML("<h3>LDA is the most suitable model to be depolyed as it has high accuracy which is about 98%.</h4>"),
                         tags$div(img(src = "https://github.com/kang20006/divorce-data-project/raw/main/lda.png")) 
                ),#Navbar4,tab
                tabPanel("About",
                         headerPanel('About the project'),
                         HTML("<h3>We are group PTS from course Predictive Modelling Jan 2021. The web app is main to deloy the best model (LDA)
                          obtained from our group assignment. The model is aimed to predict the marriage status of participants through answering 
                          the 54 questions of survey. User able to predict their marriage status by answering the survey or upload a CSV file.<h4>"),
                         HTML("<h3>Our dataset is available on UCI website https://archive.ics.uci.edu/ml/datasets/Divorce+Predictors+data+set<h4/>"),
                         
                         HTML("<h3>Our group report and codes are available on https://github.com/kang20006/divorce-data-project</h4>"),
                )         
              )#navelbar page 
)#fluidpage
