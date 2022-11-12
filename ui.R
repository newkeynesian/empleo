library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(shiny)
library(AMR)
library(data.table)
library(DT)
library(ggridges)
library(lubridate)
library(plotly)
library(qicharts2)
library(rintrojs)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(survival)
library(ggpubr)
library(survminer)
library(tidyverse)
library(viridis)
library(zoo)

# ui


dashboardPage(#skin = "red",
################################################################################
  dashboardHeader(title = span(img(src = "COLMEX.png", height = 35), "Woman Labor Conditions"), titleWidth = 300,
                  dropdownMenu(
                    type = "notifications", 
                    headerText = strong("Info"), 
                    icon = icon("info-circle"), 
                    badgeStatus = NULL,
                    notificationItem(
                      text = "World gender wage gap",
                      icon = icon("venus-mars"), href = "https://data.oecd.org/earnwage/gender-wage-gap.htm"
                    ),
                    notificationItem(
                      text = "Facts about the wage gap",
                      icon = icon("school"),  href = "https://www.weforum.org/agenda/2022/03/6-surprising-facts-gender-pay-gap/"
                    ),
                    notificationItem(
                      text = "Social Security Data",
                      icon = icon("user-md")
                    ),
                    notificationItem(
                      text = "Occupation Census",
                      icon = icon("cloud"),
                      href = "https://www.inegi.org.mx/programas/enoe/15ymas/"
                    )
                  ),
                  tags$li(
                    a(
                      strong("About the center"),
                      height = 40,
                      href = "https://cee.colmex.mx/",
                      title = "",
                      target = "_blank"
                    ),
                    class = "dropdown"
                  )
  ),
################################################################################
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Concepts", tabName = "def", icon = icon("book")),
      menuItem("Wage", tabName = "Wage", icon = icon("briefcase")),
      menuItem("Maps", tabName = "mapas", icon = icon("map-marked"), 
               menuSubItem("State", tabName = "State"),
               menuSubItem("County", tabName = "County")),
      menuItem("Working Conditions", tabName = "work", icon = icon("store-alt")),
      menuItem("Econometric Analysis", tabName = "eco", icon = icon("calculator")),
      menuItem("About the Autors", tabName = "aut", icon = icon("user-circle")) 
    )
  ),
################################################################################
  # Body 
  dashboardBody(
    
    shinyDashboardThemes(
    theme =  "poor_mans_flatly" #"purple_gradient" #"blue_gradient" #"flat_red"#"grey_light"
    ),
    tabItems(
      ###GLOSARIO
      
      tabItem(tabName = "def", # Inicia tab Inicio
              h2("
Concepts necessary to understand the conditions of the female labor market", align = "center"),
              p(align="center","We are going to study the Covid-19 effects on the working conditions of women in Mexico, between 2015 and 2022"),
              br(),
              fluidRow(
                tabBox(width = 12,
                       title = "Select a category",
                       id = "tabset1", height = "300px",
                                              tabPanel("Brief History", p(style="text-align: justify;","Women’s situation in the Mexican labor market
During the 1980s and 1990s, women's participation in the labor market increased, largely due to the impacts that the restructuring processes and economic crisis had on households. In that period, the insertion of women into the labor market acted as a compensatory mechanism for households: to face income loss by other members of the household, women had to insert themselves into market activities to preserve the purchasing power of the household. However, this expansion was later followed by a period of stagnation in the percentage of women participating in the Mexican labor market .
The low female participation in the labor market, as well as the experience of women within the labor field, is the result of the social norms that regulate the autonomy of women. These traditional social norms establish a relationship of subordination between men and women, where women are subordinate to men . This relationship of subordination has persisted over time and prevails in some current work situations for women."),
                                                       p(style="text-align: justify;","In relation to the labor market, traditional social norms establish that care work must be carried out mostly by women who live in households, this directly affects the decision of women about participating or not in the labor market. The persistence of these norms is mainly because they are transmitted from generation to generation by those men and women who preserve these types of ideologies. These norms shape the experience of women in the labor market . The explicit expression of these norms is harassment and discrimination in the workplace, such as the wage gap that characterizes the Mexican labor market.
In the Mexican case, since the end of the 20th century, the wage gap between men and women, conditioning by characteristics such as education, age, among other variables, has been reduced to a gap of 9% in the first decade of the 21st century; however, this closing of the labor gap is mainly due to the fact that women who participate in the Mexican labor market are inserted in better paid jobs than the vast majority of those in which men are inserted ."),
                                                       p(style="text-align: justify;","The crisis caused by the COVID-19 pandemic had a negative impact on the employment and working conditions of women in Latin America and the Caribbean, generating a setback of more than a decade in terms of the progress achieved in labor participation. In 2020, there was a mass exodus of women from the labor force who, having to attend to care demands at homes, did not resume the job search. 56.9% of women in Latin America and 54.3% in the Caribbean are employed in sectors where the pandemic is expected to have a greater negative effect in terms of employment and income."),
                                                       column(
                                                         width = 3),box(
                         HTML('<iframe width="800" height="600" src="https://www.youtube.com/embed/XrV6mtxwsDA" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                       )),
                       tabPanel("Social Security Data", box(
                         title = "Main Concepts", solidHeader = TRUE,
                         helpText("Choose a word to know its definition. Source: IMSS Glossary"),
                         selectInput("select_conceptos_pob", label = h4(""), 
                                     choices = glosario_pob$concepto, 
                                     selected = 1)
                       ),
                       box(title = "Definition", solidHeader = TRUE, textOutput("glosario_pob_var")
                       )),
                       tabPanel("Occupation Survey", box(
                         title = "Main Concepts", solidHeader = TRUE,
                         helpText("Choose a word to know its definition. Source: ENOE Glossary"),
                         selectInput("select_conceptos_nat", label = h4(""), 
                                     choices = glosario_nat$concepto, 
                                     selected = 1)
                       ),
                       box(title = "Definition", solidHeader = TRUE, textOutput("glosario_nat_var")
                       )),
                       tabPanel("Gender Equity Concepts", box(
                         title = "Main Concepts", solidHeader = TRUE,
                         helpText("Choose a word to know its definition. Source: UN Glossary"),
                         selectInput("select_conceptos_mor", label = h4(""), 
                                     choices = glosario_mor$concepto, 
                                     selected = 1)
                       ),
                       box(title = "Definition", solidHeader = TRUE, textOutput("glosario_mor_var")
                       ),
                       )
                       
                ) 
              ),
              
              box(
                title = "What Does the Wage Gap Mean for Mexican Women?", width = NULL, solidHeader = TRUE, status = "warning",
                p(align="center","\"An increasing wage gap means women and their families have less money to support themselves, save and invest for the future, and 
                  spend on goods and services. Women, their families, businesses and the economy suffer as a result..\"")
              ),) ,
      
      # First tab content
      tabItem(tabName = "Wage",
              h1("Wage Distribution in Mexico"),
              fluidRow(column(
                width = 3),box(title = "Why is the gender pay gap such a stubborn problem? What can we do?",
                               HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/YJXaW_sWMJo" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                )),
              h2("The following graph shows us the observed quarterly wage gap in Mexico from 2015Q1 to 2022Q2."),
              fluidRow(
                box(plotlyOutput("gr1_plotly")), 
                    box(wellPanel(pickerInput("sex","Select a Serie", choices=c("Male", "Female","Gap"),
                                              selected = unique(enoe_sal_gen_trim$sex)[3], options = list(`actions-box` = FALSE), multiple = T)),
                                            p(style="text-align: justify;","The previous graph was made with the Social Security Data (SSD), 
                        this means that the wages observed here are only of those who are registered and thus formal."),
                        p(style="text-align: justify;","We can see that the wage gap rapidly declined during the pandemic, but it has been the case since 2005.
                          The main reason for this is that  average male quarterly wage suffered a sharp fall during the first and second quarters of 2020."),
                        p(style="text-align: justify;","Furthermore,
                          it is clear that after the pandemic there was a rebound effect that reversed the previously obtained gains."),
                        downloadButton("downloadData1","Download",class="butt2"))),
              h3("How does the wage gap behave conditioned in schooling?"),
              p(style="text-align: justify;","The upcoming graphs, show us the distribution of the Log quarterly wage for each sex and 
                schooling level. It is important to mention that the distribution contemplates the period from 2015 to 2022.
                To construct these distributions we use data from the Occupational survey, so the data also contains informal labor.
                This allows us to represent better the log salary distribution, as a great proportion of employed people are informal."),
                     
               fluidRow(
                box(title = "Female", tags$img(src="dist/a2019m.png",height = 600, width = "100%", deleteFile=FALSE)),
                box(title = "Male", tags$img(src="dist/a2019h.png",height = 600, width = "100%", deleteFile=FALSE))),
              h3("Compare the average quarterly wage by schooling and year."),
              p(style="text-align: justify;","The following plot will allow you to do your own comparisons for the wage of 
                       females/males, at each schooling level and period of your liking. Pick multiple years and view how the average
                       quarterly wage moved during this period for a given education level, or compare multiple schooling levels in the
                       same year! All the data comes from the occupation census."),
              fluidRow(box(plotlyOutput("gr2_plotly", height = 600, width = "100%")),
                      box(wellPanel(pickerInput("year_m","Year", choices=c(unique(salmed$year)), 
                                         selected = unique(salmed$year)[6:7], options = list(`actions-box` = FALSE),multiple = T),
                                         pickerInput("sex_m","Sex", choices=c(unique(salmed$sex)), 
                                                     selected = unique(salmed$sex)[2], options = list(`actions-box` = FALSE),multiple = F),
                                         pickerInput("educ_m","Schooling", choices=c(unique(salmed$educ)), 
                                                     selected = unique(salmed$educ)[8:10], options = list(`actions-box` = FALSE),multiple = T),
                                                     p(style="text-align: justify;","The pandemic had heterogeneous effects on the average quarterly wage by schooling.
                                                     If we separate the female workers into two categories, one which we will name high schooling level that contains females with bachelor, master, and Ph.D., and a low schooling level with high school and below,
                                                       we will notice that one of them benefited from the pandemic, while the other does not."),
                                         p(style="text-align: justify;","For example, between 2020 and 2021 the first had an average quarterly wage loss of -12.8% 
                                         for the three categories, the Ph.D. workers were the most affected with a medium wage shrink of -7.5%, the latter category, on the other hand, 
                                           experienced an increase of its average quarterly wage in 1.8% (not counting Technical careers and normal schools)."),
                                    downloadButton("downloadData2","Download",class="butt2")
                                         )), 
                                                            )
              ),
      
      
      
      
      # Second tab content
      tabItem(tabName = "State",
              h1("Spatial Analysis on the wage gap, a Covid-19 perspective"),
                            p(style="text-align: justify;","In the wage tab, we revise the women's wage conditions in general,
              and now we present the same analysis but include the spatial aspect. In the first part, 
              we present a state analysis of the wage gap. To do this, we generate a wage gap distribution
              per year with the observed values for each state, and then we classify them into four groups. 
              This means that the first quantile is the most equal and the last quantile is the least equal, 
              in terms of the quarterly wage gap between men and women.  
                We use the Occupational Census to calculate the latter."),
              p(style="text-align: justify;","To complement the analysis with the pandemic, we focus our analysis on
                the change in the wage gap and employment during the period of 2019, 2020, and 2021."),
              h2("How does the pandemic affect the economy?"),
              p(style="text-align: justify;","The following video explains the transmission mechanisms of the pandemic 
                into the economy. At the beginning of the pandemic, it was not clear to a lot of people, 
                how this health hazard will change forever their way of living, and most important their economic
                conditions."),
              fluidRow( column(
                width = 3),box(title = "Understanding the Economic Shock of the Covid-19 Crisis",
                HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/RK2IfGPSqO0" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
              )),
              p(style="text-align: justify;","The pandemic affected everyone, but not in the same way or magnitude. In other words, Covid-19 might had have heterogeneous effects.  
      As the wage tab summarizes, the gap between the salaries of men and women shrunk during the pandemic, 
      this happened not because the salary of women increased but rather because the salary of men decreased. 
      This means that the labor conditions of everyone got worse, thus the gap closed in a certain way, 
                which appears to be a good thing in the equality fight but in reality, it does not."),
              h2("State analysis"),
              p(style="text-align: justify;","The following map helps us visualize the classification of each state into quantiles, as mentioned earlier, 
                being lower in the distribution means more equality between the wages of men and women.In the selection box labeled variable,
                you can choose between looking into the actual wage gap or the change in the wage gap. Both are interesting variables to take into account 
                because we can see which regions are less equal than others."),
              fluidRow(box(plotlyOutput("mp2_plotly")),
                       box(wellPanel(pickerInput("year_m2_est","Year", choices=c(unique(mex_map2$year)), 
                                                 selected = unique(mex_map2$year)[1], options = list(`actions-box` = FALSE),multiple = F),
                                     pickerInput("var_m","Variable", choices=c(unique(mex_map2$var)), 
                                                 selected = unique(mex_map2$var)[1], options = list(`actions-box` = FALSE),multiple = F)),
                           p(style="text-align: justify;","It is important to know the spatial distribution and 
                             characteristics of the population that is exposed to the virus, as well as the
                             differentiated effects that COVID-19 may have on them. There are different degrees 
                             of vulnerability among population groups, in demographic, socioeconomic, and health terms. 
                             Furthermore, while at the national and regional levels cities are possibly the least vulnerable
                             places, at the metropolitan scale there may be spatial patterns of internal degrees of vulnerability. 
                             Women have experienced a loss of jobs and a drop in their labor income as a result of the pandemic,
                             but this has occurred in a differentiated way between the states due to the socioeconomic
                             characteristics of each region."),
                           downloadButton("downloadData3","Download",class="butt2")
                        )),
              h2("Some facts about the mexican states"),
              fluidRow(tabBox(
                title = "Highest Average Gap",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", height = "250px",
                tabPanel("1st Guerrero", p(style="text-align: justify;","- Capital: Chilpancingo de los Bravo."),
                         p(style="text-align: justify;","- Has a population of 3,540,685."),
                         p(style="text-align: justify;","- 52% of the population are women."),
                         p(style="text-align: justify;","- 60% of its population lives in urban areas."),
                         p(style="text-align: justify;","- Contributes 1.4% of the national GDP."),
                         p(style="text-align: justify;","- Its primary activity is the service sector (around 76% of its GDP).")),
                tabPanel("2nd Colima",  p(style="text-align: justify;","- Capital: Colima."),
                         p(style="text-align: justify;","- - Has a population of 731,391."),
                         p(style="text-align: justify;","- 50.7% of the population are women."),
                         p(style="text-align: justify;","- 90% of its population lives in urban areas."),
                         p(style="text-align: justify;","- Contributes 0.6% of the national GDP."),
                         p(style="text-align: justify;","- Its primary activity is the service sector (around 70.1% of its GDP).")),
                tabPanel("3rd Yucatan",  p(style="text-align: justify;","- Capital: Merida."),
                         p(style="text-align: justify;","- Has a population of 2,320,898."),
                         p(style="text-align: justify;","- 50.9% of the population are women."),
                         p(style="text-align: justify;","- 86% of its population lives in urban areas."),
                         p(style="text-align: justify;","- Contributes 1.5% of the national GDP."),
                         p(style="text-align: justify;","- Its primary activity is the service sector (around 67.3% of its GDP)."))), 
                tabBox(
                title = "Lowest Average Gap",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset2", height = "250px",
                tabPanel("1st Chiapas", p(style="text-align: justify;","- Capital: Tuxtla Gutierrez."),
                         p(style="text-align: justify;","- Has a population of 5,534,828."),
                         p(style="text-align: justify;","- 51.2% of the population are women."),
                         p(style="text-align: justify;","- 49% of its population lives in urban areas"),
                         p(style="text-align: justify;","- Contributes 1.5% of the national GDP."),
                         p(style="text-align: justify;","- Its primary activity is the service sector (around 71.8% of its GDP).")),
                tabPanel("2nd Tabasco", p(style="text-align: justify;","- Capital: Villahermosa."),
                         p(style="text-align: justify;","- Has a population of 2,402,598."),
                         p(style="text-align: justify;","- 51.1% of the population are women."),
                         p(style="text-align: justify;","- 59% of its population lives in urban areas."),
                         p(style="text-align: justify;","- Contributes 2.3% of the national GDP."),
                         p(style="text-align: justify;","- Its primary activity is the secondary industry (around 54.2% of its GDP).")),
                tabPanel("3rd Campeche", p(style="text-align: justify;","- Capital: San Francisco de Campeche."),
                         p(style="text-align: justify;","- Has a population of 928,363."),
                         p(style="text-align: justify;","- 50.8% of the population are women."),
                         p(style="text-align: justify;","- 75% of its population lives in urban areas."),
                         p(style="text-align: justify;","- Contributes 2.1% of the national GDP"),
                         p(style="text-align: justify;","- Its primary activity is the secondary industry (around 76.9% of its GDP)."))
              )
              )
              ),
             tabItem(tabName = "County", 
                     h2("County analysis"),
              p(style="text-align: justify;","In this part, you can look up each county's position on the wage gap 
              distribution. Using the same methodology, we classify each county, 
                into the yearly distribution taking into account all counties.If you are interested in a particular state, just select it in the Entity box! 
                The map will display the counties and their classification in the distribution for each year."),
                            fluidRow(box(plotlyOutput("mp1_plotly"),p(style="text-align: justify;","Since the territory plays a preponderant role in the context
                                                                      of the pandemic in Mexico, analyzing the evolution of the wage gap between the different 
                                                                      counties of the 32 states gives us a broader picture of the heterogeneous effects of the 
                                                                      pandemic that considers the socioeconomic characteristics from each county. We can speak 
                                                                      of different degrees of vulnerability of the women's employment situation by county and 
                                                                      highlight the role of large cities and urban centers that led to the spread of covid-19 
                                                                      and influenced the weakening of women's working conditions."),
                                         p(style="text-align: justify;","Is important to point out that some municipalities do not have representativity in the 
                                           occupation census, thus, they were left out. ")),
                       box(wellPanel(pickerInput("year_m2","Year", choices=c(unique(mex_map$year)), 
                                                 selected = unique(mex_map$year)[5], options = list(`actions-box` = FALSE),multiple = F),
                                     pickerInput("est_m","Entity", choices=c(unique(mex_map$ent)), 
                                                 selected = unique(mex_map$ent)[2], options = list(`actions-box` = FALSE),multiple = F)),
                          box(column(12, align="center",tableOutput("mytable")), 
                              downloadButton("downloadData4","Download",class="butt2")),  
                              box(p(style="text-align: justify;","The table displays a ranking of which states have the biggest gender gap in each year.
                                                              The State which appear the most in different years is Quintana Roo."),
                                                            p(style="text-align: justify;","Let the table be a guide to select the entities that have a greater wage gap, and explore which counties are responsible for that.
                                                            For example, in the year 2020, the worst entity was Aguascalientes, and the worst county in that state was El Llano.")
                                                            ))
      )
      ),       ### Working Conditions
      
      tabItem(tabName = "work",
              h1("Women Working Conditions During the Pandemic"),
              p(style="text-align: justify;","Now we present a brief analysis of the female labor market conditions during the pandemic.
              We will show how female unemployment moved as a consequence of Covid-19, and also how these movements were not necessarily
                homogenous through different schooling levels or household conditions."),
              h2(""), 
              fluidRow( 
                box(title = "Change in weekly minutes worked", tags$img(src="hours.png",height = "100%", width = "100%", deleteFile=FALSE))
                ,  box(p(style="text-align: justify;","The labor market comprises a set of interactions between
                         various economic agents. These simultaneous interactions generate structures that are not always efficient, as an example, 
                         we have the case of gender differences in the labor market. Through different indicators we can approach the gender
                         differences in the labor market, among them, the labor participation measured through the change in the weekly minutes 
                         worked. We must consider that there are observable and unobservable factors that correlate with the existence of gender gaps; 
                         however, through the observable factors, we can realize the magnitude of the problem in the deficiency of labor structures.
                         Women have lost significantly more hours of work than men around the world. Overall, the pandemic has reversed progress on gender 
                         parity in labor participation. This decline in the labor market risks continuing in the long term. Likewise, the reduction 
                         in the labor participation of women has important consequences in other dimensions of employment and in the distribution of
                         unpaid work, which affect the way in which women access opportunities in the economic sphere, as well as in other spheres of 
                         life."),
                       p(style="text-align: justify;","The In the graph, we can see how the average 
                         weekly minutes for the women's labor force have moved each year."),
                       p(style="text-align: justify;","2020 stands out as the change in 
                         weekly working time measured in minutes drops by around 67 units.We think that the main cause of this dramatic drop in 
                         worked minutes is a direct result of the lockdown policy implemented during the first year of the pandemic. 
                         Another indicator is that after the lockdown policy was dropped we observed a rapid growth in the weekly minutes worked. 
                         Prior to that, we observed a steady increase in the working time of the female."),
                       p(style="text-align: justify;","Finally, we can notice that the working time, in the following years 
                         of the pandemic, has recovered to the pre-pandemic level."))
              ),
              fluidRow(box(wellPanel(pickerInput("year_w","Year", choices=c(unique(employ$year)), 
                                                 selected = unique(mex_map$year)[4:5], options = list(`actions-box` = FALSE),multiple = T),
                                     pickerInput("var_w","Variable", choices=c(unique(employ$var)), 
                                                 selected = unique(mex_map$ent)[2], options = list(`actions-box` = FALSE),multiple = F)),
                           p(style="text-align: justify;","The graph shows us the employment level in the woman's labor market. The first variable we 
                             see is the PEA (that stands for economically active population), and we compare 2019 and 2020 to observe if there
                             was a significant change in the proportion of people aged 12 years and over who engaged in some type of economic 
                             activity (employed population), or who actively sought to do so (open unemployed population)."),
                           p(style="text-align: justify;","It is important to clarify concepts such as Available and non-available, 
                           the first ones are people aged 12 years or over who did not work, nor had employment and did not actively 
                           seek one, due to discouragement or they think that they would not be given a job because of their age, studies, etc.; 
                           but, they would be willing to accept a job if they are offered, however, do not actively seek one. 
The latter are people with the same delimitation as the first one, but with the distinction that they are not willing to accept a job even when offered."),
                           p(style="text-align: justify;","Finally, the sum of employed, unemployed, available, and non-available constitute the 
                             entirety of the women's labor market. "), 
                           downloadButton("downloadData5","Download",class="butt2"),
                           ),
                       box(title = "Women labor market",plotlyOutput("w1_plotly"))),
              h1(""),
              fluidRow(box(title = "Worked Hours",plotlyOutput("h1_plotly")),
                       box(wellPanel(sliderInput("year_h", "Year", min=2015,max= 2022,value = c(2015,2022),sep = ""),
                                     pickerInput("var_h","Variable", choices=c(unique(hours$var)), 
                                                 selected = unique(hours$var)[1], options = list(`actions-box` = FALSE),multiple = F),
                           pickerInput("cat_h","Category", choices=c(unique(hours$categoria)), 
                                       selected = unique(hours$categoria)[1:5], options = list(`actions-box` = FALSE),multiple = T)),
                           p(style="text-align: justify;",'You can observe the evolution of weekly hours worked by women during the time period of 2015 to 2022. 
The additional feature here is that you can classify the female workforce by income, position, schooling, and sector. For example, if you select income, you will
                             now have five categories of income, the first being  "up to a minimum wage" and see how many hours per week the people that work for a minimum wage average through the year. '),
                           downloadButton("downloadData6","Download",class="butt2")),
                       ),
              h2("Highlights"),
              fluidRow(
                column(3, wellPanel(a("Employment, wages, and the gender gap in Mexico: Evidence of the urban labor market", href="https://doi.org/10.1016/j.latcb.2022.100055"), 
                                    br(""), (strong("Article | March 2022")))),
                column(3, wellPanel(a("The Gendered Impacts of COVID-19 on Labor Markets in Latin America and the Caribbean", href="https://www.worldbank.org/en/results/2021/05/05/the-gendered-impacts-of-covid-19-on-labor-markets-in-latin-america-and-the-caribbean"), 
                                    br(""), (strong("The World Bank | May 2021")))),
                column(3, wellPanel(a("Gender and Employment in the COVID-19 Recession: Evidence on “She-cessions", href="https://www.imf.org/en/Publications/WP/Issues/2021/03/31/Gender-and-Employment-in-the-COVID-19-Recession-Evidence-on-She-cessions-50316"), 
                                    br(""), (strong("IMF Working Paper | March 2021")))),
                column(3, wellPanel(a("COVID-19 and gender equality: Countering the regressive effects", href="https://www.mckinsey.com/featured-insights/future-of-work/covid-19-and-gender-equality-countering-the-regressive-effects"), 
                                    br(""), (strong("McKinsey & Company | July 2020")))),
              ),
              ), 
      tabItem(tabName = "eco",
              h1("Econometric Analysis"),
              p(style="text-align: justify;","We present some simple econometric analyses that will help us understand the effects of the covid pandemic on woman labor conditions. 
                As presented in prior tabs, the pandemic had a setback on all the progress in woman wages, working hours, and workforce participation. 
                The first graph shows the values of the wage gap for different points of the income distribution. The second plot, allows us to explore
                the relationship between the Covid-19 death rate and variables such as Weekly worked hours, Log wage, and the employment rate, by state and year."),
              h2("Quantile Wage gap"),             
              fluidRow(box(plotlyOutput("q1_plotly")),
                       box(wellPanel(pickerInput("year_q","Year", choices=c(unique(qreg$year)), 
                                                 selected = unique(qreg$year)[1], options = list(`actions-box` = FALSE),multiple = F)),
                       p(style="text-align: justify;","We estimated yearly quantile regressions of the wage gap, we included some covariates
                         to get a more precise estimation of it. The results are presented in the left graph, where you can choose 
                         between different years, and observe how the inequality between men and women over the quantiles has evolved since 2018."), 
                       column(12, align="center",tableOutput("qregtab")), p(style="text-align: justify;","The latter table shows us the percent change 
                                                                             of the wage gap between periods, for example, the 2019 column presents that 
                                                                             for the first quartile there was an increase of the gap of about 6.86 percent 
                                                                             between 2018 and 2019. During the first year of the pandemic, we can see that 
                                                                             the gap drastically closed for the top quantiles, but not for the first 75 percent 
                                                                             of the income distribution. After that, the inequality recovered the pre-pandemic levels."),
                       downloadButton("downloadData7","Download",class="butt2"))),
              h2("Scatter plot of the Covid Dead Rate and other Variables"),
              p(style="text-align: justify;","The covid death rate was calculated as the number of cases for every 100,000 people in the state at a given point in time. 
                This resolves the problem of comparing cities with a different number of habitats. It is not the same to compare Mexico city, which has around 28 million people, to Los Mochis which has roughly 300 thousand."),
              fluidRow(box(plotlyOutput("scatter_plotly")),
                       box(wellPanel(pickerInput("year_s","Year", choices=c(unique(scat$year)), 
                                                 selected = unique(scat$year)[1], options = list(`actions-box` = FALSE),multiple = T),
                                     pickerInput("var_s","Variable", choices=c(unique(scat$var)), 
                                                 selected = unique(scat$var)[1], options = list(`actions-box` = FALSE),multiple = F)),
                           p(style="text-align: justify;","We noticed that the relationship between the employment rate and the Death 
                             rate is negative, meaning that a higher covid death rate is paired with lower employment.  
                             The same occurs with working hours, and this is causal because of the lockdown policy. 
                             Finally, we have that the log wage is positively correlated with the death rate, meaning that
                             there was a risk premium on salaries. "),
                           downloadButton("downloadData8","Download",class="butt2")))
              ),
      
            ###autores
      tabItem(tabName = "aut",
              h1("El Colegio de Mexico", align = "center"),
              h3("Applied Econometrics 2022",br(),"PhD Raymundo Campos Vazquez", align = "center"),
              br(),
              fluidRow(
                box(h4(strong("Raul Antonio Tirado Cossio"),align="center"),width = 4,
                    br(),
                    HTML('<center><img src="r2.jpeg" height="300" width="200"></center>'),
                    br(),
                    a("rtirado@colmex.mx", href = "mailto:rtirado@colmex.mx"),br(),
                    a("@Research Gate", href = "https://www.researchgate.net/profile/Raul-Tirado-Cossio"),align="center"),
                box(h4(strong("Hector Gonzalez Magana"),align="center"),h4("The gap master"),width = 4,
                    br(),
                    HTML('<center><img src="hec.jpeg" height="300" width="200"></center>'),
                    br(),
                    a("hgonzalez@colmex.mx", href = "mailto:hgonzalez@colmex.mx"),br(),
                 a("@gmagana_hector", href = "https://twitter.com/gmagana_hector"),align="center"),
                      box(h4(strong("Arlenne Fierros Hernandez"),align="center"),width = 4,
                    br(),
                    HTML('<center><img src="arl.jpeg" height="300" width="300"></center>'),
                    br(),
                    a("afierros@colmex.mx", href = "mailto:afierros@colmex.mx"),align="center",br()
                    
                
              )),
              br(),
              br(),
              h5("",
                 tags$a(target="_blank",href="https://github.com/newkeynesian/empleo","GitHub Repository")," Thanks for visiting!",br(), 
                 strong("If you have any comments, please let us know."),align = "center"),
              h6("",
                 tags$a(imageOutput("image1"), href="https://www.caliente.mx/inicio?gclsrc=aw.ds"),align = "center"
                 )
              
      )
              
      
    )
  )
################################################################################
)
