list_of_packages <- c("shiny", "bslib", "shinythemes", "periscope", "readxl", "writexl",
                      "grid", "gridExtra", "dplyr")
lapply(list_of_packages, 
       function(x) if(!require(x,character.only = TRUE)) install.packages(x))
library(shiny) 
library(bslib)
library(periscope)
library(shinythemes)
library(readxl)
library(writexl)
library(grid)
library(gridExtra)
library(dplyr)
dir <- getwd()
setwd(dir)
summary_table <- read_xlsx("Summary_Patient_chronic_GvHD.xlsx")
previous_data <- length(summary_table$Name)
birth <- ifelse(previous_data > 0, summary_table$`Date of birth`[1], Sys.Date())
HSCT <- ifelse(previous_data > 0, summary_table$`Date HSCT`[1], Sys.Date())
ID <- if(previous_data > 0) {summary_table$`Patient ID`[1]}
treatment <- c("No CSA", "No MMF", "No mPDN", "No topical steroid", "No Ruxolitinib", "No ECP", "No Etanercept", 
               "No Infliximab", "No Abatacept", "No Belumosudil", "No Begelomab", "No Sirolimus", "No mPDN Pulse", "No MSC", 
               "No Axatilimab", "No Ibrutinib", "No FAM", "No Imatinib", "No topical ocular treatment")

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("cGvHDtrackR"),
                fluidRow(
                  column(3,
                        sidebarLayout(
                          sidebarPanel(width=12, id="sidebar1",
                                       h4("Patient general information"),
                            textInput("Name", "Name", value = summary_table$Name[1]),
                            textInput("Surname", "Surname", value = summary_table$Surname[1]),
                            dateInput("DateBirth", "Date of birth",
                                      language = "it", value = birth),
                            dateInput("DateHSCT", "Date of HSCT",
                                      language = "it", value = HSCT),
                            textInput("ID", "Patient ID", value = ID)),
                          mainPanel (width = 0)
                        )
                  ),
                  column(3,
                         sidebarLayout(
                           sidebarPanel(width=12, id="sidebar2",
                                        h4("GvHD"),
                                        dateInput("Date", "Date of today",
                                                  language = "it"),
                                        radioButtons("GvHD", "The patient has GvHD?", c("No", "Yes")),
                                        selectInput("Skin1", "Skin manifestation", 
                                                    c("No Skin manifestations", "Macupapular rash / erythema", "Liche planus-like features",
                                                      "Sclerotic features", "Papulosquamous lesions or ichthyosis", 
                                                      "Keratosis pilaris-like")),
                                        selectInput("Skin2", "BSA involvment", 
                                                    c("No BSA involved", "1-18% BSA", "19-50% BSA", ">50% BSA")),
                                        selectInput("Skin3", "Sclerotic features", 
                                                    c("No Sclerotic features", "Superficial sclerotic features (able to pinch)", 
                                                      "Deep sclerotic features / unable to pinch / impaired mobility / ulceration")),
                                        selectInput("Mouth", "Mouth symptoms", c("No symptoms", "Mild symptoms with disease signs but not limiting oral intake significantly",
                                                                                    "Moderate symptoms with partial limitation of oral intake",
                                                                                    "Severe symptoms with disease signs on examination with major limitation of oral intake")),
                                        selectInput("Eyes", "Eyes symptoms", c("No symptoms", "Mild dry eye symptoms not affecting ADL (requiring lubrificant <3x per day)",
                                                                               "Moderate dry eye symptoms partially effect ADL (requiring lubrificant >3x per day or punctual plugs) without new vision impairment due to Keratoconjunctivitis sicca (KCS)",
                                                                               "Severe dry eye symptoms significantly affecting ADL OR Unable to work because of ocular symptoms OR Loss of vision due to KCS")),
                                        selectInput("GI", "GI symptoms", 
                                                    c("No symptoms", "GI symptoms without significant weightloss (<5%) over the past 3 months", 
                                                      "GI symptoms associated with mild to moderate weightloss (5-15%) over the past 3 months OR Moderate diarrhea without significant interference with daily living", 
                                                      "GI symptoms associated with significant weightloss (>15%) over the past 3 months, Requires nutritional supplement for most caloric needs OR Esophageal dilation OR Severe diarrhea with interference with daily living")),
                                        selectInput("GI_low", "Lower GI biopsy", c("NA","No signs of GvHD", "Mild", "Moderate", "Severe")),
                                        selectInput("GI_high", "Upper GI biopsy", c("NA","No signs of GvHD", "Mild", "Moderate", "Severe")),
                                        selectInput("Liver", "Liver function", c("Normal total bilirubin and ALT or AP <3 x ULN",
                                                                                 "Normal total bilirubin with ALT >3 to 5 x ULN or AP >3 x ULN",
                                                                                 "Elevated total bilirubin but < 3mg/dL or ALT >5 x ULN",
                                                                                 "Elevated total bilirubin but > 3mg/dL")),
                                        selectInput("Liver2", "Liver biopsy", c("NA","No signs of GvHD", "Mild", "Moderate", "Severe")),
                                        selectInput("Lungs", "Lungs symptoms", c("No symptoms", "Mild symptoms (shortness of breath after climbing one flight of steps)",
                                                                                 "Moderate symptoms (shortness of breath after walking on flat ground)", "Severe symptoms (shortness of breath at rest; requiring O2")),
                                        selectInput("LungsScore", "Lungs score", c("FEV1 > 80%", "FEV1  60-79%", "FEV1 40-59%", "FEV1 < 39%")),
                                        selectInput("JointAndFascia", "Joints and Fascia", c("No symptoms", 
                                                                                             "Mild tightness of arms or legs, normal or mild decreased range of motion (ROM) AND not affecting ADL",
                                                                                             "Tightness of arms or legs OR joint contractures, eythema thought due to fascitis, moderate decreased ROM AND mild to moderate limitation of ADL",
                                                                                             "Contractures with significant decrease of ROM and significant limitation of ADL")),
                                        selectInput("GenitalTract1", "Genital Tract manifestations", c("No signs", "Erosions, fissures", "Lichen planus-like features",
                                                                                                       "Lichen sclerosus-like features", "Labial / vaginal scarring",
                                                                                                       "Phimosis")),
                                        selectInput("GenitalTract2", "Genital Tract signs", c("No signs", "Mild signs", "Moderate signs", "Severe signs with or without symptoms"))),
                           mainPanel(width  = 0)
                         )
                  ),
                  column(3,
                         sidebarLayout(
                           sidebarPanel(width=12, id="sidebar3",
                                        h4("Ongoing Therapy"),
                                        selectInput("input1", "Select Therapy", choices = c('',
                                                                                            'Cyclosporine',
                                                                                            'Mycophenolate Mofetil',
                                                                                            'Methylprednisolone',
                                                                                            'Topical steroid',
                                                                                            'Ruxolitinib',
                                                                                            'Extracorporeal photopheresis',
                                                                                            'Etanercept',
                                                                                            'Infliximab',
                                                                                            'FAM',
                                                                                            'Abatacept',
                                                                                            'Belumosudil',
                                                                                            'Axatilimab',
                                                                                            'Begelomab',
                                                                                            'Ibrutinib',
                                                                                            'Sirolimus',
                                                                                            'Imatinib',
                                                                                            'mPDN Pulse',
                                                                                            'Mesenchymal Stromal Cell Therapy',
                                                                                            'Topical ocular treatment',
                                                                                            'Other treatments')),
                                        conditionalPanel(condition = "input.input1 == ''",
                                                         h5("No Therapy")),
                                        conditionalPanel("input.input1 == 'Cyclosporine'",
                                                         selectInput("CSA", "CSA treatment", 
                                                                     c("No CSA", "Full dose", "Tapering", "Stop CSA")),
                                                         dateInput("DateCSA", "Date start /change CSA",
                                                                   language = "it", ),
                                                         dateInput("DateCSAStop", "Date stop CSA",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Mycophenolate Mofetil'",
                                                         selectInput("MMF", "MMF treatment", 
                                                                     c("No MMF", "Full dose", "Tapering", "Stop MMF")),
                                                         dateInput("DateMMF", "Date start /change MMF",
                                                                   language = "it", ),
                                                         dateInput("DateMMFStop", "Date stop MMF",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Methylprednisolone'",
                                                         selectInput("mPDN", "mPDN treatment", 
                                                                     c("No mPDN", "1.8-2.5 mg/kg", "1.5-1.7 mg/kg", "1-1.4 mg/kg", "0.5-0.9 mg/kg", "<0.5 mg/kg", "Stop mPDN")),
                                                         dateInput("DatemPDN", "Date start / change mPDN",
                                                                   language = "it"),
                                                         dateInput("DatemPDNStop", "Date stop mPDN",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Topical steroid'",
                                                         selectInput("Steroid", "Topical steroid treatment", 
                                                                     c("No topical steroid", "Topical steroid", "Stop topical steroid")),
                                                         dateInput("DateSteroid", "Date start topical steroid",
                                                                   language = "it"),
                                                         dateInput("DateSteroidStop", "Date stop topical steroid",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Ruxolitinib'",
                                                         selectInput("Ruxo", "Ruxolitinib treatment", 
                                                                     c("No Ruxolitinib", "Full dose", "Tapering", "Stop Ruxolitinib")),
                                                         dateInput("DateRuxo", "Date start / change Ruxolitinib",
                                                                   language = "it"),
                                                         dateInput("DateRuxoStop", "Date stop Ruxolitinib",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Extracorporeal photopheresis'",
                                                         selectInput("ECP", "ECP treatment", 
                                                                     c("No ECP", "ECP", "Stop ECP")),
                                                         dateInput("DateECP", "Date start ECP",
                                                                   language = "it"),
                                                         dateInput("DateECPStop", "Date stop ECP",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Etanercept'",
                                                         selectInput("Etanercept", "Etanercept treatment", 
                                                                     c("No Etanercept", "Etanercept", "Stop Etanercept")),
                                                         dateInput("DateEtanercept", "Date start Etanercept",
                                                                   language = "it"),
                                                         dateInput("DateEtanerceptStop", "Date stop Etanercept",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Infliximab'",
                                                         selectInput("Infliximab", "Infliximab treatment", 
                                                                     c("No Infliximab", "Infliximab", "Stop Infliximab")),
                                                         dateInput("DateInfliximab", "Date start Infliximab",
                                                                   language = "it"),
                                                         dateInput("DateInfliximabStop", "Date stop Infliximab",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'FAM'",
                                                         selectInput("FAM", "FAM treatment", 
                                                                   c("No FAM", "FAM", "Stop FAM")),
                                                         dateInput("DateFAM", "Date start FAM",
                                                                   language = "it"),
                                                         dateInput("DateFAMStop", "Date stop FAM",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Abatacept'",
                                                         selectInput("Abatacept", "Abatacept treatment", 
                                                                     c("No Abatacept", "Abatacept", "Stop Abatacept")),
                                                         dateInput("DateAbatacept", "Date start Abatacept",
                                                                   language = "it"),
                                                         dateInput("DateAbataceptStop", "Date stop Abatacept",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Belumosudil'",
                                                         selectInput("Belumosudil", "Belumosudil treatment", 
                                                                     c("No Belumosudil", "Belumosudil", "Stop Belumosudil")),
                                                         dateInput("DateBelumosudil", "Date start Belumosudil",
                                                                   language = "it"),
                                                         dateInput("DateBelumosudilStop", "Date stop Belumosudil",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Axatilimab'",
                                                         selectInput("Axatilimab", "Axatilimab treatment", 
                                                                     c("No Axatilimab", "Axatilimab", "Stop Axatilimab")),
                                                         dateInput("DateAxatilimab", "Date start Axatilimab",
                                                                   language = "it"),
                                                         dateInput("DateAxatilimabStop", "Date stop Axatilimab",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Begelomab'",
                                                         selectInput("Begelomab", "Begelomab treatment", 
                                                                     c("No Begelomab", "Begelomab", "Stop Begelomab")),
                                                         dateInput("DateBegelomab", "Date start Begelomab",
                                                                   language = "it"),
                                                         dateInput("DateBegelomabStop", "Date stop Begelomab",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Ibrutinib'",
                                                         selectInput("Ibrutinib", "Ibrutinib treatment", 
                                                                     c("No Ibrutinib", "Ibrutinib", "Stop Ibrutinib")),
                                                         dateInput("DateIbrutinib", "Date start Ibrutinib",
                                                                   language = "it"),
                                                         dateInput("DateIbrutinibStop", "Date stop Ibrutinib",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Sirolimus'",
                                                         selectInput("Sirolimus", "Sirolimus treatment", 
                                                                     c("No Sirolimus", "Full dose", "Tapering", "Stop Sirolimus")),
                                                         dateInput("DateSirolimus", "Date start / change Sirolimus",
                                                                   language = "it"),
                                                         dateInput("DateSirolimusStop", "Date stop Sirolimus",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Imatinib'",
                                                         selectInput("Imatinib", "Imatinib treatment", 
                                                                     c("No Imatinib", "Imatinib", "Stop Imatinib")),
                                                         dateInput("DateImatinib", "Date start Imatinib",
                                                                   language = "it"),
                                                         dateInput("DateImatinibStop", "Date stop Imatinib",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'mPDN Pulse'",
                                                         selectInput("Pulse", "mPDN Pulse treatment", 
                                                                     c("No mPDN Pulse", "mPDN Pulse", "Stop mPDN Pulse")),
                                                         dateInput("DatePulse", "Date start mPDN Pulse",
                                                                   language = "it"),
                                                         dateInput("DatePulseStop", "Date stop mPDN Pulse",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Mesenchymal Stromal Cell Therapy'",
                                                         selectInput("MSC", "MSC treatment", 
                                                                     c("No MSC", "MSC", "Stop MSC")),
                                                         dateInput("DateMSC", "Date start MSC",
                                                                   language = "it"),
                                                         dateInput("DateMSCStop", "Date stop MSC",
                                                                   language = "it")),
                                        conditionalPanel("input.input1 == 'Topical ocular treatment'",
                                                         selectInput("Ocular", "Topical ocular treatment", 
                                                                     c("No topical ocular treatment", "Topical ocular treatment", "Stop topical ocular treatment")),
                                                         dateInput("DateOcular", "Date start topical ocular treatment",
                                                                   language = "it"),
                                                         dateInput("DateOcularStop", "Date stop topical ocular treatment",
                                                                   language = "it"),
                                                         textInput("OcularText", "Specify topical ocular treatment")),  
                                        conditionalPanel("input.input1 == 'Other treatments'",
                                                         textInput("Other", "Other treatment"),  
                                                         textInput("DateOther", "Date start Treatment"),
                                                         textInput("DrugOther", "Dose"),
                                                         textInput("DrugOtherStop", "Date stop Treatment"))),
                           mainPanel(width = 0),
                         )
                  ),
                column(8,
                         sidebarLayout(
                           sidebarPanel(width=12, id="sidebar4",
                                        h4("Body Surface Area"),
                                        img(src="repview.png", width = 1000, height = 500)
                           ),
                           mainPanel(width = 0)
                        )
                )),
                mainPanel(
                  downloadButton('download',"Download data"),
                  downloadButton('update',"Update Summary"),
                  downloadButton('report',"Download daily report"),
                  textOutput("title1"),
                  tableOutput("table1"),
                  textOutput("title2"),
                  tableOutput("table2")
                )
)

server <- function(input, output) {
  data <- reactive({
    skin_grading <- ifelse(input$GvHD == "Yes" & input$Skin1 == "No Skin manifestations" & input$Skin2 == "No BSA involved" & input$Skin3 == "No Sclerotic features", 0,
                           ifelse(input$GvHD == "Yes" & input$Skin2 == ">50% BSA" | input$Skin3 == "Deep sclerotic features / unable to pinch / impaired mobility / ulceration", 3,
                                  ifelse(input$GvHD == "Yes" & input$Skin3 == "Superficial sclerotic features (able to pinch)" | (input$Skin2 == "19-50% BSA" &  input$Skin3 == "No Sclerotic features"), 2,
                                         ifelse(input$GvHD == "Yes" & input$Skin2 == "1-18% BSA", 1,0))))
    mouth_grading <- ifelse(input$GvHD == "Yes" & input$Mouth == "No symptoms", 0,
                            ifelse(input$GvHD == "Yes" & input$Mouth == "Mild symptoms with disease signs but not limiting oral intake significantly",1,
                                   ifelse(input$GvHD == "Yes" & input$Mouth == "Moderate symptoms with partial limitation of oral intake", 2,
                                          ifelse(input$GvHD == "Yes" & input$Mouth == "Severe symptoms with disease signs on examination with major limitation of oral intake",3,0))))
    eyes_grading <- ifelse(input$GvHD == "Yes" & input$Eyes == "No symptoms",0,
                           ifelse(input$GvHD == "Yes" & input$Eyes == "Mild dry eye symptoms not affecting ADL (requiring lubrificant <3x per day)",1,
                                  ifelse(input$GvHD == "Yes" & input$Eyes == "Moderate dry eye symptoms partially effect ADL (requiring lubrificant >3x per day or punctual plugs) without new vision impairment due to Keratoconjunctivitis sicca (KCS)",2,
                                         ifelse(input$GvHD == "Yes" & input$Eyes == "Severe dry eye symptoms significantly affecting ADL OR Unable to work because of ocular symptoms OR Loss of vision due to KCS", 3,0))))
    GI_grading <- ifelse(input$GvHD == "Yes" & input$GI == "No symptoms",0,
                         ifelse(input$GvHD == "Yes" & input$GI == "GI symptoms without significant weightloss (<5%) over the past 3 months",1,
                                ifelse(input$GvHD == "Yes" & input$GI == "GI symptoms associated with mild to moderate weightloss (5-15%) over the past 3 months OR Moderate diarrhea without significant interference with daily living",2,
                                       ifelse(input$GvHD == "Yes" & input$GI == "GI symptoms associated with significant weightloss (>15%) over the past 3 months, Requires nutritional supplement for most caloric needs OR Esophageal dilation OR Severe diarrhea with interference with daily living",3,0))))
    liver_grading <- ifelse(input$GvHD == "Yes" & input$Liver == "Normal total bilirubin and ALT or AP <3 x ULN",0,
                            ifelse(input$GvHD == "Yes" & input$Liver == "Normal total bilirubin with ALT >3 to 5 x ULN or AP >3 x ULN",1,
                                   ifelse(input$GvHD == "Yes" & input$Liver == "Elevated total bilirubin but < 3mg/dL or ALT >5 x ULN",2,
                                          ifelse(input$GvHD == "Yes" & input$Liver == "Elevated total bilirubin but > 3mg/dL",3,0))))
    lungs_grading <- ifelse(input$GvHD == "Yes" & input$Lungs == "No symptoms" & input$LungsScore == "FEV1 > 80%",0,
                            ifelse(input$GvHD == "Yes" & input$Lungs == "Mild symptoms (shortness of breath after climbing one flight of steps)" | input$LungsScore == "FEV1  60-79%",1,
                                   ifelse(input$GvHD == "Yes" & input$Lungs == "Moderate symptoms (shortness of breath after walking on flat ground)" | input$LungsScore == "FEV1 40-59%",2,
                                          ifelse(input$GvHD == "Yes" & input$Lungs == "Severe symptoms (shortness of breath at rest; requiring O2" | input$LungsScore == "FEV1 < 39%",3,0))))
    joints_grading <- ifelse(input$GvHD == "Yes" & input$JointAndFascia == "No symptoms",0,
                             ifelse(input$GvHD == "Yes" & input$JointAndFascia == "Mild tightness of arms or legs, normal or mild decreased range of motion (ROM) AND not affecting ADL",1,
                                    ifelse(input$GvHD == "Yes" & input$JointAndFascia == "Tightness of arms or legs OR joint contractures, eythema thought due to fascitis, moderate decreased ROM AND mild to moderate limitation of ADL",2,
                                           ifelse(input$GvHD == "Yes" & input$JointAndFascia == "Contractures with significant decrease of ROM and significant limitation of ADL",3,0))))
    genital_grading <- ifelse(input$GvHD == "Yes" & input$GenitalTract2 == "No signs",0,
                              ifelse(input$GvHD == "Yes" & input$GenitalTract2 == "Mild signs",1,
                                     ifelse(input$GvHD == "Yes" & input$GenitalTract2 == "Moderate signs",2,
                                            ifelse(input$GvHD == "Yes" & input$GenitalTract2 == "Severe signs with or without symptoms",3,0))))
    overall_grading <- ifelse((skin_grading == 1 | mouth_grading == 1 | eyes_grading == 1 | GI_grading == 1 |
                          liver_grading ==1 | joints_grading == 1 | genital_grading == 1) & lungs_grading == 0 & 
                          ((skin_grading + mouth_grading + eyes_grading + GI_grading + liver_grading + joints_grading + genital_grading) <3), "Mild",
                          ifelse((skin_grading == 1 | mouth_grading == 1 | eyes_grading == 1 | GI_grading == 1 |
                                 liver_grading ==1 | joints_grading == 1 | genital_grading == 1) & 
                                 ((skin_grading + mouth_grading + eyes_grading + GI_grading + liver_grading + joints_grading + genital_grading + lungs_grading) >2) |
                                 (skin_grading == 2 | mouth_grading == 2 | eyes_grading == 2 | GI_grading == 2 |
                                 liver_grading ==2 | joints_grading == 2 | genital_grading == 2 | lungs_grading == 1), "Moderate",
                                 ifelse(skin_grading == 3 | mouth_grading == 3 | eyes_grading == 3 | GI_grading == 3 |
                                    liver_grading ==3 | joints_grading == 3 | genital_grading == 3 | lungs_grading == 2 | lungs_grading == 3, "Severe", "No GvHD")))
    monthsinceHSCT <- (difftime(input$Date, input$DateHSCT))/30
    N_metrics <- matrix(c(input$Name, input$Surname, as.character(input$DateBirth), as.character(input$DateHSCT), monthsinceHSCT ,input$ID, as.character(input$Date),
                          input$GvHD, overall_grading,
                          input$Skin1, input$Skin2, input$Skin3, skin_grading,
                          input$Mouth, mouth_grading,
                          input$Eyes, eyes_grading,
                          input$GI, input$GI_low, input$GI_high, GI_grading, 
                          input$Liver, input$Liver2, liver_grading,
                          input$Lungs, input$LungsScore ,lungs_grading,
                          input$JointAndFascia, joints_grading,
                          input$GenitalTract1, input$GenitalTract2, genital_grading,
                          input$CSA, as.character(input$DateCSA), as.character(input$DateCSAStop),
                          input$MMF, as.character(input$DateMMF), as.character(input$DateMMFStop),
                          input$mPDN, as.character(input$DatemPDN), as.character(input$DatemPDNStop),
                          input$Steroid, as.character(input$DateSteroid), as.character(input$DateSteroidStop),
                          input$Ruxo, as.character(input$DateRuxo), as.character(input$DateRuxoStop),
                          input$ECP, as.character(input$DateECP), as.character(input$DateECPStop),
                          input$Etanercept, as.character(input$DateEtanercept), as.character(input$DateEtanerceptStop),
                          input$Infliximab, as.character(input$DateInfliximab), as.character(input$DateInfliximabStop),
                          input$FAM, as.character(input$DateFAM), as.character(input$DateFAMStop),
                          input$Abatacept, as.character(input$DateAbatacept), as.character(input$DateAbataceptStop),
                          input$Belumosudil, as.character(input$DateBelumosudil), as.character(input$DateBelumosudilStop),
                          input$Axatilimab, as.character(input$DateAxatilimab), as.character(input$DateAxatilimabStop),
                          input$Begelomab, as.character(input$DateBegelomab), as.character(input$DateBegelomabStop),
                          input$Ibrutinib, as.character(input$DateIbrutinib), as.character(input$DateIbrutinibStop),
                          input$Sirolimus, as.character(input$DateSirolimus), as.character(input$DateSirolimusStop),
                          input$Imatinib, as.character(input$DateImatinib), as.character(input$DateImatinibStop),
                          input$Pulse, as.character(input$DatePulse), as.character(input$DatePulseStop),
                          input$MSC, as.character(input$DateMSC), as.character(input$DateMSCStop),
                          input$Ocular, as.character(input$DateOcular), as.character(input$DateOcularStop), input$OcularText,
                          input$Other, input$DateOther, input$DrugOther, input$DrugOtherStop), ncol = 94)
    N_metrics <- as.data.frame(N_metrics)
    colnames(N_metrics) <- c("Name",	"Surname", "Date of birth", "Date HSCT", "Months since HSCT","Patient ID", "Date of assessment",
                             "GvHD",	"Chronic GvHD grading",
                             "Skin manifestation", "BSA involvment", "Sclerotic features", "Skin grading",
                             "Mouth symptoms", "Mouth grading",
                             "Eyes symptoms", "Eyes grading",
                             "GI symptoms", "Lower GI biopsy", "Upper GI biopsy","GI grading",
                             "Liver Function", "Liver Biopsy", "Liver grading",
                             "Lungs symptoms", "Lungs score", "Lungs grading",
                             "Joints and Fascia symptoms","Joints and Fascia grading",
                             "Genital tract manifestation", "Genital tract signs", "Genital tract grading",
                             "CSA treatment",	"Date starting / change CSA", "Date stop CSA",
                             "MMF treatment",	"Date starting / change MMF", "Date stop MMF",
                             "mPDN treatment",	"Date starting / change mPDN", "Date stop mPDN",
                             "Topical steroid treatment",	"Date starting topical steroid treatment", "Date stop topical steroid treatment",
                             "Ruxolitinib treatment",	"Date starting / change Ruxolitinib", "Date stop Ruxolitinib",
                             "ECP treatment",	"Date starting ECP", "Date stop ECP",
                             "Etanercept treatment",	"Date starting Etanercept", "Date stop Etanercept",
                             "Infliximab treatment",	"Date starting Infliximab", "Date stop Infliximab",
                             "FAM treatment",	"Date starting FAM", "Date stop FAM",
                             "Abatacept treatment",	"Date starting Abatacept", "Date stop Abatacept",
                             "Belumosudil treatment",	"Date starting Belumosudil", "Date stop Belumosudil",
                             "Axatilimab treatment",	"Date starting Axatilimab", "Date stop Axatilimab",
                             "Begelomab treatment",	"Date starting Begelomab", "Date stop Begelomab",
                             "Ibrutinib treatment",	"Date starting Ibrutinib", "Date stop Ibrutinib",
                             "Sirolimus treatment",	"Date starting / change Sirolimus", "Date stop Sirolimus",
                             "Imatinib treatment",	"Date starting / change Imatinib", "Date stop Imatinib",
                             "Pulse treatment",	"Date starting Pulse", "Date stop Pulse",
                             "MSC treatment",	"Date starting MSC", "Date stop MSC",
                             "Topical ocular treatment", "Date starting topical ocular treatment", "Date stop topical ocular treatment", "Specify ocular treatment",
                             "Other treatment",	"Date starting / change treatment", "Dose treatment", "Date stop treatment")
    N_metrics["Date starting / change CSA"] <- ifelse(N_metrics["CSA treatment"] == "No CSA", "", N_metrics["Date starting / change CSA"])
    N_metrics["Date stop CSA"] <- ifelse(N_metrics["CSA treatment"] == "No CSA", "",
                                         ifelse(N_metrics["CSA treatment"] %in% c("Full dose", "Tapering"), "Ongoing", N_metrics["Date stop CSA"]))

    N_metrics["Date starting / change MMF"] <- ifelse(N_metrics["MMF treatment"] == "No MMF", "", N_metrics["Date starting / change MMF"])
    N_metrics["Date stop MMF"] <- ifelse(N_metrics["MMF treatment"] == "No MMF", "",
                                         ifelse(N_metrics["MMF treatment"] %in% c("Full dose", "Tapering"), "Ongoing",N_metrics["Date stop MMF"]))

    N_metrics["Date starting / change mPDN"] <- ifelse(N_metrics["mPDN treatment"] == "No mPDN", "", N_metrics["Date starting / change mPDN"])
    N_metrics["Date stop mPDN"] <- ifelse(N_metrics["mPDN treatment"] == "No mPDN", "",
                                          ifelse(N_metrics["mPDN treatment"] %in% c("1.8-2.5 mg/kg", "1.5-1.7 mg/kg", "1-1.4 mg/kg", "0.5-0.9 mg/kg", "<0.5 mg/kg"), "Ongoing", N_metrics["Date stop mPDN"]))
    
    N_metrics["Date starting topical steroid treatment"] <- ifelse(N_metrics["Topical steroid treatment"] == "No topical steroid", "", N_metrics["Date starting topical steroid treatment"])
    N_metrics["Date stop topical steroid treatment"] <- ifelse(N_metrics["Topical steroid treatment"] == "No topical steroid", "",
                                                             ifelse(N_metrics["Topical steroid treatment"] == "Topical steroid", "Ongoing", N_metrics["Date stop topical steroid treatment"]))

    N_metrics["Date starting / change Ruxolitinib"] <- ifelse(N_metrics["Ruxolitinib treatment"] == "No Ruxolitinib", "", N_metrics["Date starting / change Ruxolitinib"])
    N_metrics["Date stop Ruxolitinib"] <- ifelse(N_metrics["Ruxolitinib treatment"] == "No Ruxolitinib", "",
                                                 ifelse(N_metrics["Ruxolitinib treatment"] %in% c("Full dose", "Tapering"), "Ongoing",N_metrics["Date stop Ruxolitinib"]))

    N_metrics["Date starting ECP"] <- ifelse(N_metrics["ECP treatment"] == "No ECP", "", N_metrics["Date starting ECP"])
    N_metrics["Date stop ECP"] <- ifelse(N_metrics["ECP treatment"] == "No ECP", "",
                                         ifelse(N_metrics["ECP treatment"] == "ECP", "Ongoing",N_metrics["Date stop ECP"]))

    N_metrics["Date starting Etanercept"] <- ifelse(N_metrics["Etanercept treatment"] == "No Etanercept", "", N_metrics["Date starting Etanercept"])
    N_metrics["Date stop Etanercept"] <- ifelse(N_metrics["Etanercept treatment"] == "No Etanercept", "",
                                                ifelse(N_metrics["Etanercept treatment"] == "Etanercept", "Ongoing", N_metrics["Date stop Etanercept"]))

    N_metrics["Date starting Infliximab"] <- ifelse(N_metrics["Infliximab treatment"] == "No Infliximab", "", N_metrics["Date starting Infliximab"])
    N_metrics["Date stop Infliximab"] <- ifelse(N_metrics["Infliximab treatment"] == "No Infliximab", "",
                                                ifelse(N_metrics["Infliximab treatment"] == "Infliximab", "Ongoing",N_metrics["Date stop Infliximab"]))
    
    N_metrics["Date starting FAM"] <- ifelse(N_metrics["FAM treatment"] == "No FAM", "", N_metrics["Date starting FAM"])
    N_metrics["Date stop FAM"] <- ifelse(N_metrics["FAM treatment"] == "No FAM", "" , 
                                         ifelse(N_metrics["FAM treatment"] == "FAM", "Ongoing", N_metrics["Date stop FAM"]))

    N_metrics["Date starting Abatacept"] <- ifelse(N_metrics["Abatacept treatment"] == "No Abatacept", "", N_metrics["Date starting Abatacept"])
    N_metrics["Date stop Abatacept"] <- ifelse(N_metrics["Abatacept treatment"] == "No Abatacept",  "",
                                               ifelse(N_metrics["Abatacept treatment"] == "Abatacept", "Ongoing", N_metrics["Date stop Abatacept"]))

    N_metrics["Date starting Belumosudil"] <- ifelse(N_metrics["Belumosudil treatment"] == "No Belumosudil", "", N_metrics["Date starting Belumosudil"])
    N_metrics["Date stop Belumosudil"] <- ifelse(N_metrics["Belumosudil treatment"] == "No Belumosudil", "",
                                                 ifelse(N_metrics["Belumosudil treatment"] == "Belumosudil", "Ongoing",N_metrics["Date stop Belumosudil"]))

    N_metrics["Date starting Axatilimab"] <- ifelse(N_metrics["Axatilimab treatment"] == "No Axatilimab", "", N_metrics["Date starting Axatilimab"])
    N_metrics["Date stop Axatilimab"] <- ifelse(N_metrics["Axatilimab treatment"] == "No Axatilimab", "",
                                                ifelse(N_metrics["Axatilimab treatment"] == "Axatilimab", "Ongoing", N_metrics["Date stop Axatilimab"]))

    N_metrics["Date starting Begelomab"] <- ifelse(N_metrics["Begelomab treatment"] == "No Begelomab", "", N_metrics["Date starting Begelomab"])
    N_metrics["Date stop Begelomab"] <- ifelse(N_metrics["Begelomab treatment"] == "No Begelomab", "",
                                               ifelse(N_metrics["Begelomab treatment"] == "Begelomab", "Ongoing", N_metrics["Date stop Begelomab"]))

    N_metrics["Date starting Ibrutinib"] <- ifelse(N_metrics["Ibrutinib treatment"] == "No Ibrutinib", "", N_metrics["Date starting Ibrutinib"])
    N_metrics["Date stop Ibrutinib"] <- ifelse(N_metrics["Ibrutinib treatment"] == "No Ibrutinib", "",
                                               ifelse(N_metrics["Ibrutinib treatment"] == "Ibrutinib", "Ongoing", N_metrics["Date stop Ibrutinib"]))

    N_metrics["Date starting / change Sirolimus"] <- ifelse(N_metrics["Sirolimus treatment"] == "No Sirolimus", "", N_metrics["Date starting / change Sirolimus"])
    N_metrics["Date stop Sirolimus"] <- ifelse(N_metrics["Sirolimus treatment"] == "No Sirolimus", "",
                                               ifelse(N_metrics["Sirolimus treatment"] %in% c("Full dose", "Tapering"),"Ongoing", N_metrics["Date stop Sirolimus"]))
    
    N_metrics["Date starting / change Imatinib"] <- ifelse(N_metrics["Imatinib treatment"] == "No Imatinib", "", N_metrics["Date starting / change Imatinib"])
    N_metrics["Date stop Imatinib"] <- ifelse(N_metrics["Imatinib treatment"] == "No Imatinib", "",
                                              ifelse(N_metrics["Imatinib treatment"] == "Imatinib", "Ongoing", N_metrics["Date stop Imatinib"]))

    N_metrics["Date starting Pulse"] <- ifelse(N_metrics["Pulse treatment"] == "No mPDN Pulse", "", N_metrics["Date starting Pulse"])
    N_metrics["Date stop Pulse"] <- ifelse(N_metrics["Pulse treatment"] == "No mPDN Pulse", "",
                                           ifelse(N_metrics["Pulse treatment"] == "mPDN Pulse", "Ongoing", N_metrics["Date stop Pulse"]))

    N_metrics["Date starting MSC"] <- ifelse(N_metrics["MSC treatment"] == "No MSC", "", N_metrics["Date starting MSC"])
    N_metrics["Date stop MSC"] <- ifelse(N_metrics["MSC treatment"] == "No MSC", "",
                                         ifelse(N_metrics["MSC treatment"] == "MSC", "Ongoing", N_metrics["Date stop MSC"]))
    
    N_metrics["Date starting topical ocular treatment"] <- ifelse(N_metrics["Topical ocular treatment"] == "No topical ocular treatment", "",
                                                                  N_metrics["Date starting topical ocular treatment"])
    N_metrics["Date stop topical ocular treatment"] <- ifelse(N_metrics["Topical ocular treatment"] == "No topical ocular treatment", "",
                                                              ifelse(N_metrics["Topical ocular treatment"] == "Topical ocular treatment", "Ongoing", N_metrics["Date stop topical ocular treatment"]))
    
    if(length(summary_table$Name) > 0){
      index <- length(summary_table[,1])
      N_metrics[1,c(1:4,6)] <- summary_table[index,c(1:4,6)]
      if(N_metrics[1,33] == summary_table[index,33]){
        N_metrics[1,34] <- summary_table[index,34]
      }
      if(N_metrics[1,36] == summary_table[index,36]){
        N_metrics[1,37] <- summary_table[index,37]
      }
      if(N_metrics[1,39] == summary_table[index,39]){
        N_metrics[1,40] <- summary_table[index,40]
      }
      if(N_metrics[1,42] == summary_table[index,42]){
        N_metrics[1,43] <- summary_table[index,43]
      }
      if(N_metrics[1,45] == summary_table[index,45]){
        N_metrics[1,46] <- summary_table[index,46]
      }
      if(N_metrics[1,48] == summary_table[index,48]){
        N_metrics[1,49] <- summary_table[index,49]
      }
      if(N_metrics[1,51] == summary_table[index,51]){
        N_metrics[1,52] <- summary_table[index,52]
      }
      if(N_metrics[1,54] == summary_table[index,54]){
        N_metrics[1,55] <- summary_table[index,55]
      }
      if(N_metrics[1,57] == summary_table[index,57]){
        N_metrics[1,58] <- summary_table[index,58]
      }
      if(N_metrics[1,60] == summary_table[index,60]){
        N_metrics[1,61] <- summary_table[index,61]
      }
      if(N_metrics[1,63] == summary_table[index,63]){
        N_metrics[1,64] <- summary_table[index,64]
      }
      if(N_metrics[1,66] == summary_table[index,66]){
        N_metrics[1,67] <- summary_table[index,67]
      }
      if(N_metrics[1,69] == summary_table[index,69]){
        N_metrics[1,70] <- summary_table[index,70]
      }
      if(N_metrics[1,72] == summary_table[index,72]){
        N_metrics[1,73] <- summary_table[index,73]
      }
      if(N_metrics[1,75] == summary_table[index,75]){
        N_metrics[1,76] <- summary_table[index,76]
      }
      if(N_metrics[1,78] == summary_table[index,78]){
        N_metrics[1,79] <- summary_table[index,79]
      }
      if(N_metrics[1,81] == summary_table[index,81]){
        N_metrics[1,82] <- summary_table[index,82]
      }
      if(N_metrics[1,84] == summary_table[index,84]){
        N_metrics[1,85] <- summary_table[index,85]
      }
      if(N_metrics[1,87] == summary_table[index,87]){
        N_metrics[1,88] <- summary_table[index,88]
      }
      if(N_metrics[1,91] == summary_table[index,91]){
        N_metrics[1,92] <- summary_table[index,92]
      }
      N_metrics[,5] <- round((difftime(as.character(as.Date(N_metrics[,7])),
                                       as.Date(as.character(summary_table[index,4])))),
                             digits = 0)
    }
    N_metrics
  })

  output$title1 <- renderText("Today assessment")
  output$table1 <- renderTable({
    data() %>% 
      select(where(~ !(all(is.na(.)) | all(. == "")))) %>%
      select(where(~ !all(. %in% treatment)))
  })
  
  output$title2 <- renderText("Latest assessment")
  output$table2 <- if(length(summary_table$Name) >0) {renderTable({
    summary_table[length(summary_table$Name),] %>%
      select(where(~ !(all(is.na(.)) | all(. == "")))) %>%
      select(where(~ !all(. %in% treatment)))
  })} else{renderTable({"No previous determination"})}
  
  output$download <- downloadHandler(
    filename = function(){paste0(as.character(input$Date),"_", input$Surname, "_", input$Name, ".xlsx")}, 
    content = function(fname){
      write_xlsx(data(), fname)
    })
  
  output$update <- downloadHandler(
    filename = function(){paste0("Summary_Patient_chronic_GvHD.xlsx")}, 
    content = function(fname){
      database <- as.data.frame(read_excel("Summary_Patient_chronic_GvHD.xlsx"))
      Data <- rbind(database, data()) 
      write_xlsx(x = Data, fname)
    })
}
shinyApp(ui = ui, server = server)


