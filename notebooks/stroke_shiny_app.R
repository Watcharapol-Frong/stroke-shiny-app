library(shiny)
library(readr)
library(caret)

# โหลดโมเดล
logic_model <- readRDS("logic_model.rds")

# UI
ui <- fluidPage(
  titlePanel("แบบฟอร์มประเมินความเสี่ยงโรคหลอดเลือดสมอง (Stroke)"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "อายุ (ปี)", value = 45, min = 1, max = 120),
      selectInput("hypertension", "เป็นโรคความดันโลหิตสูงหรือไม่", choices = c("ไม่เป็น" = 0, "เป็น" = 1), selected = 0),
      selectInput("heart_disease", "เป็นโรคหัวใจหรือไม่", choices = c("ไม่เป็น" = 0, "เป็น" = 1), selected = 0),
      selectInput("ever_married", "เคยแต่งงานหรือไม่", choices = c("ไม่เคย" = "No", "เคย" = "Yes"), selected = "No"),
      selectInput("work_type", "ลักษณะการทำงาน", choices = c("พนักงานเอกชน" = "Private", "ข้าราชการ" = "Govt_job",
                                                             "อาชีพอิสระ" = "Self-employed", "เด็ก" = "children",
                                                             "ไม่เคยทำงาน" = "Never_worked"), selected = "Private"),
      selectInput("Residence_type", "ที่อยู่อาศัย", choices = c("เขตเมือง" = "Urban", "ชนบท" = "Rural"), selected = "Urban"),
      selectInput("diabetes", "ป่วยเป็นโรคเบาหวานหรือไม่", choices = c("ไม่เป็น" = 0, "เป็น" = 1), selected = 0),
      
      # input bmi
      numericInput("weight", "น้ำหนัก (กิโลกรัม)", value = 65, min = 30, max = 200),
      numericInput("height", "ส่วนสูง (เซนติเมตร)", value = 170, min = 100, max = 220),
      
      selectInput("gender", "เพศ", choices = c("ชาย" = "Male", "หญิง" = "Female"), selected = "Male"),
      selectInput("smoking_status", "สถานะการสูบบุหรี่", 
                  choices = c("เคยสูบ" = "formerly smoked", "ไม่เคยสูบ" = "never smoked", 
                              "สูบอยู่ปัจจุบัน" = "smokes", "ไม่ระบุ" = "Unknown"), selected = "Unknown"),
      
      actionButton("predict_btn", "ทำนายความเสี่ยง")
    ),
    
    mainPanel(
      h3("ผลการทำนาย"),
      verbatimTextOutput("prediction"),
      tags$hr(),
      tags$div(style = "color: gray; font-size: 90%;",
               "คำเตือน: การประเมินนี้เป็นการทำนายจากโมเดลที่ฝึกด้วยข้อมูลจำกัด ซึ่งอาจไม่ครอบคลุมปัจจัยทั้งหมดที่เกี่ยวข้องกับโรคหลอดเลือดสมอง ",
               "ผลลัพธ์นี้ไม่สามารถใช้แทนคำแนะนำทางการแพทย์ได้ กรุณาปรึกษาแพทย์หรือผู้เชี่ยวชาญเพื่อการประเมินอย่างเหมาะสม")
    )
  )
)

# SERVER
server <- function(input, output) {
  
  observeEvent(input$predict_btn, {
    
    bmi_val <- input$weight / ((input$height / 100)^2)
    
    new_data <- data.frame(
      gender = factor(input$gender, levels = c("Female", "Male")),
      age = input$age,
      hypertension = as.numeric(input$hypertension),
      heart_disease = as.numeric(input$heart_disease),
      ever_married = factor(input$ever_married, levels = c("No", "Yes")),
      work_type = factor(input$work_type, levels = c("Private", "Self-employed", "Govt_job", "children", "Never_worked")),
      Residence_type = factor(input$Residence_type, levels = c("Urban", "Rural")),
      avg_glucose_level = ifelse(input$diabetes == 1, 180, 95),
      bmi = bmi_val,
      smoking_status = factor(input$smoking_status, levels = c("formerly smoked", "never smoked", "smokes", "Unknown"))
    )
    
    prob <- predict(logic_model, newdata = new_data, type = "prob")[, "1"]
    percent <- round(prob * 100, 2)
    
    output$prediction <- renderText({
      paste0("โอกาสเสี่ยงเป็นโรคหลอดเลือดสมอง: ", percent, "%")
    })
  })
}

# RUN
shinyApp(ui = ui, server = server)
