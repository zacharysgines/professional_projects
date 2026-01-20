data <- read.csv("C:/Users/zacha/Desktop/PANDASData/surveymatched.csv")

diagnoseTable <- table(data$PreDiagnoseComfort, data$PostDiagnoseComfort)
print(diagnoseTable)

treatmentTable <- table(data$PreTreatmentComfort, data$PostTreatmentComfort)
print(treatmentTable)

fisher.test(diagnoseTable, simulate.p.value = TRUE, B = 100000, hybrid = FALSE)
fisher.test(treatmentTable, simulate.p.value = TRUE, B = 100000, hybrid = FALSE)
