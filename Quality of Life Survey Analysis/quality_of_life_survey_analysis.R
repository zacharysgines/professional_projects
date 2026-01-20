autism <- read.csv("C:/Users/zacha/Desktop/autism.csv")
autism_Single_Family_Member <- autism[autism$Multiple_Family_Members == "No",]
attach(autism)

###Life Quality Last Two Weeks
Life_Quality <- table(Life_Quality_Two_Weeks)[c(4,1,2,3,5)]
Life_Quality_Families <- table(Multiple_Family_Members, Life_Quality_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Quality_Age <- table(Age, Life_Quality_Two_Weeks)[, c(4,1,2,3,5)]
Life_Quality_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Life_Quality_Two_Weeks)[c(3,1,2), c(4,1,2,3,5)]
Life_Quality_ABA <- table(ABA_Therapy, Life_Quality_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Quality_OT <- table(Occupational_Therapy, Life_Quality_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Quality_Speech <- table(Speech_Therapy, Life_Quality_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Quality_Medical_Management <- table(Medical_Management, Life_Quality_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Quality_Educational_Intervention <- table(Educational_Intervention, Life_Quality_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Quality_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Life_Quality_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Quality_DSPD_Services <- table(DSPD_Services, Life_Quality_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Quality_DSPD_Waitlist <- table(DSPD_Waitlist, Life_Quality_Two_Weeks)[c(2,1), c(4,1,2,3,5)]

chisq.test(Life_Quality)
chisq.test(Life_Quality_Families)
chisq.test(Life_Quality_Age)
chisq.test(Life_Quality_Family_Member_Age)
chisq.test(Life_Quality_ABA)
chisq.test(Life_Quality_OT)
chisq.test(Life_Quality_Speech)
chisq.test(Life_Quality_Medical_Management)
chisq.test(Life_Quality_Educational_Intervention)
chisq.test(Life_Quality_Other_Mental_Health)
chisq.test(Life_Quality_DSPD_Services)
chisq.test(Life_Quality_DSPD_Waitlist)

###Negative Feelings Last Two Weeks
Negative_Feelings <- table(Negative_Feelings_Two_Weeks)[c(1,2,5,3,4)]
Negative_Feelings_Families <- table(Multiple_Family_Members, Negative_Feelings_Two_Weeks)[c(2,1), c(1,2,5,3,4)]
Negative_Feelings_Age <- table(Age, Negative_Feelings_Two_Weeks)[, c(1,2,5,3,4)]
Negative_Feelings_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Negative_Feelings_Two_Weeks)[c(3,1,2), c(1,2,5,3,4)]
Negative_Feelings_ABA <- table(ABA_Therapy, Negative_Feelings_Two_Weeks)[c(2,1), c(1,2,5,3,4)]
Negative_Feelings_OT <- table(Occupational_Therapy, Negative_Feelings_Two_Weeks)[c(2,1), c(1,2,5,3,4)]
Negative_Feelings_Speech <- table(Speech_Therapy, Negative_Feelings_Two_Weeks)[c(2,1), c(1,2,5,3,4)]
Negative_Feelings_Medical_Management <- table(Medical_Management, Negative_Feelings_Two_Weeks)[c(2,1), c(1,2,5,3,4)]
Negative_Feelings_Educational_Intervention <- table(Educational_Intervention, Negative_Feelings_Two_Weeks)[c(2,1), c(1,2,5,3,4)]
Negative_Feelings_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Negative_Feelings_Two_Weeks)[c(2,1), c(1,2,5,3,4)]
Negative_Feelings_DSPD_Services <- table(DSPD_Services, Negative_Feelings_Two_Weeks)[c(2,1), c(1,2,5,3,4)]
Negative_Feelings_DSPD_Waitlist <- table(DSPD_Waitlist, Negative_Feelings_Two_Weeks)[c(2,1), c(1,2,5,3,4)]

chisq.test(Negative_Feelings)
chisq.test(Negative_Feelings_Families)
chisq.test(Negative_Feelings_Age)
chisq.test(Negative_Feelings_Family_Member_Age)
chisq.test(Negative_Feelings_ABA)
chisq.test(Negative_Feelings_OT)
chisq.test(Negative_Feelings_Speech)
chisq.test(Negative_Feelings_Medical_Management)
chisq.test(Negative_Feelings_Educational_Intervention)
chisq.test(Negative_Feelings_Other_Mental_Health)
chisq.test(Negative_Feelings_DSPD_Services)
chisq.test(Negative_Feelings_DSPD_Waitlist)

###Health Satisfaction Last Two Weeks
Health_Satisfaction <- table(Health_Satisfaction_Two_Weeks)[c(5,3,2,1,4)]
Health_Satisfaction_Families <- table(Multiple_Family_Members, Health_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Satisfaction_Age <- table(Age, Health_Satisfaction_Two_Weeks)[, c(5,3,2,1,4)]
Health_Satisfaction_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Health_Satisfaction_Two_Weeks)[c(3,1,2), c(5,3,2,1,4)]
Health_Satisfaction_ABA <- table(ABA_Therapy, Health_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Satisfaction_OT <- table(Occupational_Therapy, Health_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Satisfaction_Speech <- table(Speech_Therapy, Health_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Satisfaction_Medical_Management <- table(Medical_Management, Health_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Satisfaction_Educational_Intervention <- table(Educational_Intervention, Health_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Satisfaction_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Health_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Satisfaction_DSPD_Services <- table(DSPD_Services, Health_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Satisfaction_DSPD_Waitlist <- table(DSPD_Waitlist, Health_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]

chisq.test(Health_Satisfaction)
chisq.test(Health_Satisfaction_Families)
chisq.test(Health_Satisfaction_Age)
chisq.test(Health_Satisfaction_Family_Member_Age)
chisq.test(Health_Satisfaction_ABA)
chisq.test(Health_Satisfaction_OT)
chisq.test(Health_Satisfaction_Speech)
chisq.test(Health_Satisfaction_Medical_Management)
chisq.test(Health_Satisfaction_DSPD_Services)
chisq.test(Health_Satisfaction_DSPD_Waitlist)

###Sleep Satisfaction Last Two Weeks
Sleep_Satisfaction <- table(Sleep_Satisfaction_Two_Weeks)[c(5,3,2,1,4)]
Sleep_Satisfaction_Families <- table(Multiple_Family_Members, Sleep_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Sleep_Satisfaction_Age <- table(Age, Sleep_Satisfaction_Two_Weeks)[, c(5,3,2,1,4)]
Sleep_Satisfaction_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Sleep_Satisfaction_Two_Weeks)[c(3,1,2), c(5,3,2,1,4)]
Sleep_Satisfaction_ABA <- table(ABA_Therapy, Sleep_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Sleep_Satisfaction_OT <- table(Occupational_Therapy, Sleep_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Sleep_Satisfaction_Speech <- table(Speech_Therapy, Sleep_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Sleep_Satisfaction_Medical_Management <- table(Medical_Management, Sleep_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Sleep_Satisfaction_Educational_Intervention <- table(Educational_Intervention, Sleep_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Sleep_Satisfaction_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Sleep_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Sleep_Satisfaction_DSPD_Services <- table(DSPD_Services, Sleep_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Sleep_Satisfaction_DSPD_Waitlist <- table(DSPD_Waitlist, Sleep_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]

chisq.test(Sleep_Satisfaction)
chisq.test(Sleep_Satisfaction_Families)
chisq.test(Sleep_Satisfaction_Age)
chisq.test(Sleep_Satisfaction_Family_Member_Age)
chisq.test(Sleep_Satisfaction_ABA)
chisq.test(Sleep_Satisfaction_OT)
chisq.test(Sleep_Satisfaction_Speech)
chisq.test(Sleep_Satisfaction_Medical_Management)
chisq.test(Sleep_Satisfaction_Educational_Intervention)
chisq.test(Sleep_Satisfaction_Other_Mental_Health)
chisq.test(Sleep_Satisfaction_DSPD_Services)
chisq.test(Sleep_Satisfaction_DSPD_Waitlist)

###Daily Living Satisfaction Last Two Weeks
Daily_Living_Satisfaction <- table(Daily_Living_Satisfaction_Two_Weeks)[c(5,3,2,1,4)]
Daily_Living_Families <- table(Multiple_Family_Members, Daily_Living_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Daily_Living_Age <- table(Age, Daily_Living_Satisfaction_Two_Weeks)[, c(5,3,2,1,4)]
Daily_Living_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Daily_Living_Satisfaction_Two_Weeks)[c(3,1,2), c(5,3,2,1,4)]
Daily_Living_Satisfaction_ABA <- table(ABA_Therapy, Daily_Living_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Daily_Living_Satisfaction_OT <- table(Occupational_Therapy, Daily_Living_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Daily_Living_Satisfaction_Speech <- table(Speech_Therapy, Daily_Living_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Daily_Living_Satisfaction_Medical_Management <- table(Medical_Management, Daily_Living_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Daily_Living_Satisfaction_Educational_Intervention <- table(Educational_Intervention, Daily_Living_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Daily_Living_Satisfaction_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Daily_Living_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Daily_Living_Satisfaction_DSPD_Services <- table(DSPD_Services, Daily_Living_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Daily_Living_Satisfaction_DSPD_Waitlist <- table(DSPD_Waitlist, Daily_Living_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]

chisq.test(Daily_Living_Satisfaction)
chisq.test(Daily_Living_Families)
chisq.test(Daily_Living_Age)
chisq.test(Daily_Living_Family_Member_Age)
chisq.test(Daily_Living_Satisfaction_ABA)
chisq.test(Daily_Living_Satisfaction_OT)
chisq.test(Daily_Living_Satisfaction_Speech)
chisq.test(Daily_Living_Satisfaction_Medical_Management)
chisq.test(Daily_Living_Satisfaction_Educational_Intervention)
chisq.test(Daily_Living_Satisfaction_Other_Mental_Health)
chisq.test(Daily_Living_Satisfaction_DSPD_Services)
chisq.test(Daily_Living_Satisfaction_DSPD_Waitlist)

###Work Satisfaction Last Two Weeks
Work_Satisfaction <- table(Work_Satisfaction_Two_Weeks)[c(5,3,2,1,4)]
Work_Families <- table(Multiple_Family_Members, Work_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Work_Age <- table(Age, Work_Satisfaction_Two_Weeks)[, c(5,3,2,1,4)]
Work_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Work_Satisfaction_Two_Weeks)[c(3,1,2), c(5,3,2,1,4)]
Work_Satisfaction_ABA <- table(ABA_Therapy, Work_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Work_Satisfaction_OT <- table(Occupational_Therapy, Work_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Work_Satisfaction_Speech <- table(Speech_Therapy, Work_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Work_Satisfaction_Medical_Management <- table(Medical_Management, Work_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Work_Satisfaction_Educational_Intervention <- table(Educational_Intervention, Work_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Work_Satisfaction_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Work_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Work_Satisfaction_DSPD_Services <- table(DSPD_Services, Work_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Work_Satisfaction_DSPD_Waitlist <- table(DSPD_Waitlist, Work_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]

chisq.test(Work_Satisfaction)
chisq.test(Work_Families)
chisq.test(Work_Age)
chisq.test(Work_Family_Member_Age)
chisq.test(Work_Satisfaction_ABA)
chisq.test(Work_Satisfaction_OT)
chisq.test(Work_Satisfaction_Speech)
chisq.test(Work_Satisfaction_Medical_Management)
chisq.test(Work_Satisfaction_Educational_Intervention)
chisq.test(Work_Satisfaction_Other_Mental_Health)
chisq.test(Work_Satisfaction_DSPD_Services)
chisq.test(Work_Satisfaction_DSPD_Waitlist)

###Self Satisfaction Last Two Weeks
Self_Satisfaction <- table(Self_Satisfaction_Two_Weeks)[c(5,3,2,1,4)]
Self_Families <- table(Multiple_Family_Members, Self_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Self_Age <- table(Age, Self_Satisfaction_Two_Weeks)[, c(5,3,2,1,4)]
Self_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Self_Satisfaction_Two_Weeks)[c(3,1,2), c(5,3,2,1,4)]
Self_Satisfaction_ABA <- table(ABA_Therapy, Self_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Self_Satisfaction_OT <- table(Occupational_Therapy, Self_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Self_Satisfaction_Speech <- table(Speech_Therapy, Self_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Self_Satisfaction_Medical_Management <- table(Medical_Management, Self_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Self_Satisfaction_Educational_Intervention <- table(Educational_Intervention, Self_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Self_Satisfaction_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Self_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Self_Satisfaction_DSPD_Services <- table(DSPD_Services, Self_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Self_Satisfaction_DSPD_Waitlist <- table(DSPD_Waitlist, Self_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]

chisq.test(Self_Satisfaction)
chisq.test(Self_Families)
chisq.test(Self_Age)
chisq.test(Self_Family_Member_Age)
chisq.test(Self_Satisfaction_ABA)
chisq.test(Self_Satisfaction_OT)
chisq.test(Self_Satisfaction_Speech)
chisq.test(Self_Satisfaction_Medical_Management)
chisq.test(Self_Satisfaction_Educational_Intervention)
chisq.test(Self_Satisfaction_Other_Mental_Health)
chisq.test(Self_Satisfaction_DSPD_Services)
chisq.test(Self_Satisfaction_DSPD_Waitlist)

###Relationship Satisfaction Last Two Weeks
Relationship_Satisfaction <- table(Relationship_Satisfaction_Two_Weeks)[c(5,3,2,1,4)]
Relationship_Families <- table(Multiple_Family_Members, Relationship_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Relationship_Age <- table(Age, Relationship_Satisfaction_Two_Weeks)[, c(5,3,2,1,4)]
Relationship_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Relationship_Satisfaction_Two_Weeks)[c(3,1,2), c(5,3,2,1,4)]
Relationship_Satisfaction_ABA <- table(ABA_Therapy, Relationship_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Relationship_Satisfaction_OT <- table(Occupational_Therapy, Relationship_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Relationship_Satisfaction_Speech <- table(Speech_Therapy, Relationship_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Relationship_Satisfaction_Medical_Management <- table(Medical_Management, Relationship_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Relationship_Satisfaction_Educational_Intervention <- table(Educational_Intervention, Relationship_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Relationship_Satisfaction_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Relationship_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Relationship_Satisfaction_DSPD_Services <- table(DSPD_Services, Relationship_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Relationship_Satisfaction_DSPD_Waitlist <- table(DSPD_Waitlist, Relationship_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]

chisq.test(Relationship_Satisfaction)
chisq.test(Relationship_Families)
chisq.test(Relationship_Age)
chisq.test(Relationship_Family_Member_Age)
chisq.test(Relationship_Satisfaction_ABA)
chisq.test(Relationship_Satisfaction_OT)
chisq.test(Relationship_Satisfaction_Speech)
chisq.test(Relationship_Satisfaction_Medical_Management)
chisq.test(Relationship_Satisfaction_Educational_Intervention)
chisq.test(Relationship_Satisfaction_Other_Mental_Health)
chisq.test(Relationship_Satisfaction_DSPD_Services)
chisq.test(Relationship_Satisfaction_DSPD_Waitlist)

###Sex Satisfaction Last Two Weeks
Sex_Satisfaction <- table(Sex_Satisfaction_Two_Weeks)[c(6,4,3,1,5,2)]
Sex_Families <- table(Multiple_Family_Members, Sex_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Sex_Age <- table(Age, Sex_Satisfaction_Two_Weeks)[, c(6,4,3,1,5,2)]
Sex_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Sex_Satisfaction_Two_Weeks)[c(3,1,2), c(6,4,3,1,5,2)]
Sex_Satisfaction_ABA <- table(ABA_Therapy, Sex_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Sex_Satisfaction_OT <- table(Occupational_Therapy, Sex_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Sex_Satisfaction_Speech <- table(Speech_Therapy, Sex_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Sex_Satisfaction_Medical_Management <- table(Medical_Management, Sex_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Sex_Satisfaction_Educational_Intervention <- table(Educational_Intervention, Sex_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Sex_Satisfaction_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Sex_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Sex_Satisfaction_DSPD_Services <- table(DSPD_Services, Sex_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Sex_Satisfaction_DSPD_Waitlist <- table(DSPD_Waitlist, Sex_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]

chisq.test(Sex_Satisfaction)
chisq.test(Sex_Families)
chisq.test(Sex_Age)
chisq.test(Sex_Family_Member_Age)
chisq.test(Sex_Satisfaction_ABA)
chisq.test(Sex_Satisfaction_OT)
chisq.test(Sex_Satisfaction_Speech)
chisq.test(Sex_Satisfaction_Medical_Management)
chisq.test(Sex_Satisfaction_Educational_Intervention)
chisq.test(Sex_Satisfaction_Other_Mental_Health)
chisq.test(Sex_Satisfaction_DSPD_Services)
chisq.test(Sex_Satisfaction_DSPD_Waitlist)

###Friend Support Satisfaction Last Two Weeks
Friend_Support_Satisfaction <- table(Friend_Support_Satisfaction_Two_Weeks)[c(5,3,2,1,4)]
Friend_Support_Families <- table(Multiple_Family_Members, Friend_Support_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Friend_Support_Age <- table(Age, Friend_Support_Satisfaction_Two_Weeks)[, c(5,3,2,1,4)]
Friend_Support_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Friend_Support_Satisfaction_Two_Weeks)[c(3,1,2), c(5,3,2,1,4)]
Friend_Support_Satisfaction_ABA <- table(ABA_Therapy, Friend_Support_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Friend_Support_Satisfaction_OT <- table(Occupational_Therapy, Friend_Support_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Friend_Support_Satisfaction_Speech <- table(Speech_Therapy, Friend_Support_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Friend_Support_Satisfaction_Medical_Management <- table(Medical_Management, Friend_Support_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Friend_Support_Satisfaction_Educational_Intervention <- table(Educational_Intervention, Friend_Support_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Friend_Support_Satisfaction_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Friend_Support_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Friend_Support_Satisfaction_DSPD_Services <- table(DSPD_Services, Friend_Support_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Friend_Support_Satisfaction_DSPD_Waitlist <- table(DSPD_Waitlist, Friend_Support_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]

chisq.test(Friend_Support_Satisfaction)
chisq.test(Friend_Support_Families)
chisq.test(Friend_Support_Age)
chisq.test(Friend_Support_Family_Member_Age)
chisq.test(Friend_Support_Satisfaction_ABA)
chisq.test(Friend_Support_Satisfaction_OT)
chisq.test(Friend_Support_Satisfaction_Speech)
chisq.test(Friend_Support_Satisfaction_Medical_Management)
chisq.test(Friend_Support_Satisfaction_Educational_Intervention)
chisq.test(Friend_Support_Satisfaction_Other_Mental_Health)
chisq.test(Friend_Support_Satisfaction_DSPD_Services)
chisq.test(Friend_Support_Satisfaction_DSPD_Waitlist)

###Living Conditions Satisfaction Last Two Weeks
Living_Conditions_Satisfaction <- table(Living_Condition_Satisfaction_Two_Weeks)[c(6,4,3,1,5,2)]
Living_Conditions_Families <- table(Multiple_Family_Members, Living_Condition_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Living_Conditions_Age <- table(Age, Living_Condition_Satisfaction_Two_Weeks)[, c(6,4,3,1,5,2)]
Living_Conditions_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Living_Condition_Satisfaction_Two_Weeks)[c(3,1,2), c(4,3,2,1)]
Living_Conditions_Satisfaction_ABA <- table(ABA_Therapy, Living_Condition_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Living_Conditions_Satisfaction_OT <- table(Occupational_Therapy, Living_Condition_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Living_Conditions_Satisfaction_Speech <- table(Speech_Therapy, Living_Condition_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Living_Conditions_Satisfaction_Medical_Management <- table(Medical_Management, Living_Condition_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Living_Conditions_Satisfaction_Educational_Intervention <- table(Educational_Intervention, Living_Condition_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Living_Conditions_Satisfaction_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Living_Condition_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Living_Conditions_Satisfaction_DSPD_Services <- table(DSPD_Services, Living_Condition_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Living_Conditions_Satisfaction_DSPD_Waitlist <- table(DSPD_Waitlist, Living_Condition_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]

chisq.test(Living_Conditions_Satisfaction)
chisq.test(Living_Conditions_Families)
chisq.test(Living_Conditions_Age)
chisq.test(Living_Conditions_Family_Member_Age)
chisq.test(Living_Conditions_Satisfaction_ABA)
chisq.test(Living_Conditions_Satisfaction_OT)
chisq.test(Living_Conditions_Satisfaction_Speech)
chisq.test(Living_Conditions_Satisfaction_Medical_Management)
chisq.test(Living_Conditions_Satisfaction_Educational_Intervention)
chisq.test(Living_Conditions_Satisfaction_Other_Mental_Health)
chisq.test(Living_Conditions_Satisfaction_DSPD_Services)
chisq.test(Living_Conditions_Satisfaction_DSPD_Waitlist)

###Health Services Access Satisfaction Last Two Weeks
Health_Access_Satisfaction <- table(Heath_Services_Access_Satisfaction_Two_Weeks)[c(5,3,2,1,4)]
Health_Access_Families <- table(Multiple_Family_Members, Heath_Services_Access_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Access_Age <- table(Age, Heath_Services_Access_Satisfaction_Two_Weeks)[, c(5,3,2,1,4)]
Health_Access_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Heath_Services_Access_Satisfaction_Two_Weeks)[c(3,1,2), c(5,3,2,1,4)]
Health_Access_Satisfaction_ABA <- table(ABA_Therapy, Heath_Services_Access_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Access_Satisfaction_OT <- table(Occupational_Therapy, Heath_Services_Access_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Access_Satisfaction_Speech <- table(Speech_Therapy, Heath_Services_Access_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Access_Satisfaction_Medical_Management <- table(Medical_Management, Heath_Services_Access_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Access_Satisfaction_Educational_Intervention <- table(Educational_Intervention, Heath_Services_Access_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Access_Satisfaction_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Heath_Services_Access_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Access_Satisfaction_DSPD_Services <- table(DSPD_Services, Heath_Services_Access_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]
Health_Access_Satisfaction_DSPD_Waitlist <- table(DSPD_Waitlist, Heath_Services_Access_Satisfaction_Two_Weeks)[c(2,1), c(5,3,2,1,4)]

chisq.test(Health_Access_Satisfaction)
chisq.test(Health_Access_Families)
chisq.test(Health_Access_Age)
chisq.test(Health_Access_Family_Member_Age)
chisq.test(Health_Access_Satisfaction_ABA)
chisq.test(Health_Access_Satisfaction_OT)
chisq.test(Health_Access_Satisfaction_Speech)
chisq.test(Health_Access_Satisfaction_Medical_Management)
chisq.test(Health_Access_Satisfaction_Educational_Intervention)
chisq.test(Health_Access_Satisfaction_Other_Mental_Health)
chisq.test(Health_Access_Satisfaction_DSPD_Services)
chisq.test(Health_Access_Satisfaction_DSPD_Waitlist)

###Transportation Satisfaction Last Two Weeks
Transport_Satisfaction <- table(Transport_Satisfaction_Two_Weeks)[c(6,4,3,1,5,2)]
Transport_Families <- table(Multiple_Family_Members, Transport_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Transport_Age <- table(Age, Transport_Satisfaction_Two_Weeks)[, c(6,4,3,1,5,2)]
Transport_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Transport_Satisfaction_Two_Weeks)[c(3,1,2), c(4,3,2,1)]
Transport_Satisfaction_ABA <- table(ABA_Therapy, Transport_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Transport_Satisfaction_OT <- table(Occupational_Therapy, Transport_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Transport_Satisfaction_Speech <- table(Speech_Therapy, Transport_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Transport_Satisfaction_Medical_Management <- table(Medical_Management, Transport_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Transport_Satisfaction_Educational_Intervention <- table(Educational_Intervention, Transport_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Transport_Satisfaction_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Transport_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Transport_Satisfaction_DSPD_Services <- table(DSPD_Services, Transport_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]
Transport_Satisfaction_DSPD_Waitlist <- table(DSPD_Waitlist, Transport_Satisfaction_Two_Weeks)[c(2,1), c(6,4,3,1,5,2)]

chisq.test(Transport_Satisfaction)
chisq.test(Transport_Families)
chisq.test(Transport_Age)
chisq.test(Transport_Family_Member_Age)
chisq.test(Transport_Satisfaction_ABA)
chisq.test(Transport_Satisfaction_OT)
chisq.test(Transport_Satisfaction_Speech)
chisq.test(Transport_Satisfaction_Medical_Management)
chisq.test(Transport_Satisfaction_Educational_Intervention)
chisq.test(Transport_Satisfaction_Other_Mental_Health)
chisq.test(Transport_Satisfaction_DSPD_Services)
chisq.test(Transport_Satisfaction_DSPD_Waitlist)

###Self Medical Treatment Last Two Weeks
Self_Medical <- table(Self_Medical_Treatment_Two_Weeks)[c(4,1,2,3,5)]
Self_Medical_Families <- table(Multiple_Family_Members, Self_Medical_Treatment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Self_Medical_Age <- table(Age, Self_Medical_Treatment_Two_Weeks)[,c(4,1,2,3,5)]
Self_Medical_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Self_Medical_Treatment_Two_Weeks)[c(3,1,2), c(4,1,2,3,5)]
Self_Medical_ABA <- table(ABA_Therapy, Self_Medical_Treatment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Self_Medical_OT <- table(Occupational_Therapy, Self_Medical_Treatment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Self_Medical_Speech <- table(Speech_Therapy, Self_Medical_Treatment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Self_Medical_Medical_Management <- table(Medical_Management, Self_Medical_Treatment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Self_Medical_Educational_Intervention <- table(Educational_Intervention, Self_Medical_Treatment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Self_Medical_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Self_Medical_Treatment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Self_Medical_DSPD_Services <- table(DSPD_Services, Self_Medical_Treatment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Self_Medical_DSPD_Waitlist <- table(DSPD_Waitlist, Self_Medical_Treatment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]

chisq.test(Self_Medical)
chisq.test(Self_Medical_Families)
chisq.test(Self_Medical_Age)
chisq.test(Self_Medical_Family_Member_Age)
chisq.test(Self_Medical_ABA)
chisq.test(Self_Medical_OT)
chisq.test(Self_Medical_Speech)
chisq.test(Self_Medical_Medical_Management)
chisq.test(Self_Medical_Educational_Intervention)
chisq.test(Self_Medical_Other_Mental_Health)
chisq.test(Self_Medical_DSPD_Services)
chisq.test(Self_Medical_DSPD_Waitlist)

###Life Enjoyment Last Two Weeks
Life_Enjoyment <- table(Life_Enjoyment_Two_Weeks)[c(4,1,2,3,5)]
Life_Enjoyment_Families <- table(Multiple_Family_Members, Life_Enjoyment_Two_Weeks)[c(2,1),c(4,1,2,3,5)]
Life_Enjoyment_Age <- table(Age, Life_Enjoyment_Two_Weeks)[c(4,1,2,3,5)]
Life_Enjoyment_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Life_Enjoyment_Two_Weeks)[c(3,1,2),c(4,1,2,3)]
Life_Enjoyment_ABA <- table(ABA_Therapy, Life_Enjoyment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Enjoyment_OT <- table(Occupational_Therapy, Life_Enjoyment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Enjoyment_Speech <- table(Speech_Therapy, Life_Enjoyment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Enjoyment_Medical_Management <- table(Medical_Management, Life_Enjoyment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Enjoyment_Educational_Intervention <- table(Educational_Intervention, Life_Enjoyment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Enjoyment_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Life_Enjoyment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Enjoyment_DSPD_Services <- table(DSPD_Services, Life_Enjoyment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Enjoyment_DSPD_Waitlist <- table(DSPD_Waitlist, Life_Enjoyment_Two_Weeks)[c(2,1), c(4,1,2,3,5)]

chisq.test(Life_Enjoyment)
chisq.test(Life_Enjoyment_Families)
chisq.test(Life_Enjoyment_Age)
chisq.test(Life_Enjoyment_Family_Member_Age)
chisq.test(Life_Enjoyment_ABA)
chisq.test(Life_Enjoyment_OT)
chisq.test(Life_Enjoyment_Speech)
chisq.test(Life_Enjoyment_Medical_Management)
chisq.test(Life_Enjoyment_Educational_Intervention)
chisq.test(Life_Enjoyment_Other_Mental_Health)
chisq.test(Life_Enjoyment_DSPD_Services)
chisq.test(Life_Enjoyment_DSPD_Waitlist)

###Life Meaning Last Two Weeks
Life_Meaning <- table(Life_Meaning_Two_Weeks)[c(4,1,2,3,5)]
Life_Meaning_Families <- table(Multiple_Family_Members, Life_Meaning_Two_Weeks)[c(2,1),c(4,1,2,3,5)]
Life_Meaning_Age <- table(Age, Life_Meaning_Two_Weeks)[c(4,1,2,3,5)]
Life_Meaning_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Life_Meaning_Two_Weeks)[c(3,1,2),c(4,1,2,3,5)]
Life_Meaning_ABA <- table(ABA_Therapy, Life_Meaning_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Meaning_OT <- table(Occupational_Therapy, Life_Meaning_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Meaning_Speech <- table(Speech_Therapy, Life_Meaning_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Meaning_Medical_Management <- table(Medical_Management, Life_Meaning_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Meaning_Educational_Intervention <- table(Educational_Intervention, Life_Meaning_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Meaning_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Life_Meaning_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Meaning_DSPD_Services <- table(DSPD_Services, Life_Meaning_Two_Weeks)[c(2,1), c(4,1,2,3,5)]
Life_Meaning_DSPD_Waitlist <- table(DSPD_Waitlist, Life_Meaning_Two_Weeks)[c(2,1), c(4,1,2,3,5)]

chisq.test(Life_Meaning)
chisq.test(Life_Meaning_Families)
chisq.test(Life_Meaning_Age)
chisq.test(Life_Meaning_Family_Member_Age)
chisq.test(Life_Meaning_ABA)
chisq.test(Life_Meaning_OT)
chisq.test(Life_Meaning_Speech)
chisq.test(Life_Meaning_Medical_Management)
chisq.test(Life_Meaning_Educational_Intervention)
chisq.test(Life_Meaning_Other_Mental_Health)
chisq.test(Life_Meaning_DSPD_Services)
chisq.test(Life_Meaning_DSPD_Waitlist)

###Concentration Last Two Weeks
Concentration <- table(Concentration_Two_Weeks)
Concentration_Families <- table(Multiple_Family_Members, Concentration_Two_Weeks)[c(2,1),]
Concentration_Age <- table(Age, Concentration_Two_Weeks)
Concentration_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Concentration_Two_Weeks)[c(3,1,2),]
Concentration_ABA <- table(ABA_Therapy, Concentration_Two_Weeks)[c(2,1),]
Concentration_OT <- table(Occupational_Therapy, Concentration_Two_Weeks)[c(2,1),]
Concentration_Speech <- table(Speech_Therapy, Concentration_Two_Weeks)[c(2,1),]
Concentration_Medical_Management <- table(Medical_Management, Concentration_Two_Weeks)[c(2,1),]
Concentration_Educational_Intervention <- table(Educational_Intervention, Concentration_Two_Weeks)[c(2,1),]
Concentration_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Concentration_Two_Weeks)[c(2,1),]
Concentration_DSPD_Services <- table(DSPD_Services, Concentration_Two_Weeks)[c(2,1),]
Concentration_DSPD_Waitlist <- table(DSPD_Waitlist, Concentration_Two_Weeks)[c(2,1),]

###Zero values for two columns
#chisq.test(Concentration)
chisq.test(Concentration_Families)
chisq.test(Concentration_Age)
chisq.test(Concentration_Family_Member_Age)
chisq.test(Concentration_ABA)
chisq.test(Concentration_OT)
chisq.test(Concentration_Speech)
chisq.test(Concentration_Medical_Management)
chisq.test(Concentration_Educational_Intervention)
chisq.test(Concentration_Other_Mental_Health)
chisq.test(Concentration_DSPD_Services)
chisq.test(Concentration_DSPD_Waitlist)

###Safety Last Two Weeks
Safety <- table(Safety_Two_Weeks)[c(4,1,2,3)]
Safety_Families <- table(Multiple_Family_Members, Safety_Two_Weeks)[c(2,1),c(4,1,2,3)]
Safety_Age <- table(Age, Safety_Two_Weeks)[c(4,1,2,3)]
Safety_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Safety_Two_Weeks)[c(3,1,2),c(4,1,2,3)]
Safety_ABA <- table(ABA_Therapy, Safety_Two_Weeks)[c(2,1), c(4,1,2,3)]
Safety_OT <- table(Occupational_Therapy, Safety_Two_Weeks)[c(2,1), c(4,1,2,3)]
Safety_Speech <- table(Speech_Therapy, Safety_Two_Weeks)[c(2,1), c(4,1,2,3)]
Safety_Medical_Management <- table(Medical_Management, Safety_Two_Weeks)[c(2,1), c(4,1,2,3)]
Safety_Educational_Intervention <- table(Educational_Intervention, Safety_Two_Weeks)[c(2,1), c(4,1,2,3)]
Safety_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Safety_Two_Weeks)[c(2,1), c(4,1,2,3)]
Safety_DSPD_Services <- table(DSPD_Services, Safety_Two_Weeks)[c(2,1), c(4,1,2,3)]
Safety_DSPD_Waitlist <- table(DSPD_Waitlist, Safety_Two_Weeks)[c(2,1), c(4,1,2,3)]

chisq.test(c(Safety,0))
chisq.test(Safety_Families)
chisq.test(Safety_Age)
chisq.test(Safety_Family_Member_Age)
chisq.test(Safety_ABA)
chisq.test(Safety_OT)
chisq.test(Safety_Speech)
chisq.test(Safety_Medical_Management)
chisq.test(Safety_Educational_Intervention)
chisq.test(Safety_Other_Mental_Health)
chisq.test(Safety_DSPD_Services)
chisq.test(Safety_DSPD_Waitlist)

###Physical Environment Health Last Two Weeks
Environment_Health <- table(Physical_Environment_Health_Two_Weeks)[c(3,1,2,4)]
Environment_Health_Families <- table(Multiple_Family_Members, autism$Physical_Environment_Health_Two_Weeks)[c(2,1),c(3,1,2,4)]
Environment_Health_Age <- table(Age, Physical_Environment_Health_Two_Weeks)[,c(3,1,2,4)]
Environment_Health_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Physical_Environment_Health_Two_Weeks)[c(3,1,2),c(3,1,2,4)]
Environment_Health_ABA <- table(ABA_Therapy, Physical_Environment_Health_Two_Weeks)[c(2,1), c(3,1,2,4)]
Environment_Health_OT <- table(Occupational_Therapy, Physical_Environment_Health_Two_Weeks)[c(2,1), c(3,1,2,4)]
Environment_Health_Speech <- table(Speech_Therapy, Physical_Environment_Health_Two_Weeks)[c(2,1), c(3,1,2,4)]
Environment_Health_Medical_Management <- table(Medical_Management, Physical_Environment_Health_Two_Weeks)[c(2,1), c(3,1,2,4)]
Environment_Health_Educational_Intervention <- table(Educational_Intervention, Physical_Environment_Health_Two_Weeks)[c(2,1), c(3,1,2,4)]
Environment_Health_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Physical_Environment_Health_Two_Weeks)[c(2,1), c(3,1,2,4)]
Environment_Health_DSPD_Services <- table(DSPD_Services, Physical_Environment_Health_Two_Weeks)[c(2,1), c(3,1,2,4)]
Environment_Health_DSPD_Waitlist <- table(DSPD_Waitlist, Physical_Environment_Health_Two_Weeks)[c(2,1), c(3,1,2,4)]

chisq.test(Environment_Health)
chisq.test(Environment_Health_Families)
chisq.test(Environment_Health_Age)
chisq.test(Environment_Health_Family_Member_Age)
chisq.test(Environment_Health_ABA)
chisq.test(Environment_Health_OT)
chisq.test(Environment_Health_Speech)
chisq.test(Environment_Health_Medical_Management)
chisq.test(Environment_Health_Educational_Intervention)
chisq.test(Environment_Health_Other_Mental_Health)
chisq.test(Environment_Health_DSPD_Services)
chisq.test(Environment_Health_DSPD_Waitlist)

###Energy Last Two Weeks
Energy <- table(Energy_Two_Weeks)[c(1,5,4,3,2)]
Energy_Families <- table(Multiple_Family_Members, Energy_Two_Weeks)[c(2,1),c(1,5,4,3,2)]
Energy_Age <- table(Age, Energy_Two_Weeks)[,c(1,5,4,3,2)]
Energy_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Energy_Two_Weeks)[c(3,1,2),c(1,5,4,3,2)]
Energy_ABA <- table(ABA_Therapy, Energy_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Energy_OT <- table(Occupational_Therapy, Energy_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Energy_Speech <- table(Speech_Therapy, Energy_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Energy_Medical_Management <- table(Medical_Management, Energy_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Energy_Educational_Intervention <- table(Educational_Intervention, Energy_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Energy_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Energy_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Energy_DSPD_Services <- table(DSPD_Services, Energy_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Energy_DSPD_Waitlist <- table(DSPD_Waitlist, Energy_Two_Weeks)[c(2,1), c(1,5,4,3,2)]

chisq.test(Energy)
chisq.test(Energy_Families)
chisq.test(Energy_Age)
chisq.test(Energy_Family_Member_Age)
chisq.test(Energy_ABA)
chisq.test(Energy_OT)
chisq.test(Energy_Speech)
chisq.test(Energy_Medical_Management)
chisq.test(Energy_Educational_Intervention)
chisq.test(Energy_Other_Mental_Health)
chisq.test(Energy_DSPD_Services)
chisq.test(Energy_DSPD_Waitlist)

###Body Acceptance Last Two Weeks
Body_Acceptance <- table(Body_Acceptance_Two_Weeks)[c(2,6,5,4,3,1)]
Body_Acceptance_Families <- table(Multiple_Family_Members, Body_Acceptance_Two_Weeks)[c(2,1),c(2,6,5,4,3,1)]
Body_Acceptance_Age <- table(Age, Body_Acceptance_Two_Weeks)[,c(2,6,5,4,3,1)]
Body_Acceptance_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Body_Acceptance_Two_Weeks)[c(3,1,2),c(2,6,5,4,3,1)]
Body_Acceptance_ABA <- table(ABA_Therapy, Body_Acceptance_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Body_Acceptance_OT <- table(Occupational_Therapy, Body_Acceptance_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Body_Acceptance_Speech <- table(Speech_Therapy, Body_Acceptance_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Body_Acceptance_Medical_Management <- table(Medical_Management, Body_Acceptance_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Body_Acceptance_Educational_Intervention <- table(Educational_Intervention, Body_Acceptance_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Body_Acceptance_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Body_Acceptance_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Body_Acceptance_DSPD_Services <- table(DSPD_Services, Body_Acceptance_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Body_Acceptance_DSPD_Waitlist <- table(DSPD_Waitlist, Body_Acceptance_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]

chisq.test(Body_Acceptance)
chisq.test(Body_Acceptance_Families)
chisq.test(Body_Acceptance_Age)
chisq.test(Body_Acceptance_Family_Member_Age)
chisq.test(Body_Acceptance_ABA)
chisq.test(Body_Acceptance_OT)
chisq.test(Body_Acceptance_Speech)
chisq.test(Body_Acceptance_Medical_Management)
chisq.test(Body_Acceptance_Educational_Intervention)
chisq.test(Body_Acceptance_Other_Mental_Health)
chisq.test(Body_Acceptance_DSPD_Services)
chisq.test(Body_Acceptance_DSPD_Waitlist)

###Money Last Two Weeks
Money <- table(Money_Two_Weeks)[c(1,5,4,3,2)]
Money_Families <- table(Multiple_Family_Members, Money_Two_Weeks)[c(2,1),c(1,5,4,3,2)]
Money_Age <- table(Age, Money_Two_Weeks)[,c(1,5,4,3,2)]
Money_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Money_Two_Weeks)[c(3,1,2),c(1,5,4,3,2)]
Money_ABA <- table(ABA_Therapy, Money_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Money_OT <- table(Occupational_Therapy, Money_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Money_Speech <- table(Speech_Therapy, Money_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Money_Medical_Management <- table(Medical_Management, Money_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Money_Educational_Intervention <- table(Educational_Intervention, Money_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Money_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Money_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Money_DSPD_Services <- table(DSPD_Services, Money_Two_Weeks)[c(2,1), c(1,5,4,3,2)]
Money_DSPD_Waitlist <- table(DSPD_Waitlist, Money_Two_Weeks)[c(2,1), c(1,5,4,3,2)]

chisq.test(Money)
chisq.test(Money_Families)
chisq.test(Money_Age)
chisq.test(Money_Family_Member_Age)
chisq.test(Money_ABA)
chisq.test(Money_OT)
chisq.test(Money_Speech)
chisq.test(Money_Medical_Management)
chisq.test(Money_Educational_Intervention)
chisq.test(Money_Other_Mental_Health)
chisq.test(Money_DSPD_Services)
chisq.test(Money_DSPD_Waitlist)

###Information Availability Last Two Weeks
Info <- table(Information_Availability_Two_Weeks)[c(1,4,3,2)]
Info_Families <- table(Multiple_Family_Members, Information_Availability_Two_Weeks)[c(2,1), c(1,4,3,2)]
Info_Age <- table(Age, Information_Availability_Two_Weeks)[,c(1,4,3,2)]
Info_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Information_Availability_Two_Weeks)[c(3,1,2),c(1,4,3,2)]
Info_ABA <- table(ABA_Therapy, Information_Availability_Two_Weeks)[c(2,1), c(1,4,3,2)]
Info_OT <- table(Occupational_Therapy, Information_Availability_Two_Weeks)[c(2,1), c(1,4,3,2)]
Info_Speech <- table(Speech_Therapy, Information_Availability_Two_Weeks)[c(2,1), c(1,4,3,2)]
Info_Medical_Management <- table(Medical_Management, Information_Availability_Two_Weeks)[c(2,1), c(1,4,3,2)]
Info_Educational_Intervention <- table(Educational_Intervention, Information_Availability_Two_Weeks)[c(2,1), c(1,4,3,2)]
Info_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Information_Availability_Two_Weeks)[c(2,1), c(1,4,3,2)]
Info_DSPD_Services <- table(DSPD_Services, Information_Availability_Two_Weeks)[c(2,1), c(1,4,3,2)]
Info_DSPD_Waitlist <- table(DSPD_Waitlist, Information_Availability_Two_Weeks)[c(2,1), c(1,4,3,2)]

chisq.test(Info)
chisq.test(Info_Families)
chisq.test(Info_Age)
chisq.test(Info_Family_Member_Age)
chisq.test(Info_ABA)
chisq.test(Info_OT)
chisq.test(Info_Speech)
chisq.test(Info_Medical_Management)
chisq.test(Info_Educational_Intervention)
chisq.test(Info_Other_Mental_Health)
chisq.test(Info_DSPD_Services)
chisq.test(Info_DSPD_Waitlist)

###Leisure Opportunities Last Two Weeks
Leisure <- table(Leisure_Opportunities_Two_Weeks)[c(2,6,5,4,3,1)]
Leisure_Families <- table(Multiple_Family_Members, Leisure_Opportunities_Two_Weeks)[c(2,1),c(2,6,5,4,3,1)]
Leisure_Age <- table(autism$Age, autism$Leisure_Opportunities_Two_Weeks)[,c(2,6,5,4,3,1)]
Leisure_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Leisure_Opportunities_Two_Weeks)[c(3,1,2),c(2,6,5,4,3,1)]
Leisure_ABA <- table(ABA_Therapy, Leisure_Opportunities_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Leisure_OT <- table(Occupational_Therapy, Leisure_Opportunities_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Leisure_Speech <- table(Speech_Therapy, Leisure_Opportunities_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Leisure_Medical_Management <- table(Medical_Management, Leisure_Opportunities_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Leisure_Educational_Intervention <- table(Educational_Intervention, Leisure_Opportunities_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Leisure_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Leisure_Opportunities_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Leisure_DSPD_Services <- table(DSPD_Services, Leisure_Opportunities_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]
Leisure_DSPD_Waitlist <- table(DSPD_Waitlist, Leisure_Opportunities_Two_Weeks)[c(2,1), c(2,6,5,4,3,1)]

chisq.test(Leisure)
chisq.test(Leisure_Families)
chisq.test(Leisure_Age)
chisq.test(Leisure_Family_Member_Age)
chisq.test(Leisure_ABA)
chisq.test(Leisure_OT)
chisq.test(Leisure_Speech)
chisq.test(Leisure_Medical_Management)
chisq.test(Leisure_Educational_Intervention)
chisq.test(Leisure_Other_Mental_Health)
chisq.test(Leisure_DSPD_Services)
chisq.test(Leisure_DSPD_Waitlist)

###Family Support Last Two Weeks
Family_Support_Table <- table(Family_Support)[c(1,4,5,2,3)]
Family_Support_Families <- table(Multiple_Family_Members, Family_Support)[c(2,1),c(1,4,5,2,3)]
Family_Support_Age <- table(Age, Family_Support)[,c(1,4,5,2,3)]
Family_Support_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Family_Support)[c(3,1,2),c(1,4,5,2,3)]
Family_Support_ABA <- table(ABA_Therapy, Family_Support)[c(2,1), c(1,4,5,2,3)]
Family_Support_OT <- table(Occupational_Therapy, Family_Support)[c(2,1), c(1,4,5,2,3)]
Family_Support_Speech <- table(Speech_Therapy, Family_Support)[c(2,1), c(1,4,5,2,3)]
Family_Support_Medical_Management <- table(Medical_Management, Family_Support)[c(2,1), c(1,4,5,2,3)]
Family_Support_Educational_Intervention <- table(Educational_Intervention, Family_Support)[c(2,1), c(1,4,5,2,3)]
Family_Support_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Family_Support)[c(2,1), c(1,4,5,2,3)]
Family_Support_DSPD_Services <- table(DSPD_Services, Family_Support)[c(2,1), c(1,4,5,2,3)]
Family_Support_DSPD_Waitlist <- table(DSPD_Waitlist, Family_Support)[c(2,1), c(1,4,5,2,3)]

chisq.test(Family_Support_Table)
chisq.test(Family_Support_Families)
chisq.test(Family_Support_Age)
chisq.test(Family_Support_Family_Member_Age)
chisq.test(Family_Support_ABA)
chisq.test(Family_Support_OT)
chisq.test(Family_Support_Speech)
chisq.test(Family_Support_Medical_Management)
chisq.test(Family_Support_Educational_Intervention)
chisq.test(Family_Support_Other_Mental_Health)
chisq.test(Family_Support_DSPD_Services)
chisq.test(Family_Support_DSPD_Waitlist)

###Community Support Last Two Weeks
Community_Support_Table <- table(Community_Support)[c(1,4,5,2,3)]
Community_Support_Families <- table(Multiple_Family_Members, Community_Support)[c(2,1),c(1,4,5,2,3)]
Community_Support_Age <- table(Age, Community_Support)[,c(1,4,5,2,3)]
Community_Support_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Community_Support)[c(3,1,2),c(1,4,5,2,3)]
Community_Support_ABA <- table(ABA_Therapy, Community_Support)[c(2,1), c(1,4,5,2,3)]
Community_Support_OT <- table(Occupational_Therapy, Community_Support)[c(2,1), c(1,4,5,2,3)]
Community_Support_Speech <- table(Speech_Therapy, Community_Support)[c(2,1), c(1,4,5,2,3)]
Community_Support_Medical_Management <- table(Medical_Management, Community_Support)[c(2,1), c(1,4,5,2,3)]
Community_Support_Educational_Intervention <- table(Educational_Intervention, Community_Support)[c(2,1), c(1,4,5,2,3)]
Community_Support_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Community_Support)[c(2,1), c(1,4,5,2,3)]
Community_Support_DSPD_Services <- table(DSPD_Services, Community_Support)[c(2,1), c(1,4,5,2,3)]
Community_Support_DSPD_Waitlist <- table(DSPD_Waitlist, Community_Support)[c(2,1), c(1,4,5,2,3)]

chisq.test(Community_Support_Table)
chisq.test(Community_Support_Families)
chisq.test(Community_Support_Age)
chisq.test(Community_Support_Family_Member_Age)
chisq.test(Community_Support_ABA)
chisq.test(Community_Support_OT)
chisq.test(Community_Support_Speech)
chisq.test(Community_Support_Medical_Management)
chisq.test(Community_Support_Educational_Intervention)
chisq.test(Community_Support_Other_Mental_Health)
chisq.test(Community_Support_DSPD_Services)
chisq.test(Community_Support_DSPD_Waitlist)

###Professional Support Last Two Weeks
Professional_Support_Table <- table(Professional_Support)[c(1,4,5,2,3)]
Professional_Support_Families <- table(Multiple_Family_Members, Professional_Support)[c(2,1),c(1,4,5,2,3)]
Professional_Support_Age <- table(Age, Professional_Support)[,c(1,4,5,2,3)]
Professional_Support_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Professional_Support)[c(3,1,2),c(1,4,5,2,3)]
Professional_Support_ABA <- table(ABA_Therapy, Professional_Support)[c(2,1), c(1,4,5,2,3)]
Professional_Support_OT <- table(Occupational_Therapy, Professional_Support)[c(2,1), c(1,4,5,2,3)]
Professional_Support_Speech <- table(Speech_Therapy, Professional_Support)[c(2,1), c(1,4,5,2,3)]
Professional_Support_Medical_Management <- table(Medical_Management, Professional_Support)[c(2,1), c(1,4,5,2,3)]
Professional_Support_Educational_Intervention <- table(Educational_Intervention, Professional_Support)[c(2,1), c(1,4,5,2,3)]
Professional_Support_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Professional_Support)[c(2,1), c(1,4,5,2,3)]
Professional_Support_DSPD_Services <- table(DSPD_Services, Professional_Support)[c(2,1), c(1,4,5,2,3)]
Professional_Support_DSPD_Waitlist <- table(DSPD_Waitlist, Professional_Support)[c(2,1), c(1,4,5,2,3)]

chisq.test(Professional_Support_Table)
chisq.test(Professional_Support_Families)
chisq.test(Professional_Support_Age)
chisq.test(Professional_Support_Family_Member_Age)
chisq.test(Professional_Support_ABA)
chisq.test(Professional_Support_OT)
chisq.test(Professional_Support_Speech)
chisq.test(Professional_Support_Medical_Management)
chisq.test(Professional_Support_Educational_Intervention)
chisq.test(Professional_Support_Other_Mental_Health)
chisq.test(Professional_Support_DSPD_Services)
chisq.test(Professional_Support_DSPD_Waitlist)

###Family Support Quality Last Two Weeks
Family_Support_Quality_Table <- table(Family_Support_Quality)[c(1,4,3,2)]
Family_Support_Quality_Families <- table(Multiple_Family_Members, Family_Support_Quality)[c(2,1),c(1,4,3,2)]
Family_Support_Quality_Age <- table(Age, Family_Support_Quality)[,c(1,4,3,2)]
Family_Support_Quality_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Family_Support_Quality)[c(3,1,2),c(1,4,3,2)]
Family_Support_Quality_ABA <- table(ABA_Therapy, Family_Support_Quality)[c(2,1), c(1,4,3,2)]
Family_Support_Quality_OT <- table(Occupational_Therapy, Family_Support_Quality)[c(2,1), c(1,4,3,2)]
Family_Support_Quality_Speech <- table(Speech_Therapy, Family_Support_Quality)[c(2,1), c(1,4,3,2)]
Family_Support_Quality_Medical_Management <- table(Medical_Management, Family_Support_Quality)[c(2,1), c(1,4,3,2)]
Family_Support_Quality_Educational_Intervention <- table(Educational_Intervention, Family_Support_Quality)[c(2,1), c(1,4,3,2)]
Family_Support_Quality_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Family_Support_Quality)[c(2,1), c(1,4,3,2)]
Family_Support_Quality_DSPD_Services <- table(DSPD_Services, Family_Support_Quality)[c(2,1), c(1,4,3,2)]
Family_Support_Quality_DSPD_Waitlist <- table(DSPD_Waitlist, Family_Support_Quality)[c(2,1), c(1,4,3,2)]

chisq.test(Family_Support_Quality_Table)
chisq.test(Family_Support_Quality_Families)
chisq.test(Family_Support_Quality_Age)
chisq.test(Family_Support_Quality_Family_Member_Age)
chisq.test(Family_Support_Quality_ABA)
chisq.test(Family_Support_Quality_OT)
chisq.test(Family_Support_Quality_Speech)
chisq.test(Family_Support_Quality_Medical_Management)
chisq.test(Family_Support_Quality_Educational_Intervention)
chisq.test(Family_Support_Quality_Other_Mental_Health)
chisq.test(Family_Support_Quality_DSPD_Services)
chisq.test(Family_Support_Quality_DSPD_Waitlist)

###Community Support Quality Last Two Weeks
Community_Support_Quality_Table <- table(Community_Support_Quality)[c(1,4,3,2)]
Community_Support_Quality_Families <- table(Multiple_Family_Members, Community_Support_Quality)[c(2,1),c(1,4,3,2)]
Community_Support_Quality_Age <- table(Age, Community_Support_Quality)[,c(1,4,3,2)]
Community_Support_Quality_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Community_Support_Quality)[c(3,1,2),c(1,4,3,2)]
Community_Support_Quality_ABA <- table(ABA_Therapy, Community_Support_Quality)[c(2,1), c(1,4,3,2)]
Community_Support_Quality_OT <- table(Occupational_Therapy, Community_Support_Quality)[c(2,1), c(1,4,3,2)]
Community_Support_Quality_Speech <- table(Speech_Therapy, Community_Support_Quality)[c(2,1), c(1,4,3,2)]
Community_Support_Quality_Medical_Management <- table(Medical_Management, Community_Support_Quality)[c(2,1), c(1,4,3,2)]
Community_Support_Quality_Educational_Intervention <- table(Educational_Intervention, Community_Support_Quality)[c(2,1), c(1,4,3,2)]
Community_Support_Quality_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Community_Support_Quality)[c(2,1), c(1,4,3,2)]
Community_Support_Quality_DSPD_Services <- table(DSPD_Services, Community_Support_Quality)[c(2,1), c(1,4,3,2)]
Community_Support_Quality_DSPD_Waitlist <- table(DSPD_Waitlist, Community_Support_Quality)[c(2,1), c(1,4,3,2)]

chisq.test(Community_Support_Quality_Table)
chisq.test(Community_Support_Quality_Families)
chisq.test(Community_Support_Quality_Age)
chisq.test(Community_Support_Quality_Family_Member_Age)
chisq.test(Community_Support_Quality_ABA)
chisq.test(Community_Support_Quality_OT)
chisq.test(Community_Support_Quality_Speech)
chisq.test(Community_Support_Quality_Medical_Management)
chisq.test(Community_Support_Quality_Educational_Intervention)
chisq.test(Community_Support_Quality_Other_Mental_Health)
chisq.test(Community_Support_Quality_DSPD_Services)
chisq.test(Community_Support_Quality_DSPD_Waitlist)

###Professional Support Quality Last Two Weeks
Professional_Support_Quality_Table <- table(Professional_Support_Quality)[c(2,5,4,3,1)]
Professional_Support_Quality_Families <- table(Multiple_Family_Members, Professional_Support_Quality)[c(2,1),c(2,5,4,3,1)]
Professional_Support_Quality_Age <- table(Age, Professional_Support_Quality)[,c(2,5,4,3,1)]
Professional_Support_Quality_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Professional_Support_Quality)[c(3,1,2),c(2,5,4,3,1)]
Professional_Support_Quality_ABA <- table(ABA_Therapy, Professional_Support_Quality)[c(2,1), c(2,5,4,3,1)]
Professional_Support_Quality_OT <- table(Occupational_Therapy, Professional_Support_Quality)[c(2,1), c(2,5,4,3,1)]
Professional_Support_Quality_Speech <- table(Speech_Therapy, Professional_Support_Quality)[c(2,1), c(2,5,4,3,1)]
Professional_Support_Quality_Medical_Management <- table(Medical_Management, Professional_Support_Quality)[c(2,1), c(2,5,4,3,1)]
Professional_Support_Quality_Educational_Intervention <- table(Educational_Intervention, Professional_Support_Quality)[c(2,1), c(2,5,4,3,1)]
Professional_Support_Quality_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Professional_Support_Quality)[c(2,1), c(2,5,4,3,1)]
Professional_Support_Quality_DSPD_Services <- table(DSPD_Services, Professional_Support_Quality)[c(2,1), c(2,5,4,3,1)]
Professional_Support_Quality_DSPD_Waitlist <- table(DSPD_Waitlist, Professional_Support_Quality)[c(2,1), c(2,5,4,3,1)]

chisq.test(Professional_Support_Quality_Table)
chisq.test(Professional_Support_Quality_Families)
chisq.test(Professional_Support_Quality_Age)
chisq.test(Professional_Support_Quality_Family_Member_Age)
chisq.test(Professional_Support_Quality_ABA)
chisq.test(Professional_Support_Quality_OT)
chisq.test(Professional_Support_Quality_Speech)
chisq.test(Professional_Support_Quality_Medical_Management)
chisq.test(Professional_Support_Quality_Educational_Intervention)
chisq.test(Professional_Support_Quality_Other_Mental_Health)
chisq.test(Professional_Support_Quality_DSPD_Services)
chisq.test(Professional_Support_Quality_DSPD_Waitlist)


###ABA Therapy
ABA_Families <- table(Multiple_Family_Members, ABA_Therapy)[c(2,1), c(2,1)]
ABA_Age <- table(Age, ABA_Therapy)[, c(2,1)]
ABA_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$ABA_Therapy)[c(3,1,2),c(2,1)]
ABA_OT <- table(Occupational_Therapy, ABA_Therapy)[c(2,1), c(2,1)]
ABA_Speech <- table(Speech_Therapy, ABA_Therapy)[c(2,1), c(2,1)]
ABA_Medical_Management <- table(Medical_Management, ABA_Therapy)[c(2,1), c(2,1)]
ABA_Educational_Intervention <- table(Educational_Intervention, ABA_Therapy)[c(2,1), c(2,1)]
ABA_Other_Mental_Health <- table(Other_Mental_Health_Therapy, ABA_Therapy)[c(2,1), c(2,1)]
ABA_DSPD_Services <- table(DSPD_Services, ABA_Therapy)[c(2,1), c(2,1)]
ABA_DSPD_Waitlist <- table(DSPD_Waitlist, ABA_Therapy)[c(2,1), c(2,1)]

chisq.test(ABA_Families)
chisq.test(ABA_Age)
chisq.test(ABA_Family_Member_Age)
chisq.test(ABA_OT)
chisq.test(ABA_Speech)
chisq.test(ABA_Medical_Management)
chisq.test(ABA_Educational_Intervention)
chisq.test(ABA_Other_Mental_Health)
chisq.test(ABA_DSPD_Services)
chisq.test(ABA_DSPD_Waitlist)

###Occupational Therapy
table(Occupational_Therapy)[c(2,1)]
OT_Families <- table(Multiple_Family_Members, Occupational_Therapy)[c(2,1), c(2,1)]
OT_Age <- table(Age, Occupational_Therapy)[, c(2,1)]
OT_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Occupational_Therapy)[c(3,1,2),c(2,1)]
OT_Speech <- table(Speech_Therapy, Occupational_Therapy)[c(2,1), c(2,1)]
OT_Medical_Management <- table(Medical_Management, Occupational_Therapy)[c(2,1), c(2,1)]
OT_Educational_Intervention <- table(Educational_Intervention, Occupational_Therapy)[c(2,1), c(2,1)]
OT_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Occupational_Therapy)[c(2,1), c(2,1)]
OT_DSPD_Services <- table(DSPD_Services, Occupational_Therapy)[c(2,1), c(2,1)]
OT_DSPD_Waitlist <- table(DSPD_Waitlist, Occupational_Therapy)[c(2,1), c(2,1)]

chisq.test(OT_Families)
chisq.test(OT_Age)
chisq.test(OT_Family_Member_Age)
chisq.test(OT_Speech)
chisq.test(OT_Medical_Management)
chisq.test(OT_Educational_Intervention)
chisq.test(OT_Other_Mental_Health)
chisq.test(OT_DSPD_Services)
chisq.test(OT_DSPD_Waitlist)

###Speech Therapy
table(Speech_Therapy)[c(2,1)]
Speech_Families <- table(Multiple_Family_Members, Speech_Therapy)[c(2,1), c(2,1)]
Speech_Age <- table(Age, Speech_Therapy)[, c(2,1)]
Speech_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Speech_Therapy)[c(3,1,2),c(2,1)]
Speech_Medical_Management <- table(Medical_Management, Speech_Therapy)[c(2,1), c(2,1)]
Speech_Educational_Intervention <- table(Educational_Intervention, Speech_Therapy)[c(2,1), c(2,1)]
Speech_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Speech_Therapy)[c(2,1), c(2,1)]
Speech_DSPD_Services <- table(DSPD_Services, Speech_Therapy)[c(2,1), c(2,1)]
Speech_DSPD_Waitlist <- table(DSPD_Waitlist, Speech_Therapy)[c(2,1), c(2,1)]

chisq.test(Speech_Families)
chisq.test(Speech_Age)
chisq.test(Speech_Family_Member_Age)
chisq.test(Speech_Medical_Management)
chisq.test(Speech_Educational_Intervention)
chisq.test(Speech_Other_Mental_Health)
chisq.test(Speech_DSPD_Services)
chisq.test(Speech_DSPD_Waitlist)

###Medical Management
table(Medical_Management)[c(2,1)]
Medical_Management_Families <- table(Multiple_Family_Members, Medical_Management)[c(2,1), c(2,1)]
Medical_Management_Age <- table(Age, Medical_Management)[, c(2,1)]
Medical_Management_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Medical_Management)[c(3,1,2),c(2,1)]
Medical_Management_Educational_Intervention <- table(Educational_Intervention, Medical_Management)[c(2,1), c(2,1)]
Medical_Management_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Medical_Management)[c(2,1), c(2,1)]
Medical_Management_DSPD_Services <- table(DSPD_Services, Medical_Management)[c(2,1), c(2,1)]
Medical_Management_DSPD_Waitlist <- table(DSPD_Waitlist, Medical_Management)[c(2,1), c(2,1)]

chisq.test(Medical_Management_Families)
chisq.test(Medical_Management_Age)
chisq.test(Medical_Management_Family_Member_Age)
chisq.test(Medical_Management_Educational_Intervention)
chisq.test(Medical_Management_Other_Mental_Health)
chisq.test(Medical_Management_DSPD_Services)
chisq.test(Medical_Management_DSPD_Waitlist)

###Educational Intervention
table(Educational_Intervention)[c(2,1)]
Educational_Intervention_Families <- table(Multiple_Family_Members, Educational_Intervention)[c(2,1), c(2,1)]
Educational_Intervention_Age <- table(Age, Educational_Intervention)[, c(2,1)]
Educational_Intervention_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Educational_Intervention)[c(3,1,2),c(2,1)]
Educational_Intervention_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Educational_Intervention)[c(2,1), c(2,1)]
Educational_Intervention_DSPD_Services <- table(DSPD_Services, Educational_Intervention)[c(2,1), c(2,1)]
Educational_Intervention_DSPD_Waitlist <- table(DSPD_Waitlist, Educational_Intervention)[c(2,1), c(2,1)]

chisq.test(Educational_Intervention_Families)
chisq.test(Educational_Intervention_Age)
chisq.test(Educational_Intervention_Family_Member_Age)
chisq.test(Educational_Intervention_Other_Mental_Health)
chisq.test(Educational_Intervention_DSPD_Services)
chisq.test(Educational_Intervention_DSPD_Waitlist)

###Other Mental Health
table(Other_Mental_Health_Therapy)[c(2,1)]
Other_Mental_Health_Therapy_Families <- table(Multiple_Family_Members, Other_Mental_Health_Therapy)[c(2,1), c(2,1)]
Other_Mental_Health_Therapy_Age <- table(Age, Other_Mental_Health_Therapy)[, c(2,1)]
Other_Mental_Health_Therapy_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Other_Mental_Health_Therapy)[c(3,1,2),c(2,1)]
Other_Mental_Health_Therapy_DSPD_Services <- table(DSPD_Services, Other_Mental_Health_Therapy)[c(2,1), c(2,1)]
Other_Mental_Health_Therapy_DSPD_Waitlist <- table(DSPD_Waitlist, Other_Mental_Health_Therapy)[c(2,1), c(2,1)]

chisq.test(Other_Mental_Health_Therapy_Families)
chisq.test(Other_Mental_Health_Therapy_Age)
chisq.test(Other_Mental_Health_Therapy_Family_Member_Age)
chisq.test(Other_Mental_Health_Therapy_DSPD_Services)
chisq.test(Other_Mental_Health_Therapy_DSPD_Waitlist)

###DSPD Services
table(DSPD_Services)[c(2,1)]
DSPD_Services_Families <- table(Multiple_Family_Members, DSPD_Services)[c(2,1), c(2,1)]
DSPD_Services_Age <- table(Age, DSPD_Services)[, c(2,1)]
DSPD_Services_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$DSPD_Services)[c(3,1,2),c(2,1)]
DSPD_Services_DSPD_Waitlist <- table(DSPD_Waitlist, DSPD_Services)[c(2,1), c(2,1)]

chisq.test(DSPD_Services_Families)
chisq.test(DSPD_Services_Age)
chisq.test(DSPD_Services_Family_Member_Age)
chisq.test(DSPD_Services_DSPD_Waitlist)

###DSPD Waitlist
table(DSPD_Waitlist)[c(2,1)]
DSPD_Waitlist_Families <- table(Multiple_Family_Members, DSPD_Waitlist)[c(2,1), c(2,1)]
DSPD_Waitlist_Age <- table(Age, DSPD_Waitlist)[, c(2,1)]
DSPD_Waitlist_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$DSPD_Waitlist)[c(3,1,2),c(2,1)]

chisq.test(DSPD_Waitlist_Families)
chisq.test(DSPD_Waitlist_Age)
chisq.test(DSPD_Waitlist_Family_Member_Age)


###Monthly Cost
length(Monthly_Service_Cost[!is.na(Monthly_Service_Cost)])
Hours <- Monthly_Service_Cost[!is.na(Monthly_Service_Cost)]
hist(Hours)
median(sort(Hours))

length(Hours[Hours == 0])
Hours_Values <- Hours[Hours != 0]
length(Hours_Values)
median(sort(Hours_Values))

Hours_Families <- Monthly_Service_Cost[Multiple_Family_Members == "Yes" & !is.na(Monthly_Service_Cost)]
Hours_Not_Families <- Monthly_Service_Cost[Multiple_Family_Members == "No" & !is.na(Monthly_Service_Cost)]
Hours_Under_40 <- Monthly_Service_Cost[Age == "<40" & !is.na(Monthly_Service_Cost)]
Hours_40_to_49 <- Monthly_Service_Cost[Age == "40-49" & !is.na(Monthly_Service_Cost)]
Hours_over_50 <- Monthly_Service_Cost[Age == "50+" & !is.na(Monthly_Service_Cost)]
Hours_4_to_9 <- Monthly_Service_Cost[autism_Single_Family_Member$Family_Member_Age == "4 to 9" & !is.na(Monthly_Service_Cost)]
Hours_10_to_14 <- Monthly_Service_Cost[autism_Single_Family_Member$Family_Member_Age == "10 to 14" & !is.na(Monthly_Service_Cost)]
Hours_over_15 <- Monthly_Service_Cost[autism_Single_Family_Member$Family_Member_Age == "15+" & !is.na(Monthly_Service_Cost)]
Hours_ABA <- Monthly_Service_Cost[ABA_Therapy == "Yes" & !is.na(Monthly_Service_Cost)]
Hours_Not_ABA <- Monthly_Service_Cost[ABA_Therapy == "No" & !is.na(Monthly_Service_Cost)]
Hours_OT <- Monthly_Service_Cost[Occupational_Therapy == "Yes" & !is.na(Monthly_Service_Cost)]
Hours_Not_OT <- Monthly_Service_Cost[Occupational_Therapy == "No" & !is.na(Monthly_Service_Cost)]
Hours_Speech <- Monthly_Service_Cost[Speech_Therapy == "Yes" & !is.na(Monthly_Service_Cost)]
Hours_Not_Speech <- Monthly_Service_Cost[Speech_Therapy == "No" & !is.na(Monthly_Service_Cost)]
Hours_Medical_Management <- Monthly_Service_Cost[Medical_Management == "Yes" & !is.na(Monthly_Service_Cost)]
Hours_Not_Medical_Management <- Monthly_Service_Cost[Medical_Management == "No" & !is.na(Monthly_Service_Cost)]
Hours_Educational_Intervention <- Monthly_Service_Cost[Educational_Intervention == "Yes" & !is.na(Monthly_Service_Cost)]
Hours_Not_Educational_Intervention <- Monthly_Service_Cost[Educational_Intervention == "No" & !is.na(Monthly_Service_Cost)]
Hours_Other_Mental_Health <- Monthly_Service_Cost[Other_Mental_Health_Therapy == "Yes" & !is.na(Monthly_Service_Cost)]
Hours_Not_Other_Mental_Health <- Monthly_Service_Cost[Other_Mental_Health_Therapy == "No" & !is.na(Monthly_Service_Cost)]
Hours_DSPD_Services <- Monthly_Service_Cost[DSPD_Services == "Yes" & !is.na(Monthly_Service_Cost)]
Hours_Not_DSPD_Services <- Monthly_Service_Cost[DSPD_Services == "No" & !is.na(Monthly_Service_Cost)]
Hours_DSPD_Waitlist <- Monthly_Service_Cost[DSPD_Waitlist == "Yes" & !is.na(Monthly_Service_Cost)]
Hours_Not_DSPD_Waitlist <- Monthly_Service_Cost[DSPD_Waitlist == "No" & !is.na(Monthly_Service_Cost)]

wilcox.test(Hours_Families, Hours_Not_Families)
kruskal.test(list(Hours_Under_40, Hours_40_to_49, Hours_over_50))
kruskal.test(list(Hours_4_to_9, Hours_10_to_14, Hours_over_15))
wilcox.test(Hours_ABA, Hours_Not_ABA)
wilcox.test(Hours_OT, Hours_Not_OT)
wilcox.test(Hours_Speech, Hours_Not_Speech)
wilcox.test(Hours_Medical_Management, Hours_Not_Medical_Management)
wilcox.test(Hours_Educational_Intervention, Hours_Not_Educational_Intervention)
wilcox.test(Hours_Other_Mental_Health, Hours_Not_Other_Mental_Health)
wilcox.test(Hours_DSPD_Services, Hours_Not_DSPD_Services)
wilcox.test(Hours_DSPD_Waitlist, Hours_Not_DSPD_Waitlist)

wilcox.test(Hours_Under_40, Hours_40_to_49)
wilcox.test(Hours_Under_40, Hours_over_50)
wilcox.test(Hours_40_to_49, Hours_over_50)
median(Hours_Under_40); median(Hours_40_to_49); median(Hours_over_50)
median(Hours_ABA); median(Hours_Not_ABA)

###Weekly Hours
length(Weekly_Hours[!is.na(Weekly_Hours)])
Hours <- Weekly_Hours[!is.na(Weekly_Hours)]
hist(Hours)
median(sort(Hours))


Hours_Families <- Hours[Multiple_Family_Members == "Yes" & !is.na(Hours)]
Hours_Not_Families <- Hours[Multiple_Family_Members == "No" & !is.na(Hours)]
Hours_Under_40 <- Hours[Age == "<40" & !is.na(Hours)]
Hours_40_to_49 <- Hours[Age == "40-49" & !is.na(Hours)]
Hours_over_50 <- Hours[Age == "50+" & !is.na(Hours)]
Hours_4_to_9 <- Hours[autism_Single_Family_Member$Family_Member_Age == "4 to 9" & !is.na(Hours)]
Hours_10_to_14 <- Hours[autism_Single_Family_Member$Family_Member_Age == "10 to 14" & !is.na(Hours)]
Hours_over_15 <- Hours[autism_Single_Family_Member$Family_Member_Age == "15+" & !is.na(Hours)]
Hours_ABA <- Hours[ABA_Therapy == "Yes" & !is.na(Hours)]
Hours_Not_ABA <- Hours[ABA_Therapy == "No" & !is.na(Hours)]
Hours_OT <- Hours[Occupational_Therapy == "Yes" & !is.na(Hours)]
Hours_Not_OT <- Hours[Occupational_Therapy == "No" & !is.na(Hours)]
Hours_Speech <- Hours[Speech_Therapy == "Yes" & !is.na(Hours)]
Hours_Not_Speech <- Hours[Speech_Therapy == "No" & !is.na(Hours)]
Hours_Medical_Management <- Hours[Medical_Management == "Yes" & !is.na(Hours)]
Hours_Not_Medical_Management <- Hours[Medical_Management == "No" & !is.na(Hours)]
Hours_Educational_Intervention <- Hours[Educational_Intervention == "Yes" & !is.na(Hours)]
Hours_Not_Educational_Intervention <- Hours[Educational_Intervention == "No" & !is.na(Hours)]
Hours_Other_Mental_Health <- Hours[Other_Mental_Health_Therapy == "Yes" & !is.na(Hours)]
Hours_Not_Other_Mental_Health <- Hours[Other_Mental_Health_Therapy == "No" & !is.na(Hours)]
Hours_DSPD_Services <- Hours[DSPD_Services == "Yes" & !is.na(Hours)]
Hours_Not_DSPD_Services <- Hours[DSPD_Services == "No" & !is.na(Hours)]
Hours_DSPD_Waitlist <- Hours[DSPD_Waitlist == "Yes" & !is.na(Hours)]
Hours_Not_DSPD_Waitlist <- Hours[DSPD_Waitlist == "No" & !is.na(Hours)]

wilcox.test(Hours_Families, Hours_Not_Families)
kruskal.test(list(Hours_Under_40, Hours_40_to_49, Hours_over_50))
kruskal.test(list(Hours_4_to_9, Hours_10_to_14, Hours_over_15))
wilcox.test(Hours_ABA, Hours_Not_ABA)
wilcox.test(Hours_OT, Hours_Not_OT)
wilcox.test(Hours_Speech, Hours_Not_Speech)
wilcox.test(Hours_Medical_Management, Hours_Not_Medical_Management)
wilcox.test(Hours_Educational_Intervention, Hours_Not_Educational_Intervention)
wilcox.test(Hours_Other_Mental_Health, Hours_Not_Other_Mental_Health)
wilcox.test(Hours_DSPD_Services, Hours_Not_DSPD_Services)
wilcox.test(Hours_DSPD_Waitlist, Hours_Not_DSPD_Waitlist)

###Isolation Last Six Months
Isolation <- table(Isolation_Six_Months)[c(6,4,5,2,3,1)]
Isolation_Families <- table(Multiple_Family_Members, Isolation_Six_Months)[c(2,1),c(6,4,5,2,3,1)]
Isolation_Age <- table(Age, Isolation_Six_Months)[,c(6,4,5,2,3,1)]
Isolation_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Isolation_Six_Months)[c(3,1,2),c(6,4,5,2,3,1)]
Isolation_ABA <- table(ABA_Therapy, Isolation_Six_Months)[c(2,1), c(6,4,5,2,3,1)]
Isolation_OT <- table(Occupational_Therapy, Isolation_Six_Months)[c(2,1), c(6,4,5,2,3,1)]
Isolation_Speech <- table(Speech_Therapy, Isolation_Six_Months)[c(2,1), c(6,4,5,2,3,1)]
Isolation_Medical_Management <- table(Medical_Management, Isolation_Six_Months)[c(2,1), c(6,4,5,2,3,1)]
Isolation_Educational_Intervention <- table(Educational_Intervention, Isolation_Six_Months)[c(2,1), c(6,4,5,2,3,1)]
Isolation_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Isolation_Six_Months)[c(2,1), c(6,4,5,2,3,1)]
Isolation_DSPD_Services <- table(DSPD_Services, Isolation_Six_Months)[c(2,1), c(6,4,5,2,3,1)]
Isolation_DSPD_Waitlist <- table(DSPD_Waitlist, Isolation_Six_Months)[c(2,1), c(6,4,5,2,3,1)]

chisq.test(Isolation)
chisq.test(Isolation_Families)
chisq.test(Isolation_Age)
chisq.test(Isolation_Family_Member_Age)
chisq.test(Isolation_ABA)
chisq.test(Isolation_OT)
chisq.test(Isolation_Speech)
chisq.test(Isolation_Medical_Management)
chisq.test(Isolation_Educational_Intervention)
chisq.test(Isolation_Other_Mental_Health)
chisq.test(Isolation_DSPD_Services)
chisq.test(Isolation_DSPD_Waitlist)

###Unhappiness Last Six Months
Unhappiness <- table(Unhappiness_Six_Months)[c(5,3,4,1,2)]
Unhappiness_Families <- table(Multiple_Family_Members, Unhappiness_Six_Months)[c(2,1),c(5,3,4,1,2)]
Unhappiness_Age <- table(Age, Unhappiness_Six_Months)[,c(5,3,4,1,2)]
Unhappiness_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Unhappiness_Six_Months)[c(3,1,2),c(5,3,4,1,2)]
Unhappiness_ABA <- table(ABA_Therapy, Unhappiness_Six_Months)[c(2,1), c(5,3,4,1,2)]
Unhappiness_OT <- table(Occupational_Therapy, Unhappiness_Six_Months)[c(2,1), c(5,3,4,1,2)]
Unhappiness_Speech <- table(Speech_Therapy, Unhappiness_Six_Months)[c(2,1), c(5,3,4,1,2)]
Unhappiness_Medical_Management <- table(Medical_Management, Unhappiness_Six_Months)[c(2,1), c(5,3,4,1,2)]
Unhappiness_Educational_Intervention <- table(Educational_Intervention, Unhappiness_Six_Months)[c(2,1), c(5,3,4,1,2)]
Unhappiness_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Unhappiness_Six_Months)[c(2,1), c(5,3,4,1,2)]
Unhappiness_DSPD_Services <- table(DSPD_Services, Unhappiness_Six_Months)[c(2,1), c(5,3,4,1,2)]
Unhappiness_DSPD_Waitlist <- table(DSPD_Waitlist, Unhappiness_Six_Months)[c(2,1), c(5,3,4,1,2)]

chisq.test(Unhappiness)
chisq.test(Unhappiness_Families)
chisq.test(Unhappiness_Age)
chisq.test(Unhappiness_Family_Member_Age)
chisq.test(Unhappiness_ABA)
chisq.test(Unhappiness_OT)
chisq.test(Unhappiness_Speech)
chisq.test(Unhappiness_Medical_Management)
chisq.test(Unhappiness_Educational_Intervention)
chisq.test(Unhappiness_Other_Mental_Health)
chisq.test(Unhappiness_DSPD_Services)
chisq.test(Unhappiness_DSPD_Waitlist)

###Embarrassment Last Six Months
Embarrassment <- table(Embarrasment_Six_Months)[c(5,3,4,1,2)]
Embarrassment_Families <- table(Multiple_Family_Members, Embarrasment_Six_Months)[c(2,1),c(5,3,4,1,2)]
Embarrassment_Age <- table(Age, Embarrasment_Six_Months)[,c(5,3,4,1,2)]
Embarrassment_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Embarrasment_Six_Months)[c(3,1,2),c(5,3,4,1,2)]
Embarrassment_ABA <- table(ABA_Therapy, Embarrasment_Six_Months)[c(2,1), c(5,3,4,1,2)]
Embarrassment_OT <- table(Occupational_Therapy, Embarrasment_Six_Months)[c(2,1), c(5,3,4,1,2)]
Embarrassment_Speech <- table(Speech_Therapy, Embarrasment_Six_Months)[c(2,1), c(5,3,4,1,2)]
Embarrassment_Medical_Management <- table(Medical_Management, Embarrasment_Six_Months)[c(2,1), c(5,3,4,1,2)]
Embarrassment_Educational_Intervention <- table(Educational_Intervention, Embarrasment_Six_Months)[c(2,1), c(5,3,4,1,2)]
Embarrassment_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Embarrasment_Six_Months)[c(2,1), c(5,3,4,1,2)]
Embarrassment_DSPD_Services <- table(DSPD_Services, Embarrasment_Six_Months)[c(2,1), c(5,3,4,1,2)]
Embarrassment_DSPD_Waitlist <- table(DSPD_Waitlist, Embarrasment_Six_Months)[c(2,1), c(5,3,4,1,2)]

chisq.test(Embarrassment)
chisq.test(Embarrassment_Families)
chisq.test(Embarrassment_Age)
chisq.test(Embarrassment_Family_Member_Age)
chisq.test(Embarrassment_ABA)
chisq.test(Embarrassment_OT)
chisq.test(Embarrassment_Speech)
chisq.test(Embarrassment_Medical_Management)
chisq.test(Embarrassment_Educational_Intervention)
chisq.test(Embarrassment_Other_Mental_Health)
chisq.test(Embarrassment_DSPD_Services)
chisq.test(Embarrassment_DSPD_Waitlist)

###Relation To Family Last Six Months
Relation <- table(Relation_To_Family_Six_Months)[c(4,2,3,1)]
Relation_Families <- table(Multiple_Family_Members, Relation_To_Family_Six_Months)[c(2,1),c(4,2,3,1)]
Relation_Age <- table(Age, Relation_To_Family_Six_Months)[,c(4,2,3,1)]
Relation_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Relation_To_Family_Six_Months)[c(3,1,2),c(4,2,3,1)]
Relation_ABA <- table(ABA_Therapy, Relation_To_Family_Six_Months)[c(2,1), c(4,2,3,1)]
Relation_OT <- table(Occupational_Therapy, Relation_To_Family_Six_Months)[c(2,1), c(4,2,3,1)]
Relation_Speech <- table(Speech_Therapy, Relation_To_Family_Six_Months)[c(2,1), c(4,2,3,1)]
Relation_Medical_Management <- table(Medical_Management, Relation_To_Family_Six_Months)[c(2,1), c(4,2,3,1)]
Relation_Educational_Intervention <- table(Educational_Intervention, Relation_To_Family_Six_Months)[c(2,1), c(4,2,3,1)]
Relation_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Relation_To_Family_Six_Months)[c(2,1), c(4,2,3,1)]
Relation_DSPD_Services <- table(DSPD_Services, Relation_To_Family_Six_Months)[c(2,1), c(4,2,3,1)]
Relation_DSPD_Waitlist <- table(DSPD_Waitlist, Relation_To_Family_Six_Months)[c(2,1), c(4,2,3,1)]

chisq.test(Relation)
chisq.test(Relation_Families)
chisq.test(Relation_Age)
chisq.test(Relation_Family_Member_Age)
chisq.test(Relation_ABA)
chisq.test(Relation_OT)
chisq.test(Relation_Speech)
chisq.test(Relation_Medical_Management)
chisq.test(Relation_Educational_Intervention)
chisq.test(Relation_Other_Mental_Health)
chisq.test(Relation_DSPD_Services)
chisq.test(Relation_DSPD_Waitlist)

###Anger Last Six Months
Anger <- table(Anger_Six_Months)[c(5,3,4,1,2)]
Anger_Families <- table(Multiple_Family_Members, Anger_Six_Months)[c(2,1),c(5,3,4,1,2)]
Anger_Age <- table(Age, Anger_Six_Months)[,c(5,3,4,1,2)]
Anger_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Anger_Six_Months)[c(3,1,2), c(3,4,1,2)]
Anger_ABA <- table(ABA_Therapy, Anger_Six_Months)[c(2,1), c(5,3,4,1,2)]
Anger_OT <- table(Occupational_Therapy, Anger_Six_Months)[c(2,1), c(5,3,4,1,2)]
Anger_Speech <- table(Speech_Therapy, Anger_Six_Months)[c(2,1), c(5,3,4,1,2)]
Anger_Medical_Management <- table(Medical_Management, Anger_Six_Months)[c(2,1), c(5,3,4,1,2)]
Anger_Educational_Intervention <- table(Educational_Intervention, Anger_Six_Months)[c(2,1), c(5,3,4,1,2)]
Anger_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Anger_Six_Months)[c(2,1), c(5,3,4,1,2)]
Anger_DSPD_Services <- table(DSPD_Services, Anger_Six_Months)[c(2,1), c(5,3,4,1,2)]
Anger_DSPD_Waitlist <- table(DSPD_Waitlist, Anger_Six_Months)[c(2,1), c(5,3,4,1,2)]

chisq.test(Anger)
chisq.test(Anger_Families)
chisq.test(Anger_Age)
chisq.test(Anger_Family_Member_Age)
chisq.test(Anger_ABA)
chisq.test(Anger_OT)
chisq.test(Anger_Speech)
chisq.test(Anger_Medical_Management)
chisq.test(Anger_Educational_Intervention)
chisq.test(Anger_Other_Mental_Health)
chisq.test(Anger_DSPD_Services)
chisq.test(Anger_DSPD_Waitlist)

###Future Worry Last Six Months
Future_Worry <- table(Family_Member_Future_Worry_Six_Months)[c(5,3,4,1,2)]
Future_Worry_Families <- table(Multiple_Family_Members, Family_Member_Future_Worry_Six_Months)[c(2,1),c(5,3,4,1,2)]
Future_Worry_Age <- table(Age, Family_Member_Future_Worry_Six_Months)[,c(5,3,4,1,2)]
Future_Worry_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Family_Member_Future_Worry_Six_Months)[c(3,1,2), c(4,2,3,1)]
Future_Worry_ABA <- table(ABA_Therapy, Family_Member_Future_Worry_Six_Months)[c(2,1), c(5,3,4,1,2)]
Future_Worry_OT <- table(Occupational_Therapy, Family_Member_Future_Worry_Six_Months)[c(2,1), c(5,3,4,1,2)]
Future_Worry_Speech <- table(Speech_Therapy, Family_Member_Future_Worry_Six_Months)[c(2,1), c(5,3,4,1,2)]
Future_Worry_Medical_Management <- table(Medical_Management, Family_Member_Future_Worry_Six_Months)[c(2,1), c(5,3,4,1,2)]
Future_Worry_Educational_Intervention <- table(Educational_Intervention, Family_Member_Future_Worry_Six_Months)[c(2,1), c(5,3,4,1,2)]
Future_Worry_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Family_Member_Future_Worry_Six_Months)[c(2,1), c(5,3,4,1,2)]
Future_Worry_DSPD_Services <- table(DSPD_Services, Family_Member_Future_Worry_Six_Months)[c(2,1), c(5,3,4,1,2)]
Future_Worry_DSPD_Waitlist <- table(DSPD_Waitlist, Family_Member_Future_Worry_Six_Months)[c(2,1), c(5,3,4,1,2)]

chisq.test(Future_Worry)
chisq.test(Future_Worry_Families)
chisq.test(Future_Worry_Age)
chisq.test(Future_Worry_Family_Member_Age)
chisq.test(Future_Worry_ABA)
chisq.test(Future_Worry_OT)
chisq.test(Future_Worry_Speech)
chisq.test(Future_Worry_Medical_Management)
chisq.test(Future_Worry_Educational_Intervention)
chisq.test(Future_Worry_Other_Mental_Health)
chisq.test(Future_Worry_DSPD_Services)
chisq.test(Future_Worry_DSPD_Waitlist)

###Guilt Last Six Months
Guilt <- table(Guilt_Six_Months)[c(5,3,4,1,2)]
Guilt_Families <- table(Multiple_Family_Members, Guilt_Six_Months)[c(2,1),c(5,3,4,1,2)]
Guilt_Age <- table(Age, Guilt_Six_Months)[,c(5,3,4,1,2)]
Guilt_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Guilt_Six_Months)[c(3,1,2), c(5,3,4,1,2)]
Guilt_ABA <- table(ABA_Therapy, Guilt_Six_Months)[c(2,1), c(5,3,4,1,2)]
Guilt_OT <- table(Occupational_Therapy, Guilt_Six_Months)[c(2,1), c(5,3,4,1,2)]
Guilt_Speech <- table(Speech_Therapy, Guilt_Six_Months)[c(2,1), c(5,3,4,1,2)]
Guilt_Medical_Management <- table(Medical_Management, Guilt_Six_Months)[c(2,1), c(5,3,4,1,2)]
Guilt_Educational_Intervention <- table(Educational_Intervention, Guilt_Six_Months)[c(2,1), c(5,3,4,1,2)]
Guilt_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Guilt_Six_Months)[c(2,1), c(5,3,4,1,2)]
Guilt_DSPD_Services <- table(DSPD_Services, Guilt_Six_Months)[c(2,1), c(5,3,4,1,2)]
Guilt_DSPD_Waitlist <- table(DSPD_Waitlist, Guilt_Six_Months)[c(2,1), c(5,3,4,1,2)]

chisq.test(Guilt)
chisq.test(Guilt_Families)
chisq.test(Guilt_Age)
chisq.test(Guilt_Family_Member_Age)
chisq.test(Guilt_ABA)
chisq.test(Guilt_OT)
chisq.test(Guilt_Speech)
chisq.test(Guilt_Medical_Management)
chisq.test(Guilt_Educational_Intervention)
chisq.test(Guilt_Other_Mental_Health)
chisq.test(Guilt_DSPD_Services)
chisq.test(Guilt_DSPD_Waitlist)

###Resentment Last Six Months
Resent<- table(Resent_Six_Months)[c(5,3,4,1,2)]
Resent_Families <- table(Multiple_Family_Members, Resent_Six_Months)[c(2,1),c(5,3,4,1,2)]
Resent_Age <- table(Age, Resent_Six_Months)[,c(5,3,4,1,2)]
Resent_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Resent_Six_Months)[c(3,1,2), c(5,3,4,1,2)]
Resent_ABA <- table(ABA_Therapy, Resent_Six_Months)[c(2,1), c(5,3,4,1,2)]
Resent_OT <- table(Occupational_Therapy, Resent_Six_Months)[c(2,1), c(5,3,4,1,2)]
Resent_Speech <- table(Speech_Therapy, Resent_Six_Months)[c(2,1), c(5,3,4,1,2)]
Resent_Medical_Management <- table(Medical_Management, Resent_Six_Months)[c(2,1), c(5,3,4,1,2)]
Resent_Educational_Intervention <- table(Educational_Intervention, Resent_Six_Months)[c(2,1), c(5,3,4,1,2)]
Resent_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Resent_Six_Months)[c(2,1), c(5,3,4,1,2)]
Resent_DSPD_Services <- table(DSPD_Services, Resent_Six_Months)[c(2,1), c(5,3,4,1,2)]
Resent_DSPD_Waitlist <- table(DSPD_Waitlist, Resent_Six_Months)[c(2,1), c(5,3,4,1,2)]

chisq.test(Resent)
chisq.test(Resent_Families)
chisq.test(Resent_Age)
chisq.test(Resent_Family_Member_Age)
chisq.test(Resent_ABA)
chisq.test(Resent_OT)
chisq.test(Resent_Speech)
chisq.test(Resent_Medical_Management)
chisq.test(Resent_Educational_Intervention)
chisq.test(Resent_Other_Mental_Health)
chisq.test(Resent_DSPD_Services)
chisq.test(Resent_DSPD_Waitlist)

###Tired Last Six Months
Tired <- table(Tired_Six_Months)[c(5,3,4,1,2)]
Tired_Families <- table(Multiple_Family_Members, Tired_Six_Months)[c(2,1),c(5,3,4,1,2)]
Tired_Age <- table(Age, Tired_Six_Months)[,c(5,3,4,1,2)]
Tired_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Tired_Six_Months)[c(3,1,2), c(5,3,4,1,2)]
Tired_ABA <- table(ABA_Therapy, Tired_Six_Months)[c(2,1), c(5,3,4,1,2)]
Tired_OT <- table(Occupational_Therapy, Tired_Six_Months)[c(2,1), c(5,3,4,1,2)]
Tired_Speech <- table(Speech_Therapy, Tired_Six_Months)[c(2,1), c(5,3,4,1,2)]
Tired_Medical_Management <- table(Medical_Management, Tired_Six_Months)[c(2,1), c(5,3,4,1,2)]
Tired_Educational_Intervention <- table(Educational_Intervention, Tired_Six_Months)[c(2,1), c(5,3,4,1,2)]
Tired_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Tired_Six_Months)[c(2,1), c(5,3,4,1,2)]
Tired_DSPD_Services <- table(DSPD_Services, Tired_Six_Months)[c(2,1), c(5,3,4,1,2)]
Tired_DSPD_Waitlist <- table(DSPD_Waitlist, Tired_Six_Months)[c(2,1), c(5,3,4,1,2)]

chisq.test(Tired)
chisq.test(Tired_Families)
chisq.test(Tired_Age)
chisq.test(Tired_Family_Member_Age)
chisq.test(Tired_ABA)
chisq.test(Tired_OT)
chisq.test(Tired_Speech)
chisq.test(Tired_Medical_Management)
chisq.test(Tired_Educational_Intervention)
chisq.test(Tired_Other_Mental_Health)
chisq.test(Tired_DSPD_Services)
chisq.test(Tired_DSPD_Waitlist)

###Toll Last Six Months
Toll <- table(Toll_Six_Months)[c(5,3,4,1,2)]
Toll_Families <- table(Multiple_Family_Members, Toll_Six_Months)[c(2,1),c(5,3,4,1,2)]
Toll_Age <- table(Age, Toll_Six_Months)[,c(5,3,4,1,2)]
Toll_Family_Member_Age <- table(autism_Single_Family_Member$Family_Member_Age, autism_Single_Family_Member$Toll_Six_Months)[c(3,1,2), c(5,3,4,1,2)]
Toll_ABA <- table(ABA_Therapy, Toll_Six_Months)[c(2,1), c(5,3,4,1,2)]
Toll_OT <- table(Occupational_Therapy, Toll_Six_Months)[c(2,1), c(5,3,4,1,2)]
Toll_Speech <- table(Speech_Therapy, Toll_Six_Months)[c(2,1), c(5,3,4,1,2)]
Toll_Medical_Management <- table(Medical_Management, Toll_Six_Months)[c(2,1), c(5,3,4,1,2)]
Toll_Educational_Intervention <- table(Educational_Intervention, Toll_Six_Months)[c(2,1), c(5,3,4,1,2)]
Toll_Other_Mental_Health <- table(Other_Mental_Health_Therapy, Toll_Six_Months)[c(2,1), c(5,3,4,1,2)]
Toll_DSPD_Services <- table(DSPD_Services, Toll_Six_Months)[c(2,1), c(5,3,4,1,2)]
Toll_DSPD_Waitlist <- table(DSPD_Waitlist, Toll_Six_Months)[c(2,1), c(5,3,4,1,2)]

chisq.test(Toll)
chisq.test(Toll_Families)
chisq.test(Toll_Age)
chisq.test(Toll_Family_Member_Age)
chisq.test(Toll_ABA)
chisq.test(Toll_OT)
chisq.test(Toll_Speech)
chisq.test(Toll_Medical_Management)
chisq.test(Toll_Educational_Intervention)
chisq.test(Toll_Other_Mental_Health)
chisq.test(Toll_DSPD_Services)
chisq.test(Toll_DSPD_Waitlist)


