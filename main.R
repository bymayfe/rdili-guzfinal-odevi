# Gerekli kütüphaneleri yükleyin
# install.packages("tidyverse")  # Veri manipülasyonu ve görselleştirme için
# install.packages("psych")       # Normallik testi için
# install.packages("car")         # Regresyon analizi için
# tidyverse paketini yükle ve ekle
if (!requireNamespace("tidyverse", quietly = TRUE)) {
    install.packages("tidyverse")
}
library(tidyverse)

# psych paketini yükle ve ekle
if (!requireNamespace("psych", quietly = TRUE)) {
    install.packages("psych")
}
library(psych)

# car paketini yükle ve ekle
if (!requireNamespace("car", quietly = TRUE)) {
    install.packages("car")
}
library(car)

# FSA paketini yükle ve ekle
if (!requireNamespace("FSA", quietly = TRUE)) {
    install.packages("FSA")
}
library(FSA)

# Dunn testi için gerekli kütüphane
if (!requireNamespace("dunn.test", quietly = TRUE)) {
    install.packages("dunn.test")
}
library(dunn.test)

if (!requireNamespace("vioplot", quietly = TRUE)) {
    install.packages("vioplot")
}
library(vioplot)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
}
library(ggplot2)

# Gerekli kütüphane
if (!requireNamespace("beeswarm", quietly = TRUE)) {
    install.packages("beeswarm")
}
library(beeswarm)

# plotrix paketini yükle ve ekle
if (!requireNamespace("plotrix", quietly = TRUE)) {
    install.packages("plotrix")
}
library(plotrix)

# lmtest paketini yükle ve ekle
if (!requireNamespace("lmtest", quietly = TRUE)) {
    install.packages("lmtest")
}
library(lmtest)

setwd("C:\Users\seyfo\Desktop\R_Final_Odevi")

# Verilerin yüklenmesi
depression <- read.csv("Depression.csv")
anxiety <- read.csv("Anxiety.csv")
stress <- read.csv("Stress.csv")

# Değişken isimlerini güncelleyin (eğer gerekiyorsa)
colnames(depression) <- c(
    "Age", "Gender", "University", "Department", "Academic_Year",
    "Current_CGPA", "Received_Waiver_Scholarship",
    "Interest_in_Activities", "Feeling_Down_Hopeless",
    "Sleep_Issues", "Low_Energy", "Appetite_Changes",
    "Self_Esteem_Issues", "Concentration_Problems",
    "Restlessness_or_Slowness", "Thoughts_of_Self_Harm",
    "Depression_Value", "Depression_Label"
)

colnames(anxiety) <- c(
    "Age", "Gender", "University", "Department", "Academic_Year",
    "Current_CGPA", "Received_Waiver_Scholarship",
    "Nervous_Anxious_OnEdge", "Unable_to_Stop_Worrying",
    "Trouble_Relaxing", "Easily_Irritated", "Worried_Too_Much",
    "Restlessness", "Feeling_Afraid", "Anxiety_Value", "Anxiety_Label"
)

colnames(stress) <- c(
    "Age", "Gender", "University", "Department", "Academic_Year",
    "Current_CGPA", "Received_Waiver_Scholarship",
    "Upset_Due_to_Academic_Affairs", "Unable_to_Control_Important_Things",
    "Nervous_Stressed_Academic_Pressure", "Unable_to_Cope_with_Academic_Activities",
    "Confident_Handling_Academic_Problems", "Things_Going_Your_Way",
    "Able_to_Control_Irritations", "Academic_Performance_on_Top",
    "Angered_by_Bad_Performance", "Academic_Difficulties_Piling_Up",
    "Stress_Value", "Stress_Label"
)

# "Prefer not to say" olan cinsiyetleri çıkarın
depression_filtered <- depression[depression$Gender != "Prefer not to say", ]
anxiety_filtered <- anxiety[anxiety$Gender != "Prefer not to say", ]
stress_filtered <- stress[stress$Gender != "Prefer not to say", ]

# Yeni veri çerçevesinin yapısını kontrol edin
str(depression_filtered)
str(anxiety_filtered)
str(stress_filtered)

#  "X_Value" değişkenini sayısal bir değişkene dönüştürün
depression_filtered$Depression_Value <- as.numeric(depression_filtered$Depression_Value)
anxiety_filtered$Anxiety_Value <- as.numeric(anxiety_filtered$Anxiety_Value)
stress_filtered$Stress_Value <- as.numeric(stress_filtered$Stress_Value)

# Veri setindeki eksik değerleri kontrol edin
depression_missing_values <- sum(is.na(depression_filtered))
anxiety_missing_values <- sum(is.na(anxiety_filtered))
stress_missing_values <- sum(is.na(stress_filtered))

cat(
    "Depression missing values: ", depression_missing_values, "\n",
    "Anxiety missing values: ", anxiety_missing_values, "\n",
    "Stress missing values: ", stress_missing_values, "\n"
)

# Görselleştirme

# PNG olarak kaydetme
png("Depression_Anxiety_Stress_HistogramsAndBoxlots.png", width = 800, height = 500)


# Görselleştirme için canvas alanı hazırlama 2*3
par(mfrow = c(2, 3))

hist(depression_filtered$Depression_Value, main = "Depression Histogram", xlab = "Depression Value", col = "blue")
hist(anxiety_filtered$Anxiety_Value, main = "Anxiety Histogram", xlab = "Anxiety Value", col = "purple")
hist(stress_filtered$Stress_Value, main = "Stress Histogram", xlab = "Stress Value", col = "#48ff00")

# Görselleştirme 2

boxplot(depression_filtered$Depression_Value ~ depression_filtered$Gender, main = "Dep", xlab = "Depression Value", col = "blue")
boxplot(anxiety_filtered$Anxiety_Value ~ anxiety_filtered$Gender, main = "Anx", xlab = "Anxiety Value", col = "purple")
boxplot(stress_filtered$Stress_Value ~ stress_filtered$Gender, main = "Str", xlab = "Stress Value", col = "#48ff00")

# Görselleri kaydetme

dev.off()


pdf("Depression_Anxiety_Stress_HistogramsAndBoxlots.pdf", width = 12, height = 8) # Çıktıyı PDF olarak kaydet
par(mfrow = c(2, 3))

# Depresyon
hist(depression_filtered$Depression_Value, main = "Depression Histogram", xlab = "Depression Value", col = "blue")

# Anksiyete
hist(anxiety_filtered$Anxiety_Value, main = "Anxiety Histogram", xlab = "Anxiety Value", col = "purple")

# Stres
hist(stress_filtered$Stress_Value, main = "Stress Histogram", xlab = "Stress Value", col = "#48ff00")

# Görselleştirme 2
boxplot(depression_filtered$Depression_Value ~ depression_filtered$Gender, main = "Dep", xlab = "Depression Value", col = "blue")
boxplot(anxiety_filtered$Anxiety_Value ~ anxiety_filtered$Gender, main = "Anx", xlab = "Anxiety Value", col = "purple")
boxplot(stress_filtered$Stress_Value ~ stress_filtered$Gender, main = "Str", xlab = "Stress Value", col = "#48ff00")

# Çıkışı kapatma
dev.off()

# normallik testi
depression_shapiro_result <- shapiro.test(depression_filtered$Depression_Value)
anxiety_shapiro_result <- shapiro.test(anxiety_filtered$Anxiety_Value)
stress_shapiro_result <- shapiro.test(stress_filtered$Stress_Value)

cat(
    "Depression p-value: ", round(depression_shapiro_result$p.value, 3), "\n",
    "Anxiety p-value: ", round(anxiety_shapiro_result$p.value, 3), "\n",
    "Stress p-value: ", round(stress_shapiro_result$p.value, 3), "\n"
)


# Log dönüşümünü uygulayın
depression_filtered$log_Depression_Value <- log(depression_filtered$Depression_Value + 1)
anxiety_filtered$log_Anxiety_Value <- log(anxiety_filtered$Anxiety_Value + 1)
stress_filtered$log_Stress_Value <- log(stress_filtered$Stress_Value + 1)

# Log dönüşümü sonrası normallik testi
depression_shapiro_log_result <- shapiro.test(depression_filtered$log_Depression_Value)
anxiety_shapiro_log_result <- shapiro.test(anxiety_filtered$log_Anxiety_Value)
stress_shapiro_log_result <- shapiro.test(stress_filtered$log_Stress_Value)

cat(
    "Depression log p-value: ", round(depression_shapiro_log_result$p.value, 3), "\n",
    "Anxiety log p-value: ", round(anxiety_shapiro_log_result$p.value, 3), "\n",
    "Stress log p-value: ", round(stress_shapiro_log_result$p.value, 3), "\n"
)


# Karekök dönüşümünü uygulayın
depression_filtered$sqrt_Depression_Value <- sqrt(depression_filtered$Depression_Value)
anxiety_filtered$sqrt_Anxiety_Value <- sqrt(anxiety_filtered$Anxiety_Value)
stress_filtered$sqrt_Stress_Value <- sqrt(stress_filtered$Stress_Value)

# Karekök dönüşümü sonrası normallik testi
depression_shapiro_sqrt_result <- shapiro.test(depression_filtered$sqrt_Depression_Value)
anxiety_shapiro_sqrt_result <- shapiro.test(anxiety_filtered$sqrt_Anxiety_Value)
stress_shapiro_sqrt_result <- shapiro.test(stress_filtered$sqrt_Stress_Value)

cat(
    "Depression sqrt p-value: ", round(depression_shapiro_sqrt_result$p.value, 3), "\n",
    "Anxiety sqrt p-value: ", round(anxiety_shapiro_sqrt_result$p.value, 3), "\n",
    "Stress sqrt p-value: ", round(stress_shapiro_sqrt_result$p.value, 3), "\n"
)

# p-value < 0.05 olduğu için veri normal dağılmamaktadır.


# Gorsellestirme
# PNG olarak kaydetme
png("Depression_Anxiety_Stress_LOG_SQRT_HistogramsAndBoxlots2.png", width = 800, height = 500)

# Görselleştirme için canvas alanı hazırlama 2*6
par(mfrow = c(4, 3))

hist(depression_filtered$log_Depression_Value, main = "Log Depression Histogram", xlab = "Log Depression Value", col = "blue")
hist(anxiety_filtered$log_Anxiety_Value, main = "Log Anxiety Histogram", xlab = "Log Anxiety Value", col = "purple")
hist(stress_filtered$log_Stress_Value, main = "Log Stress Histogram", xlab = "Log Stress Value", col = "#48ff00")

boxplot(depression_filtered$log_Depression_Value ~ depression_filtered$Gender, main = "Log Dep", xlab = "Log Depression Value", col = "blue")
boxplot(anxiety_filtered$log_Anxiety_Value ~ anxiety_filtered$Gender, main = "Log Anx", xlab = "Log Anxiety Value", col = "purple")
boxplot(stress_filtered$log_Stress_Value ~ stress_filtered$Gender, main = "Log Str", xlab = "Log Stress Value", col = "#48ff00")

hist(depression_filtered$sqrt_Depression_Value, main = "Sqrt Depression Histogram", xlab = "Sqrt Depression Value", col = "blue")
hist(anxiety_filtered$sqrt_Anxiety_Value, main = "Sqrt Anxiety Histogram", xlab = "Sqrt Anxiety Value", col = "purple")
hist(stress_filtered$sqrt_Stress_Value, main = "Sqrt Stress Histogram", xlab = "Sqrt Stress Value", col = "#48ff00")

boxplot(depression_filtered$sqrt_Depression_Value ~ depression_filtered$Gender, main = "Sqrt Dep", xlab = "Sqrt Depression Value", col = "blue")
boxplot(anxiety_filtered$sqrt_Anxiety_Value ~ anxiety_filtered$Gender, main = "Sqrt Anx", xlab = "Sqrt Anxiety Value", col = "purple")
boxplot(stress_filtered$sqrt_Stress_Value ~ stress_filtered$Gender, main = "Sqrt Str", xlab = "Sqrt Stress Value", col = "#48ff00")

# Görselleri kaydetme
dev.off()


# NORMAL ÇIKMADIĞI İÇİN VARYANS ANALİZİ YAPILAMAZ

# Medyan Karşılaştırma Testleri Testi
depression_wilcoxon_result <- wilcox.test(Depression_Value ~ Gender, data = depression_filtered)
depression_wilcoxon_result2 <- wilcox.test(Depression_Value ~ Received_Waiver_Scholarship, data = depression_filtered)

anxiety_wilcoxon_result <- wilcox.test(Anxiety_Value ~ Gender, data = anxiety_filtered)
anxiety_wilcoxon_result2 <- wilcox.test(Anxiety_Value ~ Received_Waiver_Scholarship, data = anxiety_filtered)

stress_wilcoxon_result <- wilcox.test(Stress_Value ~ Gender, data = stress_filtered)
stress_wilcoxon_result2 <- wilcox.test(Stress_Value ~ Received_Waiver_Scholarship, data = stress_filtered)

cat(
    "Depression Wilcoxon p-value: ", round(depression_wilcoxon_result$p.value, 3), "\n",
    "Depression Wilcoxon p-value2: ", round(depression_wilcoxon_result2$p.value, 3), "\n",
    "Anxiety Wilcoxon p-value: ", round(anxiety_wilcoxon_result$p.value, 3), "\n",
    "Anxiety Wilcoxon p-value2: ", round(anxiety_wilcoxon_result2$p.value, 3), "\n",
    "Stress Wilcoxon p-value: ", round(stress_wilcoxon_result$p.value, 3), "\n",
    "Stress Wilcoxon p-value2: ", round(stress_wilcoxon_result2$p.value, 3), "\n"
)


# p-value < 0.05 olduğu için cinsiyet değişkeni depresyon, anksiyete ve stres değerleri üzerinde anlamlı bir etkiye sahiptir.
# p-value2 > 0.05 olduğu için ABD'de üniversite öğrencileri arasında burs almanın depresyon, anksiyete ve stres üzerinde anlamlı bir etkisi yoktur.

# Gorsellestirme
# burda burda burda
# PNG olarak kaydetme
png("Depression_Anxiety_Stress_MannWhitney.png", width = 800, height = 500)

# Görselleştirme için canvas alanı hazırlama 2*3
par(mfrow = c(2, 3))

# Depresyon için
vioplot(depression$Depression_Value ~ depression$Gender,
    data = depression_filtered,
    main = "Depresyon - Cinsiyete Göre",
    xlab = "Cinsiyet", ylab = "Depresyon Değeri"
)

# Anksiyete için
vioplot(anxiety$Anxiety_Value ~ anxiety$Gender,
    data = anxiety_filtered,
    main = "Anksiyete - Cinsiyete Göre",
    xlab = "Cinsiyet", ylab = "Anksiyete Değeri"
)



# Stres için
vioplot(stress$Stress_Value ~ stress$Gender,
    data = stress_filtered,
    main = "Stres - Cinsiyete Göre",
    xlab = "Cinsiyet", ylab = "Stres Değeri"
)



vioplot(depression$Depression_Value ~ depression$Received_Waiver_Scholarship,
    data = depression_filtered,
    main = "Depresyon - Burs Durumuna Göre",
    xlab = "Burs Durumu", ylab = "Depresyon Değeri"
)
vioplot(anxiety$Anxiety_Value ~ anxiety$Received_Waiver_Scholarship,
    data = anxiety_filtered,
    main = "Anksiyete - Burs Durumuna Göre",
    xlab = "Burs Durumu", ylab = "Anksiyete Değeri"
)

vioplot(stress$Stress_Value ~ stress$Received_Waiver_Scholarship,
    data = stress_filtered,
    main = "Stres - Burs Durumuna Göre",
    xlab = "Burs Durumu", ylab = "Stres Değeri"
)


dev.off()

png("Depression_Anxiety_Stress_v4.png", width = 800, height = 500)

# Grafik alanını ayarlayın
par(mfrow = c(2, 3))

# Dep # Depresyon için
stripchart(Depression_Value ~ Gender,
    data = depression_filtered,
    main = "Depresyon - Cinsiyete Göre",
    xlab = "Cinsiyet", ylab = "Depresyon Değeri",
    method = "jitter", pch = 19, col = "blue"
)

stripchart(Anxiety_Value ~ Gender,
    data = anxiety_filtered,
    main = "Anksiyete - Cinsiyete Göre",
    xlab = "Cinsiyet", ylab = "Anksiyete Değeri",
    method = "jitter", pch = 19, col = "red"
)

stripchart(Stress_Value ~ Gender,
    data = stress_filtered,
    main = "Stres - Cinsiyete Göre",
    xlab = "Cinsiyet", ylab = "Stres Değeri",
    method = "jitter", pch = 19, col = "green"
)

stripchart(Depression_Value ~ Received_Waiver_Scholarship,
    data = depression_filtered,
    main = "Depresyon - Burs Durumuna Göre",
    xlab = "Burs Durumu", ylab = "Depresyon Değeri",
    method = "jitter", pch = 19, col = "blue"
)


stripchart(Anxiety_Value ~ Received_Waiver_Scholarship,
    data = anxiety_filtered,
    main = "Anksiyete - Burs Durumuna Göre",
    xlab = "Burs Durumu", ylab = "Anksiyete Değeri",
    method = "jitter", pch = 19, col = "red"
)


stripchart(Stress_Value ~ Received_Waiver_Scholarship,
    data = stress_filtered,
    main = "Stres - Burs Durumuna Göre",
    xlab = "Burs Durumu", ylab = "Stres Değeri",
    method = "jitter", pch = 19, col = "green"
)


dev.off()


png("Depression_Anxiety_Stress_v5.png", width = 800, height = 500)

# Grafik alanını ayarla
par(mfrow = c(2, 3))

# Depression - Gender
beeswarm(Depression_Value ~ Gender,
    data = depression_filtered,
    main = "Depression by Gender", xlab = "Gender", ylab = "Depression Score",
    col = c("lightblue", "lightpink"), pch = 16
)

# Depression - Waiver/Scholarship
beeswarm(Depression_Value ~ Received_Waiver_Scholarship,
    data = depression_filtered,
    main = "Depression by Waiver Status", xlab = "Waiver/Scholarship", ylab = "Depression Score",
    col = c("lightgreen", "lightyellow"), pch = 16
)

# Anxiety - Gender
beeswarm(Anxiety_Value ~ Gender,
    data = anxiety_filtered,
    main = "Anxiety by Gender", xlab = "Gender", ylab = "Anxiety Score",
    col = c("lightblue", "lightpink"), pch = 16
)

# Anxiety - Waiver/Scholarship
beeswarm(Anxiety_Value ~ Received_Waiver_Scholarship,
    data = anxiety_filtered,
    main = "Anxiety by Waiver Status", xlab = "Waiver/Scholarship", ylab = "Anxiety Score",
    col = c("lightgreen", "lightyellow"), pch = 16
)

# Stress - Gender
beeswarm(Stress_Value ~ Gender,
    data = stress_filtered,
    main = "Stress by Gender", xlab = "Gender", ylab = "Stress Score",
    col = c("lightblue", "lightpink"), pch = 16
)

# Stress - Waiver/Scholarship
beeswarm(Stress_Value ~ Received_Waiver_Scholarship,
    data = stress_filtered,
    main = "Stress by Waiver Status", xlab = "Waiver/Scholarship", ylab = "Stress Score",
    col = c("lightgreen", "lightyellow"), pch = 16
)

dev.off()



# ------------------------------ KURUSKAL-WALLIS TESTİ ------------------------------
# Kategorik değişkenler için Kruskal-Wallis testi

depression_categorical_vars <- c(
    "University", "Department", "Academic_Year", "Interest_in_Activities", "Feeling_Down_Hopeless",
    "Sleep_Issues", "Low_Energy", "Appetite_Changes",
    "Self_Esteem_Issues", "Concentration_Problems",
    "Restlessness_or_Slowness", "Thoughts_of_Self_Harm"
)

anxiety_categorical_vars <- c(
    "University", "Department", "Academic_Year", "Nervous_Anxious_OnEdge", "Unable_to_Stop_Worrying",
    "Trouble_Relaxing", "Easily_Irritated", "Worried_Too_Much",
    "Restlessness", "Feeling_Afraid"
)

stress_categorical_vars <- c(
    "University", "Department", "Academic_Year", "Upset_Due_to_Academic_Affairs", "Unable_to_Control_Important_Things",
    "Nervous_Stressed_Academic_Pressure", "Unable_to_Cope_with_Academic_Activities",
    "Confident_Handling_Academic_Problems", "Things_Going_Your_Way",
    "Able_to_Control_Irritations", "Academic_Performance_on_Top",
    "Angered_by_Bad_Performance", "Academic_Difficulties_Piling_Up"
)

# Sonuçları saklamak için bir liste oluşturun
depression_results <- list()
anxiety_results <- list()
stress_results <- list()

# Her bir kategorik değişken için Kruskal-Wallis testini uygulayın
for (var in depression_categorical_vars) {
    test_result <- kruskal.test(as.formula(paste("Depression_Value ~", var)), data = depression_filtered)
    depression_results[[var]] <- test_result
}

for (var in anxiety_categorical_vars) {
    test_result <- kruskal.test(as.formula(paste("Anxiety_Value ~", var)), data = anxiety_filtered)
    anxiety_results[[var]] <- test_result
}

for (var in stress_categorical_vars) {
    test_result <- kruskal.test(as.formula(paste("Stress_Value ~", var)), data = stress_filtered)
    stress_results[[var]] <- test_result
}

# Sonuçları yazdırın
cat("\nDepression Kruskal-Wallis Testi Sonuçları:\n")
for (var in depression_categorical_vars) {
    p_value <- depression_results[[var]]$p.value
    cat(paste("Değişken:", var, "-> p-değeri:", round(p_value, 5), "\n"))

    # Anlamlılık durumu
    if (p_value < 0.05) {
        cat("Anlamlı bir etki vardır.\n\n")
    } else {
        cat("Anlamlı bir etki yoktur.\n\n")
    }
}

cat("\nAnxiety Kruskal-Wallis Testi Sonuçları:\n")
for (var in anxiety_categorical_vars) {
    p_value <- anxiety_results[[var]]$p.value
    cat(paste("Değişken:", var, "-> p-değeri:", round(p_value, 5), "\n"))

    # Anlamlılık durumu
    if (p_value < 0.05) {
        cat("Anlamlı bir etki vardır.\n\n")
    } else {
        cat("Anlamlı bir etki yoktur.\n\n")
    }
}

cat("\nStress Kruskal-Wallis Testi Sonuçları:\n")
for (var in stress_categorical_vars) {
    p_value <- stress_results[[var]]$p.value
    cat(paste("Değişken:", var, "-> p-değeri:", round(p_value, 5), "\n"))

    # Anlamlılık durumu
    if (p_value < 0.05) {
        cat("Anlamlı bir etki vardır.\n\n")
    } else {
        cat("Anlamlı bir etki yoktur.\n\n")
    }
}

# ------------------------------ DUNN TESTİ ------------------------------
# Dunn testi sonuçları

# Dunn testi sonuçlarını saklamak için bir liste oluşturun

depression_dunn_results <- lapply(depression_categorical_vars, function(var) {
    # Kruskal-Wallis testinin anlamlı olduğu durumlarda Dunn testini uygula
    if (depression_results[[var]]$p.value < 0.05) {
        dunn_result <- dunn.test(depression_filtered$Depression_Value,
            depression_filtered[[var]],
            method = "bonferroni",
            wrap = TRUE
        )
        return(dunn_result)
    } else {
        return(NULL) # Anlamlı değilse NULL döndür
    }
})

anxiety_dunn_results <- lapply(anxiety_categorical_vars, function(var) {
    # Kruskal-Wallis testinin anlamlı olduğu durumlarda Dunn testini uygula
    if (anxiety_results[[var]]$p.value < 0.05) {
        anxiety_dunn_result <- dunn.test(anxiety_filtered$Anxiety_Value,
            anxiety_filtered[[var]],
            method = "bonferroni",
            wrap = TRUE
        )
        return(anxiety_dunn_result)
    } else {
        return(NULL) # Anlamlı değilse NULL döndür
    }
})

stress_dunn_results <- lapply(stress_categorical_vars, function(var) {
    # Kruskal-Wallis testinin anlamlı olduğu durumlarda Dunn testini uygula
    if (stress_results[[var]]$p.value < 0.05) {
        stress_dunn_result <- dunn.test(stress_filtered$Stress_Value,
            stress_filtered[[var]],
            method = "bonferroni",
            wrap = TRUE
        )
        return(stress_dunn_result)
    } else {
        return(NULL) # Anlamlı değilse NULL döndür
    }
})

# Sonuçları isimlendirin
names(depression_dunn_results) <- depression_categorical_vars
names(anxiety_dunn_results) <- anxiety_categorical_vars
names(stress_dunn_results) <- stress_categorical_vars

# Sonuçları yazdırın
for (var in depression_categorical_vars) {
    if (!is.null(depression_dunn_results[[var]])) {
        cat("\nDepression Dunn Testi sonuçları için", var, "değişkeni:\n")
        print(depression_dunn_results[[var]])

        # Anlamlı farklılıkları göster
        significant_comparisons <- depression_dunn_results[[var]]$comparisons[depression_dunn_results[[var]]$P.adjusted < 0.05]

        if (length(significant_comparisons) > 0) {
            cat("Anlamlı farklılık gösteren gruplar için", var, "değişkeninde:\n")
            for (comparison in significant_comparisons) {
                cat(" -", comparison, "\n")
            }
        } else {
            cat("Anlamlı farklılık yok.\n")
        }
    } else {
        cat("Depression Dunn Testi için", var, "değişkeni anlamlı değildir.\n")
    }
}

for (var in anxiety_categorical_vars) {
    if (!is.null(anxiety_dunn_results[[var]])) {
        cat("\nAnxiety Dunn Testi sonuçları için", var, "değişkeni:\n")
        print(anxiety_dunn_results[[var]])

        # Anlamlı farklılıkları göster
        significant_comparisons <- anxiety_dunn_results[[var]]$comparisons[anxiety_dunn_results[[var]]$P.adjusted < 0.05]

        if (length(significant_comparisons) > 0) {
            cat("Anlamlı farklılık gösteren gruplar için", var, "değişkeninde:\n")
            for (comparison in significant_comparisons) {
                cat(" -", comparison, "\n")
            }
        } else {
            cat("Anlamlı farklılık yok.\n")
        }
    } else {
        cat("Anxiety Dunn Testi için", var, "değişkeni anlamlı değildir.\n")
    }
}

for (var in stress_categorical_vars) {
    if (!is.null(stress_dunn_results[[var]])) {
        cat("\nStress Dunn Testi sonuçları için", var, "değişkeni:\n")
        print(stress_dunn_results[[var]])

        # Anlamlı farklılıkları göster
        significant_comparisons <- stress_dunn_results[[var]]$comparisons[stress_dunn_results[[var]]$P.adjusted < 0.05]

        if (length(significant_comparisons) > 0) {
            cat("Anlamlı farklılık gösteren gruplar için", var, "değişkeninde:\n")
            for (comparison in significant_comparisons) {
                cat(" -", comparison, "\n")
            }
        } else {
            cat("Anlamlı farklılık yok.\n")
        }
    } else {
        cat("Stress Dunn Testi için", var, "değişkeni anlamlı değildir.\n")
    }
}

# ------------------------------ REGRESYON ANALİZİ ------------------------------

# Regresyon analizi

depression_dependent_var <- "Depression_Value"
depression_independent_vars <- c(
    "Age", "Current_CGPA", "Received_Waiver_Scholarship",
    "Interest_in_Activities", "Feeling_Down_Hopeless",
    "Sleep_Issues", "Low_Energy", "Appetite_Changes",
    "Self_Esteem_Issues", "Concentration_Problems",
    "Restlessness_or_Slowness", "Thoughts_of_Self_Harm"
)

anxiety_dependent_var <- "Anxiety_Value"
anxiety_independent_vars <- c(
    "Age", "Current_CGPA", "Received_Waiver_Scholarship",
    "Nervous_Anxious_OnEdge", "Unable_to_Stop_Worrying",
    "Trouble_Relaxing", "Easily_Irritated", "Worried_Too_Much",
    "Restlessness", "Feeling_Afraid"
)

stress_dependent_var <- "Stress_Value"
stress_independent_vars <- c(
    "Age", "Current_CGPA", "Received_Waiver_Scholarship",
    "Upset_Due_to_Academic_Affairs", "Unable_to_Control_Important_Things",
    "Nervous_Stressed_Academic_Pressure", "Unable_to_Cope_with_Academic_Activities",
    "Confident_Handling_Academic_Problems", "Things_Going_Your_Way",
    "Able_to_Control_Irritations", "Academic_Performance_on_Top",
    "Angered_by_Bad_Performance", "Academic_Difficulties_Piling_Up"
)

#  NON PARAMETRİK İÇİN
# Depresyon için GLM modeli
depresyon_glm <- glm(
    formula = paste(depression_dependent_var, "~", paste(depression_independent_vars, collapse = " + ")),
    family = gaussian(link = "identity"), data = depression_filtered
)
summary(depresyon_glm)

# Anksiyete için GLM modeli
anksiyete_glm <- glm(
    formula = paste(anxiety_dependent_var, "~", paste(anxiety_independent_vars, collapse = " + ")),
    family = gaussian(link = "identity"), data = anxiety_filtered
)
summary(anksiyete_glm)

# Stres için GLM modeli
stres_glm <- glm(
    formula = paste(stress_dependent_var, "~", paste(stress_independent_vars, collapse = " + ")),
    family = gaussian(link = "identity"), data = stress_filtered
)
summary(stres_glm)

# Tüm modellerin geçerlilik testleri ve analizler
# 1. Artıklıkların görsel kontrolü
par(mfrow = c(2, 2))
plot(depresyon_glm)
plot(anksiyete_glm)
plot(stres_glm)

# 2. Durbin-Watson testi ile otokorelasyon kontrolü
dwtest(depresyon_glm)
dwtest(anksiyete_glm)
dwtest(stres_glm)

# 3. Multikolinerlik testi
vif(depresyon_glm)
vif(anksiyete_glm)
vif(stres_glm)

# 4. Aykırı değerlerin tespiti
influencePlot(depresyon_glm)
influencePlot(anksiyete_glm)
influencePlot(stres_glm)

# 5. Homoskedastisite testi
bptest(depresyon_glm)
bptest(anksiyete_glm)
bptest(stres_glm)

# 6. Hataların normallik testleri
shapiro.test(residuals(depresyon_glm))
shapiro.test(residuals(anksiyete_glm))
shapiro.test(residuals(stres_glm))

# 7. Kolmogorov-Smirnov testi ile alternatif normallik testi
ks.test(residuals(depresyon_glm), "pnorm", mean(residuals(depresyon_glm)), sd(residuals(depresyon_glm)))
ks.test(residuals(anksiyete_glm), "pnorm", mean(residuals(anksiyete_glm)), sd(residuals(anksiyete_glm)))
ks.test(residuals(stres_glm), "pnorm", mean(residuals(stres_glm)), sd(residuals(stres_glm)))

# 8. Q-Q grafiği ile artıklıkların normalliğinin görsel kontrolü
qqnorm(residuals(depresyon_glm))
qqline(residuals(depresyon_glm))
qqnorm(residuals(anksiyete_glm))
qqline(residuals(anksiyete_glm))
qqnorm(residuals(stres_glm))
qqline(residuals(stres_glm))


# DOĞRUSAL REGRESYON (PARAMETRİK İÇİN)

# Depresyon regresyon modeli
depression_regression_model <- lm(formula = paste(depression_dependent_var, "~", paste(depression_independent_vars, collapse = " + ")), data = depression_filtered)
# Anksiyete regresyon modeli
anxiety_regression_model <- lm(formula = paste(anxiety_dependent_var, "~", paste(anxiety_independent_vars, collapse = " + ")), data = anxiety_filtered)
# Stres regresyon modeli
stress_regression_model <- lm(formula = paste(stress_dependent_var, "~", paste(stress_independent_vars, collapse = " + ")), data = stress_filtered)

# Regresyon modellerinin özetlerini alın
depression_model_summary <- summary(depression_regression_model)
anxiety_model_summary <- summary(anxiety_regression_model)
stress_model_summary <- summary(stress_regression_model)

depression_model_summary
anxiety_model_summary

# Depresyon modelinin F testi p-değeri
cat("Depresyon Modeli F Testi p-değeri: ", depression_model_summary$coefficients[1, 4], "\n")

# Anksiyete modelinin F testi p-değeri
cat("Anksiyete Modeli F Testi p-değeri: ", anxiety_model_summary$coefficients[1, 4], "\n")

# Stres modelinin F testi p-değeri
cat("Stres Modeli F Testi p-değeri: ", stress_model_summary$coefficients[1, 4], "\n")



# Regresyon modellerinin p değerlerini alın
depression_p_values <- coef(depression_model_summary)[, "Pr(>|t|)"]
anxiety_p_values <- coef(anxiety_model_summary)[, "Pr(>|t|)"]
stress_p_values <- coef(stress_model_summary)[, "Pr(>|t|)"]

# Depresyon regresyon modeli p değerleri
cat("\nDepresyon Regresyon Modeli P Değerleri:\n")
for (i in 2:length(depression_p_values)) {
    variable_name <- names(depression_p_values)[i]
    if (depression_p_values[i] < 0.05) {
        print(paste(variable_name, "anlamlıdır."))
    } else {
        print(paste(variable_name, "anlamlı değildir."))
    }
}

# Anksiyete regresyon modeli p değerleri
cat("\nAnksiyete Regresyon Modeli P Değerleri:\n")
for (i in 2:length(anxiety_p_values)) {
    variable_name <- names(anxiety_p_values)[i]
    if (anxiety_p_values[i] < 0.05) {
        print(paste(variable_name, "anlamlıdır."))
    } else {
        print(paste(variable_name, "anlamlı değildir."))
    }
}

# Stres regresyon modeli p değerleri
cat("\nStres Regresyon Modeli P Değerleri:\n")
for (i in 2:length(stress_p_values)) {
    variable_name <- names(stress_p_values)[i]
    if (stress_p_values[i] < 0.05) {
        print(paste(variable_name, "anlamlıdır."))
    } else {
        print(paste(variable_name, "anlamlı değildir."))
    }
}

png("Depression_Anxiety_Stress_v6.png", width = 800, height = 500)

# Grafik alanını ayarla
par(mfrow = c(2, 3))

# Depression modeli için QQ Plot
qqnorm(depression_regression_model$residuals, main = "Depression Model QQ Plot")
qqline(depression_regression_model$residuals, col = "red", lwd = 2)

# Anxiety modeli için QQ Plot
qqnorm(anxiety_regression_model$residuals, main = "Anxiety Model QQ Plot")
qqline(anxiety_regression_model$residuals, col = "red", lwd = 2)

# Stress modeli için QQ Plot
qqnorm(stress_regression_model$residuals, main = "Stress Model QQ Plot")
qqline(stress_regression_model$residuals, col = "red", lwd = 2)


# Depression modeli için Gözlenen vs. Tahmin edilen
plot(depression_filtered[[depression_dependent_var]], depression_predicted_values,
    main = "Depression: Observed vs Predicted",
    xlab = "Observed Values", ylab = "Predicted Values", pch = 16, col = "blue"
)
abline(a = 0, b = 1, col = "red", lwd = 2)

# Anxiety modeli için Gözlenen vs. Tahmin edilen
plot(anxiety_filtered[[anxiety_dependent_var]], anxiety_predicted_values,
    main = "Anxiety: Observed vs Predicted",
    xlab = "Observed Values", ylab = "Predicted Values", pch = 16, col = "green"
)
abline(a = 0, b = 1, col = "red", lwd = 2)

# Stress modeli için Gözlenen vs. Tahmin edilen
plot(stress_filtered[[stress_dependent_var]], stress_predicted_values,
    main = "Stress: Observed vs Predicted",
    xlab = "Observed Values", ylab = "Predicted Values", pch = 16, col = "purple"
)
abline(a = 0, b = 1, col = "red", lwd = 2)
dev.off()


vif(depression_regression_model)

# Regresyon modellerini kontrol edin
cat("\nDepresyon Regresyon Modeli VIF Değerleri:\n")
print(vif(depression_regression_model))

cat("\nAnksiyete Regresyon Modeli VIF Değerleri:\n")
print(vif(anxiety_regression_model))

cat("\nStres Regresyon Modeli VIF Değerleri:\n")
print(vif(stress_regression_model))

# Tahmin edilen değerleri kontrol edin
# depression_predicted_values <- round(predict(depression_regression_model, newdata = depression_filtered), 3)
# anxiety_predicted_values <- round(predict(anxiety_regression_model, newdata = anxiety_filtered), 3)
# stress_predicted_values <- round(predict(stress_regression_model, newdata = stress_filtered), 3)

depression_predicted_values <- predict(depression_regression_model, newdata = depression_filtered)
anxiety_predicted_values <- predict(anxiety_regression_model, newdata = anxiety_filtered)
stress_predicted_values <- predict(stress_regression_model, newdata = stress_filtered)

# Tahmin edilen değerleri yazdırın
cat("\nDepresyon Tahmin Edilen Değerler:\n")
print(depression_predicted_values)

cat("\nAnksiyete Tahmin Edilen Değerler:\n")
print(anxiety_predicted_values)

cat("\nStres Tahmin Edilen Değerler:\n")
print(stress_predicted_values)

# Tahmin edilen değerlerle gerçek değerleri karşılaştırın

png("Depression_Anxiety_Stress_v7.png", width = 800, height = 500)

par(mfrow = c(1, 3))

# Depresyon
plot(
    depression_filtered$Depression_Value,
    depression_predicted_values,
    xlab = "Gerçek Değerler",
    ylab = "Tahmin Edilen Değerler",
    main = "Gerçek Değerler ve Tahmin Edilen Değerler Arasındaki Karşılaştırma",
    col = "blue",
    pch = 16
)
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)

# Anksiyete
plot(
    anxiety_filtered$Anxiety_Value,
    anxiety_predicted_values,
    xlab = "Gerçek Değerler",
    ylab = "Tahmin Edilen Değerler",
    main = "Gerçek Değerler ve Tahmin Edilen Değerler Arasındaki Karşılaştırma",
    col = "blue",
    pch = 16
)
abline(a = 0, b = 1, col = "purple", lwd = 2, lty = 2)

# Stres
plot(
    stress_filtered$Stress_Value,
    stress_predicted_values,
    xlab = "Gerçek Değerler",
    ylab = "Tahmin Edilen Değerler",
    main = "Gerçek Değerler ve Tahmin Edilen Değerler Arasındaki Karşılaştırma",
    col = "blue",
    pch = 16
)
abline(a = 0, b = 1, col = "#48ff00", lwd = 2, lty = 2)
dev.off()

# Artıklar ve hatalar
depression_residuals <- residuals(depression_regression_model)
summary(depression_residuals)

anxiety_residuals <- residuals(anxiety_regression_model)
summary(anxiety_residuals)

stress_residuals <- residuals(stress_regression_model)
summary(stress_residuals)


png("Depression_Anxiety_Stress_v8.png", width = 800, height = 500)

par(mfrow = c(1, 3))

# Veri seti oluşturun (mutlak değerleri kullanarak)
labels <- c("Depresyon", "Anksiyete", "Stres")
values <- c(median(abs(depression_residuals)), median(abs(anxiety_residuals)), median(abs(stress_residuals)))
frekans <- values / sum(values) * 100
etiket <- paste(labels, "%", round(frekans, 2))

# 2D pasta grafik
pie(values, main = "Model Artıklıkları Pasta Grafiği", col = c("red", "blue", "green"), labels = etiket)

# 3D pasta grafik
pie3D(values, main = "Model Artıklıkları 3D Pasta Grafiği", col = c("red", "blue", "green"), labels = etiket, explode = 0.1)

# Fan grafik
fan.plot(values, main = "Model Artıklıkları Fan Grafiği", col = c("red", "blue", "green"), labels = etiket)

dev.off()


depression_errors <- depression_filtered$Depression_Value - depression_predicted_values
summary(depression_errors)
anxiety_errors <- anxiety_filtered$Anxiety_Value - anxiety_predicted_values
summary(anxiety_errors)
stress_errors <- stress_filtered$Stress_Value - stress_predicted_values
summary(stress_errors)

# Artıklar ve hataların dağılımını kontrol edin

depression_residuals_qq <- qqnorm(depression_residuals, main = "Depresyon Artıklarının QQ Grafiği")
qqline(depression_residuals)

anxiety_residuals_qq <- qqnorm(anxiety_residuals, main = "Anksiyete Artıklarının QQ Grafiği")
qqline(anxiety_residuals)

stress_residuals_qq <- qqnorm(stress_residuals, main = "Stres Artıklarının QQ Grafiği")
qqline(stress_residuals)

depression_errors_qq <- qqnorm(depression_errors, main = "Depresyon Hatalarının QQ Grafiği")
qqline(depression_errors)

anxiety_errors_qq <- qqnorm(anxiety_errors, main = "Anksiyete Hatalarının QQ Grafiği")
qqline(anxiety_errors)

stress_errors_qq <- qqnorm(stress_errors, main = "Stres Hatalarının QQ Grafiği")
qqline(stress_errors)

# Artıkların ve hataların normal dağılıma uygun olup olmadığını kontrol edin
depression_residuals_shapiro <- shapiro.test(depression_residuals)
anxiety_residuals_shapiro <- shapiro.test(anxiety_residuals)
stress_residuals_shapiro <- shapiro.test(stress_residuals)

depression_errors_shapiro <- shapiro.test(depression_errors)
anxiety_errors_shapiro <- shapiro.test(anxiety_errors)
stress_errors_shapiro <- shapiro.test(stress_errors)

depression_residuals_shapiro
anxiety_residuals_shapiro
stress_residuals_shapiro

depression_errors_shapiro
anxiety_errors_shapiro
stress_errors_shapiro

if (depression_residuals_shapiro$p.value > 0.05) {
    cat("Depresyon Artıklarının normal dağılıma uydugu sonucuna varılmıştır.\n")
} else {
    cat("Depresyon Artıklarının normal dağılıma uymadığı sonucuna varılmıştır.\n")
}

if (anxiety_residuals_shapiro$p.value > 0.05) {
    cat("Anksiyete Artıklarının normal dağılıma uydugu sonucuna varılmıştır.\n")
} else {
    cat("Anksiyete Artıklarının normal dağılıma uymadığı sonucuna varılmıştır.\n")
}

if (stress_residuals_shapiro$p.value > 0.05) {
    cat("Stres Artıklarının normal dağılıma uydugu sonucuna varılmıştır.\n")
} else {
    cat("Stres Artıklarının normal dağılıma uymadığı sonucuna varılmıştır.\n")
}

if (depression_errors_shapiro$p.value > 0.05) {
    cat("Depresyon Hatalarının normal dağılıma uydugu sonucuna varılmıştır.\n")
} else {
    cat("Depresyon Hatalarının normal dağılıma uymadığı sonucuna varılmıştır.\n")
}

if (anxiety_errors_shapiro$p.value > 0.05) {
    cat("Anksiyete Hatalarının normal dağılıma uydugu sonucuna varılmıştır.\n")
} else {
    cat("Anksiyete Hatalarının normal dağılıma uymadığı sonucuna varılmıştır.\n")
}

if (stress_errors_shapiro$p.value > 0.05) {
    cat("Stres Hatalarının normal dağılıma uydugu sonucuna varılmıştır.\n")
} else {
    cat("Stres Hatalarının normal dağılıma uymadığı sonucuna varılmıştır.\n")
}


# kolmogorov-smirnov testi

depression_ks_test <- ks.test(depression_residuals, "pnorm", mean = mean(depression_residuals), sd = sd(depression_residuals))
anxiety_ks_test <- ks.test(anxiety_residuals, "pnorm", mean = mean(anxiety_residuals), sd = sd(anxiety_residuals))
stress_ks_test <- ks.test(stress_residuals, "pnorm", mean = mean(stress_residuals), sd = sd(stress_residuals))

cat(
    "Depresyon Artıklarının Normal Dağılıma Uygunluğu (Kolmogorov-Smirnov Testi): ", depression_ks_test$p.value, "\n",
    "Anksiyete Artıklarının Normal Dağılıma Uygunluğu (Kolmogorov-Smirnov Testi): ", anxiety_ks_test$p.value, "\n",
    "Stres Artıklarının Normal Dağılıma Uygunluğu (Kolmogorov-Smirnov Testi): ", stress_ks_test$p.value
)

if (depression_ks_test$p.value > 0.05) {
    cat("Depresyon Artıklarının normal dağılıma uydugu sonucuna varılmıştır.\n")
} else {
    cat("Depresyon Artıklarının normal dağılıma uymadığı sonucuna varılmıştır.\n")
}

if (anxiety_ks_test$p.value > 0.05) {
    cat("Anksiyete Artıklarının normal dağılıma uydugu sonucuna varılmıştır.\n")
} else {
    cat("Anksiyete Artıklarının normal dağılıma uymadığı sonucuna varılmıştır.\n")
}

if (stress_ks_test$p.value > 0.05) {
    cat("Stres Artıklarının normal dağılıma uydugu sonucuna varılmıştır.\n")
} else {
    cat("Stres Artıklarının normal dağılıma uymadığı sonucuna varılmıştır.\n")
}


# R^2 değerlerini kontrol edin

depression_r_squared <- summary(depression_regression_model)$r.squared
anxiety_r_squared <- summary(anxiety_regression_model)$r.squared
stress_r_squared <- summary(stress_regression_model)$r.squared

cat(
    "Depresyon Regresyon Modeli R^2 Değeri: ", depression_r_squared, "\n",
    "Anksiyete Regresyon Modeli R^2 Değeri: ", anxiety_r_squared, "\n",
    "Stres Regresyon Modeli R^2 Değeri: ", stress_r_squared, "\n"
)


# ayarlanmış R^2 değerlerini kontrol edin

depression_adj_r_squared <- summary(depression_regression_model)$adj.r.squared
anxiety_adj_r_squared <- summary(anxiety_regression_model)$adj.r.squared
stress_adj_r_squared <- summary(stress_regression_model)$adj.r.squared

cat(
    "Depresyon Regresyon Modeli Ayarlanmış R^2 Değeri: ", depression_adj_r_squared, "\n",
    "Anksiyete Regresyon Modeli Ayarlanmış R^2 Değeri: ", anxiety_adj_r_squared, "\n",
    "Stres Regresyon Modeli Ayarlanmış R^2 Değeri: ", stress_adj_r_squared, "\n"
)


# Otokorelasyonu kontrol edin

# Durbin-Watson testini uygulayın
depression_dw_test <- durbinWatsonTest(depression_regression_model)
anxiety_dw_test <- durbinWatsonTest(anxiety_regression_model)
stress_dw_test <- durbinWatsonTest(stress_regression_model)

# Sonuçları yazdırın
cat("\nDepresyon Regresyon Modeli Durbin-Watson Testi Sonucu:\n")
print(depression_dw_test)

cat("\nAnksiyete Regresyon Modeli Durbin-Watson Testi Sonucu:\n")
print(anxiety_dw_test)

cat("\nStres Regresyon Modeli Durbin-Watson Testi Sonucu:\n")
print(stress_dw_test)


# Depresyon modelinin katsayılarını yazdır
cat("Depresyon Modeli Katsayıları:\n")
cat("b0 (Intercept): ", depression_model_summary$coefficients[1, 1], "\n")
for (i in 2:length(depression_independent_vars)) {
    cat(paste("b", i - 1, "(", depression_independent_vars[i - 1], "): ",
        depression_model_summary$coefficients[i, 1], "\n",
        sep = ""
    ))
}

# Anksiyete modelinin katsayılarını yazdır
cat("\nAnksiyete Modeli Katsayıları:\n")
cat("b0 (Intercept): ", anxiety_model_summary$coefficients[1, 1], "\n")
for (i in 2:length(anxiety_independent_vars)) {
    cat(paste("b", i - 1, "(", anxiety_independent_vars[i - 1], "): ",
        anxiety_model_summary$coefficients[i, 1], "\n",
        sep = ""
    ))
}

# Stres modelinin katsayılarını yazdır
cat("\nStres Modeli Katsayıları:\n")
cat("b0 (Intercept): ", stress_model_summary$coefficients[1, 1], "\n")
for (i in 2:length(stress_independent_vars)) {
    cat(paste("b", i - 1, "(", stress_independent_vars[i - 1], "): ",
        stress_model_summary$coefficients[i, 1], "\n",
        sep = ""
    ))
}
