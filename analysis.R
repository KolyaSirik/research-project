# ПЗ 4. Статистична обробка даних у R
# Тема: Трансформація інструментарію управління мікропідприємствами
#       в умовах впровадження штучного інтелекту
# Дані: dataset.csv (навчальний набір, 100 мікропідприємств)
#   score  - інтегральний бал ефективності управлінських процесів (0-100)
#   group  - control (без ШІ-інструментів) / experimental (з ШІ-інструментами)
#   age    - вік керівника, років
#   gender - стать керівника

# ---- Завдання 1. Імпорт даних та описова статистика ----

# Завантаження даних
data <- read.csv("dataset.csv")

# Структура і перший погляд на дані
str(data)
head(data)
summary(data)

# Описова статистика для score
mean(data$score)
median(data$score)
sd(data$score)
quantile(data$score, c(0.25, 0.5, 0.75))
min(data$score)
max(data$score)

# Середній бал по групах
aggregate(score ~ group, data = data, FUN = mean)

# ---- Завдання 2. Візуалізація ----
# Графіки одразу зберігаю у файли, щоб не експортувати руками

png("r_histogram.png", width = 900, height = 600, res = 120)
hist(data$score, main = "Розподіл балів", xlab = "Бали", col = "lightblue")
dev.off()

png("r_boxplot.png", width = 900, height = 600, res = 120)
boxplot(score ~ group, data = data, main = "Порівняння груп",
        xlab = "Група", ylab = "Бали", col = c("lightgreen", "lightcoral"))
dev.off()

png("r_scatter.png", width = 900, height = 600, res = 120)
plot(data$age, data$score, main = "Вік vs Бали",
     xlab = "Вік", ylab = "Бали", pch = 19, col = "blue")
dev.off()

# ---- Завдання 3. Перевірка припущень і статистичний тест ----

# Нормальність по групах (Шапіро-Уілк)
shapiro.test(data$score[data$group == "control"])
shapiro.test(data$score[data$group == "experimental"])

# Q-Q графіки для візуальної перевірки
png("r_qq.png", width = 1200, height = 600, res = 120)
par(mfrow = c(1, 2))
qqnorm(data$score[data$group == "control"], main = "Q-Q: control")
qqline(data$score[data$group == "control"])
qqnorm(data$score[data$group == "experimental"], main = "Q-Q: experimental")
qqline(data$score[data$group == "experimental"])
dev.off()

# t-тест (у R за замовчуванням Уелча, тобто з нерівними дисперсіями)
t.test(score ~ group, data = data)

# Манна-Уітні - для порівняння, якщо нормальність не підтвердиться
wilcox.test(score ~ group, data = data, exact = FALSE)
