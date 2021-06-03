# Setup
install.packages("pacman")
# Load other R pkgs
pacman::p_load(data.table, ggplot2, psych, readxl)
#--------------------------------------------------
# Load questionnaire data
data_morning <- readxl::read_xlsx("path",
                                  sheet=1,
                                  skip=2) # sheet 1 and skip 2 rows
data_afternoon <- readxl::read_xlsx("path",
                                    sheet=1,
                                    skip=2)
# Stack them with rbind()
data <- rbind(data_morning, data_afternoon)
#--------------------------------------------------
# df to data table
setDT(data)

# check variable names
names(data)

# rename variables
names(data) <- c("date", "session", "voter", "purchase_intention",
                 "wtp","gender","age","music_expense","living_costs",
                 "is_mandarin","good_assignment")
# summary of data
summary(data)

# data type conversion
data[,`:=`(wtp=as.numeric( gsub(pattern="£", replacement="", 
                                trimws(wtp), fixed=T)),
           music_expense=as.numeric( gsub(pattern="£", replacement = "",
                                          trimws(music_expense),fixed=T)),
           living_costs=as.numeric( gsub(pattern="£", replacement="",
                                         trimws(living_costs),fixed=T)),
           gender = ifelse(trimws(gender)=="I'm a handsome gentleman",1,0),
           is_mandarin = ifelse(trimws(is_mandarin)=="Yes",1,0),
           good_assignment=ifelse(trimws(good_assignment)=="Yes! I want more challenging cases in the future classes!",1,0),
           age=as.numeric( gsub(pattern=c(","),replacement="",trimws(age),
                                fixed=T)))]

# check summary of data 
summary(data)
#--------------------------------------------------
# plot relationship between wtp and living costs
ggplot(data = data, aes(y = wtp, x = living_costs)) + geom_point()
# the plot is strange because we have outliers of wtp, which is 100000000. Delete these values.
data <- data[wtp<100] #100 is a reasonable number
# plot again, seems much better now. You can choose the threshold you think is reasonable.
ggplot(data = data, aes(y = wtp, x = living_costs)) + geom_point()

# create a bar chart/box plot to check whether wtp differ across genders
ggplot(data = data, aes(y = wtp,x = factor(gender) )) + geom_boxplot()
psych::describeBy(data, data$gender)

# use group t-test
t.test(wtp~gender, data = data)
# use regression; the regression results also show that the p-value of gender is 0.554 and therefore there is indeed no gender difference
regression_result <- lm(data = data, formula =  wtp~gender)
summary(regression_result)
