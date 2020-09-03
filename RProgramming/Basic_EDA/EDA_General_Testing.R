fram_data = read.csv("C:\\Users\\91809\\Desktop\\Praxis\\MachineLearning\\4_Data\\framingham.csv")
cars = read.csv("C:\\Users\\91809\\Desktop\\Praxis\\MachineLearning\\4_Data\\cars.csv")

source("C:\\Users\\91809\\Desktop\\Praxis\\Sem2\\RProgramming\\ClassModules\\EDA_General.R")

# Testing
res = featureImp_cat_Target(fram_data,c('age','BMI'),c('male','currentSmoker'),'TenYearCHD')
res

# Testing
plot_data(fram_data,c('age','BMI'),c('male','currentSmoker','TenYearCHD'))


# Testing
create_unique_dir('C:\\Users\\91809\\Desktop\\Praxis\\Sem2\\RProgramming\\ClassModules', 'png_dir')

# Testing
plot_data_png_files(fram_data,c('age','BMI'),c('male','currentSmoker','TenYearCHD'),
                    'C:\\Users\\91809\\Desktop\\Praxis\\Sem2\\RProgramming\\ClassModules')

# Testing
plot_data_pdf_file(fram_data,c('age','BMI'),c('male','currentSmoker','TenYearCHD'),
                   'C:\\Users\\91809\\Desktop\\Praxis\\Sem2\\RProgramming\\ClassModules')

# Testing
get_factor_attributes(cars)


# Testing
get_not_factor_attributes(cars)


# Testing
transData = transform_data_to_factor_based_on_proportions(fram_data)
str(transData)


# Testing
univariate_plots(fram_data, tranformData=TRUE, minPorp=0.005)
univariate_plots(fram_data, 'png',tranformData=TRUE, minPorp=0.005)
univariate_plots(fram_data, 'pdf',tranformData=TRUE, minPorp=0.005)


# Testing
res = featureImp_cat_Target_gen(fram_data,'TenYearCHD', tranformData=TRUE)
res