# Review criterialess 
# Instructions:
#         
#         To practice the plotting techniques you have learned so far, you will be making a graphic that explores relationships between variables. This practice is useful since we will later cover creating reproducible graphics in this class. You will be looking at a subset of a United States medical expenditures dataset with information on costs for different medical conditions and in different areas of the country.
# 
# You should do the following:
#         
#         Make a plot that answers the question: what is the relationship between mean covered charges (Average.Covered.Charges) and mean total payments (Average.Total.Payments) in New York?
# Make a plot (possibly multi-panel) that answers the question: how does the relationship between mean covered charges (Average.Covered.Charges) and mean total payments (Average.Total.Payments) vary by medical condition (DRG.Definition) and the state in which care was received (Provider.State)?
# Use only the ggplot2 graphics system (not base R or lattice) to make your figure.
# 
# Please submit (1) R code that creates your plots, (2) a single pdf for plot 1 and (3) a single pdf for plot 2. You will be graded on whether you answered the questions and a number of features describing the clarity of the plots including axis labels, figure legends, figure captions, and plots. For guidelines on how to create production quality plots see Chapter 10 of the Elements of Data Analytic Style (https://www.dropbox.com/s/rybd14gq60jzira/edas_chapter10.pdf?dl=0)
# 
# To make the plots use the data in the attached .csv file. These data are a processed version of the data from the site: https://data.cms.gov/Medicare/Inpatient-Prospective-Payment-System-IPPS-Provider/97k6-zzx3
# 
# payments.csv

library('ProjectTemplate')
load.project()

chargepaymentsMedNy <- payments %>%
        select(Average.Covered.Charges, Average.Total.Payments, DRG.Definition, Provider.State) %>%
        arrange(Provider.State)

# chargeMed <- payments %>% 
#         select(average = Average.Covered.Charges, DRG.Definition, Provider.State) %>% 
#         arrange(Provider.State) %>%
#         mutate(type = "charge")
# 
# paymentMed <- payments %>% 
#         select(average = Average.Total.Payments, DRG.Definition, Provider.State) %>% 
#         arrange(Provider.State) %>%
#         mutate(type = "payment")
# 
# chargepaymentMed <- rbind(chargeMed, paymentMed)


# g <- ggplot(chargepaymentMed, aes(x = DRG.Definition, y = average)) + 
#         geom_boxplot(aes(color=factor(DRG.Definition)), outlier.colour = NA) + 
#         facet_grid(Provider.State ~ type) + 
#         scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
#         ##scale_y_continuous(breaks = trans_breaks("sqrt", function(x) x ^ 2), labels = trans_format("sqrt", math_format(.x ^2))) +
#         ##scale_y_continuous(breaks= trans_breaks(seq(0,40000, by = 1000))) +
#         ##scale_x_discrete(labels=c("Covered Charges","Total Payment")) +
#         labs(y="Mean Value", title="Mean Covered Charges versus Mean Total Payments", x = "") +
#         annotation_logticks(sides = "rl") +
#         theme(panel.grid.minor = element_blank()) +
#         ##guides(title.hjust=0.5) +
#         theme(plot.margin=unit(c(1,1,1,1),"mm")) + 
#         fte_theme()


##theme(plot.margin=unit(c(0,1,0,0),"mm"))
sts <- boxplot.stats(chargepaymentMed$average)$stats  # Compute lower and upper whisker limits
p1 <- g + coord_cartesian(ylim = c(sts[2]/2,max(sts)*1.05))

g <- ggplot(chargepaymentsMedNy, aes(x = log10(Average.Covered.Charges) , y = log10(Average.Total.Payments), colour = DRG.Definition))
g + geom_point(alpha = 0.15) +
        ##facet_grid(Provider.State ~.) +
        ##scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
        ##scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
        ##coord_cartesian(xlim = c(2500, 150000), ylim = c(2500, 30000)) +
        geom_smooth(method = "lm", se = FALSE) +
        facet_wrap(~Provider.State, ncol = 2) +
        labs(y = "Mean Total Payments") +
        labs(x = "Mean Covered Charges") +
        annotation_logticks(sides = "rl") +
        fte_theme() +
        theme(legend.position="right") +
        

      ## ALTERNATIVE 3, BAR CHART  
g <- ggplot(chargepaymentMed, aes(x = Provider.State , y = log10(average), fill = type))
g + geom_bar(stat = "identity", position = "stack") + 
        facet_wrap(~DRG.Definition) + 
        scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
        coord_cartesian(ylim = c(0, 10^5))
        
        coord_trans(y = "log10")
        
        

        
        
        
        
        g + geom_point(color = rgb(1, 0.3, 0.3, 1)) +
        facet_grid(. ~ type) +
        geom_smooth(method = "lm", se = FALSE, col = "steelblue") +
        theme_bw(base_family = "Avenir", base_size = 10) +
        labs(y = "Total Emissions") + 
        labs(x = "Year") 

        



























