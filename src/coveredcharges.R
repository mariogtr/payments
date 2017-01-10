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


chargepaymentsNy <- payments %>% 
        select(Average.Covered.Charges, Average.Total.Payments, Provider.State) %>%
        filter(Provider.State == "NY") %>%
        distinct()
        


chargeNy <- payments %>% 
        select(Average.Covered.Charges, Provider.State) %>%
        filter(Provider.State == "NY") %>%
        distinct() %>%
        select(Average.Covered.Charges)
        
        
        
        
chargeNy$type <- "charge"
names(chargeNy) <- c("average", "type")


payNy <- payments %>% 
        select(Average.Total.Payments, Provider.State) %>%
        filter(Provider.State == "NY") %>%
        distinct() %>%
        select(Average.Total.Payments)


payNy$type <- "payment"
names(payNy) <- c("average", "type")
chargepaymentsNy2 <- rbind(chargeNy, payNy)

g <- ggplot(chargepaymentsNy2, aes(x = type, y = log10(average))) + 
        geom_boxplot(aes(color=factor(type)), outlier.colour = NA) +
        ##scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
        ##scale_y_continuous(breaks = trans_breaks("sqrt", function(x) x ^ 2), labels = trans_format("sqrt", math_format(.x ^2))) +
        ##scale_y_continuous(breaks = seq(0,40000, by = 1000)) +
        scale_x_discrete(labels=c("Covered Charges","Total Payment")) +
        labs(y="Mean Value", title="Mean Covered Charges versus Mean Total Payments", x = "") +
        annotation_logticks(sides = "rl") +
        theme(panel.grid.minor = element_blank()) +
        guides(title.hjust=0.5) +
        fte_theme()
        ##theme(plot.margin=unit(c(0,1,0,0),"mm"))
        sts <- boxplot.stats(chargepaymentsNy2$average)$stats  # Compute lower and upper whisker limits
        p1 = g + coord_cartesian(ylim = c(sts[2]/2,max(sts)*1.05))



        g <- ggplot(chargepaymentsNy, aes(x = log10(Average.Covered.Charges) , y = log10(Average.Total.Payments)))
        g + geom_point(alpha = 0.15, color="#c0392b") +
                ##scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
                ##scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
                ##scale_x_continuous(breaks=seq(0,10000, by=500)) +
                ##scale_y_continuous(breaks= seq(0,30000, by = 2000)) +
                coord_cartesian(ylim = c(3.5, 4.5)) +
                geom_hline(yintercept=1, size=0.4, color="black") +
                geom_smooth(alpha=0.25, color="black", fill="black") +
                labs(x="Mean Covered Charges / 10", y="Mean Total Payments", title="Mean Covered Charges versus Mean Total Payments") +
                fte_theme()
        
        sts <- geom_point     .stats(chargepaymentsNy$average)$stats  # Compute lower and upper whisker limits
        
        





