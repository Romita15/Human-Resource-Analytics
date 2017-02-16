#group by promotion
groupedByPromotion <- group_by(df.HR, left, promotion = promotion_last_5years)
df.promotion <- summarise(groupedByPromotion, count = length(promotion_last_5years) )


# the graph shows that promotion was not an important factor for an employee leaving the firm
ggplot(data = df.promotion, aes( x= promotion, y = count, fill = left)) + 
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(aes(label = count), position = position_dodge(width = 0.8), vjust = -0.3)


#box plot to show how salary and satsifaction level are related
ggplot(data =df.HR, aes( x= salary , y = satisfaction_level))+
  geom_boxplot(aes(fill = left), outlier.color = "green")
#Employees who stay at the firm have a higher level of satisfaction compared to those who left.
# Also, the satifaction level is greater that 0.5 for the ones staying
# For the employees who left- (salary low and medium), employees show high dispersion for the satisfaction level
#         salary is high has a very small dispersion . Also with many outliers


# the relationship between the employees who left and satisfaction level varies with department
ggplot(data = df.HR, aes( x= salary, y = satisfaction_level)) + 
  geom_boxplot(aes(fill=left))+
  facet_wrap(~sales, nrow = 3)



