ggplot(z_przedzialami, aes(x = age, color = award)) +
  geom_density()+
  labs(title = "Wiek zdobywców oscara",
       x = "Wiek",
       y = "Liczba zwyciêzców") +
  scale_x_continuous(name = "Wiek", 
                     labels = c(0,10,20,30,40,50,60,70,80,90),
                     breaks = c(0,10,20,30,40,50,60,70,80,90)) +
  scale_y_continuous(name = "Liczba zwyciêzców", 
                     labels = c(0,10,20,30,40,50,60,70,80),
                     breaks = c(0,10,20,30,40,50,60,70,80))
