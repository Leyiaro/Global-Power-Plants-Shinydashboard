#Import data of global power plants

global_power_plants <- read.csv("global_power_plant_database.csv")

# Plot energy mix of countries based on estimated gwh generated in 2017

mix_plot_fn <- function(cntry){
  
  global_power_plants %>% filter(!is.na(estimated_generation_gwh_2017), country_long == cntry ) %>% group_by(primary_fuel) %>% summarise(gwh = sum(estimated_generation_gwh_2017)) %>% mutate(percent_gwh = round(gwh*100/sum(gwh),2)) %>% arrange(desc(percent_gwh)) %>% ggplot(aes(as_factor(primary_fuel), percent_gwh, fill = primary_fuel))+
    geom_col(show.legend = FALSE)+
    geom_text(aes(label = paste0(percent_gwh,"%")), nudge_y = 2, colour = "black")+
    theme_bw()+
    labs(
      x = "",
      y = "Percentage %",
      title = paste0("Energy mix of ",cntry)
      
    )+
    theme(
      panel.grid.major.y = element_line(colour = "grey50",linetype = "dotted"),
      panel.grid.minor.y = element_line(colour = "grey50", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_rect(fill= NA, color = "white"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.title = element_blank(),
      axis.line.x = element_line(colour = "black"),
      axis.text.x = element_text(face = "bold")
      
    )
  }


# Import data to provide region and sub-regions of countries
continent_data <- read.csv("continents.csv")

continents <- continent_data %>% select(name, alpha.3, region, sub.region) %>% rename(country_long = name, country = alpha.3)


# Only have countries with estimated power generated in 2017 in the continents object

global_countries <- global_power_plants %>% filter(!is.na(estimated_generation_gwh_2017)) %>% select(country) %>% unique(.) %>% pull(country)


logic_1 <- continents$country  %in% global_countries


continents <- continents %>% filter(logic_1)

# Join the data sets
y <- continents %>% select( - country_long)
x <- global_power_plants %>% filter(!is.na(estimated_generation_gwh_2017)) %>% select(country,primary_fuel, country_long,capacity_mw, estimated_generation_gwh_2017)

global_power_gwh <- inner_join(x, y, by = "country")


#function global mix per region based on estimated gwh in 2017

region_mix_plot <- function(rgion){
  if(rgion == "Whole World"){
    global_power_gwh%>% group_by(primary_fuel) %>% summarise(gwh = sum(estimated_generation_gwh_2017)) %>% mutate(percentage = round(gwh*100/sum(gwh),2)) %>% arrange((percentage)) %>%  ggplot(aes(as_factor(primary_fuel), percentage, fill = primary_fuel))+
      geom_col(show.legend = FALSE)+
      coord_flip()+
      geom_text(aes(label = paste0(percentage,"%")), nudge_y = 2, colour = "black")+
      theme_bw()+
      labs(
        x = "",
        y = "",
        title = paste0("Energy mix of the World")
        
      )+
      theme(
        panel.grid.major.y = element_line(colour = "grey50",linetype = "dotted"),
        panel.grid.minor.y = element_line(colour = "grey50", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(fill= NA, color = "white"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank()
       
      )
  }
  
  else{  
    global_power_gwh%>% filter(region == rgion) %>% group_by(primary_fuel) %>% summarise(gwh = sum(estimated_generation_gwh_2017)) %>% mutate(percentage = round(gwh*100/sum(gwh),2)) %>% arrange((percentage)) %>%  ggplot(aes(as_factor(primary_fuel), percentage, fill = primary_fuel))+
      geom_col(show.legend = FALSE)+
      coord_flip()+
      geom_text(aes(label = paste0(percentage,"%")), nudge_y = 2, colour = "black")+
      theme_bw()+
      labs(
        x = "",
        y = "",
        title = paste0("Energy mix of ", rgion)
        
      )+
      theme(
        panel.grid.major.y = element_line(colour = "grey50",linetype = "dotted"),
        panel.grid.minor.y = element_line(colour = "grey50", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(fill= NA, color = "white"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_blank()
        
      )
    
  }
}

# Function to generate plot of power generated in sub-regions of the World
subregion_plot <- function(subrgn){
  global_power_gwh %>% filter(sub.region == subrgn) %>% group_by(primary_fuel) %>% summarise(gwh = sum(estimated_generation_gwh_2017)) %>% mutate(percentage = round(gwh*100/sum(gwh),2)) %>% arrange((percentage)) %>%  ggplot(aes(as_factor(primary_fuel), percentage, fill = primary_fuel))+
    geom_col(show.legend = FALSE)+
    coord_flip()+
    geom_text(aes(label = paste0(percentage,"%")), nudge_y = 2, colour = "black")+
    theme_bw()+
    labs(
      x = "",
      y = "",
      title = paste0("Energy mix of ",subrgn)
      
    )+
    theme(
      panel.grid.major.y = element_line(colour = "grey50",linetype = "dotted"),
      panel.grid.minor.y = element_line(colour = "grey50", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_rect(fill= NA, color = "white"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.title = element_blank()
      
    )
}

# Import population data for Countries
population <- read.csv("Population.csv")

population <- population %>% select(Country.Name, Country.Code, X2017)

pop1 <- global_power_gwh %>% select(country, country_long) %>% unique()

pop2 <- population %>% select(Country.Code, X2017) %>% rename(country = Country.Code)

population <- left_join(pop1, pop2, by = "country" )


# Function that returns population for a country
pop_fn <- function(cntry){
  
  n <- population %>% filter(country_long == cntry) %>%pull(X2017)
  
  return(prettyNum(n, big.mark = ","))
  
}

# Function that returns installed power capacity (MW) for a given country
cap_fn <- function(cntry){
  
  n <- global_power_gwh %>% filter(country_long == cntry) %>% summarise(sum(capacity_mw)) %>% pull()
  
  return(paste0(prettyNum(n, big.mark = ","), " MW"))
  
}

# Function that returns total generated total estimated gwh for a given country
gwh_fn <- function(cntry){
  
  n <- global_power_gwh%>% filter(country_long == cntry) %>% summarise(sum(estimated_generation_gwh_2017)) %>% pull()
  
  return(paste0(prettyNum(n, big.mark = ",")))
  
}

# Function that returns the percentage of power estimated to be generated from renewable(green) sources
green_fn <- function(cntry){
  
  n <- global_power_gwh %>% filter(country_long == cntry, primary_fuel %in% c("Hydro", "Solar", "Geothermal", "Waste", "Wind")) %>% summarise(sum(estimated_generation_gwh_2017)) %>% pull()
  
  t <- global_power_gwh%>% filter(country_long == cntry) %>% summarise(sum(estimated_generation_gwh_2017)) %>% pull()
  
  return(paste0(round(n*100/t,2)," %"))
  
}

# Function to generate a comment on Percentage of power plants with data on estimated gwh generated in 2017
est_fn <- function(cntry){
  
  n <- global_power_gwh%>% filter(country_long == cntry) %>% summarise(sum(capacity_mw)) %>% pull()
  
  t <- global_power_plants %>% filter(country_long == cntry) %>% summarise(sum(capacity_mw)) %>% pull()
  
  return(paste0("Generated power estimate available for ", round(n*100/t,2)," % of installed capacity"))
  
}


# Join power plants data with continents data for all countries 

global_countries_2 <- global_power_plants %>% select(country) %>% unique(.) %>% pull(country)

continents_2 <- continent_data %>% select(name, alpha.3, region, sub.region) %>% rename(country_long = name, country = alpha.3)

# Add Kosovo
Kosovo <- c("Kosovo", "KOS", "Europe", "Southern Europe")

continents_2 <- rbind(continents_2, Kosovo) %>% arrange(country_long)


# Join the data sets
y2 <- continents_2 %>% select( - country_long)
x2 <- global_power_plants %>% select(country, country_long, primary_fuel, capacity_mw)

global_plants_mw<- inner_join(x2, y2, by = "country")


# Function to generate table for regions
rgn_tbl_fn <- function(rgn){
  if(rgn == "Whole World"){
    
    rgn_tbl <- global_plants_mw %>% group_by(country_long) %>% summarise(mw = round(sum(capacity_mw))) %>% arrange(desc(mw)) %>% mutate(Percentage = round(mw*100/sum(mw), 2), cum_per = cumsum(Percentage))
    
    names(rgn_tbl) <- c("Country", "Installed capacity (MW)", "% Global capacity", "Cummulative %")
    
    rgn_tbl
    
  }else {
    rgn_tbl <- global_plants_mw  %>% filter(region == rgn) %>% group_by(country_long) %>% summarise(mw = round(sum(capacity_mw))) %>% arrange(desc(mw)) %>% mutate(Percentage = round(mw*100/sum(mw), 2), cum_per = cumsum(Percentage))
    
    names(rgn_tbl) <- c("Country", "Installed capacity (MW)", paste0("% in ", rgn), "Cummulative %")
    
    rgn_tbl
    
  }
  
}

# Function to generate table for subregions
subrgn_tbl_fn <- function(subrgn){
  
  subrgn_tbl <- global_plants_mw  %>% filter(sub.region == subrgn) %>% group_by(country_long)%>% summarise(mw = round(sum(capacity_mw))) %>% arrange(desc(mw)) %>% mutate(Percentage = round(mw*100/sum(mw), 1), cum_per = cumsum(Percentage))
  
  names(subrgn_tbl) <- c("Country", "Installed capacity (MW)", paste0("% in ", subrgn), "Cummulative %")
  
  subrgn_tbl
}

# A function that returns interactive plot of energy sources

pri_fn <- function(pri_f, filtr){
  
  pri_1 <- global_power_plants %>% filter(primary_fuel == pri_f) %>% filter(!(country_long %in% filtr)) %>% group_by(country_long) %>% summarise(MW = round(sum(capacity_mw)))
  
  pri_c <- pri_1 %>% pull(country_long)
  
  pri_2 <- global_power_plants %>% group_by(country_long) %>% summarise(total_mw = round(sum(capacity_mw))) %>% filter(country_long %in% pri_c)
  
  pri_fuel <- pri_1 %>% mutate(total_mw = pri_2$total_mw) %>% mutate(Percentage = round(MW*100/total_mw)) %>% rename(Country = country_long) %>% arrange(desc(Percentage))
  
  ptly <- pri_fuel %>% ggplot(aes(x = Percentage, y= MW, label = Country))+
    geom_jitter(color = "orange", size = 0.7, width = 1)+
    labs(
      x = "Percentage of total installed capacity", 
      y = "Installed capacity (MW)",
      title = paste0("Electricity generation from ", pri_f)
    )+
    
    theme_bw()+
    theme(
      panel.grid.major.y = element_line(colour = "grey50",linetype = "dotted"),
      #panel.grid.minor.y = element_line(colour = "grey50", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_rect(fill= NA, color = "white"),
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(face = "bold"),
      #legend.title = element_blank(),
      axis.line.x = element_line(colour = "black"),
      axis.text.x = element_text(face = "bold")
      
    )
  
  ggplotly(ptly, tooltip = c("Country", "Percentage", "MW")) %>% config(displayModeBar = FALSE)
  
}

# A function to generate table ranking countries based on energy sources
rank_fn <- function(pri_f){
  rank_tbl <- global_power_plants %>% filter(primary_fuel == pri_f) %>% group_by(country_long) %>% summarise(mw = round(sum(capacity_mw))) %>% arrange(desc(mw)) %>% mutate(Percentage = round(mw*100/sum(mw), 1), cum_per = cumsum(Percentage))
  
  names(rank_tbl) <- c("Country", "Installed capacity (MW)", "% Global capacity", "Cummulative %")
  
  rank_tbl
}

# A function to plot growth in installed capacity for a given country

growth_mw <- function(cntry){
  
  global_mw_yr <- global_power_plants %>% filter(!is.na(commissioning_year)) %>% select(country_long, commissioning_year, capacity_mw, primary_fuel)
  
  
  country1 <- global_mw_yr %>% filter(country_long == cntry ) %>% select(-country_long) %>% group_by(commissioning_year, primary_fuel) %>% summarise(capacity_mw = sum(capacity_mw))
  
  country2 <- country1 %>% pivot_wider( names_from = primary_fuel, values_from = capacity_mw )
  
  country2[is.na(country2)] <- 0
  
  csum_country <- ungroup(country2) %>% select(-commissioning_year) 
  
  csum_country1 <- as_tibble(apply(csum_country, 2, cumsum))%>% mutate(commissioning_year = country2$commissioning_year) %>% select(commissioning_year, everything())
  
  
  clns <- names(csum_country1[2:ncol(csum_country1)])
  pivot <- csum_country1 %>% pivot_longer(all_of(clns), names_to = "primary_fuel", values_to = "capacity_mw" ) %>% arrange(desc(capacity_mw))
  
  v1 <- global_power_plants %>% filter(!is.na(commissioning_year), country_long == cntry) %>% summarise(mw = sum(capacity_mw)) %>% pull(mw)
  
  v2 <- global_power_plants %>% filter(country_long == cntry) %>% summarise(mw = sum(capacity_mw)) %>% pull(mw)
  
  percentage_capacity <- paste0("*",round(v1*100/v2,2), " % of installed capacity plotted")
  
  ggplot(pivot, aes(commissioning_year, capacity_mw, colour = as_factor(primary_fuel)))+
    geom_line()+
    theme_bw()+
    labs(
      title =paste0("Growth in installed power capacity of ", cntry),
      subtitle = percentage_capacity,
      x = "Year",
      y = "Installed capacity (MW)"
    )+
    theme(
      panel.grid.major.y = element_line(colour = "grey50",linetype = "dotted"),
      panel.grid.minor.y = element_line(colour = "grey50", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_rect(fill= NA, color = "white"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.title = element_blank(),
      axis.line.x = element_line(colour = "black"),
      axis.text.x = element_text(face = "bold")
      
    )
}
