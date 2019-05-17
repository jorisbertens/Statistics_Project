## preprocessing document

select_cities <-function(data){
  data = data[data$CITIES %in% c('Bruxelles / Brussel', 'Antwerpen', 'Sofia', 
                          'Praha', 'Berlin', 'Hamburg', 'München', 'Köln', 
                          'Frankfurt am Main', 'Essen', 'Stuttgart', 
                          'Leipzig', 'Dresden', 'Dortmund', 'Düsseldorf',
                          'Bremen', 'Hannover', 'Nürnberg', 'Duisburg',
                          'Tallinn', 'Madrid', 'Barcelona', 'Valencia', 
                          'Sevilla', 'Zaragoza', 'Málaga', 'Murcia', 
                          'Palma de Mallorca', 'Bilbao', 'Zagreb', 'Roma',
                          'Milano', 'Napoli', 'Torino', 'Palermo', 'Genova',
                          'Riga', 'Vilnius', 'Budapest', 'Lisboa', 'Bucuresti',
                          'Bratislava', 'Helsinki / Helsingfors', 'Stockholm',
                          'Göteborg', 'London (greater city)', 'Birmingham',
                          'Leeds', 'Glasgow City', 'Bradford', 'Liverpool',
                          'City of Edinburgh', 'Manchester', 'Sheffield',
                          'Bristol', 'Leicester (greater city)', 'Portsmouth (greater city)',
                          'Greater Nottingham', 'Kirklees'), ]
  return(data)
}

select_cities <-function(data){
  data = data[data$CITIES %in% c("Vienna", "Brussels", "Sofia","Prague", "Tallinn", "Helsinki", "Paris",
                                 "Lyon", "Berlin", "Bonn", "Hamburg", "Karlsruhe", "Athens", "Budapest",
                                 "Rome", "Varese", "Riga", "Vilnius", "Valetta", "The Hague", "Warsaw", "Lisbon", "Bucharest",
                                 "Bratislava", "Ljubljana", "Madrid", "Stockholm", "London", "Oxford", "Reading", "Oslo", "Bern", "Geneva", "Ankara" ), ]
  return(data)
}

"""
"Vienna", "Brussels", "Sofia", "Nicosia", "Prague", "Copenhagen", "Tallinn", "Helsinki", "Paris",
"Lyon", "Berlin", "Bonn", "Hamburg", "Karlsruhe", "M<fc>nchen", "Athens", "Budapest", "Dublin",
"Rome", "Varese", "Riga", "Vilnius", "Valetta", "The Hague", "Warsaw", "Lisbon", "Bucharest",
"Bratislava", "Ljubljana", "Madrid", "Stockholm", "London", "Oxford", "Reading", "Reykjavik", "Oslo", "Bern", "Geneva", "Ankara"  
"""

  