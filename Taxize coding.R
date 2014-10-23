#classifying a species or group of species

specieslist = c('sp1','sp2','sp3', etc)

classification(specieslist, db= 'itis')

# Will be able to classify a species up in full, used to compare classifications

specieslist = c('Gadus morhua','Eutrigla gurnardus','Melanogrammus aeglefinus','Trachurus trachurus','Scomber scombrus','Pollachius virens','Merlangius merlangus','Amblyraja radiata','Trisopterus esmarkii','Trigla lucerna','Trisopterus luscus','Raja clavata','Leucoraja naevus','Squalus acanthias','Galeorhinus galeus','Anarhichas lupus','Scophthalmus rhombus','Psetta maxima','Lophius piscatorius','Trisopterus minutus','Amblyraja hyperborea','Merluccius merluccius','Molva molva','Hippoglossoides platessoides','Lepidorhombus whiffiagonis','Pollachius pollachius','Dipturus batis','Aspitrigla cuculus','Raja montagui','Hyperoplus lanceolatus','Brosme brosme','Hippoglossus hippoglossus','Mustelus mustelus','Mullus surmuletus','Ammodytes')

classification(specieslist, db = 'itis')

#Use as.chacacter feature to get full taxonomy of predator species lists:
char.lowlatelasmos = as.character(unique(lowlat.elasmos$source_taxon_name))
library(taxize)
classification(char.lowlatelasmos, db = 'itis')