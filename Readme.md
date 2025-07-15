# Digitization of a butterfly collection

**Project by Felix Weber and Johannes Balkenhol**

Digitizing of a collection in form of an excel sheet and filtering and displaying  data with R and a R Shiny interface.
With 13 different filter options in the sidebar.
A family tree, data table and geographical map generated after filter selection.
A filterable graph plot of the color brightness of the butterflies in relation to the altitude of their location.

**Collector:** Arthur Bott
**Digitization:** Dr. Mirko Wölfling and Felix Weber
**Program:** Felix Weber, Johannes Balkenhol
**Concept & Project Lead** Johannes Balkenhol
**Programming languages used:** R (Shiny), HTML, Java Script, CSS.

#### Creating a FAIR-Compliant Butterfly Database: a Digital Archive Enabling Global Accessibility, Scientific Research, and Scalability

##### Abstract:

In pursuit of enhancing research data management (RDM) and adhering to the FAIR principles (Findable, Accessible, Interoperable, Reusable), we have meticulously constructed a comprehensive Butterfly Database. This database serves as a digital repository for a rich collection of butterfly specimens, thoughtfully digitized through high-quality photographs, and enriched with crucial metadata. Our project leveraged the power of R and R-Shiny to provide global access to this invaluable resource, enabling researchers and enthusiasts worldwide to seamlessly interact with the data.

The Butterfly Database encompasses the following key components:

**1\. Digital Archiving:** We painstakingly photographed and digitized an extensive archive of butterflies, ensuring high-resolution images of one individual of each species of the Heterocera family for accurate identification and study.

**2\. Metadata Enrichment:** To facilitate discoverability and traceability, we meticulously curated metadata, including suborder, family, genus, species, German name, the location and it's elevation, date of discovery, total number of a species, FHH directive, red list of Bavaria and the name of the collector.  
For further visualization a family tree and geographical map of the locations, generated after the filter selections were included. Also an added feature is the correlation of the color brightness to the elevation of the location of a species, shown in a Plot. These metadata elements adhere to the FAIR principle of being easily Findable and Accessible.

**3\. Online Accessibility:**
Leveraging R and Shiny, we transformed our local archive into an online platform, making it accessible to anyone with an internet connection. This accessibility aligns with the FAIR principle of being Interoperable.

**4\. Global Reach:**
By hosting the database on the internet, we have extended access to researchers, students, and butterfly enthusiasts worldwide, fostering international collaboration and research opportunities.

**5\. Integrated Data:** To enhance the value of the database, we integrated additional environmental data, such as elevation and precise geographical coordinates of the discovery locations, further promoting the Reusability aspect of FAIR data.

**6\. Scalable Software Infrastructure:**
Importantly, our software infrastructure is designed to be easily transferable for other archives containing images and metadata, thus ensuring that our project serves as a blueprint for other initiatives wishing to digitize and share similar collections.

In summary, our Butterfly Database project stands as a testament to the principles of good scientific practice, as it exemplifies meticulous data curation, accessibility, and integration in line with the FAIR principles and RDM guidelines advocated by the National Research Data Infrastructure (NFDI). Through this endeavor, we aim to not only preserve and share a rich cultural and scientific heritage but also encourage future research, exploration, and conservation efforts within the field of lepidopterology while providing a scalable solution for others with similar archiving needs.

##### Execution:

To run the program local on your own device, please install R and R studio, then open the R file "Butterflies" in the folder R and click on the green arrow "Run App" in the upper right corner. At the first start it could take some time while the needed packages are installed, then the overlay for the butterflies collection should open.

##### Contents:

The R folder contains the program's R script, as well as Shiny's usual www folder. The www folder contains a folder for the pictures of the butterflies, a folder for the icons of the geographical map and the Java script for searching with the Enter key.
The Data folder contains the Excel of the butterfly database, the Excel of the color brightness values in relation to the altitude and a short form of the used Red List of Bavaria.

If you encounter any Bugs/Errors, or want to give Feedback, please contact me at felix1997weber2@gmail.com or johannes.balkenhol@uni-wuerzburg.de.

##### Credits:

Dr. Johannes Balkenhol, Dr. Mirko Wölfling, Rana Salihoglu, Prof. Dr. Thomas Dandekar

Copyright © 2023, Weber Felix & Johannes Balkenhol. Released under the MIT License.