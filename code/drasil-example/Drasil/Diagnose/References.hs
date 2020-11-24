module Drasil.Diagnose.References where

import Language.Drasil


citations :: BibRef
citations = [william2018, perelson1582, weisstein, fischer6706, libretexts2020, hobbie1970]

william2018, perelson1582, weisstein, fischer6706, libretexts2020, hobbie1970 :: Citation

------------------------------------makeCiteInfo hibbeler2004 $ Page [8]
-- Definition of Tcell
william2018 = cMisc [author [mononym "William C. Shiel Jr. MD"],
  title "Definition of T cell", howPublishedU "https://www.medicinenet.com/script/main/art.asp?articlekey=11300",
  month Dec, year 2018]
  "william2018"
 

-- Life span

perelson1582 = cMisc [author [mononym "Perelson, Alan S. and Neumann, Avidan U. and Markowitz, Martin and Leonard, John M. and Ho, David D."],
  title "HIV-1 Dynamics in Vivo: Virion Clearance Rate, Infected Cell Life-Span, and Viral Generation Time", howPublishedU "https://science.sciencemag.org/content/271/5255/1582.full.pdf",
  month Jun, year 1996]
  "perelson1582"

weisstein = cMisc [author [mononym "Weisstein, Eric W"],
  title "Exponential Decay", howPublishedU "https://mathworld.wolfram.com/ExponentialDecay.html",
  month Jun, year 1996]
  "weisstein"

fischer6706 = cMisc [author [mononym "Fischer, Ulrike R. and Weisz, Willy and Wieltschnig, Claudia and Kirschner, Alexander K. T. and Velimirov, Branko"], title "Benthic and Pelagic Viral Decay Experiments: a Model-Based Analysis and Its Applicability", howPublishedU "https://aem.asm.org/content/70/11/6706",
  month Jun, year 2004]
  "fischer6706"
  
libretexts2020 = cMisc [author [mononym "Libretexts Contributors"],
  title "14.4: The Change of Concentration with Time (Integrated Rate Laws)", howPublishedU "https://chem.libretexts.org/Bookshelves/General_Chemistry/Map:_Chemistry_-_The_Central_Science_(Brown_et_al.)/14:_Chemical_Kinetics/14.4:_The_Change_of_Concentration_with_Time_(Integrated_Rate_Laws)",
  month Aug, year 2020]
  "libretexts2020"

hobbie1970 = cMisc [author [mononym "Hobbie, Russell K. and Roth, Bradley J."],
  title "Exponential Growth and Decay", howPublishedU "https://link.springer.com/chapter/10.1007/978-0-387-49885-0_2",
  month Jan, year 1970]
  "hobbie1970"
  

