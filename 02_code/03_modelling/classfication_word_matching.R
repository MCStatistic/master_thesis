# -----------------------------------------------------------------------------
# Project: Masterthesis
# Content: Wordmatching
# Contributors: Marlon Schumacher
# Last update on: 2020-03-01
# -----------------------------------------------------------------------------

# PACKAGES ----
pacman::p_load(stringr, tibble, janitor, dplyr, tidyr, lubridate, tm, ggplot2, magrittr)

# DATA ----
# combining polmine and 19. leg data
df_germa <- readRDS("01_data/02_prepared/polmine_labeld_20200302.RDS")

df_germa <- df_germa %>% 
  dplyr::select(date, agenda_item, top_content) %>% 
  dplyr::mutate(top_content = tolower(top_content)) %>% 
  dplyr::group_by(date, agenda_item) %>% 
  dplyr::summarise(top_content = dplyr::first(top_content)) %>% 
  dplyr::ungroup()

df_main <-  readRDS("01_data/02_prepared/df_main_v2_20200224_3.RDS")

df_main <- df_main %>%
  dplyr::filter(legislature == 19) %>%
  dplyr::select(date, agenda_item, top_content) %>%
  dplyr::distinct() %>%
  dplyr::mutate(top_content = stringr::str_squish(tolower(top_content)))

df <- dplyr::bind_rows(df_germa,
                       df_main) %>% 
  dplyr::filter(!is.na(top_content)) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(top_content = stringr::str_squish(top_content))

# WORDS
# string which contains all specific words 
# which are associated with the specific topic

# topic 1: macroeconomics
makroöko <- c(" ezb ", " zentralbank ", " europäische zentralbank ",
              " bundesbank "," haushaltsschulden ", "arbeitslosenquote", "geldpolitik",
              "steuer",
              " steuer ", " steuerpolitik ", " steuerreform ") %>% 
  paste0(collapse = "|")

# topic 2: Bürgerrechte
buerger <- c(" diskriminierung ", " meinungsfreiheit ", " religionsfreiheit ", 
             " partizipation ", " wahlrecht ", " gender ", " gleichberechtigung ", " gleichstellung ",
             " gleichgeschlechtliche ehen ", " gleichgeschlechtliche ehe ", " öffentlichkeitsbeteiligung ") %>% 
  paste0(collapse = "|")

# topic 3: Health Care 
gesundheit <- c(" pflege ", " pflegeversicherung ", " krankenversicherung ", " krankenkasse",
                " arzt ", " krankheit "," impfquote ", "impfung", " alkoholkonsum ", " arzneimittel ",
                " gesundheitsschutz ", " medizin ", " patienten ", " gesundheit ", " gesundheitssystem ", 
                " pflegepersonal ") %>% 
  paste0(collapse = "|")

# topic 4
agrar <- c(" agrarwirtschaft ", " landwirtschaft ", " landwirte ", " pflanzenschutzmittel "," glyphosat ",
           " milch ", " dünger ", " bse ") %>% 
  paste0(collapse = "|")

# topic 5
arbeit <- c(" gewerkschaft ", " saisonarbeit ", " arbeitsschutz ", " arbeitnehmerrechte ",
            " mindestlohn ", " mindestlöhne ", " streik ", " überstunden ",
            " arbeitszeitgesetz ", " fachkräftemangel ", " arbeitszeit ", "geringfügige beschäftigung",
            "befristete beschäftigung", " arbeitsbedingungen ", "befristeter beschäftigung",
            "leiharbeit", "arbeit auf abruf", "arbeitsunfälle", "stellenabbau", "beschäftigung",
            "niedriglöhne", " rente ", " renten ", " rentenpolitik", "rentensystem ") %>% 
  paste0(collapse = "|")
# topic 6
bildung <- c("studium", " lehrer ", "schule", "universität", "weiterbildung",
             "bafög", "hochschul", "ausbildung") %>% 
  paste0(collapse = "|")
# topic 7 
umwelt <- c(" recycling ", "atommüll", "luftverschmutzung", "plastik",
            "klimawandel", "emission", "artenschutz", "stickoxid", "wasserqualität",
            "klimaschutz", "insekten", "grundwasser", "tierschutz",
            "atomtransport", "ökologisch", "fckw") %>% 
  paste0(collapse = "|")
# topic 8
energie <- c(" kohle", "kohleausstieg", "energie", "atomkraft", "solar", "windenergie", "erneuerbare",
             "akw", "energiemix", "eeg", "kohlekraftwerk", "biogasanlagen") %>% 
  paste0(collapse = "|")

# topic 9
einwanderung <- c("flüchtling", "asyl", "migration", "einwanderer", "einwanderung", 
                  "abschiebung", "familiennachzug", "schutzsuchende", 
                  "duldung", "zuwanderung", "migranten") %>% 
  paste0(collapse = "|")
# topic 10
transport <- c("autobahn", "schienenverkehr", " bahn ", "flughafen", "lkw", " maut ", " mobilität ", 
               " schienen ", "zugverspätung", " zug ", " züge ", " verkehrswende ", 
               " verkehrsprojekt ", " radverkehr", " wasserwege ", " schifffahrt ",
               " verkehrspolitik ", " straßenbrücke ", " autobahnbrücke ") %>% 
  paste0(collapse = "|")
# topic 11
krimi <- c("steuerhinterziehung", "bandenkriminalität", " clans",
           "gefängnis", "kindesmissbrauch", "kinderpornografie", " interpol ", 
           "kindesentführung", "straftaten", "kriminalität", "sicherungsverwahrung",
           "tötungsdelikte", "haftbefehl", "bundeskriminalamt", "polizei", "gewalttaten",
           "linksextrem", "rechtsextrem", "ermittlungsverfahren", " gefährder ",
           "sicherheitspolitik") %>% 
  paste0(collapse = "|")
# topic 12
sozi <- c("arbeitslosengeld", "altersarmut", "pension", "armut", "tafel", "kindergeld",
          "jobcenter", "sgb-ii", "vermögensungleichheit", "einkommensungleichheit",
          "einkommensunterschiede", "arbeitssuchend", "arbeitssuchende",
           "sozialgesetzbuch") %>% 
  paste0(collapse = "|")

# topic 13
wohn <- c("mietpreis", " miete", "wohnung", "obdachlos", "wohnhilfe", "wohnheim",
          " brachfläche", " wohnen ", "sozialer wohnungsbau", " wohnungsbau ", "mietpreisbremse",
          "mietenspiegel", "wohnungspolitik", "immobilienpreise" ) %>% 
  paste0(collapse = "|")

# topic 14
housing <- c("bankwesen", "finanzsektor", " tourismus ", "patent", "urheberrecht",
            "finanzmarkt", "kapitalmarkt", "insolvenz", "insolvenzen", "patente", "glücksspiel",
            "verbraucherschutzrechte", "verbraucherrechte", " kredite ",
            " finanztransaktionssteuer ", "kapitalerträge", "kapitalertragssteuer", 
            "spekulationsfrist") %>% 
  paste0(collapse = "|")

# topic 15
verteidigung <- c("bundeswehr", "kriegseinsatz", "kriegseinsätze", " nato ", "auslandseinsatz", "militär",
                  "streitkräfte", "soldaten") %>% 
  paste0(collapse = "|")

# topic 16
tech_kom <- c(" internet ", " breitband", "telekommunikation", " gez ", " rundfunk",
              " digitalisierung ", " raumfahrt ", " cybersicherheit ", "osze") %>% 
  paste0(collapse = "|")

# topic 17
außenh <- c("handelsabkommen", " export", " import ", "zölle ", " zoll ", " einfuhrbeschränkung", 
            " freihandel", " außenhandel ", " export ", " waffenexport ", " rüstungsexport ",
            " ceta ", " ttip ", "transatlantic trade", " zoll ", "handelsüberschuss",
            "handelsüberschüsse") %>% 
  paste0(collapse = "|")

# topic 18
int_aus <- c("auslandshilfe", "entwicklungsländer", "brexit",
             "arbeitsbedingungen in schwellen und entwicklu", "islamischer staat", "terror",
             "bürgerkrieg ", " europäisch") %>% 
  paste0(collapse = "|")

# 20
regierung <- c("bürokratieentlastung", "bürokratieabbau", "weniger bürokratie",
               "Untersuchungsausschuss", "berateraffäre", "misstrauensvotum",
               "beamte ", " pension", " bundesamt für statistik", "statistisches bundesamt",
               "destatis ") %>% 
  paste0(collapse = "|")


# public goods muss raus, da es hierzu nahezu keinerlei Themen gibt
public_goods <- c(" denkmal", "denkmäler", "freizeitpark", "öffentliche flächen",
                  "wald ", " förster") %>% 
  paste0(collapse = "|")

# CULTURE EXCLUDED DUE TO MISSING MATCHES

glimpse(df)

df_match <- df %>% 
  dplyr::mutate(agenda_top = top_content) %>% 
  dplyr::mutate(thema = case_when(
    stringr::str_detect(agenda_top, makroöko) ~ "Makroökonomie", #1
    stringr::str_detect(agenda_top, buerger) ~ "Bürgerrechte",#2
    stringr::str_detect(agenda_top, gesundheit) ~ "Gesundheit", #3
    stringr::str_detect(agenda_top, agrar) ~ "Agrarwirtschaft", #4
    stringr::str_detect(agenda_top, arbeit) ~ "Arbeit & Beschäftigung", #5
    stringr::str_detect(agenda_top, bildung) ~ "Bildung", #6
    stringr::str_detect(agenda_top, umwelt) ~ "Umwelt", #7
    stringr::str_detect(agenda_top, energie) ~ "Energie", #8
    stringr::str_detect(agenda_top, einwanderung) ~ "Einwanderung", #9
    stringr::str_detect(agenda_top, transport) ~ "Transport", #10
    stringr::str_detect(agenda_top, krimi) ~ "Kriminalität & Familienprobleme", #11
    stringr::str_detect(agenda_top, sozi) ~ "Soziale Wohlfahrt", #12
    stringr::str_detect(agenda_top, wohn) ~ "Gemeindeentwicklung & Wohnungsprobleme", #13
    stringr::str_detect(agenda_top, housing) ~ "Banken, Finanzen & Binnenhandel", #14
    stringr::str_detect(agenda_top, verteidigung) ~ "Verteidigung", #15
    stringr::str_detect(agenda_top, tech_kom) ~ "Technologie & Kommunikation", #16
    stringr::str_detect(agenda_top, außenh) ~ "Außenhandel", #17
    stringr::str_detect(agenda_top, int_aus) ~ "Internationale Angelegenheiten & Auslandshilfe", #18
    stringr::str_detect(agenda_top, regierung) ~ "Regierungsoperationen", #19
    stringr::str_detect(agenda_top, public_goods) ~ "Öffentliche Flächen", #20
    TRUE ~ "zSonstiges"
  )) %>% 
  dplyr::mutate(buerger = case_when(str_detect(agenda_top, buerger) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(gesundheit = case_when(str_detect(agenda_top, gesundheit) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(agrar = case_when(str_detect(agenda_top, agrar) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(arbeit = case_when(str_detect(agenda_top, arbeit) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(bildung = case_when(str_detect(agenda_top, bildung) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(umwelt = case_when(str_detect(agenda_top, umwelt) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(energie = case_when(str_detect(agenda_top, energie) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(einwanderung = case_when(str_detect(agenda_top, einwanderung) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(transport = case_when(str_detect(agenda_top, transport) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(krimi = case_when(str_detect(agenda_top, krimi) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(sozi = case_when(str_detect(agenda_top, sozi) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(wohn = case_when(str_detect(agenda_top, wohn) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(makroökonomie = case_when(str_detect(agenda_top, makroöko) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(banken = case_when(str_detect(agenda_top, housing) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(verteidigung = case_when(str_detect(agenda_top, verteidigung) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(tech_kom = case_when(str_detect(agenda_top, tech_kom) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(außenh = case_when(str_detect(agenda_top, außenh) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(int_aus = case_when(str_detect(agenda_top, int_aus) ~ 1, TRUE ~ 0)) %>%
  dplyr::mutate(regierung = case_when(str_detect(agenda_top, regierung) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(public_good = case_when(str_detect(agenda_top, public_goods) ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(matches = makroökonomie + buerger + gesundheit + agrar +
           arbeit + bildung + umwelt + energie + einwanderung +
           transport + krimi + sozi + wohn + banken + verteidigung +
           tech_kom + außenh + int_aus + regierung + public_good)

# df_match match results on debate level
df_overlap %>% 
  sjmisc::frq(thema)

# checking distribution of topics
# only considering matches <= 1
df_match %>% 
  dplyr::filter(matches <= 1) %>% 
  sjmisc::frq(thema)

# distribution of matches
df_match %>% 
  sjmisc::frq(matches)

# saving data
saveRDS(df_match, "01_data/02_prepared/df_match_20200228.RDS")

# creating csv for manual adjustments
df_overlap %>% 
  dplyr::filter(matches > 1 & matches < 3) %>% 
  dplyr::select(agenda_top, -top_content, matches, date, everything()) %>% 
  dplyr::arrange(matches) %>% 
  write.csv("01_data/overlap_checking.csv")

saveRDS(df_overlap, file = "01_data/02_prepared/overlap_20200302.RDS")
df_overlap <- readRDS("01_data/02_prepared/overlap_20200302.RDS")

# reading adjusted data
df_correct <- read.csv("01_data/manual_corrected_topics_fin.csv",
                       stringsAsFactors = FALSE, sep = ";") %>% 
  as_tibble() %>% 
  dplyr::select(1:5)


corrected_adj <- readxl::read_xlsx("01_data/02_prepared/corrected_labels_fin.xlsx")
corrected_adj %>% 
  dplyr::mutate(date = excel_numeric_to_date(as.numeric(date)), date_system = "modern") %>% 
  dplyr::filter(thema_corrected != "NOT" & !is.na(thema_corrected)) %>% 
  dplyr::select(date, agenda_item, thema_corrected)

df_topics <- df_overlap %>% 
  dplyr::select(date, agenda_item, top_content, thema, overlap) %>% 
  dplyr::left_join(corrected_adj %>% 
                     dplyr::mutate(date = excel_numeric_to_date(as.numeric(date)), date_system = "modern") %>% 
                     dplyr::filter(thema_corrected != "NOT" & !is.na(thema_corrected)) %>% 
                     dplyr::select(date, agenda_item, thema_corrected),
                   by = c("date", "agenda_item")) %>% 
  dplyr::mutate(thema = case_when(overlap > 1 & !is.na(thema_corrected) ~ thema_corrected,
                                            overlap < 2 ~ thema,
                                  TRUE ~ "EXCLUDED")) %>% 
  dplyr::filter(!is.na(date)) %>% 
  
  dplyr::filter(thema != "EXCLUDED" | overlap <2)

df_base <- df_base %>% 
  dplyr::left_join(df_topics %>% 
                     dplyr::select(date, agenda_item, thema),
                   by = c("date", "agenda_item"))
