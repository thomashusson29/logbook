#RECODAGE DE TOUTES LES INTERVENTIONS
df$INTERVENTION_GROUPÉE <- NULL
df$INTERVENTION_GROUPÉE <- NA_character_


#BLOC TRANSPLANTATIONS / PMO 
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ PMO et Prélèvements multi-organes
      str_detect(INTERVENTION, regex("PMO|Pr[ée]l[èe]vement.*multi|Pr[ée]l[èe]vement.*organes", ignore_case = TRUE)) ~ "Prélèvement multi-organes",
      
      str_detect(INTERVENTION, regex(
        "kyste.*h[ée]patique|fenestration.*kyste|kystes.*h[ée]patique|kystique",
        ignore_case = TRUE)) ~ "Fenestration kyste hépatique (coelio)",
      
      str_detect(INTERVENTION, regex("re-?h[ée]patectom.*partielle", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # ✅ Prélèvements foie et pancréas spécifiques
      str_detect(INTERVENTION, regex("Pr[ée]l[èe]vement.*foie|Pr[ée]l[èe]vement.*h[ée]patique", ignore_case = TRUE)) ~ "Prélèvement hépatique",
      str_detect(INTERVENTION, regex("Pr[ée]l[èe]vement.*pancr", ignore_case = TRUE)) ~ "Prélèvement pancréatique",
      
      # ✅ Donneur vivant
      str_detect(INTERVENTION, regex("Donneur vivant.*robot", ignore_case = TRUE)) ~ "Donneur vivant (robot)",
      str_detect(INTERVENTION, regex("Donneur vivant.*coelio", ignore_case = TRUE)) ~ "Donneur vivant (coelio)",
      str_detect(INTERVENTION, regex("Donneur vivant|Don vivant|Pmo.*vivant", ignore_case = TRUE)) ~ "Donneur vivant (laparo)",
      
      # ✅ Transplantations foie
      str_detect(INTERVENTION, regex("^TH$|transplantation.*h[ée]patique|re-TH|TH secondaire|TH split", ignore_case = TRUE)) ~ "Transplantation hépatique",
      
      # ✅ Transplantations pancréas
      str_detect(INTERVENTION, regex("transplantation.*pancr[ée]atique|TPR|\\bTP\\b", ignore_case = TRUE)) ~ "Transplantation pancréatique",
      
      # ✅ Reprises de transplantation
      str_detect(INTERVENTION, regex("Reprise.*transplant", ignore_case = TRUE)) ~ "Reprise transplantation",
      
      # ✅ Back table
      str_detect(INTERVENTION, regex("Back.*Table", ignore_case = TRUE)) ~ "Back table greffe hépatique",
      
      # ✅ Transplantation hépatique
      str_detect(INTERVENTION, regex(
        "transplantation.*h[ée]patique|\\bTH\\b|TH split|Re-TH|reprise.*transplantation.*h[ée]patique|Back Table TH",
        ignore_case = TRUE)) ~ "Transplantation hépatique",
      
      # ✅ Transplantation pancréatique
      str_detect(INTERVENTION, regex(
        "transplantation.*pancr[ée]as|\\bTP\\b|TPR|Back Table TP-TR|reprise.*transplantation.*pancr[ée]as",
        ignore_case = TRUE)) ~ "Transplantation pancréatique",
      
      # ✅ PMO (prélèvement multi-organes)
      str_detect(INTERVENTION, regex(
        "PMO|don.*vivant|donneur vivant|Back Table|explantation",
        ignore_case = TRUE)) ~ "Prélèvement multi-organes",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ SPG (pancréatectomie gauche)
      str_detect(INTERVENTION, regex("SPG.*robot|pancréatectomie.*gauche.*robot|PG robot|PG Warshaw|Appleby.*robot", ignore_case = TRUE)) ~ "Pancreatectomie gauche SPG (robot)",
      str_detect(INTERVENTION, regex("SPG.*laparo|pancréatectomie.*gauche.*laparo|PG laparo|Appleby.*laparo", ignore_case = TRUE)) ~ "Pancreatectomie gauche SPG (laparo)",
      str_detect(INTERVENTION, regex("SPG|spléno[- ]?pancréatectomie|PG|Appleby|RAMPS", ignore_case = TRUE)) ~ "Pancreatectomie gauche SPG (coelio)",
      
      # ✅ DPC / DPT avec abords
      str_detect(INTERVENTION, regex("DPC.*robot|Pancréatectomie céphalique.*robot|DPT.*robot", ignore_case = TRUE)) ~ "Pancreatectomie céphalique DPC / DPT (robot)",
      str_detect(INTERVENTION, regex("DPC.*coelio|DPT.*coelio", ignore_case = TRUE)) ~ "Pancreatectomie céphalique DPC / DPT (coelio)",
      str_detect(INTERVENTION, regex("DPC.*reconstruction veineuse|DPC.*résection veineuse|DPC.*tronculaire", ignore_case = TRUE)) ~ "Pancreatectomie céphalique DPC / DPT (reconstruction veineuse)",
      str_detect(INTERVENTION, regex("DPC|Pancréatectomie céphalique|DPT", ignore_case = TRUE)) ~ "Pancreatectomie céphalique DPC / DPT (laparo)",
      
      # ✅ Reprise pancréatectomie
      str_detect(INTERVENTION, regex("Reprise.*DPC", ignore_case = TRUE)) ~ "Reprise pancréatectomie",
      
      # ✅ Pancreatectomie gauche Appleby (laparo par défaut)
      str_detect(INTERVENTION, regex("Appleby|pancréatectomie.*gauche.*Appleby", ignore_case = TRUE)) ~ "Pancreatectomie gauche Appleby (laparo)",
      
      # ✅ Pancreatectomie centrale (coelio par défaut)
      str_detect(INTERVENTION, regex("pancréatectomie.*centrale", ignore_case = TRUE)) ~ "Pancreatectomie centrale (coelio)",
      
      # ✅ Duodénectomie
      str_detect(INTERVENTION, regex("duodénec", ignore_case = TRUE)) ~ "Duodénectomie",
      
      # ✅ Pancreatectomie totale
      str_detect(INTERVENTION, regex("pancréatectomie totale", ignore_case = TRUE)) ~ "Pancreatectomie totale",
      
      # ✅ Filet résiduel
      str_detect(INTERVENTION, regex("pancr|DPC|DPT|SPG|RAMPS|Appleby|duodénec", ignore_case = TRUE)) ~ "Pancréas - autre",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

#BLOC FOIE
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ Hépatectomie majeure détaillée
      str_detect(INTERVENTION, regex(
        "Hépatectomie.*droite|Héptectomie.*droite|Hepatectomie.*droite|
         Hépatectomie.*gauche.*élargie|centrale|totale|
         H4'5'6'7'8'|H765|Seg.*IV/V|Seg.*VIII|H23|Hépatec IV/V",
        ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Hépatectomie majeure (robot)",
      
      str_detect(INTERVENTION, regex(
        "Hépatectomie.*droite|Héptectomie.*droite|Hepatectomie.*droite|
         Hépatectomie.*gauche.*élargie|centrale|totale|
         H4'5'6'7'8'|H765|Seg.*IV/V|Seg.*VIII|H23|Hépatec IV/V",
        ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("coelio|H23", ignore_case = TRUE)) ~ "Hépatectomie majeure (coelio)",
      
      # ✅ Cas motifs résiduels H'6 avec ou sans Hartmann
      str_detect(INTERVENTION, regex("H'?6", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # 🔹 Cas spécifique : Hépatectomie Dte + anastomose bilio dig
      str_detect(INTERVENTION, regex("h[ée]patectomie.*d(te|roite).*anastomose.*bilio", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # 🔹 Tous les motifs de kyste hépatique résiduels
      str_detect(INTERVENTION, regex("kyste.*h[ée]patique|kystique.*h[ée]patique|fenestration.*kyste", ignore_case = TRUE)) ~ "Fenestration kyste hépatique (coelio)",
      
      # 🔹 Ré-hépatectomie / Re-hépatectomie
      str_detect(INTERVENTION, regex("re[- ]?h[ée]patectom", ignore_case = TRUE)) ~ "Ré-hépatectomie",
      
      str_detect(INTERVENTION, regex("Resection atypique hepatique", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # ✅ Tumorectomies hépatiques coelio
      str_detect(INTERVENTION, regex("tumorectomies.*h[ée]patiques.*coelio", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      str_detect(INTERVENTION, regex("h[ée]patectomie.*(droite|Dte).*anastomose.*bilio", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      str_detect(INTERVENTION, regex(
        "Hépatectomie.*droite|Héptectomie.*droite|Hepatectomie.*droite|
         Hépatectomie.*gauche.*élargie|centrale|totale|
         H4'5'6'7'8'|H765|Seg.*IV/V|Seg.*VIII|H23|Hépatec IV/V",
        ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # ✅ Hépatectomie mineure détaillée
      str_detect(INTERVENTION, regex(
        "wedge|secteur|segmentectomie|segmenctectomie|unisegmentectomie|
         résection atypique|Resection.*hep.*atypique|RF nodule hépatique|
         Métastasectomie|Résection hep atypique|Résection hep.*méta bord du II|
         Résection hépatique Seg V.*VIII|Hepatectomie partielle|Hepatectomies partielles multiples",
        ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot|S6", ignore_case = TRUE)) ~ "Hépatectomie mineure (robot)",
      
      # ✅ Sectoriectomie antérieure
      str_detect(INTERVENTION, regex("sectoriectomie.*antérieure", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # ✅ Sectoriectomie postérieure robot
      str_detect(INTERVENTION, regex("sectoriectomie.*postérieure.*robot", ignore_case = TRUE)) ~ "Hépatectomie mineure (robot)",
      
      # ✅ Résection atypique et ablation nodule psoas
      str_detect(INTERVENTION, regex("résection.*atypique.*nodule.*psoas", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection atypique pour Meta
      str_detect(INTERVENTION, regex("résection.*atypique.*meta", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      str_detect(INTERVENTION, regex(
        "wedge|secteur|segmentectomie|segmenctectomie|unisegmentectomie|
         résection atypique|Resection.*hep.*atypique|RF nodule hépatique|
         Métastasectomie|Résection hep atypique|Résection hep.*méta bord du II|
         Résection hépatique Seg V.*VIII|Hepatectomie partielle|Hepatectomies partielles multiples",
        ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("coelio|micro ondes", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      str_detect(INTERVENTION, regex(
        "wedge|secteur|segmentectomie|segmenctectomie|unisegmentectomie|
         résection atypique|Resection.*hep.*atypique|RF nodule hépatique|
         Métastasectomie|Résection hep atypique|Résection hep.*méta bord du II|
         Résection hépatique Seg V.*VIII|Hepatectomie partielle|Hepatectomies partielles multiples",
        ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # ✅ Lobectomie gauche spécifique
      str_detect(INTERVENTION, regex("lobectomie.*gauche|Lobectomie G|Lobectomie gauche.*convertie|Lobectomie gauche donneur", ignore_case = TRUE)) ~ "Lobectomie gauche",
      
      # ✅ Réparation biliaire (motifs étendus)
      str_detect(INTERVENTION, regex(
        "réparation.*bili|anastomose.*bd|bilio biliaire|voie biliaire|
         Réfection anastomose bilio|Résection VBP|VBP|Redo anastomose bilio-digestive",
        ignore_case = TRUE)) ~ "Réparation biliaire",
      
      # ✅ Explantation hépatique + back table + reprise post TH
      str_detect(INTERVENTION, regex("explantation.*h[ée]patique|bac table TH|reprise post TH", ignore_case = TRUE)) ~ "Explantation hépatique",
      
      # ✅ Double dérivation
      str_detect(INTERVENTION, regex("double dérivation", ignore_case = TRUE)) ~ "Hépatectomie complexe (double dérivation)",
      
      # ✅ Curage si non encore pris ailleurs
      str_detect(INTERVENTION, regex("curage.*ganglionnaire", ignore_case = TRUE)) ~ "Curage ganglionnaire",
      
      # ✅ Cas Hépatectomie Dte + anastomose bilio dig
      str_detect(INTERVENTION, regex("hépatectomie.*droite.*anastomose bilio", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # ✅ Résection atypique variantes détaillées
      str_detect(INTERVENTION, regex(
        "résection partielle atypique|résection atypique.*psoas|résection atypique pour meta|
         résection atypique coelio|résection atypique hépatique|résection atypique.*segment|
         résection atypique du VII|résection atypique SVI|résection atypique SVI-VII|
         résection atypique.*micro onde|résection atypique.*sgt",
        ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Sectoriectomie postérieure coelio
      str_detect(INTERVENTION, regex("sectoriectomie.*postérieure.*coelio", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Hépatectomie gauche robot isolée
      str_detect(INTERVENTION, regex("hépatectomie gauche.*robot", ignore_case = TRUE)) ~ "Hépatectomie majeure (robot)",
      
      # ✅ H6', H4'5'6'7'8' + cholécystectomie etc.
      str_detect(INTERVENTION, regex("H6'|H45|H458|H4'5'6'7'8'|H8'", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      str_detect(INTERVENTION, regex ("résection atypique hép laparo", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      str_detect(INTERVENTION, regex("Sectoriectomie anterieure", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      str_detect(INTERVENTION, regex("Resection atypique et ablation nodule psoas", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      str_detect(INTERVENTION, regex("Resection atypique pour meta", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # ✅ Fenestration kyste variantes
      str_detect(INTERVENTION, regex(
        "fenestration.*kyste|fenestration.*biliaire|fenestration.*h[ée]patique",
        ignore_case = TRUE)) ~ "Fenestration kyste hépatique (coelio)",
      
      # ✅ Réfection anastomose bilio-dig
      str_detect(INTERVENTION, regex(
        "réfection anastomose bilio", ignore_case = TRUE)) ~ "Réparation biliaire",
      
      # ✅ Sectoriectomie postérieure coelio (doublon safety)
      str_detect(INTERVENTION, regex(
        "sectoriectomie.*postérieure.*coelio", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection hépatique atypique par robot S6
      str_detect(INTERVENTION, regex("résection.*atypique.*robot S6", ignore_case = TRUE)) ~ "Hépatectomie mineure (robot)",
      
      # ✅ Résection hépatique coelio + micro ondes
      str_detect(INTERVENTION, regex("résection.*hépatique.*coelio.*micro ondes", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection atypique + micro onde (tous formats)
      str_detect(INTERVENTION, regex("résection.*atypique.*micro onde", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection atypique coelio métastases dôme hépatique
      str_detect(INTERVENTION, regex("résection.*atypique.*coelio.*métastases.*d[ôo]me", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection atypique du VII
      str_detect(INTERVENTION, regex("résection.*atypique.*VII", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection atypique et ablation nodule psoas
      str_detect(INTERVENTION, regex("résection.*atypique.*nodule.*psoas", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection atypique pour Meta
      str_detect(INTERVENTION, regex("résection.*atypique.*meta", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Hepatectomie gauche + anastomose bilio-dig
      str_detect(INTERVENTION, regex("h[ée]patectomie.*gauche.*anastomose.*bilio", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # ✅ 1er temps ALPPS
      str_detect(INTERVENTION, regex("1[èe]re temps.*ALPPS|ALPPS", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # ✅ Hépatectomie D coelio
      str_detect(INTERVENTION, regex("h[ée]patectomie.*D coelio|h[ée]ptectomie.*droite.*coelio", ignore_case = TRUE)) ~ "Hépatectomie majeure (coelio)",
      
      # ✅ Hépatectomie gauche isolée (pas déjà matchée)
      str_detect(INTERVENTION, regex("^h[ée]patectomie gauche$", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # ✅ Hépatectomie gauche robot isolée
      str_detect(INTERVENTION, regex("^h[ée]patectomie gauche.*robot$", ignore_case = TRUE)) ~ "Hépatectomie majeure (robot)",
      
      # ✅ Re-hépatectomie partielle
      str_detect(INTERVENTION, regex("Re-h[ée]patectomie.*partielle", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # ✅ Drainage ou ponction abcès hépatique
      str_detect(INTERVENTION, regex("ponction.*abcès.*h[ée]patique|drainage.*abcès.*h[ée]patique", ignore_case = TRUE)) ~ "Drainage abcès hépatique",
      
      # ✅ RF isolé pour métastase hépatique
      str_detect(INTERVENTION, regex("RF.*h[ée]patique", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # 🔹 Hépatectomies centrales, élargies, sous-segmentectomies, bisegmentectomies, wedges résiduels
      str_detect(INTERVENTION, regex(
        "h[ée]patectomie.*centrale|h[ée]patectomie.*gauche.*double dérivation|h[ée]patectomie.*gauche.*secteur ant|h[ée]patectomie.*gauche.*voie biliaire|
   re[- ]?h[ée]patectomies.*partielles|re[- ]?h[ée]patectomie.*partielle|wedge.*h[ée]patique|wedge.*segment|sous.*segmentectomie|bisegmentectomie|
   segmenctectomie|segmentectomie|unisegmentectomie|resections.*h[ée]patiques",
        ignore_case = TRUE)
      ) ~ "Hépatectomie mineure (laparo)",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

#BLOC VÉSICULES
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ Bloc cholécystectomie coelio explicite
      str_detect(INTERVENTION, regex(
        "chol[eé]cystectomie|cholescystectomie|cheolecystectomie|v[ée]sicule|chol[eé]cystite|lavage.*chol[eé]cystectomie",
        ignore_case = TRUE)) ~ "Cholécystectomie (coelio)",
      
      # ✅ Bloc cholécystectomie laparo explicite
      str_detect(INTERVENTION, regex(
        "chol[eé]cystectomie|cholescystectomie|cheolecystectomie|v[ée]sicule|chol[eé]cystite|lavage.*chol[eé]cystectomie",
        ignore_case = TRUE)) & 
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Cholécystectomie (laparo)",
      
      # ✅ Bloc cholécystectomie coelio par défaut si pas d'abord explicite
      str_detect(INTERVENTION, regex(
        "chol[eé]cystectomie|cholescystectomie|cheolecystectomie|v[ée]sicule|chol[eé]cystite|lavage.*chol[eé]cystectomie",
        ignore_case = TRUE)) ~ "Cholécystectomie (coelio)",
      
      str_detect(INTERVENTION, regex("lavage.*chol[eé]cystectomie", ignore_case = TRUE)) ~ "Cholécystectomie (coelio)",
      
      # ✅ Sinon on laisse tel quel
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

# ✅ Bloc COU complet (corrigé)
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # TT
      str_detect(INTERVENTION, regex("TT|thyro[iï]dectomie totale|totalisation.*thyro[iï]dectomie", ignore_case = TRUE)) ~ "Thyroïdectomie totale",
      str_detect(INTERVENTION, regex("Thyroïde", ignore_case = TRUE)) ~ "Thyroïdectomie totale",
      str_detect(INTERVENTION, regex("Thyreoidectomie", ignore_case = TRUE)) ~ "Thyroïdectomie totale",
      # Lobo-isthmectomie
      str_detect(INTERVENTION, regex("lobo[- ]?isthmectomie|isthmectomie|lobo[- ]?isthmo|Lobo-isthmetomie gauche|lobo|Isthméctomie thyrodienne ", ignore_case = TRUE)) ~ "Lobo-isthmectomie",
      # Parathyroïdes incluant 4 sites et abréviations
      str_detect(INTERVENTION, regex("parathyro[iï]de|parathyroidectomie|parathyr|para|PT|HPT|P[3-4]|4 sites|explo.*4 sites|exploration.*4 sites|examen.*4 sites", ignore_case = TRUE)) ~ "Parathyroïdectomie",
      str_detect(INTERVENTION, regex("explo des qutre sites", ignore_case = TRUE)) ~ "Parathyroïdectomie",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

#Blocs HERNIES 
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ Cure RGO (robot)
      str_detect(INTERVENTION, regex("HH|Nissen|RGO", ignore_case = TRUE)) & 
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Cure RGO (robot)",
      
      # ✅ Cure RGO (coelio) sinon
      str_detect(INTERVENTION, regex("HH|Nissen|RGO", ignore_case = TRUE)) ~ "Cure RGO (coelio)",
      
      # ✅ Hernie inguinale (coelio) TAPP TEP
      str_detect(INTERVENTION, regex("TAPP|TEP|Hernie bilatérale coelio|Hernie unilatérale coelio", ignore_case = TRUE)) ~ "Hernie inguinale (coelio)",
      
      # ✅ Hernie interne (coelio)
      str_detect(INTERVENTION, regex("hernie.*interne", ignore_case = TRUE)) ~ "Hernie interne (coelio)",
      
      # ✅ Hernie ombilicale OU ligne blanche
      str_detect(INTERVENTION, regex("^HO\\s|\\sHO\\s|\\sHO$|^HO$|hernie.*ombilicale|ombilicale.*hernie|cure.*ombilicale|ligne blanche", ignore_case = TRUE)) ~ "Hernie ombilicale",
      
      # ✅ Hernie de Spiegel
      str_detect(INTERVENTION, regex("Speigel|Speigle", ignore_case = TRUE)) ~ "Hernie de Spiegel",
      
      # ✅ Hernie fémorale (inclut crurale)
      str_detect(INTERVENTION, regex("f[é|e]morale|crurale|curale", ignore_case = TRUE)) ~ "Hernie fémorale",
      
      # ✅ Hernie étranglée (si précisé sans autre site)
      str_detect(INTERVENTION, regex("Hernie étranglée|Cure de hernie étranglée", ignore_case = TRUE)) ~ "Hernie étranglée",
      
      # ✅ Hernie inguinale générale (HI, HIG, Licht, Shouldice, abord direct)
      str_detect(INTERVENTION, regex("HI|HIG|inguinale|Licht|Lichtenstein|Shouldice|abord direct|Hernie Liechtenstein", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      # Hernie TAP explicitement mentionnée
      str_detect(INTERVENTION, regex("hernie TAP|Hernie TAP", ignore_case = TRUE)) ~ "Hernie TAP",
      
      # Hernie bilatérale coelio
      str_detect(INTERVENTION, regex("hernie bilatérale coelio|Hernie bilat coelio", ignore_case = TRUE)) ~ "Hernie inguinale (coelio)",
      
      # Hernie Liechtenstein
      str_detect(INTERVENTION, regex("hern[ie|e] Liechtenstein|hernie lich", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      # Cure récidive hernie Spiegel (avec occlusion, échec fermeture péritoine)
      str_detect(INTERVENTION, regex("r[ée]cidive hernie Spiegel", ignore_case = TRUE)) ~ "Hernie de Spiegel",
      
      # Cure de hernie ombilicale (inclut variantes orthographiques et rigolotes)
      str_detect(INTERVENTION, regex("hernie ombilicale|hernie omblicale|cure de hernie omblicale", ignore_case = TRUE)) ~ "Hernie ombilicale",
      
      # Hernie inguinale Lichtenstein (version courte)
      str_detect(INTERVENTION, regex("hernie ing lich|hern[ie|e] Lichtenstein", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      # Pour éviter doublons : conserve le codage déjà existant sinon
      
      # Reclasser les CHIP/cytoréductions
      INTERVENTION_GROUPÉE == "Hernie inguinale" & 
        str_detect(INTERVENTION, regex("CHIP|cyto", ignore_case = TRUE)) ~ "Cytoréduction (laparo)",
      
      # Reclasser les hernies hiatales vers RGO  
      INTERVENTION_GROUPÉE == "Hernie inguinale" & 
        str_detect(INTERVENTION, regex("hiatale", ignore_case = TRUE)) ~ "Cure RGO (coelio)",
      
      # Reclasser les abcès inguinaux
      INTERVENTION_GROUPÉE == "Hernie inguinale" & 
        str_detect(INTERVENTION, regex("abcès inguinal", ignore_case = TRUE)) ~ "Drainage d'abcès",
      
      # === AJOUTER LES VRAIES HERNIES INGUINALES NON CLASSÉES ===
      
      # Hernies inguinales TEP et TAPP non classées
      str_detect(INTERVENTION, regex("hernie inguinale.*TEP|hernie inguinale.*TAPP|Hi engouée TAPP", ignore_case = TRUE)) ~ "Hernie inguinale (coelio)",
      
      # Reprises de hernies inguinales
      str_detect(INTERVENTION, regex("reprise hernie inguinale", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      # Lichtenstein pour récidives
      str_detect(INTERVENTION, regex("Licht.*récidive|Licht.*reprise", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      
      # ✅ Par défaut : laisse inchangé
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      str_detect(INTERVENTION, regex(
        "appendicite|appendicectomie|appendectomie|APP|App",
        ignore_case = TRUE)) ~ "Appendicectomie (coelio)",
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ Lewis Santy
      str_detect(INTERVENTION, regex("Lewis\\s*Santy|Oesophagectomie\\s*Lewis|Lewis Santy", ignore_case = TRUE)) ~ "Lewis Santy",
      str_detect(INTERVENTION, regex("Lewis", ignore_case = TRUE)) & str_detect(INTERVENTION, regex("robot|coelio", ignore_case = TRUE)) ~ "Lewis Santy (robot/coelio)",
      
      # ✅ 3 voies
      str_detect(INTERVENTION, regex("3 voies|Oesophagectomie 3 voies", ignore_case = TRUE)) ~ "3 voies",
      str_detect(INTERVENTION, regex("3 voies", ignore_case = TRUE)) & str_detect(INTERVENTION, regex("robot|coelio", ignore_case = TRUE)) ~ "3 voies (robot/coelio)",
      
      # ✅ Reprise Lewis
      str_detect(INTERVENTION, regex("Reprise Lewis", ignore_case = TRUE)) ~ "Reprise Lewis",
      str_detect(INTERVENTION, regex("démontage gastroplastie ", ignore_case = TRUE)) ~ "Reprise Lewis",
      
      # ✅ Zenker
      str_detect(INTERVENTION, regex("Zenker", ignore_case = TRUE)) ~ "Zenker",
      
      # ✅ Stripping oesophage (toujours laparo)
      str_detect(INTERVENTION, regex("Stripping oe?sophage", ignore_case = TRUE)) ~ "Stripping oesophage (laparo)",
      
      # ✅ Diverticule oesophagien
      str_detect(INTERVENTION, regex("Diverticule oe?sophagien", ignore_case = TRUE)) ~ "Diverticule oesophagien",
      
      # ✅ Phryngo-gastroplastie
      str_detect(INTERVENTION, regex("Phryngo-gastroplastie", ignore_case = TRUE)) ~ "Pharyngo-gastroplastie",
      str_detect(INTERVENTION, regex("Pharyngo-gastroplastie", ignore_case = TRUE)) ~ "Phryngo-gastroplastie",
      
      # ✅ Coloplastie et variantes
      str_detect(INTERVENTION, regex("coloplastie|colopharyngo|colopharyngoplastie", ignore_case = TRUE)) ~ "Coloplastie",
      
      # ✅ Lewis Santy
      str_detect(INTERVENTION, regex("Lewis", ignore_case = TRUE)) ~ "Lewis Santy",
      str_detect(INTERVENTION, regex("LS", ignore_case = TRUE)) ~ "Lewis Santy",
      
      # ✅ 3 voies
      str_detect(INTERVENTION, regex("3 voies", ignore_case = TRUE)) ~ "Oesophage 3 voies",
      str_detect(INTERVENTION, regex("Oeosphage 3 voies", ignore_case = TRUE)) ~ "Oesophage 3 voies",
      
      # ✅ Zenker
      str_detect(INTERVENTION, regex("Zenker", ignore_case = TRUE)) ~ "Zenker",
      
      # ✅ Stripping oesophage
      str_detect(INTERVENTION, regex("Stripping oesophage", ignore_case = TRUE)) ~ "Stripping oesophage (laparo)",
      
      # ✅ Diverticule oesophagien
      str_detect(INTERVENTION, regex("Diverticule oesophagien", ignore_case = TRUE)) ~ "Diverticule oesophagien (robot)",
      
      # ✅ Phryngo-gastroplastie
      str_detect(INTERVENTION, regex("Phryngo-gastroplastie", ignore_case = TRUE)) ~ "Phryngo-gastroplastie",
      str_detect(INTERVENTION, regex("Pharyngogastroplastie", ignore_case = TRUE)) ~ "Phryngo-gastroplastie",
      
      # ✅ Reprise Lewis
      str_detect(INTERVENTION, regex("Reprise Lewis", ignore_case = TRUE)) ~ "Reprise Lewis Santy",
      
      # ✅ Autres : laisse inchangé
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ GT
      str_detect(INTERVENTION, regex("GT|Gastrectomie", ignore_case = TRUE)) ~ "Gastrectomie totale (laparo)",
      str_detect(INTERVENTION, regex("GT.*coelio", ignore_case = TRUE)) ~ "Gastrectomie totale (coelio)",
      str_detect(INTERVENTION, regex("GT.*robot", ignore_case = TRUE)) ~ "Gastrectomie totale (robot)",
      
      # ✅ Gastrectomie partielle
      str_detect(INTERVENTION, regex("Gastrectomie partielle", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Gastrectomie partielle (robot)",
      str_detect(INTERVENTION, regex("Gastrectomie partielle", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("coelio", ignore_case = TRUE)) ~ "Gastrectomie partielle (coelio)",
      str_detect(INTERVENTION, regex("Gastrectomie partielle", ignore_case = TRUE)) ~ "Gastrectomie partielle (laparo)",
      
      # ✅ Gastrectomie 4/5
      str_detect(INTERVENTION, regex("Gastrectomie 4/5", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Gastrectomie 4/5e (robot)",
      str_detect(INTERVENTION, regex("Gastrectomie 4/5", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("coelio", ignore_case = TRUE)) ~ "Gastrectomie 4/5e (coelio)",
      str_detect(INTERVENTION, regex("Gastrectomie 4/5|Gastrectomie des 4/5 ème laparo  ", ignore_case = TRUE)) ~ "Gastrectomie 4/5e (laparo)",
      
      # ✅ Gastrectomie atypique
      str_detect(INTERVENTION, regex("Gastrectomie atypique|gastrec partielle pour GIST|GIST", ignore_case = TRUE)) ~ "Gastrectomie atypique (laparo)",
      
      # ✅ By pass
      str_detect(INTERVENTION, regex("By pass|Bypass", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Bypass gastrique (robot)",
      str_detect(INTERVENTION, regex("By pass|Bypass", ignore_case = TRUE)) ~ "Bypass gastrique (laparo)",
      
      # ✅ Sleeve
      str_detect(INTERVENTION, regex("Sleeve", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Sleeve gastrectomie (robot)",
      str_detect(INTERVENTION, regex("Sleeve", ignore_case = TRUE)) ~ "Sleeve gastrectomie (coelio)",
      
      # ✅ Gastrotomie
      str_detect(INTERVENTION, regex("Gastrotomie", ignore_case = TRUE)) ~ "Gastrotomie (laparo)",
      
      # ✅ Kinking gastroplastie
      str_detect(INTERVENTION, regex("Kinking gastroplastie", ignore_case = TRUE)) ~ "Gastroplastie (coelio)",
      
      # ✅ Démontage gastroplastie
      str_detect(INTERVENTION, regex("Démontage gastroplastie", ignore_case = TRUE)) ~ "Démontage gastroplastie (laparo)",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      
      ## ✅ Colon droit
      str_detect(INTERVENTION, regex("Colon D|Colectomie D|Colectomie droite", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Colon droit (robot)",
      str_detect(INTERVENTION, regex("Colon D|Colectomie D|Colectomie droite", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colon droit (laparo)",
      str_detect(INTERVENTION, regex("Colon D|Colectomie D|Colectomie droite", ignore_case = TRUE)) ~ "Colon droit (coelio)",
      
      ## ✅ Hartmann créateur
      str_detect(INTERVENTION, regex("\\bHartmann\\b", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Hartmann (robot)",
      str_detect(INTERVENTION, regex("\\bHartmann\\b", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Hartmann (laparo)",
      str_detect(INTERVENTION, regex("\\bHartmann\\b", ignore_case = TRUE)) ~ "Hartmann (coelio)",
      
      str_detect(INTERVENTION, regex("RIS|ACA|ileo", ignore_case = TRUE)) ~ "Rectum (coelio)",
      
      ## ✅ Colon angulaire
      str_detect(INTERVENTION, regex("angulaire", ignore_case = TRUE)) ~ "Colon angulaire (coelio)",
      
      ## ✅ Colon transverse
      str_detect(INTERVENTION, regex("transverse", ignore_case = TRUE)) ~ "Colon transverse (coelio)",
      
      ## ✅ Colon gauche (inclut sigmoidectomie et variantes)
      str_detect(INTERVENTION, regex("Colon G|Colectomie G|Sigmoidectomie|Sigmoïdectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Colon gauche (robot)",
      str_detect(INTERVENTION, regex("Colon G|Colectomie G|Sigmoidectomie|Sigmoïdectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colon gauche (laparo)",
      str_detect(INTERVENTION, regex("Colon G|Colectomie G|Sigmoidectomie|Sigmoïdectomie", ignore_case = TRUE)) ~ "Colon gauche (coelio)",
      
      ## ✅ RIC (Résection iléo-caecale)
      str_detect(INTERVENTION, regex("RIC|ileocaecale|iléo caecale|iléocaecale|Iléocolectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "RIC (robot)",
      str_detect(INTERVENTION, regex("RIC|ileocaecale|Résection iléo-caecale laparo|iléo caecale|iléocaecale|Iléocolectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "RIC (laparo)",
      str_detect(INTERVENTION, regex("RIC|ileocaecale|iléo caecale|iléocaecale|Iléocolectomie", ignore_case = TRUE)) ~ "RIC (coelio)",
      
      ## ✅ Colon total / subtotal (Totalisation)
      str_detect(INTERVENTION, regex("totalisation|Colon sub total|Colon total|Colectomie totale", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Colon total (robot)",
      str_detect(INTERVENTION, regex("totalisation|Colon sub total|Colon total|Colectomie totale", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colon total (laparo)",
      str_detect(INTERVENTION, regex("totalisation|Colon sub total|Colon total|Colectomie totale", ignore_case = TRUE)) ~ "Colon total (coelio)",
      
      ## ✅ Rectum (proctectomie, RRS, pelvectomie)
      str_detect(INTERVENTION, regex("RRS|proctectomie|Pelvectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Rectum (robot)",
      str_detect(INTERVENTION, regex("RRS|proctectomie|Pelvectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Rectum (laparo)",
      str_detect(INTERVENTION, regex("RRS|proctectomie|Pelvectomie", ignore_case = TRUE)) ~ "Rectum (coelio)",
      
      ## ✅ Rétablissement de Hartmann
      str_detect(INTERVENTION, regex("retabl", ignore_case = TRUE)) ~ "Rétablissement Hartmann|rétablissimenet de hartman",
      
      ## ✅ Colostomies (coelio par défaut sauf mention)
      str_detect(INTERVENTION, regex("Colostomie|Colosotomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colostomie (laparo)",
      str_detect(INTERVENTION, regex("Colostomie|Colosotomie", ignore_case = TRUE)) ~ "Colostomie (coelio)",
      
      ## ✅ Colon droit
      str_detect(INTERVENTION, regex("colectomie droite|colon D|côlon droit|colectomie aguche", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Colon droit (robot)",
      str_detect(INTERVENTION, regex("colectomie droite|colon D|côlon droit|colectomie aguche", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colon droit (laparo)",
      str_detect(INTERVENTION, regex("colectomie droite|colon D|côlon droit|colectomie aguche", ignore_case = TRUE)) ~ "Colon droit (coelio)",
      
      ## ✅ Colon angulaire (rare mais ok)
      str_detect(INTERVENTION, regex("colectomie angulaire", ignore_case = TRUE)) ~ "Colon angulaire (coelio)",
      
      ## ✅ RIC (résection iléo caecale)
      str_detect(INTERVENTION, regex("RIC|resection il[eé]o[- ]?caecale", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "RIC (laparo)",
      str_detect(INTERVENTION, regex("RIC|resection il[eé]o[- ]?caecale", ignore_case = TRUE)) ~ "RIC (coelio)",
      
      ## ✅ Colon gauche / sigmoidectomie
      str_detect(INTERVENTION, regex("colectomie gauche|colon G|sigmoidectomie|sigmoïdectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Colon gauche (robot)",
      str_detect(INTERVENTION, regex("colectomie gauche|colon G|sigmoidectomie|sigmoïdectomie|colectomie laparo ", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colon gauche (laparo)",
      str_detect(INTERVENTION, regex("colectomie gauche|colon G|sigmoidectomie|sigmoïdectomie", ignore_case = TRUE)) ~ "Colon gauche (coelio)",
      
      ## ✅ Colectomie totale
      str_detect(INTERVENTION, regex("colectomie totale|colon sub totalcolon sub total|subtotale|colon sub total", ignore_case = TRUE)) ~ "Colectomie totale",
      
      ## ✅ Rétablissement Hartmann
      str_detect(INTERVENTION, regex("retabl", ignore_case = TRUE)) ~ "Rétablissement Hartmann",
      
      ## ✅ Hartmann créateur
      str_detect(INTERVENTION, regex("\\bHartmann\\b", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Hartmann (robot)",
      str_detect(INTERVENTION, regex("\\bHartmann\\b", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Hartmann (laparo)",
      str_detect(INTERVENTION, regex("\\bHartmann\\b", ignore_case = TRUE)) ~ "Hartmann (coelio)",
      
      ## ✅ Rectum / Proctectomie / Pelvectomie postérieure
      str_detect(INTERVENTION, regex("rectum|proctectomie|pelvectomie|protectomie|protectomie secondaire", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Rectum (robot)",
      str_detect(INTERVENTION, regex("rectum|proctectomie|pelvectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Rectum (laparo)",
      str_detect(INTERVENTION, regex("rectum|proctectomie|pelvectomie|resection recto sigmoidienne", ignore_case = TRUE)) ~ "Rectum (coelio)",
      
      ## ✅ Colostomie
      str_detect(INTERVENTION, regex("colostomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colostomie (laparo)",
      str_detect(INTERVENTION, regex("colostomie|coleostomie coelio", ignore_case = TRUE)) ~ "Colostomie (coelio)",
      # Protectomie secondaire = rectum (proctectomie dérivée)
      str_detect(INTERVENTION, regex("protectomie secondaire", ignore_case = TRUE)) ~ "Rectum (coelio)",
      
      # Protectomie secondaire = rectum (proctectomie dérivée)
      str_detect(INTERVENTION, regex("protectomie secondaire", ignore_case = TRUE)) ~ "Rectum (coelio)",
      
      # Rétablissement Hartmann : orthographes multiples corrigées
      str_detect(INTERVENTION, regex("rétablissimenet de hartman|retablissiment de hartman|rétablissement hartmann|retablissement hartmann", ignore_case = TRUE)) ~ "Rétablissement Hartmann",
      
      # Rétablissement Hartmann : orthographes multiples corrigées
      str_detect(INTERVENTION, regex("rétablissimenet de hartman|retablissiment de hartman|rétablissement hartmann|retablissement hartmann", ignore_case = TRUE)) ~ "Rétablissement Hartmann",
      
      # Colectomie + vessie (coelio)
      str_detect(INTERVENTION, regex("colectomie.*vessie", ignore_case = TRUE)) ~ "Colectomie + vessie (coelio)",
      
      # Colectomie laparo (général)
      str_detect(INTERVENTION, regex("colectomie.*laparo", ignore_case = TRUE)) ~ "Colectomie (laparo)",
      
      ## ✅ Par défaut inchangé
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      
      # Surrénale droite robot
      str_detect(INTERVENTION, regex("surrénalectomie.*droit.*robot|surrénale droite robot|surrénalectomie D robot", ignore_case = TRUE)) ~ "Surrénalectomie droite (robot)",
      
      # Surrénale gauche robot
      str_detect(INTERVENTION, regex("surrénalectomie.*gauche.*robot|surrénale gauche robot|surrénalectomie G robot", ignore_case = TRUE)) ~ "Surrénalectomie gauche (robot)",
      
      # Surrénale droite laparo
      str_detect(INTERVENTION, regex("surrénalectomie.*droit.*laparo|surrénale droite laparo|surrénalectomie D laparo", ignore_case = TRUE)) ~ "Surrénalectomie droite (laparo)",
      
      # Surrénale gauche laparo
      str_detect(INTERVENTION, regex("surrénalectomie.*gauche.*laparo|surrénale gauche laparo|surrénalectomie G laparo", ignore_case = TRUE)) ~ "Surrénalectomie gauche (laparo)",
      
      # Surrénale droite laparotomie
      str_detect(INTERVENTION, regex("surrénalectomie.*droit.*laparotomie|surrénale droite laparotomie|surrénalectomie D laparotomie", ignore_case = TRUE)) ~ "Surrénalectomie droite (laparotomie)",
      
      # Surrénale gauche laparotomie
      str_detect(INTERVENTION, regex("surrénalectomie.*gauche.*laparotomie|surrénale gauche laparotomie|surrénalectomie G laparotomie", ignore_case = TRUE)) ~ "Surrénalectomie gauche (laparotomie)",
      
      # Surrénale droite coelio (par défaut)
      str_detect(INTERVENTION, regex("surrénalectomie.*droit|surrénale droite|surrénalectomie D", ignore_case = TRUE)) ~ "Surrénalectomie droite (coelio)",
      
      # Surrénale gauche coelio (par défaut)
      str_detect(INTERVENTION, regex("surrénalectomie.*gauche|surrénale gauche|surrénalectomie G", ignore_case = TRUE)) ~ "Surrénalectomie gauche (coelio)",
      
      # Surrénalectomie robot (non précisé côté)
      str_detect(INTERVENTION, regex("surrénalectomie.*robot|surrénale robot", ignore_case = TRUE)) ~ "Surrénalectomie (robot)",
      
      # Surrénalectomie laparo (non précisé côté)
      str_detect(INTERVENTION, regex("surrénalectomie.*laparo", ignore_case = TRUE)) ~ "Surrénalectomie (laparo)",
      
      # Surrénalectomie laparotomie (non précisé côté)
      str_detect(INTERVENTION, regex("surrénalectomie.*laparotomie", ignore_case = TRUE)) ~ "Surrénalectomie (laparotomie)",
      
      # Surrénale coelio (non précisé côté, par défaut)
      str_detect(INTERVENTION, regex("surrénale|surrénalectomie", ignore_case = TRUE)) ~ "Surrénalectomie (coelio)",
      
      # Cas spécifiques (reprise, urgence coelio blanche)
      str_detect(INTERVENTION, regex("reprise.*surrénalectomie|urgence.*coelio blanche", ignore_case = TRUE)) ~ "Reprise surrénalectomie",
      
      # Surrénale droite robot
      str_detect(INTERVENTION, regex("surrenalectomie.*droit.*robot|surrenalectomie D robot|surrenale droite robot|surrenalectomie droite robot", ignore_case = TRUE)) ~ "Surrénalectomie droite (robot)",
      
      # Surrénale gauche robot
      str_detect(INTERVENTION, regex("surrenalectomie.*gauche.*robot|surrenalectomie G robot|surrenale gauche robot|surrenalectomie gauche robot", ignore_case = TRUE)) ~ "Surrénalectomie gauche (robot)",
      
      # Surrénale droite laparo
      str_detect(INTERVENTION, regex("surrenalectomie.*droit.*laparo|surrenalectomie D laparo|surrenale droite laparo|surrenalectomie droite laparo", ignore_case = TRUE)) ~ "Surrénalectomie droite (laparo)",
      
      # Surrénale gauche laparo
      str_detect(INTERVENTION, regex("surrenalectomie.*gauche.*laparo|surrenalectomie G laparo|surrenale gauche laparo|surrenalectomie gauche laparo", ignore_case = TRUE)) ~ "Surrénalectomie gauche (laparo)",
      
      # Surrénale droite laparotomie
      str_detect(INTERVENTION, regex("surrenalectomie.*droit.*laparotomie|surrenalectomie D laparotomie|surrenale droite laparotomie|surrenalectomie droite laparotomie", ignore_case = TRUE)) ~ "Surrénalectomie droite (laparotomie)",
      
      # Surrénale gauche laparotomie
      str_detect(INTERVENTION, regex("surrenalectomie.*gauche.*laparotomie|surrenalectomie G laparotomie|surrenale gauche laparotomie|surrenalectomie gauche laparotomie", ignore_case = TRUE)) ~ "Surrénalectomie gauche (laparotomie)",
      
      # Surrénale droite coelio par défaut
      str_detect(INTERVENTION, regex("surrenalectomie.*droit|surrenalectomie D|surrenale droite|surrenalectomie droite", ignore_case = TRUE)) ~ "Surrénalectomie droite (coelio)",
      
      # Surrénale gauche coelio par défaut
      str_detect(INTERVENTION, regex("surrenalectomie.*gauche|surrenalectomie G|surrenale gauche|surrenalectomie gauche", ignore_case = TRUE)) ~ "Surrénalectomie gauche (coelio)",
      
      # Surrénalectomie robot non côté précisé
      str_detect(INTERVENTION, regex("surrenalectomie.*robot|surrenale robot", ignore_case = TRUE)) ~ "Surrénalectomie (robot)",
      
      # Surrénalectomie laparo non côté précisé
      str_detect(INTERVENTION, regex("surrenalectomie.*laparo", ignore_case = TRUE)) ~ "Surrénalectomie (laparo)",
      
      # Surrénalectomie laparotomie non côté précisé
      str_detect(INTERVENTION, regex("surrenalectomie.*laparotomie", ignore_case = TRUE)) ~ "Surrénalectomie (laparotomie)",
      
      # Surrénalectomie coelio non côté précisé (par défaut)
      str_detect(INTERVENTION, regex("surrenale|surrenalectomie", ignore_case = TRUE)) ~ "Surrénalectomie (coelio)",
      
      # Cas reprise ou urgences spécifiques surrénales
      str_detect(INTERVENTION, regex("reprise.*surrenalectomie|urgence.*coelio blanche", ignore_case = TRUE)) ~ "Reprise surrénalectomie",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

#Code PROCTO
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      
      # Examen anal sous anesthésie générale (AG)
      str_detect(INTERVENTION, regex("examen anal sous ag", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Examen anal simple (sans précision AG)
      str_detect(INTERVENTION, regex("^examen anal$", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Examen anal + interventions associées (lambeau, pose séton, dilatation, encollage, extraction corps étranger intra rectal)
      str_detect(INTERVENTION, regex("examen anal \\+ lambeau|examen anal \\+ laparo|examen anal \\+ pose endosponge|examen anal - dilatation|examen anal : avancement séton|examen anal sous ag, pose séton|examen anal sous ag: encollage fistule|examen anal sous ag: fistule acr|extraction ce intra rectal|extraction corps étranger intra rectal", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Abcès de marge anale (et variantes orthographiques)
      str_detect(INTERVENTION, regex("abcès marge anale|abces marge anale|abcès de marge|abces de marge|abcès marge \\+", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Abcès péri-anal / fessier (hors marge anale)
      str_detect(INTERVENTION, regex("abces peri anale|abcès périnéal|abcès périnéal|abcès périnéal|abcès fesse|abces fesse|Abcès de fesse|abcès fesse", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Drainage de masse anale
      str_detect(INTERVENTION, regex("drainage ma|drainage masse anale", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Examen procto simple
      str_detect(INTERVENTION, regex("^examen procto$", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Fistule anale (sans fistule anastomose oesogastrique / aorto-duodénale)
      str_detect(INTERVENTION, regex("fistule anale|fistule anus|fisutle anale", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Hémorroïdes classiques et Milligan Morgan
      str_detect(INTERVENTION, regex("hémorroïdes|hemorroide|milligan morgan", ignore_case = TRUE)) ~ "Hémorroïdes",
      
      # Recoupe Baulieu, Babcock, Beaulieux et variantes orthographiques
      str_detect(INTERVENTION, regex("recoupe baulieu|babcock|babcok|beaulieux", ignore_case = TRUE)) ~ "Recoupe Baulieu / Babcok",
      
      # Abaissement fistule anale (geste spécifique)
      str_detect(INTERVENTION, regex("abaissement fistule anale", ignore_case = TRUE)) ~ "Abaissement fistule anale",
      
      # Exclure fistule anastomose oesogastrique et fistule aorto-duodénale de la proctologie
      str_detect(INTERVENTION, regex("fistule anastomose oesogastrique|fistule aorto-duodénale", ignore_case = TRUE)) ~ INTERVENTION_GROUPÉE,
      
      # Examen anal sous anesthésie générale (AG)
      str_detect(INTERVENTION, regex("examen anal sous ag", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Examen anal simple (sans précision AG)
      str_detect(INTERVENTION, regex("^examen anal$", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Examen anal + interventions associées (lambeau, pose séton, dilatation, encollage, extraction corps étranger intra rectal)
      str_detect(INTERVENTION, regex("examen anal \\+ lambeau|examen anal \\+ laparo|examen anal \\+ pose endosponge|examen anal - dilatation|examen anal : avancement séton|examen anal sous ag, pose séton|examen anal sous ag: encollage fistule|examen anal sous ag: fistule acr|extraction corps étranger|extraction ce intra rectal", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Abcès de marge anale (et variantes orthographiques)
      str_detect(INTERVENTION, regex("abcès marge anale|abces marge anale|abcès de marge|abces de marge|abcès marge \\+|abcès de MA|abces MA|abcès MA \\+ séton", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Abcès péri-anal / fessier (hors marge anale)
      str_detect(INTERVENTION, regex("abces peri anale|abcès périnéal|abcès périnéal|abcès périnéal|abcès fesse|abces fesse|Abcès de fesse|abcès fesse", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Drainage de masse anale
      str_detect(INTERVENTION, regex("drainage ma|drainage masse anale", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Fissure anale et fissurectomie
      str_detect(INTERVENTION, regex("fissure anale|fissurectomie", ignore_case = TRUE)) ~ "Fissure anale",
      
      # Examen procto simple
      str_detect(INTERVENTION, regex("^examen procto$", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Fistule anale (sans fistule anastomose oesogastrique / aorto-duodénale)
      str_detect(INTERVENTION, regex("fistule anale|fistule anus|fisutle anale", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Hémorroïdes classiques et Milligan Morgan
      str_detect(INTERVENTION, regex("hémorroïdes|hemorroide|milligan morgan", ignore_case = TRUE)) ~ "Hémorroïdes",
      
      # Recoupe Baulieu, Babcock, Beaulieux et variantes orthographiques
      str_detect(INTERVENTION, regex("recoupe baulieu|babcock|babcok|beaulieux", ignore_case = TRUE)) ~ "Recoupe Baulieu / Babcok",
      
      # Abaissement fistule anale (geste spécifique)
      str_detect(INTERVENTION, regex("abaissement fistule anale", ignore_case = TRUE)) ~ "Abaissement fistule anale",
      
      # Exclure fistule anastomose oesogastrique et fistule aorto-duodénale de la proctologie
      str_detect(INTERVENTION, regex("fistule anastomose oesogastrique|fistule aorto-duodénale", ignore_case = TRUE)) ~ INTERVENTION_GROUPÉE,
      
      # Par défaut, garder la catégorie déjà existante
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )



df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # 1) Eventration simples
      str_detect(INTERVENTION, regex("^eventration$", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("^eventration médiane$", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("^eventration diaphragmatique$", ignore_case = TRUE)) ~
        "Eventration diaphragmatique",
      str_detect(INTERVENTION, regex("étranglée", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("eventration", ignore_case = TRUE)) ~
        "Eventration étranglée",
      
      # 2) Cas très spécifiques de cure d’éventration
      str_detect(INTERVENTION, regex("cure d'?éventration.*sous costale", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("cure d'?éventration.*lombaire gauche", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("cure d'?éventration.*médiane.*proth[eè]se retromusculaire", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("cure d'?éventration.*plaque RM", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("cure d'?éventration.*orifice de trocard", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("cure d'?éviscération couverte étranglée", ignore_case = TRUE)) ~
        "Cure d'éviscération couverte",
      
      # 3) Cure d’éventration par approche
      str_detect(INTERVENTION, regex("cure d'?éventration", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("cure d'?éventration|cure d'eventration", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("coelio|coelioscopie", ignore_case = TRUE)) ~
        "Cure d'éventration (coelio)",
      str_detect(INTERVENTION, regex("^cure d'?éventration$", ignore_case = TRUE)) ~
        "Cure d'éventration",
      
      # 4) Variantes « lipome » associées
      str_detect(INTERVENTION, regex("lipome", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("cure d'?éventration|Cure d'éventation|Cure d'éventration|Cure d'évetration", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      
      # 5) Cas d’éviscération isolée
      str_detect(INTERVENTION, regex("cure d'?éviscération|éviscération couverte", ignore_case = TRUE)) ~
        "Eviscération",
      str_detect(INTERVENTION, regex("^éviscération$|^eviscération$|^evisceration$", ignore_case = TRUE)) ~
        "Eviscération",
      
      # 6) Dépacking (souvent sur le même thème)
      str_detect(INTERVENTION, regex("^depacking$", ignore_case = TRUE)) ~
        "Depacking",
      
      # 7) Sinon, laisser ce qui était déjà groupé
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

library(dplyr)
library(stringr)

# 1) Standardise la casse et enlève les accents pour matcher plus simplement
df <- df %>%
  mutate(
    .INT_clean = stringi::stri_trans_general(INTERVENTION, "Latin-ASCII") %>%
      str_to_lower()
  )

# 2) Rattrapage global des éventrations restantes
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # Ne toucher que si c'était encore NA
      is.na(INTERVENTION_GROUPÉE) & str_detect(.INT_clean, "eventr") ~ {
        # Distingue les cas particuliers
        case_when(
          str_detect(.INT_clean, "diaphragmat")    ~ "Eventration diaphragmatique",
          str_detect(.INT_clean, "etrangl")        ~ "Eventration étranglée",
          str_detect(.INT_clean, "coelio|coelioscopie") ~ "Cure d'éventration (coelio)",
          str_detect(.INT_clean, "laparo")         ~ "Cure d'éventration (laparo)",
          TRUE                                     ~ "Cure d'éventration"
        )
      },
      TRUE ~ INTERVENTION_GROUPÉE
    )
  ) %>%
  select(-.INT_clean)

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # Corrige précisément ces deux variantes
      is.na(INTERVENTION_GROUPÉE) &
        INTERVENTION %in% c("Cure d'éventation", "Cure d'évetration") ~
        "Cure d'éventration (laparo)",
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # Fermetures de stomie
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("fermeture.*stomie", ignore_case = TRUE)) ~
        "Fermeture de stomie",
      # Fermetures d'iléostomie
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("fermeture.*il[ée]ostomie|fermeture.*ileo|fermeture.jej*", ignore_case = TRUE)) ~
        "Fermeture d'iléostomie",
      # Rétablissement de continuité (fermeture de stomie + anastomose)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("r[eé]tablissement.*continuit|r[eé]fection.*stomie", ignore_case = TRUE)) ~
        "Rétablissement de continuité",
      # Résections de grêle
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("r[eé]section.*gr[êe]le", ignore_case = TRUE)) ~
        "Résection de grêle",
      # Prolapsus (iléostomie, stomiale…)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("prolapsus", ignore_case = TRUE)) ~
        "Réparation de prolapsus",
      # Sinon on garde ce qui était déjà groupé
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # … ton code précédent …
      
      # 8) Exploration (laparo/coelio/explo)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("laparotomie expl|laparo explo|coelio explo|coelioscopie explo|exploration", ignore_case = TRUE)) ~
        "Exploration",
      
      # 9) Procédures interventionnelles
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("PIPAC|TIPS|PAC|biopsie|embolisation|drainage|endosponge|dilatation|réparation portale|fistule", ignore_case = TRUE)) ~
        "Procédure interventionnelle",
      
      # 10) Sinon on laisse NA ou ce qui est déjà groupé
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # … ton code précédent …
      
      # 11) Stomie de décharge (iléostomie, jéjunostomie, stomie)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("iléostomie|jéjunostomie|stomie", ignore_case = TRUE)) &
        !str_detect(INTERVENTION, regex("fermeture", ignore_case = TRUE)) ~
        "Stomie digestive",
      
      # 12) Fermeture de stomie (colo/ilé/jejuno-stomie)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("fermeture.*(stomie|ilé|colo|jejuno)", ignore_case = TRUE)) ~
        "Fermeture de stomie",
      
      # 13) Résection de grêle (y compris diverticule de Meckel)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("résection.*gr[êe]le|resection.*grêle|meckel", ignore_case = TRUE)) ~
        "Résection de grêle",
      
      # 14) Amputation abdomino-périnéale (APR)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("amputation abdomino.*péri", ignore_case = TRUE)) ~
        "Amputation abdomino-périnéale",
      
      # 15) TEM (chirurgie transanale)
      is.na(INTERVENTION_GROUPÉE) &
        regex("^TEM$", ignore_case = TRUE) %>% str_detect(INTERVENTION) ~
        "TEM (chirurgie transanale)",
      
      # 16) Vaginoplastie
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("vaginoplastie|vagino", ignore_case = TRUE)) ~
        "Vaginoplastie",
      
      # 17) Curage ganglionnaire
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("curage|courage ganglionnaire", ignore_case = TRUE)) ~
        "Curage ganglionnaire",
      
      # 18) Sinus pilonidal
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("sinus pilonidal|kyste pilonid", ignore_case = TRUE)) ~
        "Sinus pilonidal",
      
      # 19) Ablation d’anneau gastrique
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("ablation anneau gastrique", ignore_case = TRUE)) ~
        "Ablation anneau gastrique",
      
      # 20) Sinon, on laisse ce qui était groupé (ou NA pour la suite)
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # … ton code précédent …
      
      # 21) Cytoréduction (toutes approches)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("cyto(réduction|reduction)|debulking|PIPAC", ignore_case = TRUE)) ~
        "Cytoréduction (laparo)",
      
      # 22) Gestion des abcès
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("abc[eè]s|drainage.*abc[eè]s", ignore_case = TRUE)) ~
        "Drainage d’abcès",
      
      # 23) Splénectomie (open, coelio ou robot)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("spl[eé]nectomie", ignore_case = TRUE)) ~
        "Splénectomie",
      
      # 24) Toutes les occlusions sur bride restantes
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("occlu|bride", ignore_case = TRUE)) ~
        "Occlusion sur bride (coelio)",
      
      # 23) Amputation abdomino-périnéale (AAP)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("^(AAP|AAP + Taylor|Amput AP|Amputation abdopérinéale  |Amput.*abdomino[- ]?périnéale)$", ignore_case = TRUE)) ~
        "Amputation abdomino-périnéale (coelio)",
      
      # 24) Sinon, on laisse ce qui était groupé (ou NA pour la suite)
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

library(stringr)
library(dplyr)

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      
      # 1) Exploration abdominale (laparo/ceolio explo / peritonite / carcinose)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("ceolio explo|laparo(?:explo)?|peritonite|carcinose", ignore_case = TRUE)) ~
        "Laparotomie exploratrice",
      
      # 2) Diverticulectomie œsophagienne
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("diverticule", ignore_case = TRUE)) ~
        "Diverticulectomie œsophagienne",
      
      # 3) Sinus pilonidal
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("sinus pi", ignore_case = TRUE)) ~
        "Sinus pilonidal",
      
      # 4) Achalasie → myotomie de Heller
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("achalasie", ignore_case = TRUE)) ~
        "Myotomie de Heller",
      
      # 5) Exérèses sous-cutanées (kyste, lipome, fibrome, lésions cutanées…)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("ex[eé]r(e|èse)|boulectomie|lipome|kyste|fibrome", ignore_case = TRUE)) ~
        "Exérèse sous-cutanée",
      
      # 6) Drainage chirurgical (hématome, VAC, abcès, peritonite…)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("lavage|drain|evacuat|vac|abc[eè]s|peritonite", ignore_case = TRUE)) ~
        "Drainage chirurgical",
      
      # 7) Anastomoses vasculaires complexes
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("anastomose.*cave|d[ée]riv(ation|ation)|désobstruction portale", ignore_case = TRUE)) ~
        "Anastomose / dérivation vasculaire",
      
      # 8) Éviscération
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("évisc[ée]ration", ignore_case = TRUE)) ~
        "Éviscération",
      
      # 9) Prothèse portale / TIPS
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("proth[eè]se portale|tips", ignore_case = TRUE)) ~
        "Pose / révision de TIPS",
      
      # 10) Examen anal / proctologique
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("exam(ination)? anal|fissure|fistule|procto", ignore_case = TRUE)) ~
        "Examen anal",
      
      # 11) Rétablissement de continuité (réfections, réinsertions)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("réfecti|réinser|resta?blis", ignore_case = TRUE)) ~
        "Rétablissement de continuité",
      
      # 12) Jéjunostomie / grêle
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("j[ée]jun|gr[êe]le", ignore_case = TRUE)) ~
        "Intervention grêle / jéjunostomie",
      
      # 13) Rectopexie
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("rectopexie|kraske", ignore_case = TRUE)) ~
        "Rectopexie (coelio)",
      
      # 14) Ulcère perforé → ulcère perforé (coelio)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("ulc[eè]re.*perfor", ignore_case = TRUE)) ~
        "Ulcère perforé (coelio)",
      
      # 15) Volvulus → occlusion sur bride
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("volvulus", ignore_case = TRUE)) ~
        "Occlusion sur bride (coelio)",
      
      # ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
      # Les 3 catégories que vous vouliez :
      #   • Cytoréduction 
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("cyto(réduction|reduction)", ignore_case = TRUE)) ~
        "Cytoréduction (laparo)",
      
      #   • Abcès de marge / Fournier
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("abc[eè]s|fournier", ignore_case = TRUE)) ~
        "Abcès périnéal / Fournier",
      
      #   • Splénectomie
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("spl[eé]nectomie", ignore_case = TRUE)) ~
        "Splénectomie (coelio)",
      
      #   • AAP / amputation abdomino-périnéale
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("\\bAAP\\b|Amputation abd", ignore_case = TRUE)) ~
        "Amputation abdomino-périnéale (coelio)",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

table(df$INTERVENTION_GROUPÉE)

df %>%
  filter(is.na(INTERVENTION_GROUPÉE)) %>%
  count(INTERVENTION, sort = TRUE) %>%
  print(n = Inf)

# Code pour regrouper les 22 derniers intitulés d'interventions
# En s'inspirant des groupements déjà réalisés dans le script de regroupement

# Charger les librairies nécessaires
library(dplyr)

# Appliquer les nouveaux regroupements
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # Les groupements existants restent inchangés
      !is.na(INTERVENTION_GROUPÉE) ~ INTERVENTION_GROUPÉE,
      
      # === NOUVEAUX REGROUPEMENTS POUR LES 22 INTITULÉS RESTANTS ===
      
      # 1. Correction de faute de frappe + regroupement avec catégorie existante
      INTERVENTION == "Ablation anneau gatsrqiue" ~ "Ablation anneau gastrique",
      
      # 2. Interventions digestives spécialisées
      INTERVENTION == "Diversion duodénale" ~ "Duodénectomie",
      INTERVENTION == "Resection et refection anastomose grelo grelique" ~ "Résection de grêle",
      
      # 3. Explorations diverses (traumatiques, diagnostiques, thérapeutiques)
      INTERVENTION == "Extraction corps etranger" ~ "Exploration",
      INTERVENTION == "Lap explo + résection nodule coupole diaph" ~ "Exploration",
      INTERVENTION == "Plaie abdo arme blanche perfo estomac" ~ "Exploration",
      INTERVENTION == "Plaie arme à feu" ~ "Exploration",
      INTERVENTION == "explo paroi" ~ "Exploration",
      INTERVENTION == "torsion testiculaire" ~ "Exploration",
      
      # 4. Drainages et débridements
      INTERVENTION == "Gangrène fesse droite" ~ "Drainage chirurgical",
      INTERVENTION == "décaillotage" ~ "Drainage chirurgical",
      
      # 5. Interventions thyroïdiennes
      INTERVENTION == "Isthméctomie thyrodienne" ~ "Lobo-isthmectomie",
      
      # 6. Exérèses sous-cutanées et superficielles
      INTERVENTION == "KSC" ~ "Exérèse sous-cutanée",  # KSC = Kyste Sébacé Cutané
      INTERVENTION == "Omphalectomie" ~ "Exérèse sous-cutanée",
      INTERVENTION == "Résection nodule pariétal" ~ "Exérèse sous-cutanée",
      
      # 7. Procédures interventionnelles spécialisées
      INTERVENTION == "Pose de pansement intrabdominal" ~ "Procédure interventionnelle",
      INTERVENTION == "Trachéotomie" ~ "Procédure interventionnelle",
      INTERVENTION == "ovariectomie bilatérale sous coelio" ~ "Procédure interventionnelle",
      
      # 8. Réparations et reconstructions
      INTERVENTION == "Prolpasus stomial" ~ "Réparation de prolapsus",
      INTERVENTION == "abdominoplastie" ~ "Cure d'éventration",
      
      # 9. Curage et cytoréduction
      INTERVENTION == "Récidive ganglionnaire corticosurrénalome" ~ "Curage ganglionnaire",
      INTERVENTION == "débulking pseudomyxome" ~ "Cytoréduction (laparo)",
      
      # Garder les valeurs NA comme NA (interventions non renseignées)
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df %>%
  filter(is.na(INTERVENTION_GROUPÉE)) %>%
  count(INTERVENTION, sort = TRUE) %>%
  print(n = Inf)


# =====================================================================
# CODE À AJOUTER À LA TOUTE FIN DE "script regroupement.R"
# (Après tous les autres regroupements existants)
# =====================================================================

cat("=== APPLICATION DES CORRECTIONS FINALES ===\n")

# === CORRECTION 1 : CLASSIFICATIONS CROISÉES ===
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      
      # === CORRIGER LES INTERVENTIONS MAL CLASSÉES DANS "HERNIE INGUINALE" ===
      
      # Reclasser les CHIP/cytoréductions
      INTERVENTION_GROUPÉE == "Hernie inguinale" & 
        str_detect(INTERVENTION, regex("CHIP|cyto", ignore_case = TRUE)) ~ "Cytoréduction (laparo)",
      
      # Reclasser les hernies hiatales vers RGO  
      INTERVENTION_GROUPÉE == "Hernie inguinale" & 
        str_detect(INTERVENTION, regex("hiatale", ignore_case = TRUE)) ~ "Cure RGO (coelio)",
      
      # Reclasser les abcès inguinaux
      INTERVENTION_GROUPÉE == "Hernie inguinale" & 
        str_detect(INTERVENTION, regex("abcès inguinal", ignore_case = TRUE)) ~ "Drainage d'abcès",
      
      # === AJOUTER LES VRAIES HERNIES INGUINALES NON CLASSÉES ===
      
      # Hernies inguinales TEP et TAPP non classées
      str_detect(INTERVENTION, regex("hernie inguinale.*TEP|hernie inguinale.*TAPP|Hi engouée TAPP", ignore_case = TRUE)) ~ "Hernie inguinale (coelio)",
      
      # Reprises de hernies inguinales
      str_detect(INTERVENTION, regex("reprise hernie inguinale", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      # Lichtenstein pour récidives
      str_detect(INTERVENTION, regex("Licht.*récidive|Licht.*reprise", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      # === CORRIGER LES HERNIES OMBILICALES (règle plus stricte) ===
      
      # D'abord, reclasser les interventions mal capturées par la règle actuelle
      INTERVENTION_GROUPÉE == "Hernie ombilicale" & 
        !str_detect(INTERVENTION, regex("\\bHO\\b|hernie.*omblic|omblic.*hernie|cure.*omblic|ligne blanche", ignore_case = TRUE)) ~ "Exploration",
      
      # Reclasser les hernies ligne blanche vers leur propre catégorie
      INTERVENTION_GROUPÉE == "Hernie ombilicale" & 
        str_detect(INTERVENTION, regex("ligne blanche|éventration.*ligne", ignore_case = TRUE)) ~ "Hernie ligne blanche",
      
      # Garder tous les autres regroupements
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

# === CORRECTION 2 : INTERVENTIONS NON GROUPÉES RESTANTES ===
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # Ne modifier que les interventions non groupées (NA)
      !is.na(INTERVENTION_GROUPÉE) ~ INTERVENTION_GROUPÉE,
      
      # Thoracotomies → Exploration
      str_detect(INTERVENTION, regex("thoracotom|thoraco|boerhaave|decorticat", ignore_case = TRUE)) ~ "Exploration",
      
      # Cholangios → Procédure interventionnelle
      str_detect(INTERVENTION, regex("cholangio", ignore_case = TRUE)) ~ "Procédure interventionnelle",
      
      # Cholécystectomie mal orthographiée
      str_detect(INTERVENTION, regex("cholécystectomoie", ignore_case = TRUE)) ~ "Cholécystectomie (coelio)",
      
      # Ablation phéochromocytome → Surrénalectomie
      str_detect(INTERVENTION, regex("ablation phéochromocytome|phéochromocytome", ignore_case = TRUE)) ~ "Surrénalectomie (coelio)",
      
      # Réfections anastomoses → Exploration
      str_detect(INTERVENTION, regex("refection anastomose", ignore_case = TRUE)) ~ "Exploration",
      
      # Garder les NA comme NA
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

# =====================================================================
# CODE COMPLET POUR ANALYSER LES GESTES DU TOP 5 DES INTERVENTIONS LES PLUS AIDÉES
# =====================================================================

library(dplyr)
library(stringr)
library(gt)
library(purrr)

# === ÉTAPE 1 : IDENTIFIER LE TOP 5 DES INTERVENTIONS LES PLUS AIDÉES ===

# Résumé avec n >= 20 + calcul du pourcentage + exclusions
df_resume_intervention <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE)) %>%
  filter(!INTERVENTION_GROUPÉE %in% c("Pose de TIPS", "Autre", "Exérèse sous-cutanée", "Procédure interventionnelle", "Stomie digestive")) %>%
  group_by(INTERVENTION_GROUPÉE) %>%
  summarise(
    total_interventions = n(),
    gestes_realises = sum(Geste == "Yes", na.rm = TRUE),
    pct_gestes_realises = 100 * gestes_realises / total_interventions,
    .groups = "drop"
  ) %>%
  filter(total_interventions >= 10)

# Top 5 interventions les plus aidées
top_5_plus_aidees <- df_resume_intervention %>%
  arrange(desc(pct_gestes_realises)) %>%
  slice(1:5)

noms_top5 <- top_5_plus_aidees$INTERVENTION_GROUPÉE

# === ÉTAPE 2 : FONCTION DE REGROUPEMENT DES GESTES ===

regrouper_gestes <- function(geste_text) {
  if (is.na(geste_text)) {
    return(NA)
  }
  
  # Convertir en minuscules pour faciliter la détection
  geste_lower <- tolower(as.character(geste_text))
  
  # Règles de regroupement
  if (str_detect(geste_lower, "tout")) {
    return("Tout")  # Tout est exclusif des autres
  } else if (str_detect(geste_lower, "anastomose")) {
    return("Anastomose (+)")  # Peut être associée à d'autres
  } else if (str_detect(geste_lower, "dissection")) {
    return("Dissection (+)")  # Peut être associée à d'autres  
  } else if (str_detect(geste_lower, "paroi|incision|fermeture")) {
    return("Paroi (+)")  # Incision ou fermeture, peut être associée à d'autres
  } else {
    return("Autre")  # Pour les cas non classés
  }
}

# Appliquer le regroupement
df <- df %>%
  mutate(
    GESTE_GROUPE = map_chr(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout, regrouper_gestes)
  )

# === ÉTAPE 3 : ANALYSER LES GESTES POUR LE TOP 5 ===

# Fonction d'analyse pour une intervention
analyser_gestes_intervention <- function(nom_intervention) {
  gestes_detail <- df %>%
    filter(INTERVENTION_GROUPÉE == nom_intervention) %>%
    filter(Geste == "Yes") %>%  # Seulement les cas où il y a eu un geste
    filter(!is.na(GESTE_GROUPE)) %>%
    count(GESTE_GROUPE, sort = TRUE) %>%
    mutate(
      pourcentage = round(100 * n / sum(n), 1)
    )
  
  if (nrow(gestes_detail) > 0) {
    return(tibble(
      Intervention = nom_intervention,
      Geste_principal = gestes_detail$GESTE_GROUPE[1],
      Pourcentage = gestes_detail$pourcentage[1],
      Effectif = gestes_detail$n[1],
      Total_gestes = sum(gestes_detail$n)
    ))
  } else {
    return(tibble(
      Intervention = nom_intervention,
      Geste_principal = "Aucun",
      Pourcentage = 0,
      Effectif = 0,
      Total_gestes = 0
    ))
  }
}

# Analyser toutes les interventions du top 5
resultats_gestes_top5 <- map_dfr(noms_top5, analyser_gestes_intervention)

# === ÉTAPE 4 : CRÉATION DU TABLEAU FINAL ===

# Enrichir avec les données du résumé original
tableau_final <- top_5_plus_aidees %>%
  left_join(resultats_gestes_top5, by = c("INTERVENTION_GROUPÉE" = "Intervention")) %>%
  mutate(
    Label_intervention = paste0(
      INTERVENTION_GROUPÉE, " (",
      gestes_realises, "/", total_interventions, ", ",
      round(pct_gestes_realises, 1), "%)"
    ),
    Label_geste = paste0(
      Geste_principal, " (",
      Effectif, "/", Total_gestes, ", ",
      Pourcentage, "%)"
    )
  ) %>%
  select(
    Intervention = Label_intervention,
    `Geste le plus représenté` = Label_geste,
    `% gestes totaux` = pct_gestes_realises,
    `% geste principal` = Pourcentage
  )

# === ÉTAPE 5 : AFFICHAGE DES RÉSULTATS ===

cat("📊 TOP 5 DES INTERVENTIONS LES PLUS AIDÉES ET LEURS GESTES PRINCIPAUX\n")
cat("====================================================================\n")

print(tableau_final)

# Créer un tableau gt() pour un affichage plus élégant
tableau_gt <- tableau_final %>%
  gt() %>%
  tab_header(
    title = "Top 5 des interventions les plus aidées",
    subtitle = "Geste le plus représenté pour chaque intervention"
  ) %>%
  fmt_number(
    columns = c(`% gestes totaux`, `% geste principal`),
    decimals = 1,
    suffix = "%"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

print(tableau_gt)

# === ÉTAPE 6 : ANALYSE DÉTAILLÉE OPTIONNELLE ===

cat("\n🔍 ANALYSE DÉTAILLÉE PAR INTERVENTION\n")
cat("=====================================\n")

for (i in 1:length(noms_top5)) {
  intervention <- noms_top5[i]
  
  cat("\n", i, ".", intervention, "\n")
  cat(paste(rep("-", nchar(intervention) + 4), collapse=""), "\n")
  
  detail <- df %>%
    filter(INTERVENTION_GROUPÉE == intervention) %>%
    filter(Geste == "Yes") %>%
    filter(!is.na(GESTE_GROUPE)) %>%
    count(GESTE_GROUPE, sort = TRUE) %>%
    mutate(
      pourcentage = round(100 * n / sum(n), 1),
      label = paste0(GESTE_GROUPE, ": ", n, " cas (", pourcentage, "%)")
    )
  
  if (nrow(detail) > 0) {
    for (j in 1:nrow(detail)) {
      cat("  • ", detail$label[j], "\n")
    }
  } else {
    cat("  Aucun geste documenté\n")
  }
}

# === RÉSUMÉ FINAL ===
cat("\n🎯 RÉSUMÉ EXÉCUTIF\n")
cat("==================\n")

for (i in 1:nrow(tableau_final)) {
  intervention_courte <- str_extract(tableau_final$Intervention[i], "^[^(]+")
  geste_principal <- str_extract(tableau_final$`Geste le plus représenté`[i], "^[^(]+")
  pct_geste <- tableau_final$`% geste principal`[i]
  
  cat(paste0(i, ". ", str_trim(intervention_courte), " → ", str_trim(geste_principal), " (", pct_geste, "%)\n"))
}



















library(dplyr)
library(gt)

# =====================================================================
# TOP 5 DES INTERVENTIONS SUR LESQUELLES LES INTERNES SONT LE MOINS AIDÉS
# =====================================================================

cat("📊 ANALYSE : TOP 5 DES INTERVENTIONS LES MOINS AIDÉES\n")
cat("====================================================\n")

# === ÉTAPE 1 : CALCULER LES TAUX DE GESTES POUR TOUTES LES INTERVENTIONS ===

df_resume_moins_aidees <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE)) %>%
  # Exclusions (même logique que pour le top 5 des plus aidées)
  filter(!INTERVENTION_GROUPÉE %in% c("Pose de TIPS", "Autre", "Exérèse sous-cutanée", "Procédure interventionnelle", "Stomie digestive")) %>%
  group_by(INTERVENTION_GROUPÉE) %>%
  summarise(
    total_interventions = n(),
    gestes_realises = sum(Geste == "Yes", na.rm = TRUE),
    pas_de_geste = sum(Geste == "No", na.rm = TRUE),
    pct_gestes_realises = 100 * gestes_realises / total_interventions,
    pct_pas_de_geste = 100 * pas_de_geste / total_interventions,
    .groups = "drop"
  ) %>%
  # Filtre sur les interventions avec au moins 15 cas
  filter(total_interventions >= 15)

cat("✅ Nombre d'interventions avec ≥15 cas :", nrow(df_resume_moins_aidees), "\n\n")

# === ÉTAPE 2 : TOP 5 DES INTERVENTIONS LES MOINS AIDÉES ===

top_5_moins_aidees <- df_resume_moins_aidees %>%
  arrange(pct_gestes_realises) %>%  # Trier par taux de gestes CROISSANT (les plus faibles en premier)
  slice(1:5) %>%
  mutate(
    Label_complet = paste0(
      INTERVENTION_GROUPÉE, " (",
      gestes_realises, "/", total_interventions, ", ",
      round(pct_gestes_realises, 1), "%)"
    )
  )

# === ÉTAPE 3 : AFFICHAGE DES RÉSULTATS ===

cat("🎯 TOP 5 DES INTERVENTIONS LES MOINS AIDÉES (≥15 interventions)\n")
cat("==============================================================\n\n")

for (i in 1:nrow(top_5_moins_aidees)) {
  intervention <- top_5_moins_aidees$INTERVENTION_GROUPÉE[i]
  gestes <- top_5_moins_aidees$gestes_realises[i]
  total <- top_5_moins_aidees$total_interventions[i]
  pct <- round(top_5_moins_aidees$pct_gestes_realises[i], 1)
  
  cat(paste0(i, ". ", intervention, "\n"))
  cat(paste0("   → ", gestes, "/", total, " gestes réalisés (", pct, "%)\n"))
  cat(paste0("   → ", total - gestes, "/", total, " sans geste (", round(100 - pct, 1), "%)\n\n"))
}

# === ÉTAPE 4 : TABLEAU FORMATÉ POUR PRÉSENTATION ===

tableau_moins_aidees <- top_5_moins_aidees %>%
  select(
    Intervention = INTERVENTION_GROUPÉE,
    `Effectif total` = total_interventions,
    `Gestes réalisés` = gestes_realises,
    `Pas de geste` = pas_de_geste,
    `% gestes` = pct_gestes_realises,
    `% sans geste` = pct_pas_de_geste
  ) %>%
  mutate(
    `% gestes` = round(`% gestes`, 1),
    `% sans geste` = round(`% sans geste`, 1)
  )

# Affichage du tableau
print(tableau_moins_aidees)

# === ÉTAPE 5 : TABLEAU GT ÉLÉGANT ===

tableau_gt_moins_aidees <- tableau_moins_aidees %>%
  gt() %>%
  tab_header(
    title = "Top 5 des interventions les moins aidées",
    subtitle = "Interventions avec ≥15 cas, classées par taux de gestes croissant"
  ) %>%
  fmt_number(
    columns = c(`% gestes`, `% sans geste`),
    decimals = 1,
    suffix = "%"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_fill(color = "#ffebee"),  # Fond rouge clair pour les faibles taux
    locations = cells_body(columns = `% gestes`)
  ) %>%
  tab_style(
    style = cell_fill(color = "#e8f5e8"),  # Fond vert clair pour les forts taux de "pas de geste"
    locations = cells_body(columns = `% sans geste`)
  )

print(tableau_gt_moins_aidees)

# === ÉTAPE 6 : STATISTIQUES DESCRIPTIVES ===

cat("📈 STATISTIQUES DESCRIPTIVES\n")
cat("============================\n")

cat("Taux de gestes moyen (toutes interventions ≥15 cas) :", 
    round(mean(df_resume_moins_aidees$pct_gestes_realises), 1), "%\n")

cat("Taux de gestes médian (toutes interventions ≥15 cas) :", 
    round(median(df_resume_moins_aidees$pct_gestes_realises), 1), "%\n")

cat("\nTop 5 - Taux de gestes min :", 
    round(min(top_5_moins_aidees$pct_gestes_realises), 1), "%\n")

cat("Top 5 - Taux de gestes max :", 
    round(max(top_5_moins_aidees$pct_gestes_realises), 1), "%\n")

# === ÉTAPE 7 : COMPARAISON AVEC LE TOP 5 DES PLUS AIDÉES (si disponible) ===

# Calculer le top 5 des plus aidées pour comparaison
top_5_plus_aidees_comp <- df_resume_moins_aidees %>%
  arrange(desc(pct_gestes_realises)) %>%
  slice(1:5)

cat("\n🔄 COMPARAISON AVEC LE TOP 5 DES PLUS AIDÉES\n")
cat("=============================================\n")

cat("MOINS AIDÉES (top 5) :\n")
cat("- Taux moyen :", round(mean(top_5_moins_aidees$pct_gestes_realises), 1), "%\n")
cat("- Écart type :", round(sd(top_5_moins_aidees$pct_gestes_realises), 1), "%\n")

cat("\nPLUS AIDÉES (top 5) :\n")
cat("- Taux moyen :", round(mean(top_5_plus_aidees_comp$pct_gestes_realises), 1), "%\n")
cat("- Écart type :", round(sd(top_5_plus_aidees_comp$pct_gestes_realises), 1), "%\n")

ecart_moyennes <- mean(top_5_plus_aidees_comp$pct_gestes_realises) - mean(top_5_moins_aidees$pct_gestes_realises)
cat("\n📊 Écart entre les moyennes :", round(ecart_moyennes, 1), "points de pourcentage\n")

# === ÉTAPE 8 : EXPORT POUR RAPPORT ===

cat("\n📋 RÉSUMÉ POUR VOTRE PRÉSENTATION\n")
cat("=================================\n")

cat("Top 5 des interventions les MOINS aidées (≥15 cas) :\n\n")

for (i in 1:nrow(top_5_moins_aidees)) {
  intervention <- top_5_moins_aidees$INTERVENTION_GROUPÉE[i]
  pct <- round(top_5_moins_aidees$pct_gestes_realises[i], 1)
  total <- top_5_moins_aidees$total_interventions[i]
  
  cat(paste0(i, ". ", intervention, " : ", pct, "% (", total, " cas)\n"))
}

# Créer une liste simple pour export
liste_moins_aidees <- top_5_moins_aidees %>%
  pull(Label_complet)

cat("\n📊 LISTE FORMATÉE POUR EXPORT :\n")
for (i in 1:length(liste_moins_aidees)) {
  cat(paste0(i, ". ", liste_moins_aidees[i], "\n"))
}



