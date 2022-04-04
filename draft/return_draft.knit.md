---
output:
  # bookdown::word_document2:
    # reference_docx: "word-template.docx"
  bookdown::pdf_document2:
    toc: no
    number_sections: no
    pandoc_args: !expr rmdfiltr::add_wordcount_filter(rmdfiltr::add_citeproc_filter(args = NULL))
    latex_engine: xelatex
always_allow_html: true
header-includes:
# - \usepackage{setspace}\doublespace
  # - \usepackage{endnotes}
  # - \let\footnote=\endnote
  # - \usepackage{endfloat}
# - \setlength{\headheight}{14.5pt}
- \setlength{\headheight}{13.6pt}
- \usepackage{fancyhdr}
- \pagestyle{fancy}
- \rhead{N.I. Hoffmann}
- \lhead{March 29, 2022}
editor_options: 
  chunk_output_type: console

citeproc: no
fontfamily: mathpazo
fontsize: 11pt
geometry: margin=1in
indent: yes
link-citations: yes
linkcolor: blue
lang: 'en-US'

bibliography: "/Users/nathan/Documents/My Library.bib" 
# bibliography: "My Library.bib"  
csl: apa.csl
# csl: american-sociological-association.csl

title: "Strangers in the Homeland? The Academic Performance of Children of Return Migrants in Mexico"
# subtitle: "ASA 2022 Submission"
author: |
  | Nathan I. Hoffmann
  | Department of Sociology, UCLA

abstract: "The number of return migrants from the U.S. to Mexico has swelled in recent years, and yet we know little about the academic performance of the over 500,000 U.S.-born children who have accompanied them. This paper harnesses PISA test score data to compare U.S.-born children of return migrants in Mexico to two groups: Mexican-born students in Mexico, and students in the U.S. born to Spanish-speaking immigrant parents. Contrary to previous work highlighting the academic struggles faced by children of return migrants, these adolescents attain higher PISA scores than their Mexican-born counterparts. This advantage persists in models that control for both pre- and post-migration family characteristics. However, these adolescents' scores are much lower than similar youths in the U.S., and controlling for variables related to immigrant selection barely changes estimates of disparities. Furthermore, results vary little by possible moderators. Overall, these findings suggest that these often forcibly displaced adolescents quickly assimilate to the relatively low educational standards of Mexican schools, highlighting the importance of institutional factors in the assimilation process."

  
date: "March 29, 2022"
  
---












<!-- Despite decades of research into migration flows between Mexico and the U.S., the migration process of children -- including return and circular migration -- is only recently receiving concerted scholarly attention. A flurry of theoretical work on return and circular migrant children in Mexico has followed the calls of @orellana_2001_transnational and @dobson_2009_unpacking to center children in migration research.  -->


<!-- **Notes for the Migration Working Group**: Thanks for reading my paper! I welcome any and all feedback and suggestions. I'm especially interested in how well my theoretical discussion works, whether the hypotheses are helpful, and whether the presentation of results is clear. I'm also wondering if I'm stretching the term "0.5 generation" too much from its original meaning, and if I should just call these students "U.S.-born children of return migrants." I'm hoping to send this to either a generalist sociology or demography journal, and I'm curious which you think is more appropriate.  -->

# Introduction

Despite an abundance of research on return migration, few studies focus on children of these returnees. @azose_2019_estimation estimate that the greatest number of return migrants in the world are from the United States to Mexico. Due to both rising deportations as well as economic recession [@durand_2019_evolution], 1.3 million Mexican migrants in the U.S. made the journey back to Mexico between 2010 and 2015 [@azose_2019_estimation]. Children of these Mexican return migrants now constitute a sizable population in Mexico, and yet they are only recently receiving scholarly attention. Estimates suggest that about 500,000 American-born minors live in Mexico, constituting about 2 percent of school enrollment [@masferrer_2021_return, p. 39]. Following Víctor Zúñiga and colleagues [-@zuniga_2018_generation; @zuniga_2018_ninas; @zuniga_2021_children], I refer to these children as the "0.5 generation": children of Mexican immigrants who are born in the U.S. and later migrate to Mexico.  

What does it mean to assimilate into a society where ethnic, cultural, and legal barriers are at a minimum? Assimilation theory might predict a smooth transition, and yet the small-scale studies that exist emphasize the struggles that these children face in adapting to school in Mexico. Rich ethnographic work describes their difficulty with written language, invisibility to teachers, and stigma and exclusion from other youths [@zuniga_2018_generation; @zuniga_2018_ninas; @zuniga_2021_children; @hamann_2006_pensando; @hernandez-leon_2020_imperfect; @despagne_2019_adaptation; @bybee_2020_estamos; @meyers_2014_outsiders; @bybee_2020_estamos]. But not all research depicts these adolescents as strangers in their homeland. @despagne_2019_adaptation find that, over time, return migrant children are able to reposition themselves, capitalizing on their binational and bicultural assets to succeed in school. Other scholars recount that, despite these youths' challenges in adjusting to daily life in Mexico, teachers characterize them as "star students" in their academic achievement [@bybee_2020_estamos].  

How can we reconcile this divergence? Do the expectations of assimilation theory bear out on a national level? Because Mexico lacks national standardized testing [@santibanez_2021_contrasting], we do not know which findings generalize to the broader population of the U.S.-born 0.5 generation. Large-scale, nationally representative data are needed to determine whether the typical experience of these students is of academic advantage or disadvantage. Furthermore, if disparities exist, we do not know whether this is due to the challenges of migration or a process of selection of which families choose or are forced to migrate back to Mexico.  

To help answer these questions, this paper relies on the Program for International Student Assessment (PISA) to characterize the educational achievement of U.S.-born children of return migrants in Mexico. PISA assesses 15-year-olds’ reading, math, and science skills every three years in dozens of countries worldwide [@schleicher_2019_pisa]. This paper makes two sets of comparisons. First, to assess the degree of integration or assimilation into their ancestral society, I compare U.S.-born children of return migrants to local non-migrants. Second, I assess how these children's academic trajectories might have progressed had their parents not return-migrated, comparing these children to a similar, counterfactual group in the U.S. I also investigate the selection of return migrant parents and assess how it may impact their children's achievement. <!-- how the educational situation of the 0.5 generation has changed over time.-->  

I find that previous studies have overstated the academic challenges faced by children of return migrants in Mexico. Compared to Mexican adolescents, members of the 0.5 generation perform as well or better on PISA tests. These effect estimates are nearly identical in models that control for pre- and post-migration attributes and those that do not. Furthermore, these effects vary little by gender, locality, or age at migration. On the other hand, the 0.5 generation obtains much lower scores than similar adolescents in the U.S. Although selection into migration accounts for some of the gap, disparities remain. Adolescents in Mexican rural areas obtain especially low scores, on average, compared to counterparts in the U.S.  

By presenting results from both sides of the border side by side, this paper paints a complicated picture. On the one hand, these adolescents retain some educational advantage compared to their Mexican-born peers. Bicultural resources, greater familiarity with standardized tests, the prestige of experience in the U.S. may help bolster their PISA scores. But the starker trend is of educational assimilation: counterfactual estimates suggest that moving to Mexico results in an immediate fall in PISA scores to just above the relatively low levels. In the context of under-resourced Mexican schools [@santibanez_2021_contrasting], rapid educational assimilation weakens their educational standing.  

This paper makes three larger contributions. First, it highlights the prime importance that institutional factors can assume in the assimilation process. Results vary little across individual attributes, implying that larger institutional forces common to the full sample -- such as the Mexican school system -- play the main role in facilitating rapid educational assimilation. Second, this paper builds on the legacy of segmented assimilation theory [@portes_1993_new] and other work highlighting the downsides of rapid assimilation [e.g. @valenzuela_1999_subtractive]. Rapid assimilation may not be a desirable outcome in many contexts. Finally, this paper pushes for consideration of an increasingly significant group in Mexico and other countries of emigration: foreign-born children of return migrants.


<!-- These findings suggest that conclusions about the 0.5 generation depend heavily on the choice of reference group and showcase the strength of the dissimilation framework. Although these children struggle on PISA compared to their U.S. counterparts, they tend to perform as well or better than their Mexican classmates. In this sense, they are assimilating to the relatively low educational average of the under-resourced Mexican school system [@santibanez_2021_contrasting]. -->



# Background
## Research Context
The Mexico-U.S. migration system has changed dramatically in the 21st century [@durand_2019_evolution]. After decades of high migration from Mexico to the U.S., the number of Mexican immigrants in the U.S. peaked at 12.8 million in 2007 and has declined since, numbering 11.4 million in 2019 [@gonzalez-barrera_2021_covid19]. This fall is due both to lower rates of entry and rising rates of return [@durand_2019_evolution]. Pew estimates that from 2005 to 2010, 1.39 million Mexican immigrants returned to Mexico from the U.S., and 710,000 returned between 2013 and 2018 [@gonzalez-barrera_2021_covid19].

The bulk of return flows has been undocumented immigrants. From 2010 to 2018, 2.6 million undocumented Mexican immigrants returned to Mexico [@warren_2020_reverse]. A rapid increase in deportations is responsible for a large part of this outflow [@durand_2019_evolution, p. 37], but @warren_2020_reverse highlights that 45 percent of undocumented Mexican immigrants returned voluntarily. Precarious economic conditions in the U.S. and a stabilizing Mexican economy have increased incentives to return [@gonzalez-barrera_2021_covid19].  

As economic conditions and U.S. immigration policy have changed, so has the composition of Mexican return migrants. @campos-vazquez_2012_selfselection find a shift from positive to negative selection when compared to the full Mexican population: in 1990, return migrants had more years of schooling and higher wages than their stay-at-home compatriots, whereas in 2010 they tended to have less schooling and earn lower wages. @parrado_2016_changing argue that the lower wages of return migrants in 2010 compared to 1990 and 2000 are due to the less voluntary nature of return migration in recent years, and they also find decreases in entrepreneurship and the ability to remain inactive. For deportees, accumulation of valuable skills and capital is cut short [@cassarino_2004_theorising]. Using data from 2011 to 2013, @diaz_2016_moving show that negative selection also characterizes comparisons to Mexican immigrants who stay in the U.S.; for example, return migrants are less likely to possess a high school or college degree.  

Children are virtually absent from most discussions of return migration, yet their numbers are significant. In 2000, the Mexican census counted 258,000 U.S.-born minors; by 2010, this had more than doubled to 570,000 [@masferrer_2019_immigrants]. This number has remained steady in the years since: the 2020 Mexican census reports that 500,600 minors, or about 2 percent of the school-age population, were born in the U.S. [@masferrer_2021_return, p. 39].  

Although quantitative work remains scarce, qualitative researchers have begun to study these youths. Early work conceptualizing "(transnational) sojourner students" [@hamann_2001_theorizing; @hamann_2006_pensando] led to a large-scale survey in Nuevo León, Zacatecas, Puebla, and Jalisco to identify hundreds of students who had been born or had spent significant time in the U.S. [@zuniga_2018_ninas]. This research team dubs this population the "0.5 generation." Like the "1.5 generation" label for children who migrate at a young age [@portes_2001_legacies], "0.5 generation" foregrounds liminality; they cannot be easily grouped with either the native-born "0 generation" or the "first generation" of migrants more broadly, and this in-betweenness contributes to their particular challenges navigating an education system and society that feels both familiar and foreign.^[While Zúniga and colleagues also include minors born in Mexico who migrated to the U.S. at an early age and later returned, I more narrowly apply the "0.5 generation" label only to those born in the U.S.]

<!-- regardless of their place of birth, these children have experience abroad but currently reside in the country of their parents' birth. -->
<!-- Noticing that Mexican adult respondents in Georgia often held plans to return to Mexico with their children -- many of them U.S.-born -- Hamann [-@hamann_2001_theorizing, p. 32] defined a new category, "sojourner student," whose "two defining characteristics are their vulnerability to dislocation and their transnational background."  -->

The findings from the studies by Zúñiga, his colleagues, and other researchers working in this domain have been mixed. Most of this work emphasizes the difficulties that the 0.5 generation faces in the Mexican "homeland." Despite usually possessing oral proficiency in both Spanish and English, their written competency often lags behind [@gonzalez_2016_moving; @despagne_2019_adaptation; @gandara_2020_students; @santibanez_2021_contrasting]. Especially when they first arrive, their peers often treat them as outsiders [@meyers_2014_outsiders], and the stigma they face can be exacerbated by the political climate, such as the 2016 election [@bybee_2020_estamos]. Conflicts also occur psychologically, as these children struggle with their identity and feelings of belonging [@despagne_2019_adaptation; @bybee_2020_estamos]. Yet their appearance and oral fluency often renders them invisible to teachers [@sanchezgarcia_2016_educator], and resources for immigrant students are lacking in Mexico [@santibanez_2021_contrasting]. Even navigating the complicated bureaucracy to enroll in school or finding the money for transportation, uniforms, and school supplies may be out of reach for return migrant families [@gandara_2020_students; @mateos_2019_mestizo]. Furthermore, family separation is a frequent, stressful feature of migration that is common for the 0.5 generation as well [@gonzalez_2016_moving; @zuniga_2018_generation]. These factors are compounded by the harsh immigration regime in the U.S. that encourages and forces the departure of these families [@waldinger_2021_transnational].  

Yet not all work on return migrant children conceives of them as consigned to disadvantage. @zuniga_2018_ninas and @hernandez-leon_2020_imperfect see some of the children in their studies benefiting from dual nationality. Similarly, in an intensive ethnography in two schools in central Mexico, Bybee et al. [-@bybee_2020_estamos, p. 135] observe a "star student" discourse: "The notion of American-Mexican pupils as 'good students' was common enough in both schools that it extended beyond English classes where they had an obvious advantage." Although these students experienced bullying and exclusion due to their perceived foreignness, the privilege of binational education and other resources gave some of them a leg up in school.  


## Theories of Return Migration

How can we understand the mixed results from previous studies of the 0.5 generation in Mexico? Work on the 0.5 generation has not fully engaged with theories of return migrant integration more broadly, yet these theories can help explain both positive and negative outcomes of their U.S.-born children. In an influential review, @cassarino_2004_theorising attempts to synthesize theories of return migration. Early neoclassical economic theory assumes that return migration marks a failure to achieve one's goals [@todaro_1980_internal], resulting in economically disadvantaged return migrants. On the other hand, the New Economics of Labor Migration (NELM) considers return migration as evidence of *success* [@stark_1985_new]: when a migrant saves or remits adequate funds, return may be the next step in a long-term plan. Hence families of return migrants may enjoy economic advantage. Structural theories, on the other hand, suggest that institutional opportunities in the country of origin matter too: return happens when these appear hospitable [@cerase_1974_expectations].  

Transnationalism breaks the origin-destination dichotomy present in the other theories by conceiving of return migration as part of a circular system [@portes_1999_study; @basch_1994_nations]. Migrants often feel connected by common ethnicity to a multipolar diaspora, developing identity with the host and home countries. Upon return, they may experience alienation and marginalization, strengthening their transnational identities. Finally, network theorists do not view ties between migrants as given, as transnationalists often do, but shaped by strategic linkages and interpersonal experiences. Varying degrees of network embeddedness are possible, and social structure shapes returnees' success in their initiatives.  

In his resource mobilization-preparedness framework, @cassarino_2004_theorising attempts to integrate these theories and more explicitly incorporate forced return. Resource mobilization refers to the tangible and intangible resources (including social capital) that a person gains during the migration experience, while preparedness encompasses a migrant's readiness and willingness to return. Migrants who are prepared to return *and* have acquired significant resources while abroad are likely to succeed in the home country. On the other hand, migrants who mobilize few resources and are not prepared for return -- as is often the case with deportees -- are more susceptible to economic hardship and marginalization in the home country.  

Despite its strength in explaining both positive and negative return outcomes, Cassarino's theory does not develop a role for origin-country context. In their review of sociological studies of return migration, @hagan_2020_return make a corrective: "We argue that reintegration pathways of return migrants depend not only on (a) the accumulation of resources and (b) readiness for return, as argued by Cassarino, but also on (c) sending state, institutional, and family contexts of reception and (d) opportunities to mobilize resources in local economies to which migrants return." In an origin context that stigmatizes return migrants, or where the institutions for capital and skill investment are insufficient, the children that accompany them are likely to suffer, regardless of individual resources.  

<!-- Finally, scholarship of return migration has done little to engage with broader theories of immigrant integration. Yet children of return migrants constitute a fascinating test case for assimilation theory. Alba and Nee [-@alba_2003_remaking, p. 11] define assimilation as "the decline of an ethnic distinction and its corollary cultural and social differences." In this case, distinctions are virtually absent from the start. Gone are most markers of social difference, including cultural, ethnic, and legal. Similarly, Portes and his collaborators [@portes_1993_new; @portes_2001_legacies; @portes_2014] suggest that assimilation occurs most smoothly when immigrants are free from racial discrimination and other social markers of difference, a large coethnic community exists, and government policy is relatively favorable (e.g., immigrants are not given unfavorable legal statuses). Here, these all hold; birth and experience abroad are the only salient factors distinguishing immigrants from "natives." Virtually free from cultural and legal barriers, does an immigrant status still make a difference to these adolescents' education? The theories of Portes and colleagues and Alba and Nee suggest that, without such barriers, assimilation will proceed smoothly. And yet previous studies document many struggles. This paper aims to rectify this apparent contradiction. -->


<!-- The implicit assumption is that, as natives of the society to which they return, return migrants -->


<!-- Studies of immigrant integration often attempt to disentangle individual from institutional factors that affect the trajectories of immigrants. Theories of assimilation suggest that without legal and cultural barriers, assimilation should be easy. So why does previous qualitative work on the 0.5 generation show significant struggles in school? -->






## Empirical Expectations

These frameworks help explain the variety of experiences that children in the 0.5 generation might have. First, they help us understand possible *disadvantage* for the 0.5 generation. The perspective of neoclassical economics suggests that return migrant families may not found lucrative employment in the host country. Even in cases where they have, the transnationalist perspective emphasizes that alienation and exclusion in the "home" society can still afflict return migrants and their children as they struggle with their identities [@despagne_2019_adaptation; @bybee_2020_estamos]. Institutions for investment and activation of resources gained in the U.S. may also be lacking [@hagan_2020_return, p. 539]. In addition, migrants who are not ready and willing to migrate -- as in the case of deportation -- are less likely to possess the resources to ease re-entry into Mexican society. The result can be traumatic for children, and may give rise to the difficulties documented in the ethnographic research cited above. These expectations lead to what I call the *rough re-entry hypothesis*:

> Compared to Mexican-born children in Mexico, children of return migrants attain lower PISA scores.  

Yet the 0.5 generation may also be among the privileged in Mexico. The NELM perspective highlights how voluntary return migration can be evidence of economic success, with parents accumulating sufficient resources to lead a comfortable life in Mexico. Following Cassarino's theory, if children benefit from resource-rich social networks and parents who have achieved their goals in acquiring skills and capital while abroad, and if the family is ready and willing to move, they are more likely to be well supported upon return to the parents' country of origin. Scholars suggest that at least some of these children benefit from resources such as dual nationality, bicultural facility, and experience in better resourced schools [@gandara_2021_students]. Furthermore, in the absence of most social markers of difference, assimilation theory predicts that integration should be straightforward and rapid [@alba_2003_remaking; @portes_1993_new]. Referencing the "star student discourse" documented by @bybee_2020_estamos, the *star student hypothesis* predicts the following:

> Compared to Mexican-born children in Mexico, children of return migrants attain higher PISA scores.  

If the rough re-entry hypothesis is correct, then these students' challenges may be connected to return-migrant selection. For return migrants from the U.S. to Mexico, negative selection on characteristics such as education and income has predominated in recent years [@campos-vazquez_2012_selfselection; @parrado_2016_changing]. Observing which migrant families both voluntarily and involuntarily return to Mexico, Hernández-León et al. [-@hernandez-leon_2020_imperfect, p. 94] suggest that "U.S. policies [...] effectively externalize downward assimilation to communities of origin." Migrants who return to Mexico from the U.S. may be those having the roughest time in the host country; hence their children are a selected group, more likely to struggle in the homeland due to disadvantage accrued before return. Furthermore, less-educated migrant parents are less likely to find lucrative work and more likely to be targeted for deportation, so controlling for family background may narrow disparities. I call this third set of expectations the *selection hypothesis*: 

> Compared to children of Spanish-speaking immigrants in the U.S., children of return migrant children in Mexico attain lower PISA scores. However, in models adjusting for family background and resources, this disparity diminishes.  




# Data

Data for the results below come from the pooled 2012, 2015, and 2018 waves of PISA. As shown in Table \@ref(tab:stats), my sample of interest comprises 465 children in Mexico who were born in the U.S. to two Mexican-born parents. I compare these to 40,710 children born in Mexico to two Mexican-born parents as well as 926 children born to two Spanish-speaking immigrant parents in the U.S. Although PISA data for the U.S. do not include exact country of birth for parents, about two-thirds of the Hispanic population in the U.S. is of Mexican origin [@trevelyan_2016_characteristics, p. 12], rendering this an appropriate pseudo-control group.^[Previous studies show similar achievement among U.S.-born children of Spanish-speaking immigrants from a variety of origins. Using data from the Immigration and Intergenerational Mobility in Metropolitan Los Angeles (IIMMLA) survey and Children of Immigrants Longitudinal Study (CILS-III) for San Diego, Rumbaut [-@rumbaut_2008_coming, p. 222] finds that the high school grades of second-generation Mexican, Salvadoran, and Guatemalan students are very similar. Similarly, models by @glick_2007_academic show that in the Early Childhood Longitudinal Study, third-grade math scores for children of Mexican, Puerto Rican, and other Hispanic origin are indistinguishable.]  

Outcome variables consist of reading, math, and science scores, which the OECD constructs to have a global mean of 500 and standard deviation of 100. I use the first five plausible values for reading, math, and science scores, combining estimates and calculating standard errors as suggested by the PISA Technical Reports.^[Although the 2015 and 2018 waves of PISA contain 10 plausible values, the 2012 data contains only 5. In pooled analysis of these different test years, I follow @teltemann_2020_standardized and @jerrim_2017_global in using five plausible values for all tests.]    

Pre-migration control variables include mother's and father's education (measured in 6-category ISCED, entered as a continuous variable in regressions), cultural possessions, home educational resources, age, a categorical variable for early childhood education and care (ECEC), survey year, and two-category gender. I also consider post-migration variables as possible mediators. These include composite variables constructed by the OECD which I standardize to have a mean of 0 and standard deviation of 1 within waves: household wealth, home possessions, home information and communication technology (ICT) resources, and an index of economic, social and cultural status. I also include highest parental occupational status measured in the International Socio-Economic Index [@ganzeboom_1992_standard] and a dichotomous measure of urban locality, with one category for small and large cities and another for villages, small towns, and towns.



# Methods

For the 0.5 generation, how do we define "positive" or "negative" outcomes in school? Much depends on the comparison group and estimand [@lundberg_2021_what]. The default comparison group for nearly all studies of immigrant education are locals without a migration background. Such a comparison is useful because it alerts policymakers and educators to disparities that can have lasting impacts on inequality. Yet such a perspective neglects immigrant selection. As discussed above, the composition of Mexican return migrants has transformed dramatically in the face of changing economics and immigration policy in the U.S. Such immigrant selection has important implications for the education of their children. Feliciano [-@feliciano_2005_does; @feliciano_2017_immigrant] shows that the children of immigrants with more education than their stay-at-home compatriots -- i.e., those who are "positively selected" -- tend to attain more schooling. Yet we know little about the impact of immigrant selection on children's schooling in the context of return migration.  

Immigrant selection can be incorporated into statistical models of children's education in two ways. It can be directly included as a covariate measuring relative educational attainment, at either the group [@feliciano_2005_does] or individual [@feliciano_2017_immigrant] level. Alternatively, the researcher can find a suitable comparison group that implicitly accounts for immigrant selection. For example, @hoffmann_2020_winwin compares young Eastern European migrants in Western Europe to similar co-nationals who stayed behind in the East. This second approach has the benefit of accounting for more than measured educational selectivity: many skills are not captured by measures of formal education [@hagan_2015_skills], and immigrants are selected in ways not related to skills at all.  

This paper makes both types of comparisons. First, it places U.S.-born children of Mexican return migrants in the Mexican educational context by comparing them to local Mexican children without migration backgrounds, providing an estimate of the descriptive gap in achievement between locals and the 0.5 generation. This set of analyses follows in the tradition of assimilation literature, showing how similar to locals these migrant children have become. Second, it compares these children to a suitable group in the U.S.: Spanish-speaking children of immigrants in the U.S. The target here is a causal estimand: the scores these children would have obtained in the U.S. had they not migrated to Mexico. Finally, this paper explicitly examines immigrant selection by comparing and incorporating attributes of parents of these children in Mexico and the U.S. By considering both the U.S. and Mexico, this paper employs a cross-border perspective [@waldinger_2017_crossborder] that sheds far more light on the process and outcomes of migration than a single-country analysis would.  

<!-- These results quantify the phenomenon of "dissimilation": while assimilation refers to a process of becoming more similar to locals, dissimilation describes how immigrants become different from those they left behind [@fitzgerald_2012_comparativist].  -->

I present difference-in-means and OLS regression estimates comparing children of Mexican return migrants to both the Mexican and U.S. comparison samples. I incorporate pre-migration and post-migration attributes in turn as covariates in the OLS regressions to examine selection and integration processes. I also test the influence of the potential moderators of gender, rural/urban location, and age at migration by performing OLS regressions on subsets of the data. In all analyses, I cluster HC1 standard errors at the school level and incorporate final sampling weights.

<!-- For the Mexican-born sample, I also perform coarsened exact matching (CEM) using the automatic coarsening algorithm from the `cem` package in R [@iacus_2012]. The U.S. comparison sample is too small for coarsened exact matching. -->


\begin{table}

\caption{(\#tab:stats)Sample sizes and PISA score means. The Mexico country, U.S. birth country sample includes only children of Mexican immigrants. The U.S. country, U.S. birth country sample includes only children born to Spanish-speaking immigrant parents. Mean estimates incorporate sampling weights and account for clustering within schools.}
\centering
\begin{threeparttable}
\begin{tabular}[t]{llrrrr}
\toprule
Country of Residence & Birth Country & n & Reading & Math & Science\\
\midrule
Mexico & Mexico & 40,710 & 427 & 414 & 420\\
Mexico & U.S. & 465 & 434 & 434 & 429\\
U.S. & U.S. & 926 & 488 & 455 & 472\\
\bottomrule
\end{tabular}
\begin{tablenotes}[para]
\item \textit{Source:} 
\item PISA data for 2012, 2015, and 2018. Author's calculations.
\end{tablenotes}
\end{threeparttable}
\end{table}


# Results

## Comparisons Within the Mexican Context

The first set of analyses compares the 0.5 generation to local children in Mexico. Table \@ref(tab:stats) presents sample sizes, mean scores, and standard deviations for the three samples. Although mean scores for all groups in the table are below the global average of 500, U.S.-born children in Mexico tend to score somewhat higher than Mexican-born children, especially in math.  





![(\#fig:dim-mex)Difference-in-means (DIM) and OLS estimates comparing children of return migrants in Mexico to children in Mexico. Error bars represent 95% asymptotic confidence intervals. OLS (pre-migration) models adjust for parents' education, cultural possessions, home educational resources, age, ECEC, gender, and survey year. OLS (post-migration) models additionally adjust for household wealth; home possessions; home ICT resources; an index of economic, social and cultural status; urban or non-urban locality; and highest parental ISEI. All models cluster standard errors at the school level and incorporate sampling weights.](return_draft_files/figure-latex/dim-mex-1.pdf) 

Figure \@ref(fig:dim-mex) compares PISA scores of children of return migrants in Mexico to the local Mexican-born sample using difference-in-means (DIM) and ordinary least squares (OLS). (Coefficients for both OLS models are presented in the Supplementary Material.) The error bars show 95-percent asymptotic confidence intervals. We first consider the DIM estimates, which subtract the average reading, math, or science scores for children of return migrants from the scores for the comparison group. Compared to other adolescents in Mexico, children of return migrants perform somewhat better in all three subjects. The difference amounts to 6.9 in reading, 19 in math, and 9.2 in science. The math advantage is significant and reaches one-fifth of a standard deviation, while the differences are not significant for reading and science. The next set of estimates adjusts for pre-migration characteristics in a linear regression estimated by OLS. The advantage for each score does not diminish and is more precisely estimated. The 0.5 generation performs as well as or better than similar Mexican-born children, especially in math.  

These results are contrary to the rough re-entry hypothesis suggested by most previous work on return migrant children's experiences: rather than struggling academically, these children are doing at least as well or better than the general population of Mexican-born children. Hence the data provide partial support for the star student hypothesis: despite the travails of migration, these children are not academically disadvantaged compared to their Mexican peers, attaining somewhat higher scores, at least in math.

The third set of estimates, "OLS (post-migration)", includes possible post-migration mediators of the advantage possessed by members of the 0.5 generation. These include household wealth; home possessions; home information and communication technology (ICT) resources; an index of economic, social and cultural status; urban or non-urban locality; and highest parental occupational status (in ISEI). The academic advantaged enjoyed by the 0.5 generation does not diminish and is estimated more precisely. These models suggest that material circumstances do not account for these children's higher scores. It seems more likely that cultural and legal advantages of a bicultural education, better resourced U.S. schools, potential dual nationality, and freedom from social markers of difference have provided these children with advantages to succeed in school.  


![(\#fig:mod-desc-mex)Sample distributions of gender, school location, and age at arrival. The first two panels compare the U.S.-born 0.5 generation to Mexican-born locals, while the third panel shows the distribution only for the 0.5 generation. All estimates incorporate sampling weights.](return_draft_files/figure-latex/mod-desc-mex-1.pdf) 



![(\#fig:mod-est-mex)Moderators of the difference in PISA scores between 0.5 generation and Mexican-born adolescents. All estimates come from OLS models that adjust for the controls specified in the data section, cluster standard errors at the school level, and incorporate sampling weights.](return_draft_files/figure-latex/mod-est-mex-1.pdf) 

The next analyses consider possible moderators of these effects. How do results vary by gender, age at migration, and rural vs. urban locality? Generally, girls tend to surpass boys in school [@buchmann_2008_gender], and the advantage for immigrant girls may be even higher [@dronkers_2014_migrant]. Thus we might expect girls of the 0.5 generation to have an easier time adjusting to academic life in Mexico. The left panel of Figure \@ref(fig:mod-desc-mex) shows that the proportions of girls in the 0.5 generation and local Mexican samples are nearly identical. Does the academic performance of boys and girls of the 0.5 generation differ? The first estimates in Figure \@ref(fig:mod-est-mex) show the difference between girls of 0.5 generation and Mexican-born girls, derived from OLS models with the same control variables and standard error procedures as for the full sample above. Corresponding estimates for boys follow. Except for math, the 0.5-generation advantage for girls is greater than that for boys, but differences between genders are not statistically significant. Both groups achieve scores at least as high as those of Mexican-born adolescents.  

Worldwide, PISA scores tend to be lower in rural areas [@sullivan_2018_comparison; @echazarra_2019_learning]. @hamann_2021_what claim that the 0.5 generation is more likely to live in rural areas, where relative paucity of resources may contribute to disadvantage. The final set of estimates in Figure \@ref(fig:dim-mex) shows that including a measure of rurality does not change estimates of overall score disparities. But location could still moderate results. What proportion of the children in this sample live in rural areas, and how does achievement vary between rural and urban areas? PISA categorizes "school community" as village, small town, town, city, or large city. I test two possible dichotimizations: (1) village, small town, and town vs. city or large city, and -- in a stricter test of rural disadvantage -- (2) village vs. the other categories.  

The second panel of Figure \@ref(fig:mod-desc-mex) partially corroborates @hamann_2021_what: the 0.5 generation tends to live in somewhat less urban areas than the general population of Mexican children. However, the  proportions in villages and large cities are virtually identical. Figure \@ref(fig:mod-est-mex) shows that, contrary to expectations, score disparities do not vary by rurality. Point estimates in non-urban, urban, village, and non-village localities do not vary greatly, and differences between them are not statistically significant.  

Finally, I consider age at migration as a potential moderator. @despagne_2019_adaptation suggest that challenges faced by the 0.5 generation tend to fade with time. Furthermore, children who migrate at younger ages tend to score higher on achievement tests [@hermansen_2017_age; @lemmermann_2018_causal]. Are the results reported above due to a large proportion of children who migrated at a young age? The right-most panel in Figure \@ref(fig:mod-desc-mex) shows that the distribution of age at migration is skewed toward the younger years. In fact, a full 27 percent of the sample migrated to Mexico before the age of 1. Figure \@ref(fig:mod-est-mex) explores two possible divisions of arrival age, at 5 and 10 years. According to the figure, these age cut-offs do not make a difference. Whether these children migrate at a young or older age, they tend to perform as well or better than locals on PISA tests. This implies that experience in U.S. schools does not necessarily contribute to small advantage enjoyed by 0.5 generation. Overall, results vary little across all moderators tested; the math score advantage remains strong and significant, and point estimates never show a negative disparity for any subject.  


## Comparisons to the U.S. Context

We now turn to comparisons between the 0.5 generation in Mexico and children of Spanish-speaking immigrants in the U.S. This pseudo-control group allows us to address the counterfactual question: what would have happened to these children had they not migrated to Mexico? Put another way, how much have they dissimilated [@fitzgerald_2012_comparativist] from their counterparts in the U.S.? Descriptive statistics in Table \@ref(tab:stats) shows that although the 0.5 generation achieves somewhat higher scores than their Mexican-born peers, the U.S. comparison group performs much higher.  

![(\#fig:dim-us)Difference-in-means (DIM) and OLS estimates comparing children of return migrants in Mexico to children of Spanish-speaking immigrants in the U.S. Error bars represent 95% asymptotic confidence intervals. OLS (pre-migration) models adjust for parents' education, cultural possessions, home educational resources, age, ECEC, gender, and survey year. OLS (post-migration) models additionally adjust for household wealth; home possessions; home ICT resources; an index of economic, social and cultural status; urban or non-urban locality; and highest parental ISEI. All models cluster standard errors at the school level and incorporate sampling weights.](return_draft_files/figure-latex/dim-us-1.pdf) 

Figure \@ref(fig:dim-us) presents the disparities. Unadjusted difference-in-means estimates show stark negative differences: -54 in reading, -21 in math, and -43 in science. These amount to the 0.5 generation achieving math scores one-fifth of a standard deviation lower than their U.S. counterparts, and reading and science disparities are closer to half of a standard deviation. Despite being star students in Mexico, these children struggle on PISA exams much more than children of Spanish-speaking immigrants in the U.S. (Coefficients for both OLS models are presented in the Supplementary Material.)  


![(\#fig:mex-us-compare)Histograms comparing the family background of 0.5 generation to the chilren of Spanish-speaking immigrants in the U.S. in PISA 2012, 2015, and 2018. Estimates incorporate sampling weights.](return_draft_files/figure-latex/mex-us-compare-1.pdf) 

Are such large disparities due to selection into migration? Figure \@ref(fig:mex-us-compare) compares histograms of family attributes of the two samples. The top row shows two measures of pre-migration characteristics. First, the 0.5 generation is more likely to have been enrolled in early childhood education, and for longer. The top right panel compares  parents' highest years of education, showing that the 0.5 generation is likelier to have a parent with more years of education -- 12 years compared to the comparison group's 11 -- and also more likely to have a parent with a college degree. In this sense, the 0.5 generation appears to be *positively selected*: they benefit from greater personal and family educational advantages than their counterparts who remain in the U.S. This contradicts our expectations for the selection hypothesis.  

Hence we would expect that controlling for pre-migration characteristics will *not* account for disparities in PISA scores. Indeed, as shown in Figure \@ref(fig:dim-us), an OLS model that adjusts for these characteristics estimates slightly smaller gaps, but they remain for all three subjects.  

Since selection into migration on these measured characteristics does not account for disparities, could the post-migration experience play a role? The second row of Figure \@ref(fig:mex-us-compare) presents two measures of post-migration attributes. The bottom-left panel shows a composite measure of family wealth, based on household possessions. Despite their pre-migration advantages, the 0.5 generation has less material assets than the U.S. comparison group. The bottom-right panel presents the parents' highest ISEI. The parents of the 0.5 generation are somewhat more likely to be employed in more prestigious, less manual labor. The educational advantage enjoyed by parents of the 0.5 generation carries over to their occupation in Mexico, but it does not translate to comparable measures of wealth.  

The final set of estimates in Figure \@ref(fig:dim-us) adjusts for pre- and post-migration characteristics. Estimated disparities become even more negative, although they are not significantly different from the DIM estimates. This suggests that it is neither selection nor the post-migration material experience of the 0.5 generation that accounts for their lower achievement compared to the U.S. Rather, it is likely that the lower resources of Mexican schools as well as the stress of adapting to an unfamiliar society are the contributing factors.  



![(\#fig:mod-est-us)Moderators of the difference in PISA scores between 0.5 generation and children of Spanish-speaking immigrants in the U.S. All estimates come from OLS models that adjust for the controls specified in the data section, cluster standard errors at the school level, and incorporate sampling weights.](return_draft_files/figure-latex/mod-est-us-1.pdf) 

The final set of analyses examines factors that moderate the effect of migration. Figure \@ref(fig:mod-est-us) presents the same set of moderators as Figure \@ref(fig:mod-est-mex), fitting the "OLS (pre-migration)" model to subsets of the data. First, effects differ little by gender: girls and boys in the 0.5 generation in Mexico achieve lower scores than daughters and sons of Spanish-speaking immigrants in the U.S., respectively, and differences between them are not statistically significant.  

The urban-rural divide shows greater disparities, corroborating the expectations of @hamann_2021_what regarding the potential disadvantage of rural areas. The 0.5 generation in non-urban areas obtains PISA scores that are significantly lower than their non-urban U.S. counterparts. Restricting the sample to residents of villages reveals even starker disparities: estimated reading and science scores are more than one standard deviation below their U.S. counterparts in rural areas, and math scores lie half of a standard deviation below. On the other hand, expected PISA scores for the 0.5 generation in Mexican urban areas are indistinguishable from their urban counterparts in the U.S. This is not due to U.S.-specific urban disadvantage; in the U.S., children of Spanish-speaking immigrants in urban areas perform better than those in rural areas.^[For example, children of Spanish-speaking immigrants in urban areas obtain an average of 452 on math tests, while the average for those in non-urban areas is 423.] This contrasts the findings from the assimilation analyses above: although the 0.5 generation does not perform worse that similar Mexican children living in the same type of locality, students in rural areas in Mexico face significant disadvantage compared to their counterparts in rural areas in the U.S.  

Finally, we see little variation in effect by age of migration. If we restrict the analysis to children who migrated to Mexico below the age of 5 or 10, estimated disparities are just as negative as for children who migrated at older ages. Students in Mexican schools obtain lower PISA scores that children in the U.S.  



## Sensitivy Analysis

Results may be sensitive to choice of estimation strategy or omission of a confounding variable. To assess the robustness of results, this section presents results from alternative estimation techniques. The supplementary material also analyses the sensitivity of results to omitted confounders, finding that disparities between the 0.5 generation and children of Spanish-speaking immigrants in the U.S. are fairly robust to high levels of potential confounding.  



![(\#fig:alt-mods-fig)Alternate estimates comparing children of return migrants in Mexico to children in Mexico and children of Spanish-speaking immigrants in the U.S. Error bars represent 95% asymptotic confidence intervals. Models adjust for parents' education, cultural possessions, home educational resources, age, ECEC, gender, and survey year. OLS is the baseline model, PSM uses propensity score matching, matches on Mahalnobis distance, Optimal Full implements the matching algorithm from Hansen and Klopfer (2006), and IPW incorporates (stabilized) inverse propbability weights.](return_draft_files/figure-latex/alt-mods-fig-1.pdf) 

Figure \@ref(fig:alt-mods-fig) presents five types of estimates of the effect of being in the 0.5 generation. The left panel corresponds to the comparisons with Mexican-born adolescents from Figure \@ref(fig:dim-mex), and the right panel is related to Figure \@ref(fig:dim-us) and compares with children of Spanish-speaking immigrants in the U.S. The estimates labeled "OLS" reproduce the results for "OLS (pre-migration)" from those figures, and the other estimates test the robustness of this baseline estimate. The next three sets of estimates use forms from the MatchIt package [@ho_2011_matchit]. "PSM" and "Mahalanobis" present estimates 1:1 matching for propensity scores estimated from logistic regression [@rosenbaum_1983_central] and Mahalanobis distance, respectively. "Optimal Full" employs optimal full matching [@hansen_2006_optimal], which places all units into subclasses in order to minimize the sum of absolute within-subclass distances. Finally, "IPW" uses stabilized inverse probability weights [@austin_2015_moving, p. 3663], performing bivariate weighted OLS regressions with the full samples.   

Estimates are highly robust to choice of estimator. Across all specifications, the U.S.-born 0.5 generation outperforms Mexican-born locals in math, and they score lower across all subjects when compared to children of Spanish-speaking immigrants in the U.S.


<!-- "CEM 1" and "CEM 2" both use coarsened exact matching [@iacus_2012], reducing the complexity of variables by sorting them into a finite number of "bins" and then matching exactly on these. Any case that cannot be matched exactly on these coarsened variables is dropped. "CEM 1" uses the same matching variables as the OLS models, while "CEM 2" prevents dropping as many cases by combining parents' education into a single dichotomous variable taking the value of 1 if either parent has a high school degree and 0 if neither does.  -->




# Discussion and Conclusion

This paper aimed to quantify the educational situation of U.S.-born children of return migrants in Mexico. Previous research on this "0.5 generation" [@zuniga_2018_ninas] has presented a mixed picture: although these children face stigma, language difficulties, and bureaucratic hurdles [@gonzalez_2016_moving; @despagne_2019_adaptation; @zuniga_2018_generation; @gandara_2020_students; @mateos_2019_mestizo], some scholars have documented the advantages bestowed by bicultural facility and time in relatively well resourced U.S. schools, at least for some of these students [@zuniga_2018_ninas; @bybee_2020_estamos; @hernandez-leon_2020_imperfect]. To help settle this empirical question, I presented two types of comparisons using PISA data. First, in the tradition of assimilation and integration studies, I compared the 0.5 generation to the general population of Mexican 15-year-olds. Second, I compared these adolescents to a pseudo-control group: children of Spanish-speaking immigrants in the U.S. While the first comparison presents the descriptive perspective of Mexican society, the second gets closer to estimating a causal effect of migration, or the scores that these students would have obtained had their families not returned to Mexico. In addition, models that control for pre-migration attributes allow explicit examination of the effect of immigrant selection. By considering two countries in cross-border perspective [@waldinger_2017_crossborder], this paper yields deeper insights than analyses bounded by a single national unit.    

In the comparisons with Mexican children, I find few disparities: the 0.5 generation performs just as well Mexican children on reading and science PISA tests, and they outperform locals in math by one-fifth of a standard deviation. These effects remain nearly unchanged in models that control for both pre- and post-migration attributes, and the potential moderators of gender, urban or rural locality, and age at migration make little difference. Comparisons with the U.S. sample, however, tell a story of disadvantage. Juxtaposed with this pseudo-control group, children of Mexican return migrants obtain much lower scores, ranging from one-quarter to one-half of a standard deviation. Contrary to general work on Mexican return migrants, the parents of these adolescents are positively selected on education and work in slightly more prestigious occupations than their U.S. counterparts. Controlling for pre- or post-migration family characteristics does little to change effect estimates.  


<!-- This paper also offers a commentary on assimilation theory. Assimilation theory has been largely absent from discussion of return migration, and yet it is put into question by the 0.5 generation.  -->
Children of return migrants test the boundary of the immigrant category. Despite their nominal status as immigrants, these adolescents speak Spanish, appear ethnically Mexican, and have immediate access to citizenship. Theories of assimilation suggest that without legal and cultural barriers, assimilation should proceed quickly and smoothly [@alba_2003_remaking; @portes_1993_new]. And this is what this study finds: the 0.5 generation's academic achievement is very similar to the Mexican mainstream's. So why does most previous qualitative work on the 0.5 generation document significant struggles in school? A likely answer is the choice of reference group.  

The assimilation perspective often halts at the national border. With the local mainstream as the only comparison group, assimilation theory disregards the full life-course trajectory of immigrants. The cross-border perspective, on the other hand, places achievement in an international context, taking the perspective of the immigrants themselves. Moving from the U.S. to Mexico appears to negatively impact the achievement of the 0.5 generation, so it is no surprise that researchers working closely with these students document challenges and struggles. But, on average, these adolescents retain a slight advantage over their new classmates. Assimilation does proceed smoothly, but its endpoint is the relatively low educational average of Mexican schools.
<!-- What is of tantamount importance here is not the immigrants themselves -- indeed, their individual characteristics play little role in moderating their achievement -- but instead the education system in which they find themselves.   -->

<!-- As Santibañez [-@santibanez_2021_contrasting, p. 23] writes, "even the richest students in Mexico score only slightly higher on the international PISA test than the poorest students in the United States."  -->
Relatively advantaged family resources do not prevent a near-total assimilation to the educational outcomes of Mexican students, which tend to be low in a global perspective [@schleicher_2019_pisa]. <!-- Although the bottom-left panel of Figure \@ref(fig:mex-us-compare) suggests that the 0.5 generation is more materially deprived than similar youths in the U.S., models that control for family wealth estimate larger rather than smaller disparities.  -->
The most likely contributing factor is Mexican schools, which receive far fewer resources than those in the U.S. In 2014, Mexico spent 2,000 USD per pupil, while the U.S. spent 18,000 USD [@santibanez_2021_contrasting, p. 25]. Even when adjusted for purchasing power parity, this funding difference results in fewer resources for learning. The school day is short (4.5 hours in elementary and 7 hours in secondary school), few extracurricular or enrichment programs exist, and many teachers in Mexico must navigate hectic hours, shifting between schools and sets of students in the middle of the schoolday [@santibanez_2021_contrasting]. All of these strains are likely to lower student achievement. Yet the 0.5 generation retains a small advantage in math achievement compared to Mexican-born students. This may result from the bicultural resources or prestige bestowed by experience in the U.S. Alternatively, facility with standardized tests, which are much more common in the U.S. than Mexico, may enable their success in PISA, regardless of actual learning. A more sobering possibility is that bureaucratic hurdles prevent the most disadvantaged students from enrolling in school [@mateos_2019_mestizo] or they have dropped out completely [@zuniga_2020_migracion]; in either case they are excluded from the PISA sample. Further research is needed to explore these possibilities.

This study suggests a number of future directions for research on the 0.5 generation. First, this study has not been able to differentiate between voluntary and involuntary return migration. The threat of deportation has been shown to negatively impact student achievement [@gandara_2021_schools], and deportation itself is likely to have even more negative effects; future work should attempt to differentiate the effects of different types of return. Second, qualitative studies of the Mexican 0.5 generation have often pooled together students born in the U.S. and those born in Mexico who migrate to the U.S. at an early age [@zuniga_2018_generation; @zuniga_2018_ninas; @zuniga_2021_children]. Although identifying the latter is not possible in PISA data, future research should assess the achievement of this group as well. Lastly, we do not know if achievement varies by age. Previous work has shown that disparities for children of immigrants may be greatest at younger ages [@hoffmann_2018_cognitive], and educational parity at older ages may not translate to equality in the labor market [@lee_2021_asian, p. 192-193]. Future work should investigate achievement at younger ages as well as the school-to-work transition for the 0.5 generation.  

This study constitutes an important corrective to literature on the 0.5 generation. First, by showing that the educational situation of these students is one of advantage rather than disadvantage when compared to other students in Mexico, this study contradicts much previous research on this group that depicts these students as strangers in the homeland who struggle academically. Second, this paper shows that studies that focus on only one country fail to capture the importance of institutional context. The U.S. comparisons presented here show that migration from more- to less-advantaged educational contexts can entail unfortunate consequences for children's educational outcomes. Since many of these returns result from deportation of parents, this paper documents yet another way that U.S.-citizen children of undocumented immigrants are harmed by punitive immigration policy. 


<!-- We see that, indeed, these adolescents struggle in school relative to their peers in the U.S. Yet what appears to be  -->
<!-- PISA excludes 15-year-olds who have been unable to enroll in school, and these may be among the most disadvantaged of the 0.5 generation. -->
<!-- Differences from previous studies could be due to a number of reasons that future research should attempt to disentangle. -->


\newpage

# References