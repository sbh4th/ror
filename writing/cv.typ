// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): block.with(
    fill: luma(230), 
    width: 100%, 
    inset: 8pt, 
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != "string" {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    new_title_block +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: white, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: (),
  fontsize: 11pt,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)

  if title != none {
    align(center)[#block(inset: 2em)[
      #text(weight: "bold", size: 1.5em)[#title]
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)
#show: doc => article(
  margin: (x: 1.87cm,y: 1.87cm,),
  font: ("Arial",),
  fontsize: 11pt,
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)


#strong[Sam Harper - Curriculum Vitae]

#strong[1 Personal Information]

#emph[Current position]

- Full Professor, Department of Epidemiology, Biostatistics & Occupational Health, McGill University

#emph[Education:]

- PhD (#emph[Epidemiologic Science];), University of Michigan Ann Arbor, USA 2005.
- MSCPH (#emph[Epidemiology];), University of South Carolina, USA 1999
- BA (#emph[Biology];), Westminster College, USA, 1995

Website: #link("https://samharper.org")[#text(fill: blue)[samharper.org]]

#strong[2 Personal statement]

My background is well suited to direct this project. I have a strong track record of research funding and productivity. Since 2019 I have led or co-led 5 research grants (both small and large) and generated substantial research output, with nearly 6000 citations since 2020. I have supervised and mentored over 15 trainees while also serving as Director of the PhD Program in Epidemiology at McGill from 2014-2024.

This proposal brings together two areas of research that have been passions of mine over the course of my career: equity and research transparency. Throughout my research career I have have maintained a strong research interest in both the measurement and analysis of social inequalities in health, and I am eager to extend this work to assessing inequalities in how the grant peer system may (or may not) contribute to differences in funding success for vulnerable populations. I have also long maintained strong interests in metascience, reproducible research, and health equity, as noted below. Moreover, my intersest in this project also derives from my personal experience. Over the past several years my service to CIHR in the capacity of Scientific Officer for the Public, Community and Population Health Committee has generated important questions for me regarding fairness in the peer review process for grants that I hope to explore in more detail with this proposal.

My work past integrating surveillance and policy on health inequalities has had an impact on policy and practice in Canada and internationally. I was a co-founding member of the McGill Public Policy and Population Health Observatory (#link("https://www.3po.ca/")[#text(fill: blue)[3PO]];) since 2016. My international reputation as a scholar also led to an Endowed Professorship in #emph[Impact of Health and Social Policy on Health Inequalities] at Erasmus University Medical Center, Rotterdam from 2018-2021.

#strong[3 Most significant contributions]

Measuring and monitoring inequalities are crucial activities for providing evidence on the impact of interventions and policies on vulnerable populations, including for how structures such as grant peer review may disproportionately affect vulnerable subgroups. My prior work has provided key methodological insights and tools to advance the science of measuring health inequalities. Inequality is a value-laden construct, and my early work demonstrated the need to integrate ethical parameters into inequality measurement since different inequality metrics can provide diverging evidence on inequalities. This work led to two monographs solicited by the US National Cancer Institute and several highly-cited papers in top public health journals. My subsequent work in this area has been extended globally by my trainees.

My research on this topic has led to international recognition. The Pan American Health Organization (PAHO) translated one of my key papers into Spanish and my online health inequalities training #link("https://open.umich.edu/find/open-educational-resources/public-health/measuring-health-disparities")[#text(fill: blue)[module]] into Spanish and Portuguese. I was chosen to serve as a member of the World Health Organization’s (WHO) 2010-11 #emph[Scientific Resource Group on Equity Analysis and Research] from over 450 nominations from around the world. I drafted the first WHO guidelines on monitoring social inequalities in health, which served as the foundation for the the first WHO Handbook on Measuring Health Inequalities @Hosseinpoor:2013aa. I serve on the Steering Committee for the Public Health Agency of Canada’s #emph[Pan-Canadian Health Inequalities Reporting Initiative];. In 2022 I was invited to join the WHO’s Expert Review Group for the development of the second edition of their #emph[Health Inequality Monitoring Handbook];. I have been invited to provide technical advice and research workshops to PAHO, the US CDC, the Office of Environmental Justice at the US EPA, and the US National Cancer Institute. Most recently, I was invited by the US National Academy of Sciences, Engineering & Medicine to discuss my research on measuring inequalities for at their 2023 roundtable #link("https://www.nationalacademies.org/event/06-21-2023/the-ecosystem-of-health-equity-measures-a-workshop")[#text(fill: blue)[series]] on Population Health Improvement: #emph[The Ecosystem of Health Equity Measures: A Workshop];.

Relevant recent papers:

- Chen D, Momen NC, Ejlskov L, Bødkergaard K, Werenberg Dreier J, et al.~Socioeconomic inequalities in mortality associated with mental disorders: a population‐based cohort study. World Psychiatry 2025;24(1):92-102.
- Hetherington E\*, Darling E, Harper S, Nguyen F, Schummers L, et al.~Inequalities in access to prenatal care during the COVID‐19 pandemic: Analysis of a population‐based cohort. Paediatr Perinat Epidemiol 2024;38:291–301.
- Carabali M, Harper S, Lima Neto AS, Dos Santos De Sousa G, Caprara A, Restrepo BN, et al.~Decomposition of socioeconomic inequalities in arboviral diseases in Brazil and Colombia (2007-2017). Trans R Soc Trop Med Hyg 2022;116(8):717-726.
- Capurro DA\*, Harper S. Socioeconomic inequalities in health care utilization in Paraguay: Description of trends from 1999 to 2018. J Health Serv Res Policy 2022;27(3):180-189.
- Sreeramareddy CT, Harper S. Trends in educational and wealth inequalities in adult tobacco use in Nepal 2001-2016: secondary data analyses of four Demographic and Health Surveys. BMJ Open. 2019 Sep 6;9(9):e029712.

Foundational papers:

- Harper S, Lynch J. Measuring and Decomposing Health Inequalities. In: Oakes JM, Kaufman JS (eds). #emph[Methods in Social Epidemiology];, 2nd Edition; San Francisco, CA: Jossey-Bass, 2017, pp.~91-131.
- Harper S, King N, Meersman SC, Breen N, Reichman ME, Lynch J. Implicit value judgments in the measurement of health inequalities. Milbank Quarterly 2010;88:4-29.
- Harper S, Lynch J, Meersman SC, Breen N, Davis WW, Reichman ME. An overview of methods for monitoring social disparities in cancer with an example using trends in lung cancer incidence by socioeconomic position and race-ethnicity, 1992-2004. Am J Epidemiol 2008;167(8):889-99.
- Hosseinpoor AR et al.~Handbook on health inequality monitoring: with a special focus on low-and middle-income countries. World Health Organization, 2013

#emph[Metascience and Reproducible Research]

I also maintain a strong commitment to methodological transparency and reproducible science @Austin:2016ab@Hamra:2019aa@Harper:2019ab@Goldstein:2020aa. When possible, I have made the raw data and statistical code to reproduce the tables and figures in my team’s published papers freely available by uploading them to public repositories such as #link("https://dataverse.harvard.edu/dataverse/samharper")[#text(fill: blue)[Dataverse]] and Open Science Foundation #link("https://osf.io/profile/")[#text(fill: blue)[page]];. I have uploaded data and code for more than a dozen of my published papers, which have been downloaded over 12,000 times. I was invited to give a #link("https:samharper.org/reproducibility-workshop-2020")[#text(fill: blue)[workshop]];, #emph[Reproducible Research: Why and How];, by the Society for Epidemiological Research, which covered scientific integrity problems, design and analytic solutions, dissemination and examples of how to conduct reproducible research. I also sought out more explicit training and took the Research Transparency and Reproducibility Training (RT2) course given by the Berkeley Insititute for Transparent Social Science in 2019. Finally, I was was selected and served as a 2019-2020 Faculty Fellow at #link("https://www.projecttier.org/")[#text(fill: blue)[Project TIER]] that focuses on Teaching Integrity in Empirical Research.

Relevant papers:

- Austin N\*, Harper S, Kaufman JS, Hamra G. Challenges in Reproducing Results from Publicly Available Data: An Example of Sexual Orientation and Cardiovascular Disease Risk. J Epidemiol Community Health 2016 2016;70(8):807-12.
- Goldstein ND, Hamra GB, Harper S. Are Descriptions of Methods Alone Sufficient for Study Reproducibility? An Example From the Cardiovascular Literature. Epidemiology. 2020 Mar;31(2):184-188.
- Hamra GB, Goldstein ND, Harper S. Resource Sharing to Improve Research Quality. Journal of the American Heart Association. 2019;8(15):e012292.
- Harper S. A future for observational epidemiology: Clarity, credibility, transparency. Am J Epidemiol 2019;188:840-845.

#emph[Evaluating Interventions]

This project will also provide potential insights into how changes in the peer review system (e.g., virtual vs.~in-person panels) may have affected funding. I have a substantial track record, much of it with my co-applicant Arijit Nandi, in conducting studies to estimate the impact of interventions on overall outcomes and inequalities. Thematically, this work all focuses on moving beyond surveillance and associations to get credible causal estimates of social and economic intervention effects, largely using quasi-experimental and experimental methods. I have conducted extensive work on the impacts of US mandatory seat belt laws on seatbelt use @Harper:2014ab, traffic crash deaths @Harper:2017aa@Harper:2019aa, and police discrimination @Riddell:2020aa. I have also provided strong evidence on the impacts of medical marijuana legalization @Harper:2012ab@Windle:2022aa, tobacco taxes @Manivong:2017aa, abortion restrictions @Austin:2019aa@Hawkins:2023aa, clinical obstetric interventions @Hutcheon:2017aa@Hutcheon:2020aa@Hutcheon:2022aa, as well as studies looking at the impact of economic recessions on health @Harper:2015ab@Harper:2017ac.

To expand this work in 2016 I co-founded the Public Policy and Population Health Observatory with Arijit Nandi (#link("https://3po.ca")[#text(fill: blue)[3PO]];) at McGill to promote an integrated research agenda, create and publish policy databases, and evaluate the impacts of interventions on health. We have worked closely with academic and non-academic partners to develop and answer research questions and disseminate findings, as well as training doctoral students from McGill and six other countries. A large part of this work has produced high-profile papers on the impacts of paid leave policies @Nandi:2016aa@Nandi:2018aa. In addition to analysis of secondary data, through 3PO I have also (with partners) developed and implemented two large-scale randomized evaluations, one on daycare in India @Nandi:2020aa and one on a complex maternal and newborn health intervention in Tanzania @Hetherington:2023aa. Our evidence on affordable daycare impacts in India was recently #link("https://www.povertyactionlab.org/policy-insight/access-childcare-improve-womens-economic-empowerment")[#text(fill: blue)[featured]] by MIT’s Poverty Action Lab.

My work on the use and value of experimental and quasi-experimental designs for public health has led to international recognition from journals through solicited commentaries on the future of epidemiology @Harper:2012ac@Harper:2019ab. I was invited to give the keynote address at the 2019 Canadian Society for Epidemiology & Biostatistics Annual Meeting, as well as three international talks on the value of natural experiments for public health at #link("https://ucsf.app.box.com/s/jwyk8ge8gh3zqx4subkeyiuh7oyuhayy")[#text(fill: blue)[UC Berkeley]];, #link("https://drexel.edu/uhc/events-workshops/symposium/2015/")[#text(fill: blue)[Drexel]];, and #link("https://samharper.org/talk/kickoff/")[#text(fill: blue)[Erasmus University]];.

Relevant Recent Papers:

- Nandi A, Agarwal P, Chandrashekar A, Maloney S, Richardson R, Thakur L, Harper S. Access to affordable daycare and women’s mental health in Rajasthan, India: Evidence from a cluster-randomised social intervention. J Glob Health 2024;14:04063.
- Hetherington E\*, Harper S, Davidson R, Festo C, Lampkin N, Mtenga S, Teixeira C, Vincent I, Nandi A. Impact evaluation of the TAMANI project to improve maternal and child health in Tanzania. J Epidemiol Community Health 2023;77(6):410-6.
- Nandi A, Charters TJ, Quamruzzaman A, Strumpf EC, Kaufman JS, Heymann J, Mukherji A, Harper S. Health care services use, stillbirth, and neonatal and infant survival following implementation of the Maternal Health Voucher Scheme in Bangladesh: A difference-in-differences analysis of Bangladesh Demographic and Health Survey data, 2000 to 2016. PLoS Med 2022;19(8).
- Lopes FV\*, Bakx P, Harper S, Ravesteijn B, Van Ourti T. The effects of supported housing for individuals with mental disorders. Health Econ 2022;31(S2):115-133.
- Hawkins SS, Harper S, Baum CF, Kaufman JS. Associations between State-Level Changes in Reproductive Health Services and Indicators of Severe Maternal Morbidity. JAMA Pediatr 2023;177(1):93-95.
- Harper S. Would stronger seat belt laws reduce motor vehicle crash deaths? A semi-Bayesian analysis. Epidemiology 2019;30:380-7.
- Harper S, Palayew A. The annual cannabis holiday and fatal traffic crashes. Inj Prev 2019;25:433–437.

#strong[4 Other contributions to knowledge]

I have a strong record of service and my expertise is routinely sought by my peers. I have conducted over 180 peer reviews across a wide range of disciplines including medicine, epidemiology, public health, biostatistics, economics, and general social science. I have served on numerous grant panels for Canada (including several years as a Scientific Officer for CIHR) and international funding agencies, in addition to routinely serving a session organizer and reviewer of abstracts for annual meetings at the leading conferences in my discipline. I have also been a identified as an #emph[Top Reviewer] by Web of Science platform the and earned an award as a #emph[Top reviewers in Social Sciences - September 2019];.

#strong[Ad hoc reviews]

#emph[Epidemiology and Public Health]

American Journal of Public Health; Annals of Epidemiology; Epidemiology; International Journal of Epidemiology; Journal of Epidemiology & Community Health; Social Science & Medicine; Scandinavian Journal of Public Health; BMC Public Health; Paediatrics & Child Health; International Journal for Equity in Health; Caries Research; Journal of Urban Health; Health & Place; American Journal of Epidemiology; International Journal of Obesity; Cancer Management and Research; Open Medicine; International Journal of Pediatric Obesity; Cancer Epidemiology, Biomarkers & Prevention; British Journal of Cancer; BMC Cancer; European Journal of Public Health; Cancer Causes & Control; PLOS ONE; Addiction; Population Health Metrics; Global Health Action; Traffic Injury Prevention; Cancer; Oxford University Press (book proposal); Journal of Drug Issues; Environmental Health; Nature Communications Medicine; Nature; Nature Medicine; The Lancet Regional Health – Southeast Asia

#emph[General Medicine]

Annals of Internal Medicine; BMJ (top 10% of peer reviewers in 2007); The Lancet; PLOS Medicine; JAMA; New England Journal of Medicine; Canadian Medical Association Journal; JAMA Pediatrics; Canadian J Cardiology

#emph[Health Policy]

Milbank Quarterly; Health Affairs; Health Policy; Health Services Research; Evaluation Review

#emph[Social Science]

Demography; Population Studies; Social Compass; Social Science History; Sociological Focus; Journal of Health and Social Behavior; Population Research and Policy Review; Journal of Health and Social Behavior

#emph[Biostatistics]

Journal of the Royal Statistical Society (A); Statistics in Medicine

#emph[Economics]

B.E. Journal of Economics and Policy; Health Economics; Oxford Development Studies; Journal of Economic Issues; Applied Economics Letters; American Journal of Economics & Sociology

#strong[Grant Reviews]

#emph[Panel Member]

#figure(
  align(center)[#table(
    columns: (15%, 85%),
    align: (left,left,),
    [2018-2023], [Scientific Officer, Canadian Institutes for Health Research (CIHR), Public, Community & Population Health Committee],
    [2017-2018], [Member, Canadian Institutes for Health Research (CIHR), Public, Community & Population Health Committee],
  )]
  , kind: table
  )

#emph[Ad hoc reviews]

#figure(
  align(center)[#table(
    columns: (15%, 85%),
    align: (left,left,),
    [2021], [Robert Wood Johnson Foundation Interdisciplinary Research Leaders Competition],
    [2019], [CIHR Canada Research Chairs (Tier 1)],
    [2019], [Faculty of Medicine, Internal grant review (2)],
  )]
  , kind: table
  )

#emph[Recent Invited Lectures, Talks, Presentations]

In the past 6 years I have also been invited to give international lectures on the history and application of multilevel models in social epidemiology, as well as talks on the measurement of health inequalities, and quasi-experimental study designs.

#figure(
  align(center)[#table(
    columns: (15%, 85%),
    align: (left,left,),
    [2024], [Keynote Address: #emph[Multilevel Models in Social Epidemiology: Past, Present, and Future.] Danish Epidemiology Society Annual Meeting, Aarhus University, Aarhus, DK Nov 7, 2024],
    [2024], [Invited Workshop: #emph[Difference-in-Differences for Social Epidemiology];. Danish Epidemiology Society Annual Meeting, Aarhus University, Aarhus, DK Nov 6, 2024],
    [2024], [#emph[Despair and Disadvantage: Some Questions] Presentation given at the MORTAL workshop, Nuffield College, Oxford University, UK Jun 10-11, 2024],
    [2023], [#emph[Measuring Health Equity: Beyond Description.] US National Academy of Sciences, Engineering, & Medicine, The Ecosystem of Health Equity Measures: A Workshop, Oakland, CA, USA June 21, 2023 (remote)],
    [2023], [#emph[A Quarter Century of Multilevel Models in Social Epidemiology: A Short Story in Three Acts.] Society for Epidemiologic Research Annual Meeting, Portland, OR, USA June 14, 2023],
    [2020], [Invited Workshop: #emph[Reproducible Research: Why and How.] Pre-conference Workshop for the Society for Epidemiologic Research Annual Meeting (virtual) October 30, 2020],
  )]
  , kind: table
  )

My expertise on measuring health inequalities has also been sought by national and international organizations, where I serve as a member of several standing committees:

#figure(
  align(center)[#table(
    columns: (20%, 80%),
    align: (left,left,),
    [2024-present], [Member, Expert Review Group on #emph[Collaborative Health Equity Measurement];, Maternal and Child Health Bureau, US Health Resources and Services Administration],
    [2023-present], [Member, Expert Review Group, #emph[Health Inequality Monitoring Handbook];, World Health Organization],
    [2017-present], [Member, Working Group, #emph[Health Equity Toolkit Assessment];, World Health Organization],
    [2022-present], [Steering Group Member, #emph[Pan-Canadian Health Inequalities Reporting Initiative];, Public Health Agency of Canada],
    [2019-2021], [Working Group Member, #emph[Health Inequalities Reporting Initiative];, Canadian Institute for Health Information],
  )]
  , kind: table
  )

#strong[5 Supervisory and mentoring activities]

I have strong track record of supervision. Below I list the postdocs, doctoral students, and master’s degree students I have supervised or co-supervised in the past 6 years.

#emph[Postdoctoral fellows]

#figure(
  align(center)[#table(
    columns: (20%, 80%),
    align: (left,left,),
    [2020-2023], [Erin Hetherington],
  )]
  , kind: table
  )

#emph[Doctoral students]

#figure(
  align(center)[#table(
    columns: (20%, 80%),
    align: (left,left,),
    [2022-present], [Siobhan Carroll (co-supervised with Jill Baumgartner)],
    [2021-present], [Wenlu Yuan (co-supervised with Jill Baumgartner)],
    [2021-present], [Peter Socha (co-supervised with Jennifer Hutcheon)],
    [2021-present], [Talia Sternbach (co-supervised with Jill Baumgartner)],
    [2021-present], [Imen Farhat (co-supervised with Dimitra Panagiotoglu)],
    [2020-2024], [Francisca Vargas Lopez (Erasmus MC)],
    [2018-2023], [Walid Al-Soneidar],
    [2017-present], [Diego Capurro Fernandez],
    [2014-2019], [Nichole Austin],
  )]
  , kind: table
  )

#emph[Master’s degree students]

#figure(
  align(center)[#table(
    columns: (20%, 80%),
    align: (left,left,),
    [2021-2022], [Melia Alcantara (co-supervised with Seungmi Yang)],
    [2020-2021], [Peter Socha (co-supervised with Jennifer Hutcheon)],
    [2018-2020], [Hiba El-Haj (co-supervised with Shelley Clarke)],
    [2019-2021], [Talia Sternbach (co-supervised with Jill Baumgartner)],
  )]
  , kind: table
  )

#strong[6 Other relevant information]

None.

#set bibliography(style: "harvard-cite-them-right")

#bibliography("personal.bib")

