// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

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

#show raw.where(block: true): set block(
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
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
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
  if type(it.kind) != str {
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
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
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
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
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

#set page(
  paper: "us-letter",
  margin: (x: 1.87cm,y: 1.87cm,),
  numbering: "1",
)

#show: doc => article(
  title: [The Influence of Reviewer Expertise and Engagement on Peer Review of Grants],
  font: ("C059",),
  fontsize: 11pt,
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

= Project Description
<project-description>
== Objectives
<objectives>
The overall aim of this project is to investigate how the peer review process may affect reliability and fairness in the distribution of scores and funding success of CIHR Project Grant applications.

== Specific Aims
<specific-aims>
+ To evaluate how reviewer expertise and engagement affects the impact of grant panel discussion on application scores.

+ To assess whether the effects of reviewer expertise and engagement differ by applicant characteristics such as gender or career stage.

+ To evaluate potential alternative schemes such as differential weighting of scores or partial randomization to determine funding for qualified applications.

== Background and Rationale
<background-and-rationale>
#emph[Peer Review Model for Grants.] CIHR's Project Grant Program annually invests hundreds of millions of dollars in proposals to fund the "best ideas" and advance the health of Canadians (Government of Canada 2016a). Prioritizing research through funding comes from both strategic investments and an evaluation of competitive applications by Peer Review Committees (PRCs). PRCs covering over 50 basic and applied research domains are composed of roughly 20-30 members identified and assigned based on having sufficient expertise and experience to evaluate, rate, and discuss applications (Government of Canada 2016b).

Grant review panels using PRCs are common in many agencies and countries (Azoulay and Li 2020; Guthrie, Ghiga, and Wooding 2018; Thorngate, Dawes, and Foddy 2011). The rationale for using panels is that committees encompass a diverse range of perspectives and overlapping expertise that can correct misunderstandings among panel members, clarify ambiguities in scoring justifications, and converge on a more calibrated assessment (Azoulay and Li 2020). However, committees may also suffer from a range of potential biases that affect group decision making, such as groupthink, conformity, free riding, polarization, or the influence of asymmetric information (Thorngate, Dawes, and Foddy 2011). Such biases may be exacerbated when reviewers and non-reviewers of applications have access to different information.

A key challenge in grant panel review is human resource limitations (Azoulay and Li 2020). PRCs receive many submissions, so each application is only evaluated by 3 reviewers who thoroughly read each application and identify its strengths and weaknesses with respect to the main adjudication criteria (significance, approaches and methods, expertise and resources). Based on the initial reviewer scores, the bottom-scoring 60% of applications are streamlined, with the remaining applications discussed by the panel. In the meeting the 3 reviewers of each application provide their initial scores (on a scale from 0 \[poor\] to 4.9 \[outstanding\]), and summarize the application's strengths and weaknesses, followed by panel discussion to arrive at a consensus score among the three reviewers. Each panel member then provides a final score (0 to 4.9) that must be within +/- 0.5 of the consensus score. The average of these scores, with equal weight given to all panel members, are used as inputs to the final decisions regarding funding.

The discretion of panel members to deviate from the consensus score could have meaningful consequences for the final score and ranking of applications (Banal-Estañol et al. 2023; Olbrecht and Bornmann 2010; Tamblyn et al. 2023). Panel members' scores may be influenced by reviewer characteristics such as expertise regarding the application, level of engagement (reviewer or non-reviewer), gender, and prior reviewing experience. Such influences may also vary by characteristics of the applicant (gender, career stage). These factors may be particularly influential for panel members who #emph[do not review] the application and must rely on panel discussion to form their final scores. Thus, in panels where only a few members have read the application, uneven access to information may lead to less-than-fair judgements (e.g., an influence of non-experts on final scores). The dynamics of group interactions, pressure to conform, and the subjective nature of discussions may all play critical roles in shaping which grants ultimately receive funding (Azoulay and Li 2020; Thorngate, Dawes, and Foddy 2011; Olbrecht and Bornmann 2010). These challenges may also be affected by virtual rather than face-to-face panel discussions (Gallo, Carpenter, and Glisson 2013; Carpenter et al. 2015).

#emph[Prior Literature];. Most prior research on grant peer review has focused on reviewer reliability (Guthrie, Ghiga, and Wooding 2018). Several studies suggest that inter-rater reliabilities for applications are moderate or low (Thorngate, Dawes, and Foddy 2011; Tamblyn et al. 2018; Erosheva et al. 2020; Pier et al. 2018; Mutz, Bornmann, and Daniel 2012; Cole, Cole, and Simon 1981; Pier et al. 2017), vary across committee domains (Tamblyn et al. 2018), and require many reviewers to be consistent (Mayo et al. 2006; Kaplan, Lacetera, and Kaplan 2008). However, low reliability is not necessarily inconsistent with identifying quality applications and can reflect a healthy diversity of opinions, expertise, or value judgements about research quality (Bailar 1991; Derrick and Samuel 2017). There are also longstanding concerns that grant peer review systematically disadvantages some groups, including by gender (Tamblyn et al. 2018; Witteman et al. 2019; Burns et al. 2019; Schmaling and Gallo 2023), race or ethnic background (Erosheva et al. 2020; Ginther et al. 2011), and career stage (Tamblyn et al. 2023, 2018). Panel discussions could lead to final scores that reduce the competitiveness of applications from under-represented groups if non-reviewing members rely on heuristics (such as applicant reputation or institution) rather than on a careful evaluation of content.

Differences between consensus scores and the final scores from the full panel may reflect systematic impacts of panel member expertise and engagement. However, evaluations of panel discussions in grant peer review are rare (Guthrie, Ghiga, and Wooding 2018). Past work shows that the expertise, experience, and level of engagement (reviewer vs non-reviewer) of each panel member can affect the distribution of final scores (Tamblyn et al. 2018; Fogelholm et al. 2012; Johnson 2008; Hodgson 1997). However, little evidence exists on whether these factors may also affect the difference between consensus and final scores within a committee, which has implications for fairness. Prior work suggests that committee discussions fail to improve consistency and often demonstrate "strategic rating" among panel members to influence funding of specific applications (Thorngate, Dawes, and Foddy 2011; Pier et al. 2017; Fogelholm et al. 2012; Obrecht, Tibelius, and D'Aloisio 2007). Direct observation of committee discussions also suggests concerns about fairness due to the influence of committee chairs, variation in the length of discussion and discussion topics, and attempts to ensure the funding of specific applications (Thorngate, Dawes, and Foddy 2011; Carpenter et al. 2015; Pier et al. 2017; Obrecht, Tibelius, and D'Aloisio 2007). Overall, the prior literature raises concerns that the dynamics of group decision making in grant panels can impede efficiency and exacerbate inconsistencies in scoring rather than mitigate them (Pier et al. 2017; Derrick and Samuel 2017).

Finally, prior evidence suggests that not only can panel review affect changes in the scoring of applications, it can have important consequences for which applications are ultimately funded, particularly those in the middle range of 'potentially fundable' applications. Prior work suggests that anywhere from 10% to 30% of application funding results are affected by discussion (Carpenter et al. 2015; Pier et al. 2018; Johnson 2008; Hodgson 1997; Obrecht, Tibelius, and D'Aloisio 2007; Martin, Kopstein, and Janice 2010).

#emph[Evidence Gaps.] Prior evidence suggests the need for a systematic evaluation of how reviewer expertise and engagement may affect grant panel outcomes. Furthermore, whether these effects may disproportionately impact applicants that have historically experienced barriers to funding is unknown--this has important implications for equity in grant funding. Finally, there remains a need to explore whether alternative schemes such as weighting votes by reviewer engagement status or partial randomization of funding (Fang and Casadevall 2016) could produce more reliable outcomes.

== Methods
<methods>
=== #emph[Data and Design]
<data-and-design>
This is an observational study and we will utilize restricted application-level data provided by agreement with CIHR's Funding Analytics Team. The dataset will include application-level information on funding result and amount, initial reviewer scores, consensus score, keywords or domain of inquiry (e.g., biomedical, clinical, etc.), re-submission status, number of investigators, funding amount requested, and PRC. Key data on the panel members will include our main covariates of interest: #strong[self-described expertise] (high, medium, low, not enough expertise) and #strong[engagement] (reviewer or non-reviewer), as well as gender, experience, career-stage, past funding success, and conflicts of interest. The Funding Analytics Team will also provide data on the gender and career stage of the applicants.

=== Statistical Plan
<statistical-plan>
#emph[Aim 1.] Our aim is to assess how changes from the initial consensus score to the final review scores may vary with panel expertise and engagement. Given the multilevel structure of the data (applications nested within committees and reviewers) we will use hierarchical models to account for clustering at the committee, reviewer, and application level). Our primary outcome will be the difference between consensus and final scores, which captures any impact of changes during panel discussion. A simple linear model for this data is:

$ d_(i j k) = alpha + beta_r R e v + beta_e E x p + gamma X + delta Z + (u_(0 j) + u_(0 k) + epsilon_(i j)) $

Where $d_(i j k)$ is the #emph[difference] between the consensus and final score for the #emph[i];th review of the #emph[j];th application in PRC #emph[k];. The key coefficients of interest are $beta_r$ and $beta_e$ that capture any differences in the change in scores by reviewer engagement or expertise. $X$ and $Z$ are vectors of applicant-level and application-level characteristics, respectively, and a compound error term in brackets ( ) encompasses random effects at the reviewer and committee level plus a within-application independent Gaussian error term.

#emph[Aim 2.] Given the existing evidence that the impact of reviewer expertise may vary with other characteristics (Tamblyn et al. 2018; Gallo, Sullivan, and Glisson 2016), as well as evidence that applicant-level differences (e.g., gender, career stage) may also vary with other factors such as committee (Burns et al. 2019) we will expand the above model to assess whether reviewer engagement and expertise differentially affect applicants by gender and career stage.

#emph[Aim 3.] Aim 3 will be exploratory and will evaluate the consequences of potential alternative evaluation schemes. For example, a modified scheme of upweighting scores by the level of engagement with the application or random assignment to funding of applications within a 'grey-zone' in the fundable range. We will compare the predicted distribution of outcomes by gender and career stage for these alternatives with those of the standard model above.

=== Feasibility
<feasibility>
We have considerable past experience with multilevel data and evaluation (Nandi et al. 2022; Nandi et al. 2020, 2016; Hetherington et al. 2023; Jahagirdar et al. 2017). The data required for this project goes beyond publicly available data on the #link("https://open.canada.ca/data/en/dataset/49edb1d7-5cb4-4fa7-897c-515d1aad5da3")[Open Data Portal];. We have confirmation from CIHR's Funding Analytics Team that the application-level data required to assess the impact of reviewer characteristics on scores is feasible for extraction for analysis by our team. Data on applicant and reviewer personal characteristics (e.g., gender, career stage) is confidential and subject to additional security precautions. For these analyses we have agreed to supply computer code to the Funding Analytics Team, who will run the analyses and return the results to our research team.

=== Timeline
<timeline>
#box(image("media/timeline.png"))

== Expected outcomes
<expected-outcomes>
The primary outputs from this project will be training in reproducible research practices (Harper 2020), peer-reviewed publications, presentations, reproducible analytic code, and non-technical policy briefs. Our results can provide new insights to CIHR's ongoing discussions regarding how it structures and designs its peer review processes, as well as potential alternative reforms (e.g., Pier et al. 2018; Fogelholm et al. 2012). CIHR is also a core partner of the Research on Research Institute (RoRI), and the aims of this research bear directly on RoRI's project on peer review, including the recent RoRI #link("https://researchonresearch.org/project/peer-review/")[Atlas of Peer Review] project that aims to synthesize existing work on peer review and propose innovative methods for improving the quality of peer review across multiple domains, including funding (Gregory, Waltman, and Pinfield 2024). Our results will also be relevant to other competitions or agencies that use a similar multi-stage review process.

== Training and mentoring
<training-and-mentoring>
Our training objectives are to: (1) introduce trainees to rigorous methods for evaluating peer review; (2) strengthen trainees' data science skills via developing code and analysis plans to be executed remotely; (3) expose trainees to best practices for open, reproducible and ethical conduct of social sciences research; and (4) involve trainees in the development of a knowledge mobilization plan and strategies for communicating research findings to non-academic stakeholders. We will recruit and train one postdoctoral scholar for two years that will spearhead the project analysis, develop the code, and lead the writing of reports. We will also hire one part-time research assistant to systematically review all of the existing research on the impact of panel discussions on funding scores and outcomes, coordinate meetings and dialogue with the remote analysis team, create and maintain a project website, and set-up and be responsible for maintaining infrastructure for reproducibility via the Open Science Foundation.

== Knowledge mobilization
<knowledge-mobilization>
Our primary KM plan is to publish and distribute our findings to stakeholders in the discussion around peer review reform, as well as public facing outputs in social media and a dedicated website. This includes CIHR and the RoR Institute. We aim to publish 3 papers in open access topical (e.g., Research Evaluation) and health (e.g., CMAJ, Lancet) journals and we will make all of our code and reports available on the Open Science Foundation repository. We plan to reach out to CIHR and the RoR Institute with our findings and to submit and present our work at the annual Metascience #link("https://metascience.info/prior-conferences/")[conference];, as well as the Canadian and US public health and policy conferences.

#pagebreak()
= Bibliography
<bibliography>
#block[
#block[
Azoulay, Pierre, and Danielle Li. 2020. “Scientific Grant Funding.” In #emph[Innovation and Public Policy];, 117--50. University of Chicago Press.

] <ref-azoulay2020>
#block[
Bailar, John C. 1991. “Reliability, Fairness, Objectivity and Other Inappropriate Goals in Peer Review.” #emph[Behavioral and Brain Sciences] 14 (1): 137--38. #link("https://doi.org/10.1017/S0140525X00065705");.

] <ref-bailar1991>
#block[
Banal-Estañol, Albert, Qianshuo Liu, Inés Macho-Stadler, and David Pérez-Castrillo. 2023. “Similar-to-Me Effects in the Grant Application Process: Applicants, Panellists, and the Likelihood of Obtaining Funds.” #emph[R&D Management] 53 (5): 819--39. #link("https://doi.org/10.1111/radm.12601");.

] <ref-banal-estanol2023>
#block[
Burns, Karen E. A., Sharon E. Straus, Kuan Liu, Leena Rizvi, and Gordon Guyatt. 2019. “Gender Differences in Grant and Personnel Award Funding Rates at the Canadian Institutes of Health Research Based on Research Content Area: A Retrospective Analysis.” Edited by Bjoern Weiss. #emph[PLOS Medicine] 16 (10): e1002935. #link("https://doi.org/10.1371/journal.pmed.1002935");.

] <ref-burns2019>
#block[
Carpenter, Afton S, Joanne H Sullivan, Arati Deshmukh, Scott R Glisson, and Stephen A Gallo. 2015. “A Retrospective Analysis of the Effect of Discussion in Teleconference and Face-to-Face Scientific Peer-Review Panels.” #emph[BMJ Open] 5 (9): e009138. #link("https://doi.org/10.1136/bmjopen-2015-009138");.

] <ref-carpenter2015>
#block[
Cole, Stephen, Jonathan R. Cole, and Gary A. Simon. 1981. “Chance and Consensus in Peer Review.” #emph[Science] 214 (4523): 881--86. #link("https://doi.org/10.1126/science.7302566");.

] <ref-cole1981>
#block[
Derrick, Gemma, and Gabrielle Samuel. 2017. “The Future of Societal Impact Assessment Using Peer Review: Pre-Evaluation Training, Consensus Building and Inter-Reviewer Reliability.” #emph[Palgrave Communications] 3 (1): 17040. #link("https://doi.org/10.1057/palcomms.2017.40");.

] <ref-derrick2017>
#block[
Erosheva, Elena A., Sheridan Grant, Mei-Ching Chen, Mark D. Lindner, Richard K. Nakamura, and Carole J. Lee. 2020. “NIH Peer Review: Criterion Scores Completely Account for Racial Disparities in Overall Impact Scores.” #emph[Science Advances] 6 (23): eaaz4868. #link("https://doi.org/10.1126/sciadv.aaz4868");.

] <ref-erosheva2020>
#block[
Fang, Ferric C., and Arturo Casadevall. 2016. “Research Funding: The Case for a Modified Lottery.” #emph[mBio] 7 (2): e00422--16. #link("https://doi.org/10.1128/mBio.00422-16");.

] <ref-fang2016>
#block[
Fogelholm, Mikael, Saara Leppinen, Anssi Auvinen, Jani Raitanen, Anu Nuutinen, and Kalervo Väänänen. 2012. “Panel Discussion Does Not Improve Reliability of Peer Review for Medical Research Grant Proposals.” #emph[Journal of Clinical Epidemiology] 65 (1): 47--52. #link("https://doi.org/10.1016/j.jclinepi.2011.05.001");.

] <ref-fogelholm2012>
#block[
Gallo, Stephen A., Afton S. Carpenter, and Scott R. Glisson. 2013. “Teleconference Versus Face-to-Face Scientific Peer Review of Grant Application: Effects on Review Outcomes.” Edited by Lutz Bornmann. #emph[PLoS ONE] 8 (8): e71693. #link("https://doi.org/10.1371/journal.pone.0071693");.

] <ref-gallo2013>
#block[
Gallo, Stephen A., Joanne H. Sullivan, and Scott R. Glisson. 2016. “The Influence of Peer Reviewer Expertise on the Evaluation of Research Funding Applications.” Edited by Eldad Yechiam. #emph[PLOS ONE] 11 (10): e0165147. #link("https://doi.org/10.1371/journal.pone.0165147");.

] <ref-gallo2016>
#block[
Ginther, Donna K., Walter T. Schaffer, Joshua Schnell, Beth Masimore, Faye Liu, Laurel L. Haak, and Raynard Kington. 2011. “Race, Ethnicity, and NIH Research Awards.” #emph[Science] 333 (6045): 1015--19. #link("https://doi.org/10.1126/science.1196783");.

] <ref-ginther2011>
#block[
Government of Canada, Canadian Institutes of Health Research. 2016a. “Project Grant Program - CIHR.” https:\/\/irsc-cihr.gc.ca/e/49051.html.

] <ref-governmentofcanada2016>
#block[
---------. 2016b. “Project Grant Program: Review Process - CIHR.” https:\/\/irsc-cihr.gc.ca/e/49807.html.

] <ref-governmentofcanada2016a>
#block[
Gregory, Kathleen, Ludo Waltman, and Stephen Pinfield. 2024. “Peer Review in Funding Organizations: An Analytical Literature Review. (RoRI Working Paper No.11),” 1085074 Bytes. #link("https://doi.org/10.6084/M9.FIGSHARE.26861680.V5");.

] <ref-gregory2024>
#block[
Guthrie, Susan, Ioana Ghiga, and Steven Wooding. 2018. “What Do We Know about Grant Peer Review in the Health Sciences? An Updated Review of the Literature and Six Case Studies.” RAND Corporation.

] <ref-guthrie2018>
#block[
Harper, Sam. 2020. “Reproducible Research: Why and How.” Pre-Conference Workshop. Virtual.

] <ref-harper2020>
#block[
Hetherington, Erin, Sam Harper, Rebecca Davidson, Charles Festo, Nadia Lampkin, Sally Mtenga, Clarissa Teixeira, Ilona Vincent, and Arijit Nandi. 2023. “Impact Evaluation of the TAMANI Project to Improve Maternal and Child Health in Tanzania.” #emph[J Epidemiol Community Health];, April. #link("https://doi.org/10.1136/jech-2022-219995");.

] <ref-hetherington2023>
#block[
Hodgson, Corinne. 1997. “How Reliable Is Peer Review? An Examination of Operating Grant Proposals Simultaneously Submitted to Two Similar Peer Review Systems.” #emph[Journal of Clinical Epidemiology] 50 (11): 1189--95. #link("https://doi.org/10.1016/S0895-4356(97)00167-4");.

] <ref-hodgson1997>
#block[
Jahagirdar, Deepa, Sam Harper, Jody Heymann, Hema Swaminathan, Arnab Mukherji, and Arijit Nandi. 2017. “The Effect of Paid Maternity Leave on Early Childhood Growth in Low-Income and Middle-Income Countries.” #emph[BMJ Global Health] 2 (3): e000294. #link("https://doi.org/10.1136/bmjgh-2017-000294");.

] <ref-Jahagirdar:2017aa>
#block[
Johnson, Valen E. 2008. “Statistical Analysis of the National Institutes of Health Peer Review System.” #emph[Proceedings of the National Academy of Sciences] 105 (32): 11076--80. #link("https://doi.org/10.1073/pnas.0804538105");.

] <ref-johnson2008>
#block[
Kaplan, David, Nicola Lacetera, and Celia Kaplan. 2008. “Sample Size and Precision in NIH Peer Review.” Edited by Tom Tregenza. #emph[PLoS ONE] 3 (7): e2761. #link("https://doi.org/10.1371/journal.pone.0002761");.

] <ref-kaplan2008>
#block[
Martin, Michael R., Andrea Kopstein, and Joy M. Janice. 2010. “An Analysis of Preliminary and Post-Discussion Priority Scores for Grant Applications Peer Reviewed by the Center for Scientific Review at the NIH.” Edited by Tom Tregenza. #emph[PLoS ONE] 5 (11): e13526. #link("https://doi.org/10.1371/journal.pone.0013526");.

] <ref-martin2010>
#block[
Mayo, Nancy E., James Brophy, Mark S. Goldberg, Marina B. Klein, Sydney Miller, Robert W. Platt, and Judith Ritchie. 2006. “Peering at Peer Review Revealed High Degree of Chance Associated with Funding of Grant Applications.” #emph[Journal of Clinical Epidemiology] 59 (8): 842--48. #link("https://doi.org/10.1016/j.jclinepi.2005.12.007");.

] <ref-mayo2006>
#block[
Mutz, Rüdiger, Lutz Bornmann, and Hans-Dieter Daniel. 2012. “Heterogeneity of Inter-Rater Reliabilities of Grant Peer Reviews and Its Determinants: A General Estimating Equations Approach.” Edited by Giuseppe Biondi-Zoccai. #emph[PLoS ONE] 7 (10): e48509. #link("https://doi.org/10.1371/journal.pone.0048509");.

] <ref-mutz2012>
#block[
Nandi, Arijit, Parul Agarwal, Anoushaka Chandrashekar, and Sam Harper. 2020. “Access to Affordable Daycare and Women's Economic Opportunities: Evidence from a Cluster-Randomised Intervention in India.” #emph[Journal of Development Effectiveness] 12 (3): 219--39. #link("https://doi.org/10.1080/19439342.2020.1773898");.

] <ref-nandi2020>
#block[
Nandi, Arijit, Thomas J. Charters, Amm Quamruzzaman, Erin C. Strumpf, Jay S. Kaufman, Jody Heymann, Arnab Mukherji, and Sam Harper. 2022. “Health Care Services Use, Stillbirth, and Neonatal and Infant Survival Following Implementation of the Maternal Health Voucher Scheme in Bangladesh: A Difference-in-Differences Analysis of Bangladesh Demographic and Health Survey Data, 2000 to 2016.” #emph[PLOS Medicine] 19 (8): e1004022. #link("https://doi.org/10.1371/journal.pmed.1004022");.

] <ref-nandi2022>
#block[
Nandi, Arijit, Mohammad Hajizadeh, Sam Harper, Alissa Koski, Erin C Strumpf, and Jody Heymann. 2016. “Increased Duration of Paid Maternity Leave Lowers Infant Mortality in Low- and Middle-Income Countries: A Quasi-Experimental Study.” #emph[PLoS Medicine] 13 (3): e1001985. #link("https://doi.org/10.1371/journal.pmed.1001985");.

] <ref-nandi2016>
#block[
Obrecht, Michael, Karl Tibelius, and Guy D'Aloisio. 2007. “Examining the Value Added by Committee Discussion in the Review of Applications for Research Awards.” #emph[Research Evaluation] 16 (2): 79--91. #link("https://doi.org/10.3152/095820207X223785");.

] <ref-obrecht2007>
#block[
Olbrecht, Meike, and Lutz Bornmann. 2010. “Panel Peer Review of Grant Applications: What Do We Know from Research in Social Psychology on Judgment and Decision-Making in Groups?” #emph[Research Evaluation] 19 (4): 293--304. #link("https://doi.org/10.3152/095820210X12809191250762");.

] <ref-olbrecht2010>
#block[
Pier, Elizabeth L., Markus Brauer, Amarette Filut, Anna Kaatz, Joshua Raclaw, Mitchell J. Nathan, Cecilia E. Ford, and Molly Carnes. 2018. “Low Agreement Among Reviewers Evaluating the Same NIH Grant Applications.” #emph[Proceedings of the National Academy of Sciences] 115 (12): 2952--57. #link("https://doi.org/10.1073/pnas.1714379115");.

] <ref-pier2018>
#block[
Pier, Elizabeth L., Joshua Raclaw, Anna Kaatz, Markus Brauer, Molly Carnes, Mitchell J. Nathan, and Cecilia E. Ford. 2017. “‘Your Comments Are Meaner Than Your Score': Score Calibration Talk Influences Intra- and Inter-Panel Variability During Scientific Grant Peer Review.” #emph[Research Evaluation] 26 (1): 1--14. #link("https://doi.org/10.1093/reseval/rvw025");.

] <ref-pier2017>
#block[
Schmaling, Karen B., and Stephen A. Gallo. 2023. “Gender Differences in Peer Reviewed Grant Applications, Awards, and Amounts: A Systematic Review and Meta-Analysis.” #emph[Research Integrity and Peer Review] 8 (1): 2. #link("https://doi.org/10.1186/s41073-023-00127-3");.

] <ref-schmaling2023>
#block[
Tamblyn, Robyn, Nadyne Girard, James Hanley, Bettina Habib, Adrian Mota, Karim M. Khan, and Clare L. Ardern. 2023. “Ranking Versus Rating in Peer Review of Research Grant Applications.” Edited by Julian D. Cortes. #emph[PLOS ONE] 18 (10): e0292306. #link("https://doi.org/10.1371/journal.pone.0292306");.

] <ref-tamblyn2023>
#block[
Tamblyn, Robyn, Nadyne Girard, Christina J. Qian, and James Hanley. 2018. “Assessment of Potential Bias in Research Grant Peer Review in Canada.” #emph[Canadian Medical Association Journal] 190 (16): E489--99. #link("https://doi.org/10.1503/cmaj.170901");.

] <ref-tamblyn2018>
#block[
Thorngate, Warren, Robyn M. Dawes, and Margaret Foddy. 2011. #emph[Judging Merit];. Hoboken: Taylor and Francis.

] <ref-thorngate2011>
#block[
Witteman, Holly O, Michael Hendricks, Sharon Straus, and Cara Tannenbaum. 2019. “Are Gender Gaps Due to Evaluations of the Applicant or the Science? A Natural Experiment at a National Funding Agency.” #emph[The Lancet] 393 (10171): 531--40. #link("https://doi.org/10.1016/S0140-6736(18)32611-4");.

] <ref-witteman2019>
] <refs>
#pagebreak()
= Budget Justification
<budget-justification>
== Personnel costs (172697)
<personnel-costs-172697>
One postdoc for 2 years \@\$50,000 per year salary plus 25% benefits (\$62,500). The rationale for a postdoc rather than a PhD student is the time constraints and level of skill needed to execute the work. This requires the development and application of a remote analysis plan for Aim 2, the application of multilevel models, methods for measuring bias and the need to engage with the literature on peer review in a short time frame. To help facilitate the above tasks we will hire one part-time research assistant over the course of the project \@\$25/hr + 22.3% benefits x 10 hrs/wk x 156 weeks for a total of \$47,697. The main tasks of the RA will be to synthesize and systematically review all of the research on the impact of panel discussions, as well as to help coordinate meetings and dialogue with the CIHR Funding Analytics Team, and to set up and maintain the project's reproducible code infrastructure on the Open Science Foundation's website, and to design and maintain a website for the project itself that will contain and display research outputs.

== Travel costs (7820)
<travel-costs-7820>
Three team members (PI, co-PI, and postdoc) will make two trips to Ottawa (one early to discuss the datasets, analysis plan and mechanics of remote working, and one for the end of study as required by the RFP). We will need to travel to Ottawa in the first year to develop a detailed analysis plan with the Funding Analytics Team that can be executed remotely (For Aim 2), and to gain a better understanding of the details of the confidential fields of data on applicants that can be used to evaluate how reviewers may impact equity in grants. The second trip, as detailed in the RFP, will also be to Ottawa to participate in a knowledge mobilization event or conference to share findings of the research. For these trips we estimate \$720 in round-trip train travel from Montreal to Ottawa (\$120 per person x 3 people x 2 trips), \$1200 in hotel costs (\$200 per night for 1 night x 3 people x 2 trips) and per diem costs of \$450 (McGill local rate of \$75/day for 2 days x 3 people x 2 trips). We also budget \$2500 for 1 scientific conference presentation in Year 2 for the postdoc (we plan on the annual MetaScience conference, likely for 2026 or 2027) and \$2500 for 1 conference for the PI in Year 3 to present overall findings at a public health conference (Canadian Society of Epidemiology and Biostatistics or Society for Epidemiologic Research 2027).

== Other expenses (11500)
<other-expenses-11500>
=== Non-disposable equipment
<non-disposable-equipment>
The recruited postdoc and research assistant will each require a laptop for conducting relevant background research, coding policies, analyzing data, writing up manuscripts and theses (2 x \$2000 based on McGill University pricing). Total cost: \$4,000.

=== Publication fees
<publication-fees>
We plan to make our research results widely available and estimate publishing 3 manuscripts: 1) Aim 1: evaluating the impact of reviewer expertise and role on overall scores and funding; 2) Aim 2: variations in the impact of reviewer expertise and role on funding by gender and career stage; Aim 3: evaluation of potential alternative strategies for rating and funding applications. As per the KMB plan we have specific journals targeted for these manuscripts, and we will publish these in open access format at an estimated cost of \$2500 per manuscript. Total cost: \$7,500.
