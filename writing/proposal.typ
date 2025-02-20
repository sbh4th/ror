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


= Project Description
<project-description>
== Objectives
<objectives>
The overall aim of this project is to investigate the impact of committee group discussion on fairness in the distribution of scores and funding success of CIHR Project Grant applications.

Although CIHR has now effectively transitioned to virtual peer review for the Project Grant scheme, this still requires considerable reviewer and panelist time, and it is unclear whether that time is needed or could possibly be allocated to additional reviews that may improve overall reliability of applications.

== Background
<background>
Empirical evaluations of the peer review process for funding are uncommon. However, past work suggests The prior work most similar was a 2007 study of CIHR fellowship applications that estimated the impact of committee discussions (Obrecht, Tibelius, and D’Aloisio 2007) and found no evidence that discussions improved fairness.

Considerable questions have been raised about whether existing grant panel review processes may affect funding inequalities by gender, ethnicity, or career stage. Eroshiva (Erosheva et al. 2020)

However, much of the past work on how committee dynamics and composition may affect funding success has focused more

CIHR lists 4 principles for peer review: confidentiality, absence of conflicts of interest, fairness, and transparency.

== Methods
<methods>
To estimate the impact of

#strong[Applicant characteristics];.

== Expected outcomes
<expected-outcomes>
== Training and mentoring
<training-and-mentoring>
Since the analysis for this project requires both an in-depth understanding of multi-level modeling as well as the ability to write computer remotely, we aim to

== Knowledge mobilization
<knowledge-mobilization>
The Canadian Institutes for Health Research provides X% of funding for health research in Canada (cite). Determinations of priorities for research funds comes from both strategic investments as well as a process of competetive proposals. Determining which proposals are ultimately selected for funding depends chiefly on the process of review of proposals by committees of peers, charged with identifying the most promising ideas for research.

From CIHR: \> The Project Grant program is open to applicants in all areas of health research that are aligned with the CIHR mandate. It is designed to capture ideas with the greatest potential for important advances in fundamental or applied health-related knowledge, health care, health systems, and/or health outcomes, by supporting projects of research conducted by individual researchers or groups of researchers in all areas of health. The best ideas may stem from new, incremental, innovative, and/or high-risk lines of inquiry or knowledge translation approaches.

#quote(block: true)[
Project Grant applications follow a committee-based peer review process. This process involves the evaluation of applications by a group of reviewers, who have the required experience and expertise to assess the quality and potential impact of the proposed research and research related activities, within the context of the program’s objectives. These reviewers are grouped into Peer Review Committees based on their expertise and the topics of applications submitted to these committees.
]

Although the process of peer review for funding CIHR Project Scheme applications is transparent, there remain important questions regarding whether the current procedures are optimal with respect. In practice, each application is only read and evaluated by 3 committee members assigned to each application. These reviews are detailed and the reviewers are asked to judge

It is an open question as to how non-reviewing committee members typically assign their scores and ultimately evaluate the overall score for each application. It may be that the committee members effectively adopt the consensus score that is agreed to by the 3 reviewers after discussion. However, all committee members (including each of the reviewers) may assign their final score within a range of +/- 0.5 from the consensus score.

== Applicant Characteristics
<applicant-characteristics>
Several studies have shown evidence of a roughly 5% funding disadvantage for female investigators in CIHR competitions (Tamblyn et al. 2018; Witteman et al. 2019; Burns et al. 2019).

#pagebreak()
= Bibliography
<bibliography>
#block[
#block[
Burns, Karen E. A., Sharon E. Straus, Kuan Liu, Leena Rizvi, and Gordon Guyatt. 2019. “Gender Differences in Grant and Personnel Award Funding Rates at the Canadian Institutes of Health Research Based on Research Content Area: A Retrospective Analysis.” Edited by Bjoern Weiss. #emph[PLOS Medicine] 16 (10): e1002935. #link("https://doi.org/10.1371/journal.pmed.1002935");.

] <ref-burns2019>
#block[
Erosheva, Elena A., Sheridan Grant, Mei-Ching Chen, Mark D. Lindner, Richard K. Nakamura, and Carole J. Lee. 2020. “NIH Peer Review: Criterion Scores Completely Account for Racial Disparities in Overall Impact Scores.” #emph[Science Advances] 6 (23): eaaz4868. #link("https://doi.org/10.1126/sciadv.aaz4868");.

] <ref-erosheva2020>
#block[
Obrecht, Michael, Karl Tibelius, and Guy D’Aloisio. 2007. “Examining the Value Added by Committee Discussion in the Review of Applications for Research Awards.” #emph[Research Evaluation] 16 (2): 79–91. #link("https://doi.org/10.3152/095820207X223785");.

] <ref-obrecht2007>
#block[
Tamblyn, Robyn, Nadyne Girard, Christina J. Qian, and James Hanley. 2018. “Assessment of Potential Bias in Research Grant Peer Review in Canada.” #emph[Canadian Medical Association Journal] 190 (16): E489–99. #link("https://doi.org/10.1503/cmaj.170901");.

] <ref-tamblyn2018>
#block[
Witteman, Holly O, Michael Hendricks, Sharon Straus, and Cara Tannenbaum. 2019. “Are Gender Gaps Due to Evaluations of the Applicant or the Science? A Natural Experiment at a National Funding Agency.” #emph[The Lancet] 393 (10171): 531–40. #link("https://doi.org/10.1016/S0140-6736(18)32611-4");.

] <ref-witteman2019>
] <refs>
#pagebreak()
= Budget Justification
<budget-justification>
== Personnel costs
<personnel-costs>
== Travel costs
<travel-costs>
== Other expenses
<other-expenses>
=== Non-disposable equipment
<non-disposable-equipment>
Each PhD student will require a laptop for conducting relevant background research, coding policies, analyzing data, writing up manuscripts and theses (2 x \$1500). Total cost: \$3,000.

=== Publication fees
<publication-fees>
We plan to make our research results widely available and estimate publishing 5 manuscripts: 1) Aim 1: construction of the database; 2) Aim 1: Changes in ECEC policy characteristics over time; 3) Aim 2: Impact of ECEC policies on cognitive outcomes; 4) Aim 2: Impact of ECEC policies on non-cognitive outcomes; 5) Aim 3: Impact of ECEC participation on child outcomes. As per the KMB plan we have specific journals targeted for these manuscripts, and we will publish these in open access format at an estimated cost of \$2500 per manuscript. Total cost: \$12,500.

#pagebreak()
= Letters of Support
<letters-of-support>



