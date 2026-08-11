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
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
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
  pagenumbering: "1",
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: pagenumbering,
  )
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
           or heading-color != black or heading-decoration == "underline"
           or heading-background-color != none) {
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

#show: doc => article(
  title: [Simulation Design and Modeling Strategy],
  subtitle: [The Influence of Reviewer Expertise and Engagement on Peer Review of Grants],
  authors: (
    ( name: [Sam Harper & Arijit Nandi],
      affiliation: [],
      email: [] ),
    ),
  date: [2026-08-10],
  margin: (x: 1.87cm,y: 1.87cm,),
  font: ("C059",),
  fontsize: 11pt,
  pagenumbering: "1",
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

= Purpose of this document
<purpose-of-this-document>
This is a design and validation exercise for a project that aims to assess how reviewer engagement and expertise may affect grant scores during CIHR peer review. Given the restrictive nature of CIHR funding data, this document does not use real CIHR data; rather it lays out the data-generating process we believe matches CIHR's Project Grant peer review, the model we intend to fit against it, and evidence that the models we will use can actually recover simulated effects.

It is not a full pre-analysis plan (no pre-specified hypotheses, stopping rules, or multiplicity strategy yet), but it's close in spirit, and a subsequent revision could become one.

#strong[Status and audience.] This draft is for Arijit first -- comments on the outcome definition, the two-part model structure, the Aim 2 interaction specification, or the priors are all welcome before this goes anywhere else. A revised version will go to CIHR's Funding Analytics Team (Matt Hogel and colleagues) as part of finalizing the data-sharing and analysis-plan agreement. It supersedes the `sim-data.qmd` note shared with CIHR earlier, which predates the move to a Bayesian framework and the current two-part, ordinal outcome model -- the underlying data structure it simulates is very different from what follows here.

= Research questions
<research-questions>
Full detail is in the funded proposal (`cihr-ror-proposal.qmd`); briefly:

+ #strong[Aim 1] -- How do reviewer expertise (self-described: high/medium/low/not enough) and engagement (assigned reviewer vs.~non-reviewing panelist) affect the impact of panel discussion on scores?
+ #strong[Aim 2] -- Do these effects differ by applicant characteristics, namely gender or career stage?
+ #strong[Aim 3] (exploratory) -- How would alternative funding-decision schemes (e.g.~reweighting by engagement, partial randomization near the funding threshold) compare to the status quo?

= The review process we're modeling
<the-review-process-were-modeling>
- The 3 assigned reviewers read the application, score it, discuss, and agree a #strong[consensus score];.
- All panel members -- including the 3 reviewers themselves -- then submit a #strong[final score];. Reviewers are not bound to their own consensus number; they can move too.
- The final score must fall within #strong[±0.5] of the consensus score, and is entered to #strong[one decimal place];.
- The (equally weighted) average of final scores across all panel members feeds the funding decision.

= Data structure
<data-structure>
We're assuming a three-level structure: committees, discussed applications nested in committees, and panel members nested in committees (crossed with applications, since a member reviews many applications within a cycle). Based on recent Project Grant committee sizes, our working numbers are #strong[50 committees × 15 discussed applications per committee × 24 members per committee];.

CIHR's Funding Analytics Team confirmed by email (2025-12-04) which fields are actually extractable for a data pull, versus fields that can only ever be touched by a CIHR analyst running our code in-house on the real data (full table and follow-up questions in `code/ror-research-log.qmd`). Headline for this document: committee/application/member identifiers, role (reviewer vs.~panelist), self-described expertise, initial reviewer scores, consensus score, final scores, and funding result are all extractable. #strong[Applicant gender and career stage -- Aim 2's entire basis -- are not];, and won't appear even in CIHR's own distribution-matched dummy data. That constraint is why this document exists: the simulation below is the only rehearsal Aim 2's code gets before it runs once, unsupervised, on real data.

= Primary outcome: modeling the difference, not the level
<primary-outcome-modeling-the-difference-not-the-level>
Let $d_(i j k)$ be the final score minus the consensus score, for the $i$th panel member on the $j$th application in the $k$th committee:

$ d_(i j k) = upright("final score")_(i j k) - upright("consensus score")_(j k) $

We model $d_(i j k)$ directly, rather than modeling the final score with consensus score as a covariate, for three reasons:

+ #strong[Point mass at zero] Hypothesizing that most or many members will go with the consensus score, which leads to a mass either at the consensus (if you modeled the score) or a mass at zero if you model the difference. A member who doesn't move from consensus has $d_(i j k) = 0$ regardless of which application they're scoring; a `final_score ~ consensus_score + ...` model still has that spike, just relocated to wherever a given application's consensus happened to land. Differencing standardizes the spike's location across every application, which is what makes a shared two-part model tractable.
+ #strong[Avoids reintroducing confounding.] A freely estimated slope on consensus in a `final_score ~ consensus + ...` model has to recover something close to 1 from the data; if reviewer composition happens to correlate with consensus level across committees, that estimation can leak into the coefficients we actually care about. Differencing with an implicit slope of exactly 1 -- the true administrative rule, not an assumption -- removes that channel by construction.
+ #strong[Matches the actual research question.] Aim 1 is about the #emph[change from consensus] induced by discussion, not the #emph[level] of the final score (driven mostly by application quality, which isn't under study here). Nothing is lost: $upright("final score") = upright("consensus") + d_(i j k)$ is a trivial identity, so anything Aim 3 needs at the score level for funding-decision simulations can be reconstructed downstream.

= Why a two-part model
<why-a-two-part-model>
We expect $d_(i j k)$ to have a spike at exactly zero (members who simply adopt the consensus score) plus variation among those who depart from it. So the outcome is split into two linked models:

+ #strong[Did this member deviate at all?] A Bernoulli model on $1 [d_(i j k) eq.not 0]$.
+ #strong[If they deviated, by how much?] A model for the (signed) magnitude, conditional on deviating.

$ E [d_(i j k)] = P (upright("deviate")) times E [upright("magnitude") divides upright("deviate")] $

This isn't just theoretically motivated -- we checked it empirically before committing to the extra complexity. If most of the covariate signal about reviewer engagement/expertise actually lived in the #emph[average size] of the deviation rather than #emph[whether] a deviation happens, a single linear model on $d_(i j k)$ would be simpler and would suffice.

```r
d1 <- read_csv(here("data", "sim-data-aim1.csv"), show_col_types = FALSE)

m_linear <- lm(deviation ~ job + exp, data = d1)
m_logistic <- glm(deviated ~ job + exp, data = d1, family = binomial())

modelsummary(
  list("Linear: deviation" = m_linear, "Logistic: deviated (0/1)" = m_logistic),
  gof_omit = "DF|Deviance|R2|AIC|BIC|RMSE|Log.Lik|F",
  escape = TRUE
)
```

#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "0_1": 0, "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "10_1": 0, "11_1": 0, "0_2": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "10_2": 0, "11_2": 0, "0_0": 1, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "7_0": 1, "8_0": 1, "9_0": 1, "10_0": 1, "11_0": 1
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center,),
    (align: left,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 3, stroke: 0.05em + black),
 table.hline(y: 11, start: 0, end: 3, stroke: 0.05em + black),
 table.hline(y: 12, start: 0, end: 3, stroke: 0.08em + black),
 table.hline(y: 0, start: 0, end: 3, stroke: 0.08em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[ ], [Linear: deviation], [Logistic: deviated (0\/1)],
    ),
    // tinytable header end

    // tinytable cell content after
[(Intercept)], [\-0.001], [\-0.964],
[], [(0.002)], [(0.034)],
[jobreviewer], [\-0.004], [\-0.274],
[], [(0.003)], [(0.049)],
[explow], [0.004], [0.654],
[], [(0.003)], [(0.050)],
[expmed], [0.000], [0.447],
[], [(0.002)], [(0.041)],
[expnone], [0.001], [0.938],
[], [(0.003)], [(0.050)],
[Num.Obs.], [18000], [18000],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
The linear model's coefficients on `job`/`exp` are tiny and mostly non-significant (largest around 0.005 points, on an outcome bounded at $plus.minus 0.5$). The logistic model on the same covariates recovers the large, highly significant effect pattern actually built into the simulator (`code/ror-sim-deviation.R`). Because deviation magnitude is roughly symmetric around zero conditional on deviating, essentially all the covariate signal lives in #emph[whether] someone deviates -- a model that only looks at the conditional mean of $d_(i j k)$ structurally cannot see it. This confirms the two-part structure is doing real work here, not adding complexity for its own sake.

= Model specification (Aim 1)
<model-specification-aim-1>
== Simulating the data-generating process
<simulating-the-data-generating-process>
Fifty committees, 15 discussed applications per committee, 24 members per committee; 3 of the 24 members on each application are the assigned reviewers, the rest are non-reviewing panelists. A consensus score is drawn per application (committee- and application-level random effects only, no member-level noise, since it's agreed before any individual scoring happens). Whether each member deviates from that consensus is a function of their role and self-described expertise; if they deviate, the signed magnitude is drawn from a truncated distribution and rounded to the nearest tenth, matching CIHR's one-decimal-place scoring (with rejection sampling so a "deviated" row can never round down to a contradictory zero).

#block[
```r
# define parameters
cmte_n   = 50     # number of committees
app_n    = 15     # number of discussed applications per committee
mem_n    = 24     # number of committee members per committee

b0       = 4.1    # intercept for consensus score
u0c_sd   = 0.1    # random intercept SD for committee (consensus level)
u0a_sd   = 0.3    # random intercept SD for application (consensus level)

# probability of *any* deviation from consensus (logit scale) --
# this is the "hu"-equivalent part
a0       = -0.8   # baseline logit prob. of deviating
a1       =  0.3   # panelist vs. reviewer
a2       = -0.4   # high expertise
a3       =  0.2   # low expertise
a4       =  0.5   # no expertise

# signed magnitude of deviation, given deviation occurs, truncated to
# +/- 0.5 (CIHR's stated bound on final vs. consensus score)
dev_bias = 0      # placeholder: no systematic direction yet
dev_sd   = 0.15
dev_min  = -0.5
dev_max  =  0.5

# ... committee/application/member structure, reviewer assignment, and
# expertise assignment omitted here -- see code/ror-sim-deviation.R for
# the complete script

data <- data |>
  mutate(
    p_dev = plogis(a0 + (a1 * panelist) + (a2 * exp_high) +
      (a3 * exp_low) + (a4 * exp_none)),
    deviated = rbinom(n(), 1, p_dev),
    deviation = if_else(
      deviated == 1,
      round_tenth(rtruncnorm(n(), a = dev_min, b = dev_max,
        mean = dev_bias, sd = dev_sd)),
      0)
  )
```

]
Every parameter above is an illustrative placeholder, not an estimate from real data -- there is no real data yet. The point of the simulation is structural (does our modeling approach recover a known effect of a given size?), not predictive of what CIHR's actual numbers will look like.

== Simulated data
<simulated-data>
What did we generate with the parameters above? #ref(<fig-scores>, supplement: [Figure]) shows the distribution of initial scores from the three reviewers, the consensus score and then a distribution of overall scores allowing for +/- 0.5 point deviations. \[Include some descriptives here, score variation, consensus variation, variation across committee, etc.\]

#figure([
#box(image("ror-modeling-strategy_files/figure-typst/fig-scores-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Distribution of simulated intial, consensus, and overall scores
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-scores>


#ref(<fig-var>, supplement: [Figure]) shows how our initial simulation incorporates a small degree of variation across committees in the overall scores, as well as the average variation within committees across applications.

#figure([
#box(image("ror-modeling-strategy_files/figure-typst/fig-var-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Distribution of score variation by committee and application
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-var>


== Part 1: did this member deviate?
<part-1-did-this-member-deviate>
#block[
```r
m1_deviate <-
  brm(data = d1,
      family = bernoulli(),
      deviated ~ 1 + job + exp + (1 | cmte) + (1 | cid) + (1 | app),
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      control = list(adapt_delta = 0.95))
```

]
Random intercepts for committee, member (`cid`), and application; fixed effects for role and expertise. Weakly informative priors throughout -- `normal(0, 1.5)` on the (logit-scale) intercept, `normal(0, 0.5)` on coefficients, `exponential(1)` on group-level SDs. `sample_prior = "yes"` so we always have prior-predictive draws to check against before trusting the posterior.

== Part 2: how large, given a deviation?
<part-2-how-large-given-a-deviation>
CIHR scores are entered to one decimal place, so a deviator's score can only land on one of 10 discrete steps ($plus.minus 0.1 dots.h plus.minus 0.5$ relative to consensus). That's genuinely ordinal/discrete data, not a continuous quantity with occasional extreme values -- a continuous family (`student()`/`gaussian()`) would assign probability to impossible values and has no natural bound at $plus.minus 0.5$ without truncation hacks. We use an ordinal `cumulative()` model instead, with flexible (non-equidistant) thresholds, since there's no reason to assume the 10 steps are equally likely -- and in the simulated data they clearly aren't (concentrated near $plus.minus 0.1$, sparse at $plus.minus 0.5$, which happen to be the swings most relevant to Aim 3's funding-decision question).

#block[
```r
m1_magnitude <-
  brm(data = d1_dev,
      family = cumulative(link = "logit", threshold = "flexible"),
      deviation ~ 1 + job + exp + (1 | cmte) + (1 | cid) + (1 | app),
      prior = c(prior(normal(0, 1.5), class = Intercept),
                prior(normal(0, 0.5), class = b),
                prior(exponential(1), class = sd)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      sample_prior = "yes",
      control = list(adapt_delta = 0.95))
```

]
The cost: $E [upright("magnitude") divides upright("deviate")]$ from an ordinal model is not a linear prediction -- it's a probability-weighted sum over the 10 category values, computed per posterior draw, not read off a default `marginaleffects` contrast. That combination step (and the corresponding $E [d_(i j k)] = P (upright("deviate")) times E [upright("magnitude") divides upright("deviate")]$ calculation across both models) is written but not yet implemented in code -- see Open Questions below.

== Software
<software>
Bayesian throughout, via `brms` + `cmdstanr` (Stan backend), matching the conventions in our other multilevel work. `marginaleffects` for posterior contrasts; models cached to `code/fits/` and deliberately re-fit (not silently reused) whenever formula or data change.

= Extending to Aim 2: applicant characteristics
<extending-to-aim-2-applicant-characteristics>
Since applicant gender and career stage will never be extractable -- not even as CIHR's own dummy data -- this simulation is the only pre-deployment test that code touching those variables gets. We added them as #strong[application-level] attributes (one value per application, like consensus score, not per member) to a new, separate simulation script (`code/ror-sim-aim2.R`) rather than growing the Aim 1 script further -- see "A note on simulation architecture" below.

Critically, we didn't just add #emph[main] effects of gender/career stage. Aim 2's actual question is whether the #emph[engagement/expertise effect on deviation] differs by applicant characteristic -- which requires an #strong[interaction] term to exist in the generating process, or there's nothing for the fitted model to recover.

#block[
```r
# applicant-level main effects (modest -- not the effects of interest)
g1       =  0.1   # applicant gender = female
c1       = -0.1   # applicant career stage = early

# Aim 2's actual target: does the reviewer-engagement / expertise effect
# on P(deviate) differ by applicant gender / career stage? Baked in large
# on purpose so we can confirm the two-part model machinery actually
# recovers a real interaction, not just a real main effect.
i_panelist_female     =  0.5  # job(panelist) x gender(female)
i_expnone_earlycareer =  0.6  # exp(none) x career_stage(early)

data <- data |>
  mutate(
    p_dev = plogis(a0 + (a1 * panelist) + (a2 * exp_high) +
      (a3 * exp_low) + (a4 * exp_none) +
      (g1 * female) + (c1 * early_career) +
      (i_panelist_female * panelist * female) +
      (i_expnone_earlycareer * exp_none * early_career)),
    deviated = rbinom(n(), 1, p_dev)
    # ... deviation magnitude drawn the same way as Aim 1; applicant
    # terms are deliberately left out of the magnitude part, consistent
    # with the Aim 1 finding above that signal lives in whether someone
    # deviates, not in the average size of the deviation
  )
```

]
#strong[To be clear about what these numbers are and aren't:] they are chosen to be large enough to detect cleanly in a simulation of this size, not estimates -- or even guesses -- about the true magnitude or even direction of any real effect in CIHR review. The goal is purely to confirm our statistical machinery would notice an interaction if the real data contained one.

```r
d2 <- read_csv(here("data", "sim-aim2-data.csv"), show_col_types = FALSE)

m_aim2 <- glm(deviated ~ job * gender + exp * career_stage,
  data = d2, family = binomial())

coefs <- summary(m_aim2)$coefficients
coefs[grepl(":", rownames(coefs)), ] |>
  as_tibble(rownames = "term") |>
  tt(digits = 3)
```

#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
  )

  #let style-array = ( 
    // tinytable cell style after
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 5, stroke: 0.05em + black),
 table.hline(y: 5, start: 0, end: 5, stroke: 0.08em + black),
 table.hline(y: 0, start: 0, end: 5, stroke: 0.08em + black),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[term], [Estimate], [Std. Error], [z value], [Pr(>|z|)],
    ),
    // tinytable header end

    // tinytable cell content after
[jobreviewer:gendermale], [0.51164], [0.0966], [5.2955], [0.00000011866],
[explow:career_stageestablished], [-0.00456], [0.0977], [-0.0466], [0.96280257387],
[expmed:career_stageestablished], [0.00247], [0.0793], [0.0311], [0.97516026324],
[expnone:career_stageestablished], [-0.6084], [0.0999], [-6.0914], [0.00000000112],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
Both interactions are recovered cleanly and with high significance (the sign flips relative to the parameters above are just R's alphabetical choice of reference category -- `job`'s reference is "panelist", `gender`'s is "female", `career_stage`'s is "early", so e.g. `jobreviewer:gendermale` is algebraically the same interaction as `job(panelist):gender(female)`, just from the opposite corner of the 2x2).

#strong[Not yet built:] an Aim-2-specific version of `code/ror-analysis-score-models.R` that actually includes these interaction terms in the `brm()` formulas. The simulation is ready; the model script that would be tested against it isn't written yet.

= Aim 3: not yet designed
<aim-3-not-yet-designed>
Aim 3 asks how alternative funding-decision schemes -- reweighting scores by engagement, or partially randomizing funding decisions for applications near the threshold -- would compare to the status quo. That's a structurally different kind of simulation: an intervention/counterfactual layered on top of the funding decision itself, not just an extension of Aims 1-2's data-generating process for reviewer behavior. We haven't started designing it, and would welcome input on what the alternative schemes worth simulating actually are before building anything.

= A note on simulation architecture
<a-note-on-simulation-architecture>
We're deliberately keeping a separate, self-contained simulation script per aim (`ror-sim-deviation.R` for Aim 1, `ror-sim-aim2.R` for Aim 2, and eventually an Aim 3 script) rather than one script that keeps growing. Some duplication of the committee/application/member setup across scripts is an accepted cost of each one being independently runnable and easy to reason about in isolation.

= What this simulation validates, and what it doesn't
<what-this-simulation-validates-and-what-it-doesnt>
It validates that: (1) the two-part model structure is necessary, not over-engineering, for this outcome; (2) the ordinal magnitude submodel is the right match for CIHR's one-decimal-place scoring; (3) the model can recover both main effects (Aim 1) and interaction effects (Aim 2) of a detectable size, using only the fields CIHR has confirmed are extractable plus the fields that would only ever be touched via an in-house CIHR-run analysis.

It does #strong[not] tell us anything about the true size, direction, or even existence of any of these effects in actual CIHR peer review -- every parameter in both simulations is an illustrative placeholder chosen for statistical detectability, not drawn from evidence.

= Open questions
<open-questions>
== For Arijit
<for-arijit>
- Is $d_(i j k)$ (final $-$ consensus) the right primary outcome, or is there a better way to frame the estimand for Aim 1/2?
- Does the interaction specification for Aim 2 (`job x gender`, `exp x career_stage`, both entering only the deviation-probability part) match how you'd operationalize the research question, or would you frame it differently?
- Any concerns about the priors, the random-effects structure, or the choice of an ordinal rather than continuous family for the magnitude part?
- Anything about the overall statistical strategy that looks wrong before this goes to CIHR?

== For CIHR's Funding Analytics Team
<for-cihrs-funding-analytics-team>
(Carried over from `code/ror-research-log.qmd`; repeated here since this document is the more likely vehicle for actually sending them.)

+ For fields that can't be extracted (applicant gender/career stage, reviewer gender/experience, etc.) -- can the dummy-data step still include #emph[synthetic] versions of those columns for us to pipeline-test against, or is the in-house-run path a black box until the real run?
+ Resubmission status is only available from 2023 onward -- does that mean our cohort has to start at the 2023 competition if we want that field, or can it be requested as a partially-missing covariate over a longer window?
+ How should we define/measure "reviewer experience" for the fields where that's the blocker to a yes/no on extractability?
+ Can you confirm our understanding of the review process itself (the $plus.minus 0.5$ rule, one-decimal-place scoring, equal-weight averaging into the funding decision) is accurate?
+ How much variation exists in the number of members across committees, applications per committee, and scores per application (given conflicts)?
+ Discussion time? Do we know how long the discussion period goes at the application level?

= What comes next
<what-comes-next>
Once the Aim 2 model script exists and `FIT_MODELS` is switched on -- after confirming CIHR's execution environment can actually compile/run Stan via `cmdstanr`, and after prior-predictive checks on both submodels -- this document (or its next revision) will add a results section presenting posterior marginal effects via `marginaleffects` for both the deviation-probability and magnitude submodels, and showing recovery of the known simulated effects at the full posterior level, the same way the frequentist checks above do at a point-estimate level.

= References
<references>
#block[
] <refs>



