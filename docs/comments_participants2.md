# Comments

## 1. (page 1)

Could we change my affiliation to Tecnológico de Monterrey?

## 2. (page 3)  

Comment: 

> The sample consisted of 25 participants (5 male, 20 female), ranging in age from 18 to 23 years.

## 3. (page 4) 

- Correction about the magnitude effect:

> "The exponential and direct difference integration rules do not predict this pattern of choices. The other rules can account for magnitude effects in various ways: the proportional difference and ITCH models use relative differences, the tradeoff and unified tradeoff models incorporate diminishing marginal sensitivity to larger rewards, and the hyperbolic and hyperboloid models can do so when their $kappa$ parameter is allowed to vary with reward magnitude."

- Comment about the correction:

There's a larger discussion behind it: with just one global $\kappa$ parameter, the model as is can't
actually account for the magnitude effect, because it's mathematically invariant to proportional scaling of rewards.
What Green and colleagues did was let $\kappa$ vary with the magnitude of the reward, 
that's how they got it to show up empirically. 
So I tried to mention that distinction without going too deep into explaining why the hyperboloid and hyperbolic models describe the magnitude effect empirically but not mathematically.

What do you think?


## 4. (page 4)

The IA comment about "Clarify which implication of the alternative-based..." 
is wrong, it is mixing the preference reversals and the additivity assumtion, so no need to fix anything about that. 

## 5. (page 4)


### Major correction: our participants show superadditivity, not subadditivity!

Going back to the definitions carefully, I realized our participants actually show superadditivity: choosing the larger-later alternative in the subintervals but the smaller-sooner one in the complete interval, the opposite of what we emphasize in the text. :S

The modeling itself is fine, $\vartheta$ can capture either direction, and mathematically nothing needs to change there. This was a confusion in the wording, not in the model (sorry about that!). This also resolves the later comment about $\varthetas$'s verbal interpretation (p. 12): "lower discounting over subintervals and higher discounting over full intervals", which was correctly written (it describes superadditivity). 

I think everything else still applies, except for a few specifications through the text. Here's my proposed correction for the additivity paragraph:


> "This assumption of additive intervals has, however, been observed to be violated, in what are known as interval effects (McAlvanah, 2010; Read, 2001; Scholten et al., 2014). Interval effects take two forms. Subadditivity corresponds to people being more patient over an entire time interval than over a sequence of steps making up that interval: people choose the larger-later alternative for the long time interval but the smaller-sooner alternative for the subdivisions. Superadditivity corresponds to the opposite pattern: people are patient through a series of small intervals but lose patience when presented with the whole interval. Although superadditivity is rarely observed empirically (Scholten & Read, 2006), a substantial subset of our participants showed this pattern, as described later."

## 6. (page 5)

Minor correction to Figure 1. the larger later reward for problem 1 (alternative 2) is 5300 not 5400. 

## 7. (page 7)

The description for the colors is inverted for blue and green. I suggest:

> ... blue for large intervals with large rewards, and olive (or sand) for large intervals with small rewards. 



## 8. Hyperboloid model (page 9)

The first paragraph from the hyperboloid model is not accurate. The hyperboloid model was proposed to better fit discounting curves in general, not to explain the magnitude effect. The hyperboloid and the hyperbolic models can both empirically describe the magnitude effect using the same method: letting $\kappa$ vary according to magnitude. I think we should remove it. 

## 9. (page 11)

There's a mislabeling in this paragraph, connected to the subadditivity/superadditivity swap. 

Scholten et al. (2014) state that superadditivity vanishes when $\vartheta=1$, so $\vartheta$ is the superadditivity parameter, not subadditivity as we wrote. The mechanism we describe right after ('lower discounting over subintervals, higher over full intervals') is also superadditivity, so the fix is just the label.

We did keep $\vartheta$ and omit a (the subadditivity parameter), I confirmed this in the original equations and in the code. So the data actually goes against of what was rare. So this is my suggestion for this model description:

> "The parameter $\vartheta > 0$ governs how differences in perceived time are scaled and induces superadditivity in interval perception. Larger values of $\vartheta$ compress smaller time differences more than larger ones, leading to relatively lower discounting over subintervals and higher discounting over full intervals. In the full tradeoff model developed by Scholten et al. (2014) there is also a parameter that allows for subadditivity. Because our data show substantial superadditivity, we omit this parameter from the version of the tradeoff model."

What do you think?

## 10. (page 12)

In the third paragraph there is the explanation for subadditivity via time bias (which is correct) but given that we observe mostly supperadditivity this doesn't add up with our findings. Should we remove it?

## 11. (page 13)


Answering the comment, I suggest the following:


> "... are parameters corresponding to the weights of the relative
and absolute differences. It would be expected that the reward coefficients ($\beta_{vr}, \beta_{va)}$ to be positive, since a larger reward advantage for the larger-later option should increase the probability of choosing it, 
and the time coefficients ($\beta_{tr}, \beta_{ta}$) to be negative, since a longer wait for the larger-later option should decrease that probability. The $\beta_0$ constant term parameter corresponds to a bias for favoring..."



## 12. (page 19, Figure 3)

There is a missing point at the end of the figure description. 


## 13. Participants behavior 

> The panels in Figure 2 summarize the behavioral data for each participant. The participants are ordered
from the one who made the most overall smaller-sooner choices to the one who made the most larger-later
choices. There are clear individual differences along this basic behavioral measure, and further
differences in the choice patterns across different types of problems: most participants show progressive 
impatience as time intervals become longer and as rewards become smaller, but they do so for different 
problems and with different levels of consistency across the ten repetitions of each problem. Most 
participants also show a magnitude effect: the interval-sensitive patterns are most evident on the large-
reward problems, while on the small-reward problems  (sand squares) participants overwhelmingly choose the smaller-sooner
reward regardless of how the interval is subdivided. We highlight four broadly representative patterns.

> Most participants show a superadditive pattern: they choose the larger-later reward in shorter 
subintervals (yellow and red squares) but switch to the smaller-sooner reward for larger, undivided intervals (blue squartes), though the point at
which that switch happens varies considerably across individuals. Some switch quickly, as soon as
subintervals grow even slightly longer; others continue choosing the larger-later reward well into longer 
subintervals, only switching for the largest, undivided intervals. Participants E, H, and N are broadly
representative of this pattern.

> A second group of participants shows considerably more uncertainty in their choices: almost no problem is
answered consistently across all ten repetitions, with larger-later counts often falling between 3 and 8
out of 10. This uncertainty is not reduced to problems with medium-length intervals, it is also present in the smallest subintervals and the largest intervals, unlike the more decisive patterns seen elsewhere. Participants L and T are broadly representative of this pattern.

> A third group shows no interval effect. Participant V, for example, is clearly sensitive to reward and time magnitude, but not specifically to whether an interval is subdivided.

> A fourth group of participants, illustrated by A, B and Y, chooses the same type of alternative, either smaller-sooner or larger-later, across nearly every problem, irrespective of the interval or reward magnitude involved.

> The remaining participants fall along a continuum between these patterns.

## 14. (page 22)

I'm not sure I follow the specific pairing for Participant K (problem 6 vs. 9, problem 12 vs. 21). I'm not sure if we mentioned this in the meeting, but is there a specific reason you chose those? I think the magnitude effect shows up more broadly as a contrast between the whole set of large-reward problems versus the whole set of small-reward problems.

# 15. (page 23, Figure 6)

In the specification of the ITCH model you wrote $\beta_{vr}$ and $\beta_{va}$ but in the figure and in the 
description on page 24 you used $\beta_{RA}$ and $\beta_{RR}$. 

