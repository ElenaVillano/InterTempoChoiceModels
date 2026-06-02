# Observations on participant patterns and models

- Some comments about these models:
    - The DD model predicts the same type of response regardless of interval size. Only in some cases (e.g., participant 16) does it distinguish between large and small reward magnitudes.
    - The PD model shows a very abrupt shift between smaller intervals and larger ones, and does not appear sensitive to individual participant response patterns. It always assumes subadditivity, and also implies a magnitude effect: for rewards delivered sooner (shorter delays) it does not predict subadditivity, but for more distant rewards it does. However, this pattern does not match any participant in our data, making the PD model a poor fit throughout.

- Participants 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 17, 18, 19, 21, 24 show some form of subadditivity. In all of them, subadditivity is progressive and present even for small reward magnitudes. I can distinguish two types of subadditivity in this group:
    - **Participants 4–8**: Tend to choose the LL reward in the shorter subintervals, but switch to the SS reward for larger intervals (roughly from problems 6 and 12 considering the interval size). This represents a clear subadditive pattern.
    > I would choose participant 7 to illustrate this pattern.
    - **Participants 9–24** (within this group): Show varying degrees of subadditivity, choosing LL in subintervals but switching to SS as interval size increases.
    > Could be participant 17, 18, or 19, depending on how clearly you want to illustrate this pattern.

- Participants 15, 16, 20, 22, 23 do not show interval effects, which is consistent with none of them being best fit by the UT model.
    - Empirically, participants 15, 16, and 20 tend to be more inconsistent in the shorter subintervals. I think this uncertainty leads the UT model to constrain its predictions toward lower LL choice counts. Despite the absence of subadditivity, the UT model still generates posterior predictives favoring SS choices on problems 6 and 12 (the full intervals that cover the smallest subintervals). In contrast, neither ITCH nor DD predicts subadditivity in these cases, which is likely what makes them better fits for these participants.
        - For example, participant 20 does not show interval effects at all, they choose LL consistently across both subintervals and full intervals. The UT model predicts subadditivity where there is none, while ITCH does not impose this effect, which is (I think) why it fits better.
        > I would choose participant 20 as an example of this.
    - Participants 22 and 23 have stronger, more consistent preferences and show no interval effects. For participant 22, the DD model was inferred, likely because it is simpler than ITCH for capturing a flat, no-interval-effect pattern. Participant 23 shows some LL choices on small reward magnitudes, suggesting a slight tendency that partially differentiates them from participant 22.
