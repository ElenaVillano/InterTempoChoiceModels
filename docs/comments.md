# Comments:   
  
## 1. (p. 5)

### Comment: 

elena: check this; it doesn’t match your figure, but it is what i got from the raw data  
files. for example, you have 6 and 13 as the same, but in the raw data they have differ-  
ent rewards and times  
  
### Answer:  
  
At the time, some questions did not have clearly defined subintervals, 
which is why I initially chose to exclude them from the analysis.  
  
My logic (as you know): question 6 represents the full interval corresponding to questions 1–5,
and similarly, question 12 corresponds to the full interval for questions 7–11. 
At the same time, questions 6, 12, and 14 can also be interpreted as subintervals for later questions (e.g., questions 16–18).
This is why I assigned a double ID to questions 6 and 12.  
  
In contrast, questions 13 and 15 do not have well-defined corresponding subintervals in the same way as the others, 
which is why I decided not to include them in the final specification. 
That said, I agree that we could include them to better understand what is happening in the intermediate questions.  
  
## 2. (p. 5): 

We need to update this: 240 trials instead of 220, and 24 problems instead of 22 (now including questions 13 and 15).

## 3. (p. 7): 

I would suggest removing the first paragraph of the hyperboloid section, 
as it is not entirely accurate, and replacing it with the following:  

```
Hyperboloid.  
  
The hyperboloid model (Green et al., 1994) generalizes the hyperbolic formulation 
by allowing for non-linear scaling in the perception of delayed rewards, 
consistent with classic psychophysical findings (Fechner, 1966). 
In this model, the subjective value of a delayed reward is given by  
  
[Equation]  
  
where $\kappa > 0$ continues to determine the rate at which value declines with delay,
while the parameter $\tau > 0$ controls the curvature of the discounting function. 
When $\tau = 1$, the model reduces to the standard hyperbolic form. 
The additional parameter provides greater flexibility in capturing observed patterns 
of intertemporal choice. The integration rule remains the difference between the values of the two alternatives:   
```

## 4. (p. 9): 

I feel more comfortable discussing $\vartheta$ in the context of interval effects. 
I propose replacing the current description with the following:  

```  
The parameter $\vartheta > 0$ governs how differences in perceived time are scaled 
and induces subadditivity in interval perception. 
Larger values of $\vartheta$ compress smaller time differences more than larger ones, 
leading to relatively lower discounting over subintervals and higher discounting over full intervals.  
```
  
## 5. (p. 18): 

On your comment regarding parameter inference and the robustness analyses, 
I am also not entirely sure how to interpret the lack of robustness. 
I agree that there seems to be something about the models that makes them difficult to use as stable mechanisms for describing intertemporal choice at the parameter level.

I wonder if this could be related to what Read mentioned:


```
As well as increasing our understanding of how attention influences intertemporal
choice, our results further support the conclusion that the behavioral discounting of a given
outcome does not arise from a fixed individual parameter such as a pure rate of time preference but
is a highly context-sensitive process (Lempert & Phelps, 2016; Reeck et al., 2017; Scholten et al.,
2024).
```


Also, I’m not sure about the Unified Tradeoff Model (2024). 
I need to look at it more carefully, but it seems to introduce additional complexity, 
and I am not sure how much it would change our main results. What do you think?

  
## 6. (p. 20): 

### Comment: 

Have previous model evaluations in this literature made this mistake? 

### Answer: 

I'm not entirely sure, Doc? 

