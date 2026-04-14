# Comments 2

It took me some time to understand the Unified Tradeoff Model. They get very specific about intertemporal choices, but mainly on losses. I also have to acknowledge that they have a huge set of participants — I guess they do have money, ha.

I agree with Dr. Arturo about trying to publish the article, and that we should definitely test the UTM.

First, I revisited the equations that Arturo commented on and they look fine to me (equation 11):

$$v_{ll}=\frac{1}{\lambda} \log (1+ \lambda(r_{ll} − c))$$

$$v_{ss}=\frac{1}{\lambda} \log (1+ \lambda(r_{ss}))$$

I think it is important to mention how time bias ($c$) works conceptually:

1. It assumes that, just by the fact that you have to wait for something, there is a penalty applied to the later reward. This penalty does not depend on how long you have to wait — it is a fixed cost for waiting at all. That's why it is integrated into the value of the later reward and not into the time-weighing function. According to them, it captures a *first reaction* of impatience ("oh no, I have to wait!") that is distinct from the time preference, which scales with the length of the delay ("...and I have to wait a whole year!").
2. The explanation of subadditivity under the UTM follows directly from this: when you split an interval into subintervals, you mentally "pay" this fixed penalty multiple times (once per subinterval), whereas over the undivided interval you pay it only once. So delay aversion accumulates more over divided intervals than over undivided ones, producing subadditivity. This is the mechanism the UTM uses to explain subadditivity in gains.


Then, the other parts of the model go as Arturo mentioned (equations 12, 13, 14):

$$w_{ll} = \frac{1}{\tau} \log(1+\tau t_{ll})$$

$$w_{ss} = \frac{1}{\tau} \log(1+\tau t_{ss})$$

$$T = w_{ll} - w_{ss}$$

$$Q(T) = \kappa * T$$

$$\Delta_{utm} = (v_{ll} - v_{ss}) - Q(T)$$

As you can see, the $\vartheta$ parameter from our original Tradeoff implementation is gone. In the UTM, $\vartheta$ (subthreshold hyposensitivity) is the mechanism the model uses to generate superadditivity, which according to them appears only when subintervals are very short. Since our participants do not show superadditivity, I agree with the idea of dropping $\vartheta$ from the UTM implementation ($Q(T)=\kappa \frac{T}{\vartheta}$, equation 25 in the article). 

So, according to this, the final UTC should be implemented as:

```
  # UNIFIED TRADEOFF MODEL (UTM)
  sigmaUT  ~ dlnorm(0, precision)
  gammaUT ~ dlnorm(0, precision)
  tauUT ~ dlnorm(0, precision)
  kappaUT ~ dlnorm(0, precision)
  cUT ~ dlnorm(log(100), 1) T(0, 1000)  # time bias
  for (j in 1:nTrials){
    valueLLUT[j] = 1/gammaUT*log(1 + gammaUT*(rLL[j] - cUT))        
    valueSSUT[j] = 1/gammaUT*log(1 + gammaUT*rSS[j])
    weightLLUT[j] = 1/tauUT*log(1 + tauUT*tLL[j])
    weightSSUT[j] = 1/tauUT*log(1 + tauUT*tSS[j])
    qValueUT[j] = valueLLUT[j] - valueSSUT[j]
    qTimeUT[j] = kappaUT*(weightLLUT[j] - weightSSUT[j])
    theta[j, 11] = phi((qValueUT[j] - qTimeUT[j])/sigmaUT)
  }
```


Now, about the prior for `cUT`, there are things we need to consider, especially in light of what Michael commented on prior sensitivity.

First, the model requires $r_{ll} > c$ for the penalty to make sense. Given than the smallest amount in $rll$ is 1250, I suggest truncating the prior so it doesn't go beyond something around 1000.  

Second, as I understand, $c$ is expressed in the same monetary units as the rewards, so very small values of the penalty are not very plausible either. That's why I proposed centering the prior in larger magnitudes (100). 

That said, I'm not confident this is the right choice. The article doesn't discuss what a plausible magnitude for the time bias looks like, and the authors don't actually estimate the UTM parameters anywhere in the paper, all their analyses are qualitative and based on directional predictions about preference patterns. They explicitly acknowledge this (and I think this is helpful for us): 

> "We have restricted ourselves to a qualitative analysis of intertemporal choice, using scope as the criterion of model validity... That said, the unified tradeoff model awaits specification as a computational model, and validation by criteria that are proper to quantitative analyses."

So we might actually be among the first to set up a quantitative specification of this model, as well as the prior of $c$, which is interesting per se, and it connects directly with Michael's point about the importance of priors in intertemporal choice modeling. I think it would be worth seeing how sensitive the UTM's performance is to this experiment. I'm excited about this, hehe. 

Finally, I agree with the other things that Arturo commented on, about the choice rule, the references, and his final comment.


