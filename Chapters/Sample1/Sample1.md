## Handling Grabouzes with a Zerotroner
@cha:grabouzes

_Authors:_ Prof. T. Tournesol -- Moulinsart Lab, Belgium -- tryphon.tournesol@moulinsart.be and Prof. H. Ochanomizu -- AstroLabs, Japan -- hiro.ochanomizu@astroLabs.jp

##### Abstract
Grabouzes make development tedious and error prone. 
In this article we present how our new Zerotroner handle grabouzes.
We show the challenges posed by grabouzes, present our Zerotroners. 
We present some key applications. In addition, we present the key 
insights of our Zerotroner implementation. Indeed, while modern execution engines offer
excellent execution speed they also offer the possibility to implement efficient zerotroners. 
We finish the articles by showing some result we obtained while applying our Zerotroner
to industrial code. 

### Challenges raised by grabouzes

As shown in Figure *@bluefig@*

![Pharo logo is great. %width=50&anchor=bluefig](figures/pharo.png)

### Presentat

Notice that 
- section titles are not uppercased.
- there is a period at the end of figure captions.

### Implementation insights

```anchor=list1
ZerotronerHandler >> depositron: aCol

	...
```

### Anecdotal use and evidence

As shown in {!citation|ref=Tourn23a!}, our zerotoner can handle grabouzes.


### Conclusion

While Pharo is a great language and IDE {!citation|ref=Blac09a!}, it does not handle well grabouzes. 
In this chapter, we presented how a simple Zerotroner can dramatically improve the situation.