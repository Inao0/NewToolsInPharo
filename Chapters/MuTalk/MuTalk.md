## MuTalk : A Mutation Testing Framework


_Authors:_ Iona Thomas -- Univ. Lille, Inria, CNRS, Centrale Lille, UMR 9189 CRIStAL, Lille, France -- iona.thomas@inria.fr and Pol Durieux -- Univ. Lille, Inria, CNRS, Centrale Lille, UMR 9189 CRIStAL, Lille, France -- pol.durieux@inria.fr

Mutation testing is a way of measuring the quality and completeness of a project's test suite. While code coverage is used to make sure that tests execute each method or line of code, mutation testing evaluates the ability to detect new errors. It involves introducing errors and running the tests to see if they are detected. MuTalk is a flexible mutation framework that provides good default values for analyzing a test suite. It can be customized to fit specific domains or projects.
In this chapter we will discuss :
- 1. Introduction to Mutation Testing
- 2. How to use MuTalk / Quick Start
- 3. Available options in MuTalk
- 4. Mutation analysis : variants on mutation testing (TO CHECK)

### Introduction to Mutation Testing

#### Principles of Mutation Testing and Vocabulary

Mutation testing allows one to measure the quality and completeness of a project's test suite. This is done by introducing errors, known as **mutations**, into the source code of a project, for example in a method. This modified method is called a **mutant**. The version of the code containing a mutant is called the **mutated code**. The idea behind mutations is to simulate real mistakes that developers may make.

![Mutation vocabulary](./figures/Mutant.png)


Then the tests are run for each mutant to see if they are able to detect it. There are two possible scenarios:
* if the tests are green, the mutants were not detected. They are said to have **survived**.
* if the tests are red, the mutants have been detected. They are said to have been **killed**.

![Two possible results for each mutants](./figures/Test.png)

A common observation when performing mutation testing on a project is that certain use cases have not been tested.


#### An Example

Let's look at the following example method from the class `MyVehicle` :
```smalltalk
hasFourWheels:
^ self numberOfWheels = 4
```

Two different examples of mutations are changing the number of wheels and changing the comparison sign. This could result in the following mutants :

- Decreasing a "4" into a "3"
```smalltalk
"Mutant on the number of wheels"
hasFourWheels:
^ self numberOfWheels = 3
```

- Turning a "=" into a ">="
```smalltalk
"Mutant on comparison sign"
hasFourWheels:
^ self numberOfWheels >= 4
```

Let's imagine that we have only the following test :
```smalltalk
testHasFourWheels
 "Create a vehicule object with 4 wheels"
 | aVehicle |
 aVehicle := MyVehicle newWithWheels: 4.
 self assert: aVehicle hasFourWheels
```
Mutants are introduced one at a time. The mutant on the number of wheels will cause this test to fail. This mutant has been killed.
The mutant on the comparison sign will pass the test. This mutant has survived.


#### The Mutation Score : a Test Suite Metric.

The ideal case for a test suite is to kill all mutants, since the tests are designed to detect errors in the code. If mutants survive, this means that if a real error is introduced into the project's source code during a development phase, it may not be detected before deployment and cause bugs later on.

The percentage of mutants killed is summarized with a mutation score, defined as:

$$ %label=mutationScore
Mutation\:score = \frac{Number\:of\:killed\:mutants}{Total\:number\:of\:mutants}
$$


### How to Use MuTalk

MuTalk is Pharo's mutation testing library. It can be found on [GitHub](https://github.com/pharo-contributions/mutalk) {!footnote | https://github.com/pharo-contributions/mutalk !} and is loaded using Metacello (See Quick Start (*@quickStart@*) for code snippet). It allows you to perform mutation testing on a set of Pharo classes or on a set of packages.

#### The 4 Steps of MuTalk Analysis

MuTalk mutation testing analysis works with two main elements: the source code to mutate and the tests that go with it.  
Once these are provided, the analysis then goes through 4 phases:
* [An initial test run](#Initial-test-run)
* [A coverage analysis](#Coverage-analysis)
* [The generation of mutants](#Mutant-generation)
* [The evaluation of mutants, also called the results generation](#Evaluation-of-mutants)  

##### Initial Test Run

The analysis will run every tests it was given. If a test fails, the analysis stops and informs the user that they should fix the tests. To avoid this, check the red tests filter (*@redTestFilter@*).

##### Coverage Analysis

This phase is a regular coverage analysis used for test selection and mutant selection strategies. It makes the following analysis to be more efficient.

##### Mutant Generation

Here the analysis generates the mutants of the source code using its mutant operators.

##### Evaluation of Mutants

Finally, the analysis takes the mutants and applies them one by one. It installs a mutant and runs each selected test: this is the mutant evaluation. The evaluation stops at the first failing test or when every test has been run. Then the mutant is uninstalled, and it moves on to the next mutant.


#### Quick Start
@quickStart

##### Loading MuTalk
Latest release (2.6.0):
```Smalltalk
Metacello new
  baseline: 'MuTalk';
  repository: 'github://pharo-contributions/mutalk:v2.6.0/src';
  load.
```
Latest version (unstable):
```Smalltalk
Metacello new
  baseline: 'MuTalk';
  repository: 'github://pharo-contributions/mutalk/src';
  load.
```

##### Running an Analysis

To begin with, you need to create a mutation testing analysis with the *MTAnalysis* class:
```smalltalk
analysis := MTAnalysis new.
```

In order to work, MuTalk needs the classes or packages to be mutated, as well as the tests that go with them, because as said before, the purpose of mutation testing is to test tests.  
To provide a list of classes:
```smalltalk
analysis classesToMutate: { MyVehicle }.
analysis testClasses: { MyVehicleTest }.
```
or to provide a list of packages:
```smalltalk
analysis packagesToMutate: { 'MuTalk-Examples' }.
analysis testPackages: { 'MuTalk-Examples-Tests' }.
```
MuTalk also has a number of configurable options, but these have default values. So you can run the analysis as it is and get the results:
```smalltalk
analysis run.
analysis generalResult inspect
```


### Analysis Options in MuTalk
MuTalk offers several options for configuring the analysis and good default values. They allow you to select the kind of mutation to apply, which methods to mutate, to allocate budgets for the analysis, to filter the tests to use and to log. This section goes into detail about each of these options. If you are not interested in the details of these options, you can skip ahead to the next section on [exploring the results](#exploring-the-results)


#### Choosing the Type of Mutations

Mutation operators are objects that define a type of mutation and how to perform it. For example, there's a mutation operator that changes a `+` into a `-`. In MuTalk, they all have an *Operators* tag and are subclasses of `MTAbstractMutantOperator`. By default, all of them are used, but if you want to use only some of them, proceed as follows *(don't forget to give actual instances of operators, not just the class)*:
```smalltalk
analysis operators: { MTRemoveCaretOperator new. MTReplaceDivisionWithMultiplicationMutantOperator new. MTReplaceIfFalseReceiverWithTrueOperator new }.
``` 

There are many operators, available by default in MuTalk. The table below provides a list of what the current operators do. If these operators are not sufficient and/or if you require domains specific mutations, you can add operators by subclassing ```MTAbstractMutantOperator``` and adding them as operators to the analysis.

![](./figures/Operators%20tab1.pdf)

![](./figures/Operators%20tab2.pdf)

#### Selection of Methods to Be Mutated

Mutant generation strategies are ways of choosing which methods to mutate. Each strategy is implemented as a subclass of `MTMutantGenerationStrategy`. They are tagged *Mutant generation strategies* in the `MuTalk-Model` package and are used as follows:  
```smalltalk
analysis mutantGenerationStrategy: myMutantGenerationStrategy.
```
##### `MTAllMutantGenerationStrategy`  
The default strategy is to mutate all methods of the provided classes.
```smalltalk
myMutantGenerationStrategy := MTAllMutantGenerationStrategy new.
```

##### `MTSelectingFromCoverageMutantGenerationStrategy`  
Another strategy is to mutate only the methods that are covered by tests to save execution time.
```smalltalk
myMutantGenerationStrategy := MTSelectingFromCoverageMutantGenerationStrategy new.
```

##### `MTManualMutatedMethodGenerationStrategy`  
This strategy is based on a manual selection of the methods to be mutated, i.e. providing the mutant selection strategy with a collection of methods. The mutations will then be applied to these methods and not to the methods specified during analysis creation.  
To use this strategy:
```smalltalk
myMutantGenerationStrategy := MTManualMutatedMethodGenerationStrategy new targetMethods: { MyVehicle>>#numberOfWheels: . MyVehicle>>#hasFourWheels }.
```


#### Budgeted Analysis
@budgets

Execution budgets define limits for the analysis, such as an execution time limit. In MuTalk, they have the tag *Budgets* and are used as follows:
```smalltalk
analysis budget: myBudget.
```

##### `MTTimeBudget`  
The default budget sets a time limit for the analysis. After this time, the analysis stops.  
To use it, you specify a duration, for example:
```smalltalk
myBudget := MTTimeBudget for: 2 minutes.
```
##### `MTFreeBudget`  
This budget does not limit the analysis.
```smalltalk
myBudget := MTFreeBudget new.
```

##### `MTFixedNumberOfMutantsBudget` and `MTPercentageOfMutantsBudget`.  
These budgets impose a limit on the number of mutants analyzed, respectively either in absolute number of mutants or in percentage of mutants.  
To use them, you specify a number of mutants or a percentage, as follows:
```smalltalk
myBudget := MTFixedNumberOfMutantsBudget for: 100.
“or“
myBudget := MTPercentageOfMutantsBudget for: 50.
```

It is interesting to note that it is not necessary to evaluate all mutants to get a general idea of the mutation score of some classes.

For example, this is a graph of the mutation score as a function of the percentage of mutants evaluated.
![Boxplots showing the mutation score distribution for each percentage of mutants evaluated over the total number of mutants available. Mutation analysis were repeated 10 times for each percentage of mutants](./figures/Score%20percent%20graph.png)
For each percent, an analysis was run 10 times with a simple random mutant selection strategy, and the mutation score of each analysis was calculated. Then a boxplot was drawn with these 10 scores. 

It shows that although the lower the percentage is the higher the variance is, the median is still relatively close to the mutation score when all mutants are evaluated, i.e. at 100%.

#### Selection of Mutants to Be Evaluated for Budgeted Analysis

When running an analysis with a budget (*@budgets@*), not all mutants can be evaluated. In this context it is important to think about which mutants will be selected for the analysis, and not to lose too much information. Even though there is no way to measure the value of a mutant in an analysis in MuTalk yet, a basic assumption is that it is important to have diversity.  
Mutant selection strategies define which mutants are used for analysis, and in which order. They are attributes of mutant generation strategies. In MuTalk, they have the tag *Mutant selection strategies* and are used as follows:
```smalltalk
analysis mutantSelectionStrategy: myMutantSelectionStrategy.
```

##### `MTRandomClassMutantSelectionStrategy`, `MTRandomMethodMutantSelectionStrategy` and `MTRandomOperatorMutantSelectionStrategy`  
These strategies randomly shuffle mutants, but in a specific way. They respectively randomly select a class, method or mutation operator, and then select a mutant from that class, method or operator. Operator selection is MuTalk's default strategy.  
These strategies are particularly useful when reducing the number of mutants analyzed with budgets (*@budgets@*). They allow classes/methods/operators that produce few mutants to still be represented in the final results when the number of mutants is reduced.
```smalltalk
myMutantSelectionStrategy := MTRandomClassMutantSelectionStrategy new.
“or"
myMutantSelectionStrategy := MTRandomMethodMutantSelectionStrategy new.
"or"
myMutantSelectionStrategy := MTRandomOperatorMutantSelectionStrategy new.
```


![Process of specific random selections](./figures/Random2.png)
Let's say there are four operators: A, B, C and D, and each of them produces some mutants. A produces half of the total mutants, D produces very few of them, while B and C produce the rest. For the purpose of the example we will assume that all the operators still generate a lot of mutants, just that the representation in the total number of generated mutants are different.  
With a mutant selection that chooses randomly and without bias a mutant for the analysis, the final selection would look like this:
![Expected mutant distribution among 4 operators with basic random selection. This distribution in fact matches the actual distribution %width=50](./figures/Distrib.png)  
Here, A is overrepresented in the final results, while D is barely present. However an operator is not necessarily less meaningful in the analysis because 
![Expected mutant distribution among 4 operators with random operator selection (if there are enough mutants for each operator to select the same amount for each of them) %width=50](./figures/Distrib%202.png)

In this example, a random operator selection strategy ensures that operator D is almost certainly present in the results, which is not guaranteed with conventional random selection. 

##### `MTRandomMutantSelectionStrategy`  
This strategy is to randomly and indiscriminately shuffle all mutants.
```smalltalk
myMutantSelectionStrategy := MTRandomMutantSelectionStrategy new.
```

##### `MTMutantSelectionStrategy`  
This strategy consists simply of not shuffling the list of mutants and returning it as it is. This should NOT be used with a budgeted analysis as it will result in a very biased selection.
```smalltalk
myMutantSelectionStrategy := MTMutantSelectionStrategy new.
```


#### General Test Filters
@testFilters

Test filters allow you to block certain tests for the entire analysis according to certain criteria. These blocked tests will then not be executed during mutant evaluation. Filters are tagged as *Test filters* in MuTalk and are used as follows:
```smalltalk
analysis testFilter: myTestFilter.
```

##### `MTCompositeTestFilter`  
This is the default test filter used by the analysis, used with a red test filter (to exclude failing tests) and a time test filter (to exclude tests that are too long to run).
This test filter works as a combination of multiple test filters. It has a collection of test filters as an attribute, and the test collection passes through each filter. A test is blocked by the composite if any of its filters blocks the test, and it passes through the composite if it passes through all of its filters.  
To use it:
```smalltalk
myTestFilter := MTCompositeTestFilter for: { MTRedTestFilter new . MTTimeTestFilter for: 2 seconds }.
```

##### `MTFreeTestFilter`  
This filter lets everything through.
```smalltalk
myTestFilter := MTFreeTestFilter new.
```

##### `MTBlockTestFilter`  
This filter only lets through tests that respect the condition of the block it receives.  
It is used in this way, for example:
```smalltalk
myTestFilter := MTBlockTestFilter for: [ :testCase | testCase selector endsWith: 'test' ].
```

##### `MTPragmaSelectionTestFilter`  
This filter uses Pharo's pragmas as a condition for filtering tests. It only keeps tests that contain a given pragma.  
To use this filter:
```smalltalk
myTestFilter := MTPragmaSelectionTestFilter for: #aPragma.
```

##### `MTPragmaRejectionTestFilter`  
This filter works similarly to the previous one, but instead it blocks tests that contain the given pragma.  
To use it:
```smalltalk
myTestFilter := MTPragmaRejectionTestFilter for: #aPragma.
```

##### `MTRedTestFilter`
@redTestFilter

This filter blocks tests that fail, i.e. that are red. For MuTalk it is important that all tests are green before applying the mutations, because when it evaluates the mutants it is the red tests that will tell whether they are killed or not, hence the usefulness of this filter.
```smalltalk
myTestFilter := MTRedTestFilter new.
```


##### `MTTimeTestFilter`  
This filter blocks tests that take longer than a given time to run. If the duration is chosen well, it can eliminate tests that are too time-consuming.  
This is how it is used:
```smalltalk
myTestFilter := MTTimeTestFilter for: 2 seconds.
```


#### Per Mutant Test Selection

Test selection strategies are ways of choosing which tests to run for each mutant evaluation. In MuTalk, they have the tag *Test selection strategies* and are used as follows:
```smalltalk
analysis testSelectionStrategy: myTestSelectionStrategy.
```
##### `MTSelectingFromCoverageTestSelectionStrategy`  
The default is to select only tests that cover mutants, to speed up the analysis. In other words when evaluating a mutant, if a test does not use the mutated method, it will not be run for this evaluation.
```smalltalk
myTestSelectionStrategy := MTSelectingFromCoverageTestSelectionStrategy new.
```

##### `MTAllTestsMethodsRunningTestSelectionStrategy`  
Another more basic strategy is to run all tests all the time, but this is more time-consuming.
```smalltalk
myTestSelectionStrategy := MTAllTestsMethodsRunningTestSelectionStrategy new.
```

#### Loggers

Loggers provide traces of analysis execution. Depending on the logger used, this trace can be found in different places. In MuTalk, they have the *Logging* tag and are used as follows:
```smalltalk
analysis logger: myLogger.
```

##### `MTProgressBarLogger`  
This logger is selected by default for any analysis. It uses a progress bar to display the progress of the analysis in real time.
```smalltalk
myLogger := MTProgressBarLogger new.
```

##### `MTNullLogger`  
This logger logs nothing.
```smalltalk
myLogger := MTNullLogger new.
```

##### `MTFileLogger`  
This logger writes the trace to a file whose name must be provided. The logger creates the file.  
To use it:
```smalltalk
myLogger := MTFileLogger toFileNamed: 'log.txt'.
```

##### `MTTranscriptLogger`  
This logger writes the trace in the Pharo's Transcript.
```smalltalk
myLogger := MTTranscriptLogger new.
```


### Handling Core Classes

Mutation testing modifies the classes under analysis. This is fine if we are studying an application with its own specific classes. However, once we want to analyse classes that are part of the core of the Pharo system, it gets more complicated. Imagine introducing a bug into the UI of all windows with a mutation. Or into the definition of `Object` or `Class`. These mutations can crash your Pharo instance.

One solution to run mutation testing on such classes is to create a copy of these classes and their tests. This copy can be mutated without it impacting the stability of  the system. Pharo comes with a tools to help you copy packages and identify dependencies : package duplication with regex to replace class names and the `Dependencies browser`. The package duplication modify the classes in the copy to reference each other instead of referencing the original package. The dependencies browser allows one to check what are the remaining dependencies after the duplication.

Let's take the example of `Color`, which is used for all the UI. For this example we will temporarily move the test class of `Color` into the `Colors` package. With them together, we can duplicate both at the same time, allowing the duplication to rewrite references to `Color` into the tests to `MyColor`, the duplicated version.

![Duplicating the Colors package](./figures/ColorPackageDuplication.png)

Using a regular expression, we copy all the classes and we prefix their duplicated name with `My` to differentiate them. 

![Renaming classes with a regex](./figures/RegexDuplication.png)

Once the classes are duplicated, the `MyColor` class needs to be initialized. Once this is done, the 16 tests of `MyColorTest` are green. 

![Initalising the MyColor Class](./figures/ColorClassInitialisation.png)

At this point, we can check that we have no remaining dependencies on the original `Colors` package by right-clicking on the package name and selecting `Browse dependencies`. We can check in the picture below that in the left most column we do not see the `Colors` package. 

![Dependencies browser for MyColors package](./figures/MyColorsDependenciesBrowser.png)

We can now run a mutation analysis on our duplicated version of `Colors`:

```Smalltalk
analysis := MTAnalysis new.
analysis classesToMutate: { MyColor }.
analysis testClasses: { MyColorTest }.
analysis run.
analysis generalResult inspect
```

However, even these tools have some limitations. For example, implicit references such as pseudo variables or creating objects with literals won't be caught. For example, most tests for booleans use the `true` and `false` pseudo-variables. These make the reference to the `Boolean` class implicit, and it won't be rewritten to point to the new class boolean class when duplicating these. Using a literal notation in tests like `#( 1 2 7)` won't be rewritten when trying to duplicate `Array` for mutation testing. Finally, some syntactic sugar, such as sending a message to a first class to create an instance of a second one, also blinds the rewritting during duplication. When duplicating `Point` and its tests, tests that use the `@` message to create points (e.g.: 1@6 creates a point where x is 1 and y is 6) are not rewritten.

Studying such core classes requires more work to create an accurate copy before running the mutation analysis on it.


### Exploring the Results

Once an analysis is run, we get a `MTGeneralResult` object which comes with its specific inspector pannels. MuTalk also includes tools that use the results of a mutation testing analysis to extract data.
In this section, we go over how to understand the results of the analysis.

#### Example of Analysis

Here is an example of a mutation testing analysis using the example classes provided. Let's look at the results.
```smalltalk
analysis := MTAnalysis new
	            classesToMutate: { MyVehicle };
	            testClasses: { MyVehicleTest }.
"parametrization goes here"
analysis run.
analysis generalResult inspect
```

#### The Result Object

![Inspector on generalResult, showing the "Killed mutants" tab. The "Surviving mutants" and "Terminated mutants" tabs look the same. Under the tab there is the list of mutants with their full name. Under it, there is a comparison between the original method and the mutant. At the bottom there is the usual inspector playground.](./figures/Inspector.png)

This is the inspector on the result object of the analysis. From top to bottom :
* The numbers of evaluated, killed, surviving and terminated mutants are displayed at the top of the window.
* The inspector contains 4 more tabs than the normal inspector that are specific to the mutation analysis:
    - The `Surviving Mutants` tab lists all the mutants that survived.
    - The `Killed Mutants` tab lists all the mutants that were killed.
    - The `Terminated Mutants` tab lists the mutants for which there was an issue while performing the installation or uninstallation during the evaluation.
    - The `Excluded Tests` tab lists the tests that were rejected by the test filter (*@testFilters@*) and why they were rejected.
* For the mutants tabs, the inspector displays a list of mutants with their names. This includes the type of mutation applied, and the mutated class and method (e.g.: `Increase a literal integer in MyVehicle class>>#newSimpleCar`). When clicking on a mutant, the original code of the method is displayed on the left and the mutated code on the right. Differences are highlighted in green and red. On the image below, you can see that a number was decreased by the mutation from 4 to 3.

By looking at the first tab about surviving mutants, you can see which cases should be targeted by new tests to improve the test suite.


#### Mutation Matrix: a Visual Representation

The mutation matrix is a matrix that represents the results of tests for each mutant. It shows exactly which test killed which mutant. This can be useful for further analysis of the test suite.

There are several ways to use it:
* with a collection of classes, assuming the associated test classes have the same names with the “Test” suffix:
    ```smalltalk
    matrix := MTMatrix forClasses: { MyVehicle }
    ```
* with a collection of classes and a collection of test classes:
    ```smalltalk
    matrix := MTMatrix forClasses: { MyVehicle } andTests: { MyVehicleTest }
    ```
* with a collection of packages, assuming that the corresponding test packages have the same names with the suffix “-Tests”:
    ```smalltalk
    matrix := MTMatrix forPackages: { 'MuTalk-Examples' }
    ```
* with a collection of packages and a collection of test packages:
    ```smalltalk
    matrix := MTMatrix forPackages: { 'MuTalk-Examples' } andTestPackages: { 'MuTalk-Examples-Tests' }
    ```

Once created, the matrix must be created with `build` to run the corresponding mutation analysis. Then it can be displayed with `generateMatrix`. Here is the snippet to analyze the `MyVehicle` example class:

```smalltalk
matrix := MTMatrix forClasses: { MyVehicle }.
matrix build.
matrix generateMatrix.
```
Here are the first few lines of a matrix built from the analysis of `MyVehicle`:

![Matrix of MyVehicle](./figures/MyVehicleMatrixCropped.pdf)

Each row represents a mutant, and each column represents a test. A blue cell means that the mutant is killed by the test.
In this matrix, the mutation that increased a literal integer in MyVehicle>>#hasFourWheels was killed by both #testHasFourWheels and #testSimpleCarHasFourWheels. 


A heatmap can also be generated. It groups tests by class and mutants by mutation operator, class, method, or any other grouping condition specified as a block, to better summarize the data.  
The heatmap can be displayed as follows, when mutants are grouped by method:
```smalltalk
matrix generateHeatmapByMethod
```

This is also the heatmap built from the analysis of `MyVehicle`, but a version that groups mutants by mutation operators:
![Heatmap of MyVehicle](./figures/MyVehicleHeatmapCropped.pdf)  
Here the number in each cell is the percentage of tests in the test class that killed the mutants of each type of operator used in the analysis.

It is now possible to get several pieces of information:

##### Trivial Mutants
Trivial mutants are mutants that are killed by all tests:
```smalltalk
matrix trivialMutants inspect
```
Since these mutants are killed by all tests, we can assume that the real errors they represent will always be detected in real-life situations. This means that these mutants may not be necessary for the data provided.

##### Equivalent Mutants
Equivalent mutants are mutants that are killed by the same tests:
```smalltalk
matrix equivalentMutants inspect
```
This results in several groups of mutants, each containing mutants that are equivalent to each other.  
If two mutants are equivalent, this may mean that they are redundant from the point of view of the tests. In other words, they have the same “profile” with respect to the tests, so one of them can be eliminated.

##### Included Mutants
A mutant A is included in a mutant B if it is killed by a subset of the test set that kills B. In other words, if B is killed by tests 1, 2 and 3, and A is killed by tests 1 and 3, then A is included in B. To get them:
```smalltalk
matrix includedMutants inspect
```
A dictionary is returned, where the keys are each mutant and the values are the mutants included in the key mutant.  
If one mutant is included in another, we can also say here that there is redundancy and that it's better to keep the “bigger” mutant (in the sense of “killed by a greater number of tests”).

#### Mutation Operator Analysis 

Mutation operator analysis is used to find out how many mutants Mutalk's mutation operator set generates on given classes or packages. In other words, you can find out which operators produce at least a certain number of mutants, and which produce at most a certain number of mutants.  
To use it:
```smalltalk
operatorAnalysis := MTMutantOperatorAnalysis forClasses: { MyVehicle }
```
or:
```smalltalk
operatorAnalysis := MTMutantOperatorAnalysis forPackages: { 'MuTalk-Examples' }
```
and then:
```smalltalk
operatorAnalysis operatorsProducingAtMost: 2
```
and:
```smalltalk
operatorAnalysis operatorsProducingAtLeast: 10
```

![Results of a mutation operator analysis run on MyVehicle. Here are the operators that produced at least 3 mutants %width=75](./figures/Operator%20Analysis.png)

#### Non-Mutated Methods

The analysis of non-mutated methods allows you to find methods on which MuTalk was unable to apply mutations, i.e. methods whose body does not contain any code corresponding to the application domains of the mutation operators.  
This analysis also applies to classes or packages:
```smalltalk
analysis := MTNonMutatedMethodsAnalysis forClasses: { MyVehicle }
```
or:
```smalltalk
analysis := MTNonMutatedMethodsAnalysis forPackages: { 'MuTalk-Examples' }
```
Finally, to have the methods without mutation:
```smalltalk
analysis methodsWithoutMutation inspect
```

### Conclusion
In this chapter we presented Mutalk, a mutation analysis framework. We explained the principle of mutation testing and its original goal: the evaluation of a test suite. We provided examples and documentation on how to run and parameterize such an analysis. We presented the default mutations and how to optimize analysis's run time with budgets, especially for use cases such as continuous integration.

However, the use of this framework is not limited to mutation testing. Mutation analysis uses the mutation testing principle of modifying the code a little, running the tests, and collecting the results. However, mutation analysis can also be used to investigate issues other than the quality of the test suite. Assuming the test suite is good enough, one can use mutation analysis and custom mutation to study the use of reflection (TODO) or to test a refactoring framework for example (TODO)).



