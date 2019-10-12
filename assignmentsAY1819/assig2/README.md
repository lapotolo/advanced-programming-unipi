<body>
<div id="content">
<h1 class="title">Assignment: Haskell and Java 8 Features</h1>

<div id="outline-container-org3ab52d3" class="outline-2">
<h2 id="org3ab52d3">Instructions</h2>
<div class="outline-text-2" id="text-org3ab52d3">
<p>
This assignment is made of three parts, the third one (proposing additional exercises in Haskell) is optional.
</p>
</div>
</div>

<div id="outline-container-org9aea2d1" class="outline-2">
<h2 id="org9aea2d1">Part 1 - Functional programming in Haskell</h2>
<div class="outline-text-2" id="text-org9aea2d1">
<p>
This assignment requires you to implement a type constructor providing the functionalities of <a href="https://en.wikipedia.org/wiki/Multiset">Multisets</a>.
Your implementation must be based on the following <i>concrete</i> Haskell
definition of the <code>ListBag</code> type constructor:
</p>
<pre class="example">
data ListBag a = LB [(a, Int)]
  deriving (Show, Eq)
</pre>
<p>
Therefore a <code>ListBag</code> contains a list of pairs whose first component is the actual element of the multiset, and the second component is its <i>multiplicity</i>, that is the number of occurrences of such element in the multiset.
A <code>ListBag</code> is <b>well-formed</b> if it does not contain two pairs <code>(v, k)</code> and <code>(v', k')</code> with <code>v = v'</code>. 
</p>
</div>
</div>

<div id="outline-container-orgcece427" class="outline-2">
<h2 id="orgcece427">Exercise 1: Constructors and operations</h2>
<div class="outline-text-2" id="text-orgcece427">
<p>
The goal of this exercise is to write an implementation of multisets represented concretely as elements of the type constructor <code>ListBag</code>. 
As described below, the proposed implementation must be well documented and must pass the provided tests.
</p>

<ul class="org-ul">
<li>Using the type constructor  <code>ListBag</code> described above, implement the predicate <code>wf</code> that applied to a <code>ListBag</code> returns <code>True</code> if and only if the argument is well-formed. 
Check that the inferred type is  <code>wf :: Eq a =&gt; ListBag a -&gt; Bool</code>.</li>
</ul>

<p>
<b>Important</b>: All the operations of the present exercise that return a <code>ListBag</code> <code>bag</code> must ensure that the result is well-formed, i.e., that <code>wf bag == True</code>.
</p>

<ul class="org-ul">
<li>Implement the following constructors:
<ol class="org-ol">
<li><code>empty</code>, that returns an empty <code>ListBag</code></li>
<li><code>singleton v</code>, returning a <code>ListBag</code> containing just one occurrence of element <code>v</code></li>
<li><code>fromList lst</code>, returning a <code>ListBag</code> containing all and only the elements of <code>lst</code>, each with the right multiplicity</li>
</ol></li>

<li>Implement the following operations:
<ol class="org-ol">
<li><code>isEmpty bag</code>, returning <code>True</code> if and only if <code>bag</code> is empty</li>
<li><code>mul v bag</code>, returning the multiplicity of <code>v</code> in the <code>ListBag bag</code> if <code>v</code> is an element of <code>bag</code>, and <code>0</code> otherwise</li>
<li><code>toList bag</code>, that returns a list containing all the elements of the <code>ListBag bag</code>, each one repeated a number of times equal to its multiplicity</li>
<li><code>sumBag bag bag'</code>, returning the <code>ListBag</code> obtained by adding all the elements of <code>bag'</code> to <code>bag</code></li>
</ol></li>
</ul>

<p>
<b>Testing:</b> The attached file <code>testEx1.hs</code> contains some tests that can be used to check the correctness of the implemented functions. To run the test, it is probably necessary to install the <b>Test.HUnit</b> Haskell module. Next it is sufficient to load <code>testEx1.hs</code> in the interpreter, and execute <code>main</code>.
Some more tests will be released in the following days.
<b>Note that solutions that do not pass such tests will not be evaluated, and a revision will be requested.</b>
</p>

<p>
<b>Expected output:</b> A Haskell source file called <code>Ex1.hs</code> containing a <a href="http://learnyouahaskell.com/modules">Module (see Section "Making our own modules")</a> called <code>Ex1</code>, defining the data type <code>ListBag</code> (copy it from above) and <i>at least</i>  all the functions described above. 
The module can include other functions as well, if convenient. 
<b>Note:</b> The file has to be adequately commented, and each function definition must be preceded by its type, as inferred by the Haskell compiler.
</p>
</div>
</div>

<div id="outline-container-orgbbff0b4" class="outline-2">
<h2 id="orgbbff0b4">Exercise 2: Mapping and folding</h2>
<div class="outline-text-2" id="text-orgbbff0b4">
<p>
The goal of this exercise is to experiment with class constructors by adding some functions to the module developed for <a href="#orgcece427">Exercise 1</a>. 
</p>

<ol class="org-ol">
<li>Define an instance of the constructor class <code>Foldable</code> for the constructor  <code>ListBag</code> defined in <a href="#orgcece427">Exercise 1</a>. To this aim, choose a  minimal set of functions to be implemented,  as described in the documentation of <code>Foldable</code>. Intuitively, folding a <code>ListBag</code> with a binary function should apply the function to the elements of the multiset, ignoring the multiplicities.</li>

<li>Define a function <code>mapLB</code> that takes a function <code>f :: a -&gt; b</code> and a <code>ListBag</code> of type <code>a</code> as an argument, and returns the <code>ListBag</code> of type  <code>b</code> obtained by applying <code>f</code> to all the elements of its second argument.</li>

<li>Explain (in a comment in the same file) why it is not possible to define an instance of <code>Functor</code> for <code>ListBag</code> by providing <code>mapLB</code> as the implementation of <code>fmap</code>.</li>
</ol>

<p>
<b>Testing:</b> Also for this exercise a test file will be provided, to be used for checking the code.
</p>

<p>
<b>Expected output:</b> A Haskell source file <code>Ex2.hs</code> containing a module called <code>Ex2</code>, which imports module <code>Ex1</code>
and includes <b>only</b> the new functions defined for this exercise. 
<b>Note:</b> The file has to be adequately commented, and each function definition has to be preceded by its type, as inferred by the Haskell compiler.
</p>
</div>
</div>

<div id="outline-container-org61daf13" class="outline-2">
<h2 id="org61daf13">Part 2: Programming in Java 8 with the Stream API</h2>
<div class="outline-text-2" id="text-org61daf13">
<p>
The following exercises require using the files
<code>oscar_age_female.csv</code> and <code>oscar_age_male.csv</code> that store
information about the winners of the Oscar prize. These are text
files where each line is a record storing the following fields in
csv format:
</p>
<pre class="example">
Index, Year, Age, Name, Movie
</pre>
<p>
where the field <i>Year</i> denotes the year when the Oscar was won, and
<i>Age</i> the age the winners had when they were awarded. The other
fields are self-explanatory.
</p>
</div>
</div>

<div id="outline-container-org000e1a3" class="outline-2">
<h2 id="org000e1a3">Exercise 3: Basic structures</h2>
<div class="outline-text-2" id="text-org000e1a3">
<p>
Write a class  <code>WinnerImpl</code> that 
</p>
<ol class="org-ol">
<li>implements the simple interface <code>Winner</code> (attached) that represents a record of the winner database</li>
<li>contains the static method <code>loadData</code> that given a <code>String[]</code>  containing the paths of some winner databases returns a <code>Collection&lt;Winner&gt;</code> representing the content of the databases. Note that the first line of each file describes the format of the fields and that the values of fields <code>Name</code>  and <code>Movie</code> are enclosed in double quotes, e.g., "Coquette". Your  solution must suitably deal with these cases.</li>
</ol>

<p>
<b>Expected output:</b>  A Java source file <code>WinnerImpl.java</code> providing the required functionalities. 
<b>Note:</b> The file has to be adequately commented.
</p>
</div>
</div>

<div id="outline-container-orgfd2cd84" class="outline-2">
<h2 id="orgfd2cd84">Exercise 4: Methods using the Stream API</h2>
<div class="outline-text-2" id="text-orgfd2cd84">
<p>
Write a class <code>WinnerOperations</code> that contains the following <b>static</b> methods, which should make extensive use of the Stream API:
</p>
<ol class="org-ol">
<li>method <code>oldWinners</code> that given a <code>Stream&lt;Winner&gt;</code> returns a new <code>Stream&lt;String&gt;</code> containing the names of the  winners that are <i>older</i> than 35 <i>sorted alphabetically</i>.</li>
<li>method <code>extremeWinners</code> that given a <code>Stream&lt;Winner&gt;</code> returns a <code>Stream&lt;String&gt;</code> containing the names of all the youngest and of all the oldest winners, <i>sorted in inverse alphabetical ordering</i>.</li>
<li>method <code>multiAwardedFilm</code> that given a <code>Stream&lt;Winner&gt;</code> returns a <code>Stream&lt;String&gt;</code> containing the title of films who won two prizes. The elements of the stream must be in <i>chronological order</i>, i.e. in increasing order of year of the corresponding  records in the database.</li>
<li>method  <code>runJobs</code> that given a <code>Stream&lt;Function&lt;Stream&lt;T&gt;,Stream&lt;U&gt;&gt;&gt;</code> of jobs and a <code>Collection&lt;T&gt; coll</code> returns a <code>Stream&lt;U&gt;</code> obtained by concatenating the results of the execution of all the jobs on the data contained in <code>coll</code>.</li>
<li>method <code>main</code>, that reads the databases of Winners <code>oscar_age_female.csv</code> and <code>oscar_age_male.csv</code> using method <code>loadData</code> of <code>WinnerImpl</code>, and prints the result of invoking methods <code>oldWinners</code>, <code>extremeWinners</code> and <code>multiAwardedFilm</code> on the databases by exploiting <code>runJobs</code>.</li>
</ol>


<p>
<b>Expected output:</b>  A Java source file <code>WinnerOperations</code> providing the required functionalities. The execution of the program should print the expected result assuming that the two <code>csv</code> files are in the same directory of the <code>class</code> file.
<b>Note:</b> The file has to be adequately commented.
</p>
</div>
</div>


<div id="outline-container-org1ab7a3a" class="outline-2">
<h2 id="org1ab7a3a">Part 3 - Functional programming in Haskell [Optional]</h2>
<div class="outline-text-2" id="text-org1ab7a3a">
<p>
The next exercises build over those presented in <a href="#org9aea2d1">Part 1</a>. 
</p>
</div>
</div>

<div id="outline-container-orgd379c63" class="outline-2">
<h2 id="orgd379c63">Exercise 5: Multisets as Monad [Optional]</h2>
<div class="outline-text-2" id="text-orgd379c63">
<p>
The goal of this exercise is to understand how multisets can be equipped with a monadic structure. The resulting monad represents conceptully both a container (a bag of distinct elements, each one with its multiplicity), and 
a computational effect (a function <code>a -&gt; ListBag b</code>  is a non-deterministic function returning the multiset of possible results). 
</p>

<ol class="org-ol">
<li>Define the operations  <code>returnLB</code> and <code>bindLB</code>, taking inspiration from the operations of the <code>Monad</code> type constructor.</li>
<li>Try to define an instance of <code>Monad</code> for <code>ListBag</code> using the functions just defined. Discuss whether this is possible or not, and if not what conditions have to be released in order to obtain an instance of <code>Monad</code></li>
<li>Write some tests for the functions just implemented.</li>
</ol>

<p>
<b>Expected output:</b>  A Haskell source file <code>Ex5.hs</code> containing a module called <code>Ex5</code>, which imports modules <code>Ex1</code> and <code>Ex2</code>,
and includes <b>only</b> the new functions defined for this exercise, the requested comments, and the tests. 
<b>Note:</b> The file has to be adequately commented, and each function definition has to be preceded by its type, as inferred by the Haskell compiler.
</p>
</div>
</div>

<div id="outline-container-org181274c" class="outline-2">
<h2 id="org181274c">Exercise 6: Abstract Data Type [Optional]</h2>
<div class="outline-text-2" id="text-org181274c">
<p>
Consider abstract data types as defined <a href="https://wiki.haskell.org/Abstract_data_type">here</a>, and in particular in Section 3.1. 
The goal of this exercise is to re-engineer the definition of <code>ListBag</code> of <a href="#orgcece427">Exercise 1</a> and <a href="#orgbbff0b4">Exercise 2</a> as specific instances of an Abstract Data Type Constructor called <code>MultiSet</code>.
</p>
<ol class="org-ol">
<li>Define a new constructor class <code>MultiSet</code> defining an abstract data type with the same constructors and operations as <code>ListBag</code></li>
<li>Make <code>ListBag</code> an instance of <code>MultiSet</code></li>
<li>Provide a second, different instance of <code>MultiSet</code>, by either exploiting a new concrete representation of multisets or by reusing some data structure provided by the Haskell API.</li>
<li>Write a simple function manipulating <code>Multiset</code>, and show that it can be applied to both implementations.</li>
</ol>

<p>
<b>Expected output:</b>  A Haskell source file <code>Ex6.hs</code> containing a module called <code>Ex6</code>, which imports modules <code>Ex1</code> and <code>Ex2</code>,
and includes <b>only</b> the new definitions developed for this exercise.
<b>Note:</b> The file has to be adequately commented, and each function definition has to be preceded by its type, as inferred by the Haskell compiler.
</p>
</div>
</div>
</div>
